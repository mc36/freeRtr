Type memoryType=array[0..1] of byte;
Var
  memoryData:^memoryType;
  memorySize:LongInt;
Var
  regs_d:array[0..7] of LongInt;
  regs_s:array[0..7] of LongInt;
  reg_cr0,reg_flg,reg_eip:LongInt;
Const
  reg_es=0;
  reg_cs=1;
  reg_ss=2;
  reg_ds=3;
  reg_fs=4;
  reg_gs=5;
  reg_eax=0;
  reg_ecx=1;
  reg_edx=2;
  reg_ebx=3;
  reg_esp=4;
  reg_ebp=5;
  reg_esi=6;
  reg_edi=7;
  reg_al=0;
  reg_cl=1;
  reg_dl=2;
  reg_bl=3;
  reg_ah=4;
  reg_ch=5;
  reg_dh=6;
  reg_bh=7;



Procedure emulateRestart;
Var i:Longint;
Begin;
for i:=0 to 7 do begin;
  regs_d[i]:=0;
  regs_s[i]:=0;
  end;
reg_cr0:=1;
reg_flg:=$213202;
reg_eip:=0;
regs_d[reg_esp]:=memorySize;
End;

Procedure emulateSetupUse16;
Begin;
reg_cr0:=reg_cr0 and $fffffffe;
End;

Procedure emulateSetupUse32;
Begin;
reg_cr0:=reg_cr0 or 1;
End;


Function emulateOneOpcode:LongInt;
Label op0F,ok,vege;
Const
  bitVals:array[-1..32] of LongInt=(
    0,
    $1,$2,$4,$8,$10,$20,$40,$80,$100,$200,$400,$800,$1000,$2000,$4000,$8000,
    $10000,$20000,$40000,$80000,$100000,$200000,$400000,$800000,
    $1000000,$2000000,$4000000,$8000000,$10000000,$20000000,$40000000,$80000000,
    0);
  typ_rg=1;
  typ_ad=2;
Var
  result:LongInt;
  extAdr:Boolean;
  extSiz:Boolean;
  extLck:Boolean;
  protMode:Boolean;
  extSeg:LongInt;
  extRep:LongInt; {0=none, 1=rep, 2=repne}
  IntSiz:LongInt;
  a:String;


Function checkRange(p:LongInt):Boolean;
Begin;
if (p>=memorySize) or (p<0) then begin;
  result:=2;
  checkRange:=True;
  exit;
  end;
checkRange:=False;
End;

Function getCodeInt(siz:LongInt;sig:Boolean):LongInt;
Var i,o:LongInt;
Begin;
o:=(regs_s[reg_cs] shl 4)+reg_eip;
case siz of
  1:begin;
    i:=memoryData^[o];
    inc(reg_eip);
    if sig then i:=shortint(i);
    end;
  2:begin;
    i:=ReadWordLSB(memoryData^[o]);
    inc(reg_eip,2);
    if sig then i:=integer(i);
    end;
  4:begin;
    i:=ReadLongLSB(memoryData^[o]);
    inc(reg_eip,4);
    end;
  else i:=0;
  end;
getCodeInt:=i;
End;


Function readReg8(num:LongInt;sign:Boolean):LongInt;
Var i:LongInt;
Begin;
i:=regs_d[num and 3];
if (num and 4<>0) then i:=i shr 8;
i:=i and $ff;
if sign then i:=shortint(i);
readReg8:=i;
End;

Function readReg16(num:LongInt;sign,dw:Boolean):LongInt;
Var i:LongInt;
Begin;
i:=regs_d[num and 7];
if not dw then begin;
  i:=i and $ffff;
  if sign then i:=integer(i);
  end;
readReg16:=i;
End;

Procedure writeReg8(num,val:LongInt);
Var i:LongInt;
Begin;
val:=val and $ff;
if (num and 4<>0) then begin;
  val:=val shl 8;
  i:=$ffff00ff;
  end else begin;
  i:=$ffffff00;
  end;
num:=num and 3;
regs_d[num]:=(regs_d[num] and i) or val;
End;

Procedure writeReg16(num,val:LongInt;dw:Boolean);
Begin;
num:=num and 7;
if dw then begin;
  regs_d[num]:=val;
  end else begin;
  regs_d[num]:=(regs_d[num] and $ffff0000) or (val and $ffff);
  end;
End;




function push2stack(dat,siz:LongInt):Boolean;
Begin;
push2stack:=True;
if (siz<0) then begin;
  if protMode then siz:=4 else siz:=2;
  end;
dec(regs_d[reg_esp],siz);
if checkRange(regs_d[reg_esp]) then exit;
case siz of
  1:memoryData^[regs_d[reg_esp]]:=dat;
  2:WriteWordMSB(memoryData^[regs_d[reg_esp]],dat);
  4:WriteLongMSB(memoryData^[regs_d[reg_esp]],dat);
  else exit;
  end;
push2stack:=False;
End;

Function pop4stack(var dat:LongInt;siz:LongInt):Boolean;
Begin;
pop4stack:=True;
if (siz<0) then begin;
  if protMode then siz:=4 else siz:=2;
  end;
if checkRange(regs_d[reg_esp]) then exit;
case siz of
  1:dat:=memoryData^[regs_d[reg_esp]];
  2:dat:=ReadWordMSB(memoryData^[regs_d[reg_esp]]);
  4:dat:=ReadLongMSB(memoryData^[regs_d[reg_esp]]);
  else exit;
  end;
inc(regs_d[reg_esp],siz);
pop4stack:=False;
End;



Procedure decodeModeReg(mrm:LongInt;var typ,val:LongInt;noSeg:Boolean);
{typ: 1=reg, 2=addr}
Label f1,f2,vege;
Var
  i,o,p:LongInt;
  sib,def:LongInt;
Begin;
typ:=0;
def:=reg_ds;
typ:=0;
if extAdr then goto f1;

{16 bit addressing}
case mrm and $c0 of
  $00:begin;
    case mrm and 7 of
      0:val:=regs_d[reg_ebx]+regs_d[reg_esi];
      1:val:=regs_d[reg_ebx]+regs_d[reg_edi];
      2:begin; val:=regs_d[reg_ebp]+regs_d[reg_esi];def:=reg_ss; end;
      3:begin; val:=regs_d[reg_ebp]+regs_d[reg_edi];def:=reg_ss; end;
      4:val:=regs_d[reg_esi];
      5:val:=regs_d[reg_edi];
      6:val:=getCodeInt(2,true);
      7:val:=regs_d[reg_ebx];
      end;
    val:=val and $ffff;
    typ:=typ_ad;
    goto vege;
    end;
  $40:begin;
    case mrm and 7 of
      0:val:=regs_d[reg_ebx]+regs_d[reg_esi];
      1:val:=regs_d[reg_ebx]+regs_d[reg_edi];
      2:begin; val:=regs_d[reg_ebp]+regs_d[reg_esi];def:=reg_ss; end;
      3:begin; val:=regs_d[reg_ebp]+regs_d[reg_edi];def:=reg_ss; end;
      4:val:=regs_d[reg_esi];
      5:val:=regs_d[reg_edi];
      6:begin; val:=regs_d[reg_ebp];def:=reg_ss; end;
      7:val:=regs_d[reg_ebx];
      end;
    val:=(getCodeInt(1,true)+val) and $ffff;
    typ:=typ_ad;
    goto vege;
    end;
  $80:begin;
    case mrm and 7 of
      0:val:=regs_d[reg_ebx]+regs_d[reg_esi];
      1:val:=regs_d[reg_ebx]+regs_d[reg_edi];
      2:begin; val:=regs_d[reg_ebp]+regs_d[reg_esi];def:=reg_ss; end;
      3:begin; val:=regs_d[reg_ebp]+regs_d[reg_edi];def:=reg_ss; end;
      4:val:=regs_d[reg_esi];
      5:val:=regs_d[reg_edi];
      6:begin; val:=regs_d[reg_ebp];def:=reg_ss; end;
      7:val:=regs_d[reg_ebx];
      end;
    val:=(getCodeInt(2,true)+val) and $ffff;
    typ:=typ_ad;
    goto vege;
    end;
  $c0:begin;
    val:=mrm and 7;
    typ:=typ_rg;
    goto vege;
    end;
  end;
goto vege;

f1:
{32 bit addressing}
case mrm and $c0 of
  $00:begin;
    case mrm and 7 of
      0:val:=regs_d[reg_eax];
      1:val:=regs_d[reg_ecx];
      2:val:=regs_d[reg_edx];
      3:val:=regs_d[reg_ebx];
      4:begin; val:=0;goto f2; end;
      5:val:=getCodeInt(4,true);
      6:val:=regs_d[reg_esi];
      7:val:=regs_d[reg_edi];
      end;
    typ:=typ_ad;
    goto vege;
    end;
  $40:begin;
    case mrm and 7 of
      0:val:=regs_d[reg_eax];
      1:val:=regs_d[reg_ecx];
      2:val:=regs_d[reg_edx];
      3:val:=regs_d[reg_ebx];
      4:begin; val:=1;goto f2; end;
      5:begin; val:=regs_d[reg_ebp];def:=reg_ss; end;
      6:val:=regs_d[reg_esi];
      7:val:=regs_d[reg_edi];
      end;
    inc(val,getCodeInt(1,true));
    typ:=typ_ad;
    goto vege;
    end;
  $80:begin;
    case mrm and 7 of
      0:val:=regs_d[reg_eax];
      1:val:=regs_d[reg_ecx];
      2:val:=regs_d[reg_edx];
      3:val:=regs_d[reg_ebx];
      4:begin; val:=4;goto f2; end;
      5:begin; val:=regs_d[reg_ebp];def:=reg_ss; end;
      6:val:=regs_d[reg_esi];
      7:val:=regs_d[reg_edi];
      end;
    inc(val,getCodeInt(4,true));
    typ:=typ_ad;
    goto vege;
    end;
  $c0:begin;
    val:=mrm and 7;
    typ:=typ_rg;
    goto vege;
    end;
  end;
goto vege;

f2:
{32 bit sib addressing}
sib:=getCodeInt(1,false);
val:=getCodeInt(val,false);
case sib and $38 of
  $00:i:=regs_d[reg_eax];
  $08:i:=regs_d[reg_ecx];
  $10:i:=regs_d[reg_edx];
  $18:i:=regs_d[reg_edx];
  $20:i:=0;
  $28:i:=regs_d[reg_ebp];
  $30:i:=regs_d[reg_esi];
  $38:i:=regs_d[reg_edi];
  end;
case sib and $c0 of
  $00:inc(val,i);
  $40:inc(val,i+i);
  $80:inc(val,i*4);
  $c0:inc(val,i*8);
  end;
case sib and 7 of
  0:inc(val,regs_d[reg_eax]);
  1:inc(val,regs_d[reg_ecx]);
  2:inc(val,regs_d[reg_edx]);
  3:inc(val,regs_d[reg_ebx]);
  4:inc(val,regs_d[reg_esp]);
  5:if (mrm and $c0=0) then inc(val,getCodeInt(4,true)) else inc(val,regs_d[reg_ebp]);
  6:inc(val,regs_d[reg_esi]);
  7:inc(val,regs_d[reg_edi]);
  end;
typ:=typ_ad;
goto vege;

vege:
if (typ<>typ_ad) then exit;
if noSeg then exit;
if (extSeg>=0) then def:=extSeg;
inc(val,regs_s[def] shl 4);
End;

Function ReadDecoded(typ,val,siz:LongInt;signed:Boolean;var data:LongInt):Boolean;
Begin;
ReadDecoded:=True;
case typ of
  1:case siz of
    1:data:=readReg8(val,signed);
    2:data:=readReg16(val,signed,false);
    4:data:=readReg16(val,signed,true);
    end;
  2:begin;
    if checkRange(val) then begin; result:=2;exit; end;
    case siz of
      1:data:=memoryData^[val];
      2:data:=ReadWordLSB(memoryData^[val]);
      4:data:=ReadLongLSB(memoryData^[val]);
      else begin; result:=1;exit; end;
      end;
    end;
  else begin; result:=1;exit; end;
  end;
case siz of
  1:if signed then data:=shortint(data) else data:=data and $ff;
  2:if signed then data:=integer(data) else data:=data and $ffff;
  4:;
  else begin; result:=1;exit; end;
  end;
ReadDecoded:=False;
End;

Function WriteDecoded(typ,val,siz,data:LongInt):Boolean;
Begin;
WriteDecoded:=True;
case typ of
  1:case siz of
    1:writeReg8(val,data);
    2:writeReg16(val,data,false);
    4:writeReg16(val,data,true);
    else begin; result:=1;exit; end;
    end;
  2:begin;
    if checkRange(val) then begin; result:=2;exit; end;
    case siz of
      1:memoryData^[val]:=data;
      2:WriteWordLSB(memoryData^[val],data);
      4:WriteLongLSB(memoryData^[val],data);
      else begin; result:=1;exit; end;
      end;
    end;
  else begin; result:=1;exit; end;
  end;
WriteDecoded:=False;
End;


Function doRead(mr,siz:LongInt;signed:Boolean;var data:LongInt):Boolean;
Var typ,val:LongInt;
Begin;
decodeModeReg(mr,typ,val,false);
doRead:=ReadDecoded(typ,val,siz,signed,data);
End;

Function doWrite(mr,siz:LongInt;data:LongInt):Boolean;
Var typ,val:LongInt;
Begin;
decodeModeReg(mr,typ,val,false);
doWrite:=WriteDecoded(typ,val,siz,data);
End;





Procedure setFlagsZSP(val,siz:LongInt);
Begin;
case siz of
  1:val:=shortint(val);
  2:val:=integer(val);
  4:;
  else val:=0;
  end;
if (val=0) then reg_flg:=reg_flg or $40 else reg_flg:=reg_flg and $ffffffbf;
if (val<0) then reg_flg:=reg_flg or $80 else reg_flg:=reg_flg and $ffffff7f;
if (val and 1<>0) then reg_flg:=reg_flg or $04 else reg_flg:=reg_flg and $fffffffb;
End;

Procedure setFlagsC(val:boolean);
Begin;
if val then reg_flg:=reg_flg or $01 else reg_flg:=reg_flg and $fffffffe;
End;

Procedure setFlagsD(val:boolean);
Begin;
if val then reg_flg:=reg_flg or $400 else reg_flg:=reg_flg and $fffffbff;
End;

Procedure setFlagsI(val:boolean);
Begin;
if val then reg_flg:=reg_flg or $200 else reg_flg:=reg_flg and $fffffdff;
End;

Procedure setFlagsP(val:boolean);
Begin;
if val then reg_flg:=reg_flg or $04 else reg_flg:=reg_flg and $fffffffb;
End;

Procedure setFlagsA(val:boolean);
Begin;
if val then reg_flg:=reg_flg or $10 else reg_flg:=reg_flg and $ffffffef;
End;

Procedure setFlagsZ(val:boolean);
Begin;
if val then reg_flg:=reg_flg or $40 else reg_flg:=reg_flg and $ffffffbf;
End;

Procedure setFlagsS(val:boolean);
Begin;
if val then reg_flg:=reg_flg or $80 else reg_flg:=reg_flg and $ffffff7f;
End;

Procedure setFlagsO(val:boolean);
Begin;
if val then reg_flg:=reg_flg or $800 else reg_flg:=reg_flg and $fffff7ff;
End;

Function getFlagsC:boolean;
Begin;
getFlagsC:=(reg_flg and $01<>0);
End;

Function getFlagsP:boolean;
Begin;
getFlagsP:=(reg_flg and $04<>0);
End;

Function getFlagsA:boolean;
Begin;
getFlagsA:=(reg_flg and $10<>0);
End;

Function getFlagsZ:boolean;
Begin;
getFlagsZ:=(reg_flg and $40<>0);
End;

Function getFlagsS:boolean;
Begin;
getFlagsS:=(reg_flg and $80<>0);
End;

Function getFlagsD:boolean;
Begin;
getFlagsD:=(reg_flg and $400<>0);
End;

Function getFlagsO:boolean;
Begin;
getFlagsO:=(reg_flg and $800<>0);
End;

Procedure setFlagsZSPCOA(a,b,siz:LongInt);
Var r,i,o,p:LongInt;
Begin;
r:=a+b;
setFlagsZSP(r,siz);
case siz of
  1:;
  2:begin; a:=a shr 8;b:=b shr 8;r:=r shr 8; end;
  4:begin; a:=a shr 24;b:=b shr 24;r:=r shr 24; end;
  else begin; a:=0;b:=0;r:=0; end;
  end;
i:=a and 8;
o:=b and 8;
p:=(r and 8) xor 8;
setFlagsA((i and o) or (i and p) or (o and p)<>0);
i:=a and $80;
o:=b and $80;
p:=(r and $80) xor $80;
setFlagsC((i and o) or (i and p) or (o and p)<>0);
setFlagsO(((i xor $80) and (o xor $80) and (p xor $80)) or (i and o and p)<>0);
End;

Procedure setFlagsZSPCOB(a,b,siz:LongInt);
Var r,i,o,p:LongInt;
Begin;
r:=a-b;
setFlagsZSP(r,siz);
case siz of
  1:;
  2:begin; a:=a shr 8;b:=b shr 8;r:=r shr 8; end;
  4:begin; a:=a shr 24;b:=b shr 24;r:=r shr 24; end;
  else begin; a:=0;b:=0;r:=0; end;
  end;
i:=(a and 8) xor 8;
o:=b and 8;
p:=r and 8;
setFlagsA((i and o) or (i and p) or (o and p)<>0);
i:=(a and $80) xor $80;
o:=b and $80;
p:=r and $80;
setFlagsC((i and o) or (i and p) or (o and p)<>0);
setFlagsO(((i xor $80) and (o xor $80) and (p xor $80)) or (i and o and p)<>0);
End;

Function evalCond(i:Longint):Boolean;
Var c,p,a,z,s,o:Boolean;
Begin;
c:=(reg_flg and $01<>0);
p:=(reg_flg and $04<>0);
a:=(reg_flg and $10<>0);
z:=(reg_flg and $40<>0);
s:=(reg_flg and $80<>0);
o:=(reg_flg and $800<>0);
case i and $f of
  $0:c:=o;                      {o}
  $1:c:=not o;                  {no}
  $2:c:=c;                      {c, b, nae}
  $3:c:=not c;                  {nc, ae, nb}
  $4:c:=z;                      {z, e}
  $5:c:=not z;                  {nz, ne}
  $6:c:=c or z;                 {be, na}
  $7:c:=not (c or z);           {a}
  $8:c:=s;                      {s}
  $9:c:=not s;                  {ns}
  $a:c:=p;                      {p, pe}
  $b:c:=not p;                  {np, po}
  $c:c:=(s xor o);              {l, nge}
  $d:c:=not (s xor o);          {ge, nl}
  $e:c:=(s xor o) or z;         {le, ng}
  $f:c:=not ((s xor o) or z);   {g, nbe, nle}
  else c:=false;
  end;
evalCond:=c;
End;




Procedure doANY2(typ,siz:LongInt;sig:Boolean;var a,b:LongInt);
Var
  i,o,p:LongInt;
  bb:Boolean;
Begin;
case typ of
  $02:begin; {adc}
    if getFlagsC then inc(b);
    setFlagsZSPCOA(a,b,siz);
    inc(a,b);
    exit;
    end;
  $00:begin; {add}
    setFlagsZSPCOA(a,b,siz);
    inc(a,b);
    exit;
    end;
  $04:begin; {and}
    a:=a and b;
    setFlagsZSPCOA(a,0,siz);
    exit;
    end;
  $07:begin; {cmp}
    setFlagsZSPCOB(a,b,siz);
    exit;
    end;
  $01:begin; {or}
    a:=a or b;
    setFlagsZSPCOA(a,0,siz);
    exit;
    end;
  $03:begin; {sbb}
    if getFlagsC then inc(b);
    setFlagsZSPCOB(a,b,siz);
    dec(a,b);
    exit;
    end;
  $05:begin; {sub}
    setFlagsZSPCOB(a,b,siz);
    dec(a,b);
    exit;
    end;
  $06:begin; {xor}
    a:=a xor b;
    setFlagsZSPCOA(a,0,siz);
    exit;
    end;
  $f600:begin; {test}
    setFlagsZSP(b and a,siz);
    exit;
    end;
  $1001:begin; {bsf}
    setFlagsZ(true);
    for i:=0 to 31 do begin;
      if (bitVals[i] and b=0) then continue;
      setFlagsZ(false);
      a:=i;
      exit;
      end;
    setFlagsZ(true);
    exit;
    end;
  $1002:begin; {bsr}
    setFlagsZ(true);
    for i:=31 downto 0 do begin;
      if (bitVals[i] and b=0) then continue;
      setFlagsZ(false);
      a:=i;
      exit;
      end;
    setFlagsZ(true);
    exit;
    end;
  $0ba4:begin; {bt}
    i:=bitVals[b and $1f];
    setFlagsC(a and i<>0);
    exit;
    end;
  $0ba7:begin; {btc}
    i:=bitVals[b and $1f];
    setFlagsC(a and i<>0);
    a:=a xor i;
    exit;
    end;
  $0ba6:begin; {btr}
    i:=bitVals[b and $1f];
    setFlagsC(a and i<>0);
    a:=(not i) and a;
    exit;
    end;
  $0ba5:begin; {bts}
    i:=bitVals[b and $1f];
    setFlagsC(a and i<>0);
    a:=a or i;
    exit;
    end;
  $d002:begin; {rcl}
    a:=a shl 1;
    if getFlagsC then inc(a);
    a:=rotateLeft(a,(siz*8)+1,b);
    setFlagsC(a and 1<>0);
    a:=a shr 1;
    exit;
    end;
  $d003:begin; {rcr}
    a:=a shl 1;
    if getFlagsC then inc(a);
    a:=rotateRight(a,(siz*8)+1,b);
    setFlagsC(a and 1<>0);
    a:=a shr 1;
    exit;
    end;
  $d000:begin; {rol}
    a:=rotateLeft(a,siz*8,b);
    setFlagsC(a and 1<>0);
    exit;
    end;
  $d001:begin; {ror}
    a:=rotateRight(a,siz*8,b);
    setFlagsC(bitVals[(siz*8)-1] and a<>0);
    exit;
    end;
  $d004:begin; {shl}
    siz:=(siz*8)-1;
    b:=b and siz;
    setFlagsC(bitVals[siz-b+1] and a<>0);
    a:=a shl b;
    exit;
    end;
  $d005:begin; {shr}
    siz:=(siz*8)-1;
    b:=b and siz;
    setFlagsC(bitVals[b-1] and a<>0);
    a:=a shr b;
    exit;
    end;
  $d007:begin; {sar}
    siz:=(siz*8)-1;
    b:=b and siz;
    bb:=(bitVals[siz] and a=0);
    setFlagsC(bitVals[b-1] and a<>0);
    a:=a shr b;
    if bb then exit;
    b:=siz-b;
    b:=(1 shl b)-1;
    a:=(not b) or a;
    exit;
    end;
  end;
End;

Procedure doANY1(typ,siz:LongInt;sig:Boolean;var a:LongInt);
Var
  i,o,p:LongInt;
  b:Boolean;
Begin;
case typ of
  $fe01:begin; {dec}
    b:=getFlagsC;
    setFlagsZSPCOB(a,1,siz);
    setFlagsC(b);
    dec(a);
    exit;
    end;
  $fe00:begin; {inc}
    b:=getFlagsC;
    setFlagsZSPCOA(a,1,siz);
    setFlagsC(b);
    inc(a);
    exit;
    end;
  $f603:begin; {neg}
    setFlagsZSPCOB(0,a,siz);
    setFlagsC(a<>0);
    a:=-a;
    exit;
    end;
  $f602:begin; {not}
    a:=not a;
    exit;
    end;
  end;
End;



Function doANYmi(typ,siz,immSiz:LongInt;sig:Boolean;rm:LongInt):Boolean;
Var i,o,p,q:LongInt;
Begin;
doANYmi:=True;
if (immSiz<1) then immSiz:=siz;
decodeModeReg(rm,q,o,false);
if ReadDecoded(q,o,siz,sig,p) then exit;
i:=getCodeInt(immSiz,sig);
doANY2(typ,siz,sig,p,i);
if WriteDecoded(q,o,siz,p) then exit;
doANYmi:=False;
End;

Function doANYmc(typ,siz:LongInt;sig:Boolean;rm,imm:LongInt):Boolean;
Var i,o,p,q:LongInt;
Begin;
doANYmc:=True;
decodeModeReg(rm,q,o,false);
if ReadDecoded(q,o,siz,sig,p) then exit;
doANY2(typ,siz,sig,p,imm);
if WriteDecoded(q,o,siz,p) then exit;
doANYmc:=False;
End;

Function doANYmr(typ,siz:LongInt;sig:Boolean):Boolean;
Var i,o,p,q,rm:LongInt;
Begin;
doANYmr:=True;
rm:=getCodeInt(1,false);
decodeModeReg(rm,q,o,false);
rm:=(rm shr 3) and 7;
if ReadDecoded(q,o,siz,sig,p) then exit;
if ReadDecoded(typ_rg,rm,siz,sig,i) then exit;
doANY2(typ,siz,sig,p,i);
if WriteDecoded(q,o,siz,p) then exit;
doANYmr:=False;
End;

Function doANYrm(typ,siz:LongInt;sig:Boolean):Boolean;
Var i,o,p,q,rm:LongInt;
Begin;
doANYrm:=True;
rm:=getCodeInt(1,false);
decodeModeReg(rm,q,o,false);
rm:=(rm shr 3) and 7;
if ReadDecoded(q,o,siz,sig,i) then exit;
if ReadDecoded(typ_rg,rm,siz,sig,p) then exit;
doANY2(typ,siz,sig,p,i);
if WriteDecoded(typ_rg,rm,siz,p) then exit;
doANYrm:=False;
End;



Function doANYr(typ,siz:LongInt;sig:Boolean;rm:LongInt):Boolean;
Var i:LongInt;
Begin;
doANYr:=True;
rm:=rm and 7;
if ReadDecoded(typ_rg,rm,siz,sig,i) then exit;
doANY1(typ,siz,sig,i);
if WriteDecoded(typ_rg,rm,siz,i) then exit;
doANYr:=False;
End;

Function doANYm(typ,siz:LongInt;sig:Boolean;rm:LongInt):Boolean;
Var i,o,p:LongInt;
Begin;
doANYm:=True;
decodeModeReg(rm,p,o,false);
if ReadDecoded(p,o,siz,sig,i) then exit;
doANY1(typ,siz,sig,i);
if WriteDecoded(p,o,siz,i) then exit;
doANYm:=False;
End;



Function doString(typ,siz:LongInt):Boolean;
Label f1,f2;
Var
  seg,seg2:LongInt;
  i,o,p:LongInt;
Begin;
doString:=True;
if (typ in [1,3,4,5]) then seg:=reg_ds else seg:=reg_es;
if (extSeg>=0) then seg:=extSeg;
seg:=regs_s[seg] shl 4;
seg2:=regs_s[reg_es] shl 4;
f1:
if (extRep>0) then if (readReg16(reg_ecx,false,extAdr)=0) then goto f2;
case typ of
  1:begin; {cmps}
    p:=seg+readReg16(reg_esi,false,extAdr);
    if ReadDecoded(typ_ad,p,siz,true,i) then exit;
    p:=seg2+readReg16(reg_edi,false,extAdr);
    if ReadDecoded(typ_ad,p,siz,true,o) then exit;
    setFlagsZSPCOB(i,o,siz);
    if getFlagsD then i:=-siz else i:=siz;
    writeReg16(reg_esi,readReg16(reg_esi,false,extAdr)+i,extAdr);
    writeReg16(reg_edi,readReg16(reg_edi,false,extAdr)+i,extAdr);
    end;
  2:begin; {ins}
    p:=seg+readReg16(reg_edi,false,extAdr);
    i:=readPort(regs_d[reg_edx] and $ffff,siz);
    if writeDecoded(typ_ad,p,siz,i) then exit;
    if getFlagsD then i:=-siz else i:=siz;
    writeReg16(reg_edi,readReg16(reg_edi,false,extAdr)+i,extAdr);
    end;
  3:begin; {lods}
    p:=seg+readReg16(reg_esi,false,extAdr);
    if ReadDecoded(typ_ad,p,siz,true,i) then exit;
    if writeDecoded(typ_rg,$c0,siz,i) then exit;
    if getFlagsD then i:=-siz else i:=siz;
    writeReg16(reg_esi,readReg16(reg_esi,false,extAdr)+i,extAdr);
    end;
  4:begin; {movs}
    p:=seg+readReg16(reg_esi,false,extAdr);
    if ReadDecoded(typ_ad,p,siz,true,i) then exit;
    p:=seg2+readReg16(reg_edi,false,extAdr);
    if WriteDecoded(typ_ad,p,siz,i) then exit;
    if getFlagsD then i:=-siz else i:=siz;
    writeReg16(reg_esi,readReg16(reg_esi,false,extAdr)+i,extAdr);
    writeReg16(reg_edi,readReg16(reg_edi,false,extAdr)+i,extAdr);
    end;
  5:begin; {outs}
    p:=seg+readReg16(reg_esi,false,extAdr);
    if ReadDecoded(typ_ad,p,siz,true,i) then exit;
    writePort(regs_d[reg_edx] and $ffff,siz,i);
    if getFlagsD then i:=-siz else i:=siz;
    writeReg16(reg_esi,readReg16(reg_esi,false,extAdr)+i,extAdr);
    end;
  6:begin; {scas}
    p:=seg+readReg16(reg_edi,false,extAdr);
    if ReadDecoded(typ_ad,p,siz,false,i) then exit;
    if ReadDecoded(typ_rg,$c0,siz,false,o) then exit;
    setFlagsZSPCOB(o,i,siz);
    if getFlagsD then i:=-siz else i:=siz;
    writeReg16(reg_edi,readReg16(reg_edi,false,extAdr)+i,extAdr);
    end;
  7:begin; {stos}
    p:=seg+readReg16(reg_edi,false,extAdr);
    if ReadDecoded(typ_rg,$c0,siz,false,i) then exit;
    if WriteDecoded(typ_ad,p,siz,i) then exit;
    if getFlagsD then i:=-siz else i:=siz;
    writeReg16(reg_edi,readReg16(reg_edi,false,extAdr)+i,extAdr);
    end;
  else exit;
  end;
if (extRep<1) then goto f2;
writeReg16(reg_ecx,readReg16(reg_ecx,false,extAdr)-1,extAdr);
if not (typ in [1,5]) then goto f1;
if (extRep=1) then begin;
  {repe}
  if not getFlagsZ then goto f2;
  end else begin;
  {repne}
  if getFlagsZ then goto f2;
  end;
goto f1;
f2:
doString:=False;
End;

Function doCMPXCHG(rm,siz:LongInt):Boolean;
Var
  typ,val,reg:LongInt;
  i,o:LongInt;
Begin;
doCMPXCHG:=True;
reg:=(rm shr 3) and 7;
decodeModeReg(rm,typ,val,false);
if ReadDecoded(typ,val,siz,false,o) then exit;
if ReadDecoded(typ_rg,0,siz,false,i) then exit;
if (i=o) then begin;
  setFlagsZ(true);
  if ReadDecoded(typ_rg,reg,siz,false,i) then exit;
  if WriteDecoded(typ,val,siz,i) then exit;
  end else begin;
  setFlagsZ(false);
  if WriteDecoded(typ_rg,0,siz,o) then exit;
  end;
doCMPXCHG:=False;
End;

Procedure doCPUID(a:String);
Var i:LongInt;
Begin;
a:=a+#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;
move(a[1],i,sizeof(i));
WriteLongLSB(regs_d[reg_eax],i);
move(a[5],i,sizeof(i));
WriteLongLSB(regs_d[reg_ebx],i);
move(a[9],i,sizeof(i));
WriteLongLSB(regs_d[reg_ecx],i);
move(a[13],i,sizeof(i));
WriteLongLSB(regs_d[reg_edx],i);
End;

Function doENTER(lev,siz:LongInt):Boolean;
Label f1;
Var
  i,o,tmp:LongInt;
  b:Boolean;
Begin;
doENTER:=True;
if push2stack(regs_d[reg_ebp],-1) then exit;
tmp:=regs_d[reg_esp];
if (lev<1) then goto f1;
for i:=1 to lev-1 do begin;
  if push2stack(regs_d[reg_ebp],intSiz) then exit;
  end;
if push2stack(tmp,intSiz) then exit;
f1:
writeReg16(reg_ebp,tmp,protMode);
writeReg16(reg_esp,regs_d[reg_ebp]-siz,protMode);
doENTER:=False;
End;

Function doLSS(seg:LongInt):Boolean;
Var i,o:LongInt;
Begin;
doLSS:=True;
o:=getCodeInt(1,false);
if doRead(o,4,true,i) then exit;
writeReg16((o shr 3) and 7,i and $ffff,extSiz);
regs_s[seg]:=i shr 16;
doLSS:=False;
End;

Function doMOVSX(sig,tSiz:Boolean;sSiz:LongInt):Boolean;
Var i,o:LongInt;
Begin;
doMOVSX:=True;
o:=getCodeInt(1,false);
if doRead(o,sSiz,sig,i) then exit;
writeReg16((o shr 3) and 7,i,tSiz);
doMOVSX:=False;
End;

Function doPOPSR(reg:LongInt):Boolean;
Var i:LongInt;
Begin;
doPOPSR:=True;
if pop4stack(i,intSiz) then exit;
regs_s[reg]:=i;
doPOPSR:=False;
End;

Function doPUSHSR(reg:LongInt):Boolean;
Var i:LongInt;
Begin;
doPUSHSR:=True;
i:=regs_s[reg];
if push2stack(i,intSiz) then exit;
doPUSHSR:=False;
End;

Function doSHLD(mode:Boolean;typ,val,reg,bit:LongInt):Boolean;
Var i,o,siz:LongInt;
Begin;
doSHLD:=True;
siz:=(intSiz*8)-1;
bit:=bit and siz;
reg:=readReg16(reg and 7,false,extSiz);
if ReadDecoded(typ,val,intSiz,false,o) then exit;
i:=siz-bit+1;
if mode then begin;
  {shld}
  setFlagsC(bitVals[siz-bit+1] and o<>0);
  o:=o shl bit;
  reg:=reg shr i;
  o:=o or reg;
  end else begin;
  {shrd}
  setFlagsC(bitVals[bit-1] and o<>0);
  o:=o shr bit;
  reg:=reg shl i;
  o:=o or reg;
  end;
setFlagsZSP(o,intSiz);
if WriteDecoded(typ,val,intSiz,o) then exit;
doSHLD:=False;
End;

Function doXADD(siz:LongInt):Boolean;
Var typ,val,reg,a,b,c:LongInt;
Begin;
doXADD:=True;
reg:=getCodeInt(1,false);
decodeModeReg(reg,typ,val,false);
reg:=(reg shr 3) and 7;
if ReadDecoded(typ,val,siz,false,a) then exit;
if ReadDecoded(typ_rg,reg,siz,false,b) then exit;
setFlagsZSPCOA(a,b,siz);
c:=a+b;
b:=a;
a:=c;
if WriteDecoded(typ,val,siz,a) then exit;
if WriteDecoded(typ_rg,reg,siz,b) then exit;
doXADD:=False;
End;

Function doXCHG(rm,siz:LongInt):Boolean;
Var typ,val,a,b,c:LongInt;
Begin;
doXCHG:=True;
decodeModeReg(rm,typ,val,false);
rm:=(rm shr 3) and 7;
if ReadDecoded(typ,val,siz,false,a) then exit;
if ReadDecoded(typ_rg,rm,siz,false,b) then exit;
c:=a;
a:=b;
b:=c;
if WriteDecoded(typ,val,siz,a) then exit;
if WriteDecoded(typ_rg,rm,siz,b) then exit;
doXCHG:=False;
End;



Label f1;
Var i,o,p,q,r,s:LongInt;
Begin;
result:=1;
protMode:=(reg_cr0 and 1<>0);
extAdr:=protMode;
extSiz:=protMode;
extLck:=false;
extSeg:=-1;
extRep:=0;

if checkRange((regs_s[reg_cs] shl 4)+reg_eip) then goto vege;
f1:
i:=getCodeInt(1,false);
if extSiz then intSiz:=4 else intSiz:=2;

case i of
  $66:begin; extSiz:=not protMode;goto f1; end;
  $67:begin; extAdr:=not protMode;goto f1; end;
  $26:begin; extSeg:=reg_es;goto f1; end;
  $2e:begin; extSeg:=reg_cs;goto f1; end;
  $36:begin; extSeg:=reg_ss;goto f1; end;
  $3e:begin; extSeg:=reg_ds;goto f1; end;
  $64:begin; extSeg:=reg_fs;goto f1; end;
  $65:begin; extSeg:=reg_gs;goto f1; end;
  $f3:begin; extRep:=1;goto f1; end;
  $f2:begin; extRep:=2;goto f1; end;
  $f0:begin; extLck:=true;goto f1; end;
  $0f:goto op0F;
  $37:begin;
    {$ifdef debug}writeln('aaa');{$endif}
    i:=readReg8(reg_al,false);
    o:=readReg8(reg_ah,false);
    if (i and $f>9) or getFlagsA then begin;
      inc(i,6);
      inc(o);
      setFlagsA(true);
      setFlagsC(true);
      end else begin;
      setFlagsA(false);
      setFlagsC(false);
      end;
    i:=i and $f;
    writeReg8(reg_al,i);
    writeReg8(reg_ah,o);
    goto ok;
    end;
  $d5:begin;
    {$ifdef debug}writeln('aad');{$endif}
    i:=readReg8(reg_al,false);
    o:=readReg8(reg_ah,false);
    inc(i,o*getCodeInt(1,false));
    setFlagsZSP(i,1);
    writeReg8(reg_al,i);
    goto ok;
    end;
  $d4:begin;
    {$ifdef debug}writeln('aam');{$endif}
    o:=getCodeInt(1,false);
    if (o=0) then begin; result:=4;goto vege; end;
    i:=readReg8(reg_al,false);
    writeReg8(reg_ah,i div o);
    i:=i mod o;
    setFlagsZSP(i,1);
    writeReg8(reg_al,i);
    goto ok;
    end;
  $3f:begin;
    {$ifdef debug}writeln('aas');{$endif}
    i:=readReg8(reg_al,false);
    o:=readReg8(reg_ah,false);
    if (i and $f>9) or getFlagsA then begin;
      dec(i,6);
      dec(o);
      setFlagsA(true);
      setFlagsC(true);
      end else begin;
      setFlagsA(false);
      setFlagsC(false);
      end;
    i:=i and $f;
    writeReg8(reg_al,i);
    writeReg8(reg_ah,o);
    goto ok;
    end;
  $80:begin;
    i:=getCodeInt(1,false);
    case (i shr 3) and 7 of
      2:begin;
        {$ifdef debug}writeln('adc rm,num8');{$endif}
        if doANYmi(2,1,0,false,i) then goto vege;
        goto ok;
        end;
      0:begin;
        {$ifdef debug}writeln('add rm,num8');{$endif}
        if doANYmi(0,1,0,false,i) then goto vege;
        goto ok;
        end;
      4:begin;
        {$ifdef debug}writeln('and rm,num8');{$endif}
        if doANYmi(4,1,0,false,i) then goto vege;
        goto ok;
        end;
      7:begin;
        {$ifdef debug}writeln('cmp rm,num8');{$endif}
        if doANYmi(7,1,0,false,i) then goto vege;
        goto ok;
        end;
      1:begin;
        {$ifdef debug}writeln('or rm,num8');{$endif}
        if doANYmi(1,1,0,false,i) then goto vege;
        goto ok;
        end;
      3:begin;
        {$ifdef debug}writeln('sbb rm,num8');{$endif}
        if doANYmi(3,1,0,false,i) then goto vege;
        goto ok;
        end;
      5:begin;
        {$ifdef debug}writeln('sub rm,num8');{$endif}
        if doANYmi(5,1,0,false,i) then goto vege;
        goto ok;
        end;
      6:begin;
        {$ifdef debug}writeln('xor rm,num8');{$endif}
        if doANYmi(6,1,0,false,i) then goto vege;
        goto ok;
        end;
      else goto vege;
      end;
    end;
  $81:begin;
    i:=getCodeInt(1,false);
    case (i shr 3) and 7 of
      2:begin;
        {$ifdef debug}writeln('adc rm,num16');{$endif}
        if doANYmi(2,intSiz,0,false,i) then goto vege;
        goto ok;
        end;
      0:begin;
        {$ifdef debug}writeln('add rm,num16');{$endif}
        if doANYmi(0,intSiz,0,false,i) then goto vege;
        goto ok;
        end;
      4:begin;
        {$ifdef debug}writeln('and rm,num16');{$endif}
        if doANYmi(4,intSiz,0,false,i) then goto vege;
        goto ok;
        end;
      7:begin;
        {$ifdef debug}writeln('cmp rm,num16');{$endif}
        if doANYmi(7,intSiz,0,false,i) then goto vege;
        goto ok;
        end;
      1:begin;
        {$ifdef debug}writeln('or rm,num16');{$endif}
        if doANYmi(1,intSiz,0,false,i) then goto vege;
        goto ok;
        end;
      3:begin;
        {$ifdef debug}writeln('sbb rm,num16');{$endif}
        if doANYmi(3,intSiz,0,false,i) then goto vege;
        goto ok;
        end;
      5:begin;
        {$ifdef debug}writeln('sub rm,num16');{$endif}
        if doANYmi(5,intSiz,0,false,i) then goto vege;
        goto ok;
        end;
      6:begin;
        {$ifdef debug}writeln('xor rm,num16');{$endif}
        if doANYmi(6,intSiz,0,false,i) then goto vege;
        goto ok;
        end;
      else goto vege;
      end;
    end;
  $83:begin;
    i:=getCodeInt(1,false);
    case (i shr 3) and 7 of
      2:begin;
        {$ifdef debug}writeln('adc rm,num16');{$endif}
        if doANYmi(2,intSiz,0,true,i) then goto vege;
        goto ok;
        end;
      0:begin;
        {$ifdef debug}writeln('add rm,num16');{$endif}
        if doANYmi(0,intSiz,0,true,i) then goto vege;
        goto ok;
        end;
      4:begin;
        {$ifdef debug}writeln('and rm,num16');{$endif}
        if doANYmi(4,intSiz,0,true,i) then goto vege;
        goto ok;
        end;
      7:begin;
        {$ifdef debug}writeln('cmp rm,num16');{$endif}
        if doANYmi(7,intSiz,0,true,i) then goto vege;
        goto ok;
        end;
      1:begin;
        {$ifdef debug}writeln('or rm,num16');{$endif}
        if doANYmi(1,intSiz,0,true,i) then goto vege;
        goto ok;
        end;
      3:begin;
        {$ifdef debug}writeln('sbb rm,num16');{$endif}
        if doANYmi(3,intSiz,0,true,i) then goto vege;
        goto ok;
        end;
      5:begin;
        {$ifdef debug}writeln('sub rm,num16');{$endif}
        if doANYmi(5,intSiz,0,true,i) then goto vege;
        goto ok;
        end;
      6:begin;
        {$ifdef debug}writeln('xor rm,num16');{$endif}
        if doANYmi(6,intSiz,0,true,i) then goto vege;
        goto ok;
        end;
      else goto vege;
      end;
    end;
  $14:begin;
    {$ifdef debug}writeln('adc al,num8');{$endif}
    if doANYmi(2,1,0,false,$c0) then goto vege;
    goto ok;
    end;
  $15:begin;
    {$ifdef debug}writeln('adc ax,num16');{$endif}
    if doANYmi(2,intSiz,0,false,$c0) then goto vege;
    goto ok;
    end;
  $10:begin;
    {$ifdef debug}writeln('adc rm,r8');{$endif}
    if doANYmr(2,1,false) then goto vege;
    goto ok;
    end;
  $11:begin;
    {$ifdef debug}writeln('adc rm,r16');{$endif}
    if doANYmr(2,intSiz,false) then goto vege;
    goto ok;
    end;
  $12:begin;
    {$ifdef debug}writeln('adc r8,rm');{$endif}
    if doANYrm(2,1,false) then goto vege;
    goto ok;
    end;
  $13:begin;
    {$ifdef debug}writeln('adc r16,rm');{$endif}
    if doANYrm(2,intSiz,false) then goto vege;
    goto ok;
    end;
  $04:begin;
    {$ifdef debug}writeln('add al,num8');{$endif}
    if doANYmi(0,1,0,false,$c0) then goto vege;
    goto ok;
    end;
  $05:begin;
    {$ifdef debug}writeln('add ax,num16');{$endif}
    if doANYmi(0,intSiz,0,false,$c0) then goto vege;
    goto ok;
    end;
  $00:begin;
    {$ifdef debug}writeln('add rm,r8');{$endif}
    if doANYmr(0,1,false) then goto vege;
    goto ok;
    end;
  $01:begin;
    {$ifdef debug}writeln('add rm,r16');{$endif}
    if doANYmr(0,intSiz,false) then goto vege;
    goto ok;
    end;
  $02:begin;
    {$ifdef debug}writeln('add r8,rm');{$endif}
    if doANYrm(0,1,false) then goto vege;
    goto ok;
    end;
  $03:begin;
    {$ifdef debug}writeln('add r16,rm');{$endif}
    if doANYrm(0,intSiz,false) then goto vege;
    goto ok;
    end;
  $24:begin;
    {$ifdef debug}writeln('and al,num8');{$endif}
    if doANYmi(4,1,0,false,$c0) then goto vege;
    goto ok;
    end;
  $25:begin;
    {$ifdef debug}writeln('and ax,num16');{$endif}
    if doANYmi(4,intSiz,0,false,$c0) then goto vege;
    goto ok;
    end;
  $20:begin;
    {$ifdef debug}writeln('and rm,r8');{$endif}
    if doANYmr(4,1,false) then goto vege;
    goto ok;
    end;
  $21:begin;
    {$ifdef debug}writeln('and rm,r16');{$endif}
    if doANYmr(4,intSiz,false) then goto vege;
    goto ok;
    end;
  $22:begin;
    {$ifdef debug}writeln('and r8,rm');{$endif}
    if doANYrm(4,1,false) then goto vege;
    goto ok;
    end;
  $23:begin;
    {$ifdef debug}writeln('and r16,rm');{$endif}
    if doANYrm(4,intSiz,false) then goto vege;
    goto ok;
    end;
  $e8:begin;
    {$ifdef debug}writeln('call num16');{$endif}
    i:=getCodeInt(intSiz,true);
    if push2stack(reg_eip,intSiz) then goto vege;
    inc(reg_eip,i);
    goto ok;
    end;
  $9a:begin;
    {$ifdef debug}writeln('call num16:num16');{$endif}
    i:=getCodeInt(intSiz,false);
    o:=getCodeInt(2,false);
    if push2stack(regs_s[reg_cs],2) then goto vege;
    if push2stack(reg_eip,intSiz) then goto vege;
    reg_eip:=i;
    regs_s[reg_cs]:=o;
    goto ok;
    end;
  $98:begin;
    {$ifdef debug}writeln('cbw');{$endif}
    if (intSiz<4) then begin;
      i:=readReg8(reg_al,true);
      writeReg16(reg_eax,i,false);
      end else begin;
      i:=readReg16(reg_eax,true,false);
      writeReg16(reg_eax,i,true);
      end;
    goto ok;
    end;
  $99:begin;
    {$ifdef debug}writeln('cwd');{$endif}
    i:=readReg16(reg_eax,true,extSiz);
    if (i<0) then i:=-1 else i:=0;
    writeReg16(reg_edx,i,extSiz);
    goto ok;
    end;
  $f8:begin;
    {$ifdef debug}writeln('clc');{$endif}
    setFlagsC(false);
    goto ok;
    end;
  $fc:begin;
    {$ifdef debug}writeln('cld');{$endif}
    setFlagsD(false);
    goto ok;
    end;
  $fa:begin;
    {$ifdef debug}writeln('cli');{$endif}
    setFlagsI(false);
    goto ok;
    end;
  $f5:begin;
    {$ifdef debug}writeln('cmc');{$endif}
    setFlagsC(not getFlagsC);
    goto ok;
    end;
  $3c:begin;
    {$ifdef debug}writeln('cmp al,num8');{$endif}
    if doANYmi(7,1,0,false,$c0) then goto vege;
    goto ok;
    end;
  $3d:begin;
    {$ifdef debug}writeln('cmp ax,num16');{$endif}
    if doANYmi(7,intSiz,0,false,$c0) then goto vege;
    goto ok;
    end;
  $38:begin;
    {$ifdef debug}writeln('cmp rm,r8');{$endif}
    if doANYmr(7,1,false) then goto vege;
    goto ok;
    end;
  $39:begin;
    {$ifdef debug}writeln('cmp rm,r16');{$endif}
    if doANYmr(7,intSiz,false) then goto vege;
    goto ok;
    end;
  $3a:begin;
    {$ifdef debug}writeln('cmp r8,rm');{$endif}
    if doANYrm(7,1,false) then goto vege;
    goto ok;
    end;
  $3b:begin;
    {$ifdef debug}writeln('cmp r16,rm');{$endif}
    if doANYrm(7,intSiz,false) then goto vege;
    goto ok;
    end;
  $a6:begin;
    {$ifdef debug}writeln('cmpsb');{$endif}
    if doString(1,1) then goto vege;
    goto ok;
    end;
  $a7:begin;
    {$ifdef debug}writeln('cmpsw');{$endif}
    if doString(1,intSiz) then goto vege;
    goto ok;
    end;
  $27:begin;
    {$ifdef debug}writeln('daa');{$endif}
    i:=readReg8(reg_al,false);
    if (i and $f>9) or getFlagsA then begin;
      setFlagsC(true);
      setFlagsA(true);
      inc(i,6);
      end else begin;
      setFlagsA(false);
      end;
    if (i and $f0>$90) or getFlagsC then begin;
      setFlagsC(true);
      inc(i,$60);
      end else begin;
      setFlagsC(false);
      end;
    writeReg8(reg_al,i);
    goto ok;
    end;
  $2f:begin;
    {$ifdef debug}writeln('das');{$endif}
    i:=readReg8(reg_al,false);
    if (i and $f>9) or getFlagsA then begin;
      setFlagsC(true);
      setFlagsA(true);
      dec(i,6);
      end else begin;
      setFlagsA(false);
      end;
    if (i and $f0>$90) or getFlagsC then begin;
      setFlagsC(true);
      dec(i,$60);
      end else begin;
      setFlagsC(false);
      end;
    writeReg8(reg_al,i);
    goto ok;
    end;
  $fe:begin;
    i:=getCodeInt(1,false);
    case (i shr 3) and 7 of
      1:begin;
        {$ifdef debug}writeln('dec rm8');{$endif}
        if doANYm($fe01,1,true,i) then goto vege;
        goto ok;
        end;
      0:begin;
        {$ifdef debug}writeln('inc rm8');{$endif}
        if doANYm($fe00,1,true,i) then goto vege;
        goto ok;
        end;
      end;
    goto vege;
    end;
  $ff:begin;
    i:=getCodeInt(1,false);
    case (i shr 3) and 7 of
      2:begin;
        {$ifdef debug}writeln('call rm');{$endif}
        if doRead(i,intSiz,true,i) then goto vege;
        if push2stack(reg_eip,intSiz) then goto vege;
        reg_eip:=i;
        goto ok;
        end;
      3:begin;
        {$ifdef debug}writeln('call rm:rm');{$endif}
        if doRead(i,4,true,i) then goto vege;
        if push2stack(regs_s[reg_cs],2) then goto vege;
        if push2stack(reg_eip,intSiz) then goto vege;
        reg_eip:=i and $ffff;
        regs_s[reg_cs]:=i shr 16;
        goto ok;
        end;
      1:begin;
        {$ifdef debug}writeln('dec rm16');{$endif}
        if doANYm($fe01,intSiz,true,i) then goto vege;
        goto ok;
        end;
      0:begin;
        {$ifdef debug}writeln('inc rm16');{$endif}
        if doANYm($fe00,intSiz,true,i) then goto vege;
        goto ok;
        end;
      4:begin;
        {$ifdef debug}writeln('jmp rm');{$endif}
        if doRead(i,intSiz,true,i) then goto vege;
        reg_eip:=i;
        goto ok;
        end;
      5:begin;
        {$ifdef debug}writeln('jmp rm:rm');{$endif}
        if doRead(i,4,true,i) then goto vege;
        reg_eip:=i and $ffff;
        regs_s[reg_cs]:=i shr 16;
        goto ok;
        end;
      6:begin;
        {$ifdef debug}writeln('push rm16');{$endif}
        if doRead(i,intSiz,true,i) then goto vege;
        if push2stack(i,intSiz) then goto vege;
        goto ok;
        end;
      end;
    goto vege;
    end;
  $48..$4f:begin;
    {$ifdef debug}writeln('dec r16');{$endif}
    if doANYr($fe01,intSiz,true,i) then goto vege;
    goto ok;
    end;
  $f6:begin;
    i:=getCodeInt(1,false);
    case (i shr 3) and 7 of
      6:begin;
        {$ifdef debug}writeln('div rm8');{$endif}
        if doRead(i,1,false,o) then goto vege;
        p:=readReg16(reg_eax,false,false);
        result:=4;
        if (o=0) then goto vege;
        q:=p div o;
        p:=p mod o;
        if (q>$ff) then goto vege;
        writeReg8(reg_al,q);
        writeReg8(reg_ah,p);
        setFlagsZSP(q,1);
        goto ok;
        end;
      7:begin;
        {$ifdef debug}writeln('idiv rm8');{$endif}
        if doRead(i,1,true,o) then goto vege;
        p:=readReg16(reg_eax,true,false);
        result:=4;
        if (o=0) then goto vege;
        q:=p div o;
        p:=p mod o;
        if (q<-$80) or (q>$7f) then goto vege;
        writeReg8(reg_al,q);
        writeReg8(reg_ah,p);
        setFlagsZSP(q,1);
        goto ok;
        end;
      4:begin;
        {$ifdef debug}writeln('mul rm8');{$endif}
        if doRead(i,1,false,o) then goto vege;
        p:=readReg8(reg_al,false)*o;
        writeReg16(reg_eax,p,false);
        setFlagsZSP(p,2);
        goto ok;
        end;
      5:begin;
        {$ifdef debug}writeln('imul rm8');{$endif}
        if doRead(i,1,true,o) then goto vege;
        p:=readReg8(reg_al,true)*o;
        writeReg16(reg_eax,p,false);
        setFlagsZSP(p,2);
        goto ok;
        end;
      3:begin;
        {$ifdef debug}writeln('neg rm8');{$endif}
        if doANYm($f603,1,false,i) then goto vege;
        goto ok;
        end;
      2:begin;
        {$ifdef debug}writeln('not rm8');{$endif}
        if doANYm($f602,1,false,i) then goto vege;
        goto ok;
        end;
      0:begin;
        {$ifdef debug}writeln('test rm8,num8');{$endif}
        if doANYmi($f600,1,1,false,i) then goto vege;
        goto ok;
        end;
      end;
    end;
  $f7:begin;
    i:=getCodeInt(1,false);
    case (i shr 3) and 7 of
      6:begin;
        {$ifdef debug}writeln('div rm16');{$endif}
        p:=readReg16(reg_eax,false,extSiz);
        q:=readReg16(reg_edx,false,extSiz);
        if not extSiz then begin;
          p:=(q shl 16) or p;
          q:=0;
          end;
        if doRead(i,intSiz,false,o) then goto vege;
        result:=4;
        if (o=0) then goto vege;
        doDivU64(q,p,o);
        writeReg16(reg_eax,q,extSiz);
        writeReg16(reg_edx,p,extSiz);
        setFlagsZSP(q,intSiz);
        goto ok;
        end;
      7:begin;
        {$ifdef debug}writeln('idiv rm16');{$endif}
        p:=readReg16(reg_eax,true,extSiz);
        q:=readReg16(reg_edx,false,extSiz);
        if not extSiz then begin;
          p:=(q shl 16) or p;
          q:=0;
          end;
        if doRead(i,intSiz,true,o) then goto vege;
        result:=4;
        if (o=0) then goto vege;
        doDivS64(q,p,o);
        writeReg16(reg_eax,q,extSiz);
        writeReg16(reg_edx,p,extSiz);
        setFlagsZSP(q,intSiz);
        goto ok;
        end;
      4:begin;
        {$ifdef debug}writeln('mul rm16');{$endif}
        p:=readReg16(reg_eax,false,extSiz);
        if doRead(i,intSiz,false,o) then goto vege;
        doMulU32hl(p,o);
        if not extSiz then begin;
          p:=o shr 16;
          o:=o and $ffff;
          end;
        writeReg16(reg_eax,o,extSiz);
        writeReg16(reg_edx,p,extSiz);
        setFlagsZSP(p,intSiz);
        goto ok;
        end;
      5:begin;
        {$ifdef debug}writeln('imul rm16');{$endif}
        p:=readReg16(reg_eax,true,extSiz);
        if doRead(i,intSiz,true,o) then goto vege;
        doMulS32hl(p,o);
        if not extSiz then begin;
          p:=o shr 16;
          o:=o and $ffff;
          end;
        writeReg16(reg_eax,o,extSiz);
        writeReg16(reg_edx,p,extSiz);
        setFlagsZSP(p,intSiz);
        goto ok;
        end;
      3:begin;
        {$ifdef debug}writeln('neg rm16');{$endif}
        if doANYm($f603,intSiz,false,i) then goto vege;
        goto ok;
        end;
      2:begin;
        {$ifdef debug}writeln('not rm16');{$endif}
        if doANYm($f602,intSiz,false,i) then goto vege;
        goto ok;
        end;
      0:begin;
        {$ifdef debug}writeln('test rm16,num16');{$endif}
        if doANYmi($f600,intSiz,0,false,i) then goto vege;
        goto ok;
        end;
      end;
    end;
  $c8:begin;
    {$ifdef debug}writeln('enter num16,num8');{$endif}
    i:=getCodeInt(2,false);
    o:=getCodeInt(1,false);;
    if doENTER(i,o) then goto vege;
    goto ok;
    end;
  $f4:begin;
    {$ifdef debug}writeln('hlt');{$endif}
    goto ok;
    end;
  $6b:begin;
    {$ifdef debug}writeln('imul r16,rm,num8');{$endif}
    i:=getCodeInt(1,false);
    if doRead(i,intSiz,true,p) then goto vege;
    o:=getCodeInt(1,true);
    doMulS32hl(p,o);
    setFlagsC(p<>0);
    setFlagsO(p<>0);
    writeReg16((i shr 3) and 7,o,extSiz);
    setFlagsZSP(o,intSiz);
    goto ok;
    end;
  $69:begin;
    {$ifdef debug}writeln('imul r16,rm,num16');{$endif}
    i:=getCodeInt(1,false);
    if doRead(i,intSiz,true,p) then goto vege;
    o:=getCodeInt(intSiz,true);
    doMulS32hl(p,o);
    setFlagsC(p<>0);
    setFlagsO(p<>0);
    writeReg16((i shr 3) and 7,o,extSiz);
    setFlagsZSP(o,intSiz);
    goto ok;
    end;
  $e4:begin;
    {$ifdef debug}writeln('in al,num8');{$endif}
    i:=getCodeInt(1,false);
    writeReg8(0,readPort(i,1));
    goto ok;
    end;
  $e5:begin;
    {$ifdef debug}writeln('in ax,num8');{$endif}
    i:=getCodeInt(1,false);
    writeReg16(0,readPort(i,intSiz),extSiz);
    goto ok;
    end;
  $ec:begin;
    {$ifdef debug}writeln('in al,dx');{$endif}
    writeReg8(0,readPort(regs_d[reg_edx] and $ffff,1));
    goto ok;
    end;
  $ed:begin;
    {$ifdef debug}writeln('in ax,dx');{$endif}
    writeReg16(0,readPort(regs_d[reg_edx] and $ffff,intSiz),extSiz);
    goto ok;
    end;
  $40..$47:begin;
    {$ifdef debug}writeln('inc r16');{$endif}
    if doANYr($fe00,intSiz,true,i) then goto vege;
    goto ok;
    end;
  $6c:begin;
    {$ifdef debug}writeln('insb');{$endif}
    if doString(2,1) then goto vege;
    goto ok;
    end;
  $6d:begin;
    {$ifdef debug}writeln('insw');{$endif}
    if doString(2,intSiz) then goto vege;
    goto ok;
    end;
  $cc:begin;
    {$ifdef debug}writeln('int3');{$endif}
    goto vege;
    end;
  $cd:begin;
    {$ifdef debug}writeln('int num8');{$endif}
    i:=getCodeInt(1,false);
    goto vege;
    end;
  $ce:begin;
    {$ifdef debug}writeln('into');{$endif}
    if getFlagsO then goto vege;
    goto ok;
    end;
  $cf:begin;
    {$ifdef debug}writeln('iret');{$endif}
    if pop4stack(i,intSiz) then goto vege;
    reg_eip:=i;
    if pop4stack(i,2) then goto vege;
    regs_s[reg_cs]:=i;
    if pop4stack(i,intSiz) then goto vege;
    reg_flg:=i;
    goto ok;
    end;
  $e3:begin;
    {$ifdef debug}writeln('jcxz num8');{$endif}
    i:=getCodeInt(1,true);
    if (readReg16(reg_ecx,false,extAdr)=0) then inc(reg_eip,i);
    goto ok;
    end;
  $70..$7f:begin;
    {$ifdef debug}writeln('jcc num8');{$endif}
    o:=getCodeInt(1,true);
    if evalCond(i) then inc(reg_eip,o);
    goto ok;
    end;
  $eb:begin;
    {$ifdef debug}writeln('jmp num8');{$endif}
    i:=getCodeInt(1,true);
    inc(reg_eip,i);
    goto ok;
    end;
  $e9:begin;
    {$ifdef debug}writeln('jmp num16');{$endif}
    i:=getCodeInt(intSiz,true);
    inc(reg_eip,i);
    goto ok;
    end;
  $ea:begin;
    {$ifdef debug}writeln('jmp num16:num16');{$endif}
    i:=getCodeInt(intSiz,false);
    o:=getCodeInt(2,false);
    reg_eip:=i;
    regs_s[reg_cs]:=o;
    goto ok;
    end;
  $9f:begin;
    {$ifdef debug}writeln('lahf');{$endif}
    writeReg8(reg_ah,reg_flg);
    goto ok;
    end;
  $c5:begin;
    {$ifdef debug}writeln('lds reg16,m16');{$endif}
    if doLSS(reg_ds) then goto vege;
    goto ok;
    end;
  $c4:begin;
    {$ifdef debug}writeln('les reg16,m16');{$endif}
    if doLSS(reg_es) then goto vege;
    goto ok;
    end;
  $8d:begin;
    {$ifdef debug}writeln('lea reg16,m16');{$endif}
    i:=getCodeInt(1,false);
    decodeModeReg(i,o,p,true);
    writeReg16((i shr 3) and 7,p,extSiz);
    goto ok;
    end;
  $c9:begin;
    {$ifdef debug}writeln('leave');{$endif}
    writeReg16(reg_esp,readReg16(reg_ebp,false,protMode),protMode);
    if pop4stack(i,intSiz) then goto vege;
    writeReg16(reg_ebp,i,extSiz);
    goto ok;
    end;
  $ac:begin;
    {$ifdef debug}writeln('lodsb');{$endif}
    if doString(3,1) then goto vege;
    goto ok;
    end;
  $ad:begin;
    {$ifdef debug}writeln('lodsw');{$endif}
    if doString(3,intSiz) then goto vege;
    goto ok;
    end;
  $e2:begin;
    {$ifdef debug}writeln('loop num8');{$endif}
    i:=getCodeInt(1,true);
    o:=readReg16(reg_ecx,false,extAdr);
    dec(o);
    writeReg16(reg_ecx,o,extAdr);
    if (o=0) then goto ok;
    inc(reg_eip,i);
    goto ok;
    end;
  $e1:begin;
    {$ifdef debug}writeln('loopz num8');{$endif}
    i:=getCodeInt(1,true);
    o:=readReg16(reg_ecx,false,extAdr);
    dec(o);
    writeReg16(reg_ecx,o,extAdr);
    if (o=0) then goto ok;
    if not getFlagsZ then goto ok;
    inc(reg_eip,i);
    goto ok;
    end;
  $e0:begin;
    {$ifdef debug}writeln('loopnz num8');{$endif}
    i:=getCodeInt(1,true);
    o:=readReg16(reg_ecx,false,extAdr);
    dec(o);
    writeReg16(reg_ecx,o,extAdr);
    if (o=0) then goto ok;
    if getFlagsZ then goto ok;
    inc(reg_eip,i);
    goto ok;
    end;
  $88:begin;
    {$ifdef debug}writeln('mov rm,r8');{$endif}
    i:=getCodeInt(1,false);
    o:=readReg8((i shr 3) and 7,false);
    if doWrite(i,1,o) then goto vege;
    goto ok;
    end;
  $89:begin;
    {$ifdef debug}writeln('mov rm,r16');{$endif}
    i:=getCodeInt(1,false);
    o:=readReg16((i shr 3) and 7,false,extSiz);
    if doWrite(i,intSiz,o) then goto vege;
    goto ok;
    end;
  $8a:begin;
    {$ifdef debug}writeln('mov r8,rm');{$endif}
    i:=getCodeInt(1,false);
    if doRead(i,1,false,o) then goto vege;
    writeReg8((i shr 3) and 7,o);
    goto ok;
    end;
  $8b:begin;
    {$ifdef debug}writeln('mov r16,rm');{$endif}
    i:=getCodeInt(1,false);
    if doRead(i,intSiz,false,o) then goto vege;
    writeReg16((i shr 3) and 7,o,extSiz);
    goto ok;
    end;
  $8c:begin;
    {$ifdef debug}writeln('mov rm,sreg');{$endif}
    i:=getCodeInt(1,false);
    o:=regs_s[(i shr 3) and 7];
    if doWrite(i,intSiz,o) then goto vege;
    goto ok;
    end;
  $8e:begin;
    {$ifdef debug}writeln('mov sreg,rm');{$endif}
    i:=getCodeInt(1,false);
    if doRead(i,intSiz,false,o) then goto vege;
    regs_s[(i shr 3) and 7]:=o;
    goto ok;
    end;
  $a0:begin;
    {$ifdef debug}writeln('mov al,[ofs]');{$endif}
    if extAdr then i:=5 else i:=6;
    if doRead(i,1,false,o) then goto vege;
    writeReg8(reg_al,o);
    goto ok;
    end;
  $a1:begin;
    {$ifdef debug}writeln('mov ax,[ofs]');{$endif}
    if extAdr then i:=5 else i:=6;
    if doRead(i,intSiz,false,o) then goto vege;
    writeReg16(reg_eax,o,extSiz);
    goto ok;
    end;
  $a2:begin;
    {$ifdef debug}writeln('mov [ofs],al');{$endif}
    if extAdr then i:=5 else i:=6;
    o:=readReg8(reg_al,false);
    if doWrite(i,1,o) then goto vege;
    goto ok;
    end;
  $a3:begin;
    {$ifdef debug}writeln('mov [ofs],ax');{$endif}
    if extAdr then i:=5 else i:=6;
    o:=readReg16(reg_al,false,extSiz);
    if doWrite(i,intSiz,o) then goto vege;
    goto ok;
    end;
  $b0..$b7:begin;
    {$ifdef debug}writeln('mov r8,num8');{$endif}
    o:=getCodeInt(1,false);
    writeReg8(i and 7,o);
    goto ok;
    end;
  $b8..$bf:begin;
    {$ifdef debug}writeln('mov r16,num16');{$endif}
    o:=getCodeInt(intSiz,false);
    writeReg16(i and 7,o,extSiz);
    goto ok;
    end;
  $c6:begin;
    i:=getCodeInt(1,false);
    case (i shr 3) and 7 of
      0:begin;
        {$ifdef debug}writeln('mov rm,num8');{$endif}
        decodeModeReg(i,o,p,false);
        i:=getCodeInt(1,false);
        if WriteDecoded(o,p,1,i) then goto vege;
        goto ok;
        end;
      end;
    goto vege;
    end;
  $c7:begin;
    i:=getCodeInt(1,false);
    case (i shr 3) and 7 of
      0:begin;
        {$ifdef debug}writeln('mov rm,num16');{$endif}
        decodeModeReg(i,o,p,false);
        i:=getCodeInt(intSiz,false);
        if WriteDecoded(o,p,intSiz,i) then goto vege;
        goto ok;
        end;
      end;
    goto vege;
    end;
  $a4:begin;
    {$ifdef debug}writeln('movsb');{$endif}
    if doString(4,1) then goto vege;
    goto ok;
    end;
  $a5:begin;
    {$ifdef debug}writeln('movsw');{$endif}
    if doString(4,intSiz) then goto vege;
    goto ok;
    end;
  $90:begin;
    {$ifdef debug}writeln('nop');{$endif}
    goto ok;
    end;
  $0c:begin;
    {$ifdef debug}writeln('or al,num8');{$endif}
    if doANYmi(1,1,0,false,$c0) then goto vege;
    goto ok;
    end;
  $0d:begin;
    {$ifdef debug}writeln('or ax,num16');{$endif}
    if doANYmi(1,intSiz,0,false,$c0) then goto vege;
    goto ok;
    end;
  $08:begin;
    {$ifdef debug}writeln('or rm,r8');{$endif}
    if doANYmr(1,1,false) then goto vege;
    goto ok;
    end;
  $09:begin;
    {$ifdef debug}writeln('or rm,r16');{$endif}
    if doANYmr(1,intSiz,false) then goto vege;
    goto ok;
    end;
  $0a:begin;
    {$ifdef debug}writeln('or r8,rm');{$endif}
    if doANYrm(1,1,false) then goto vege;
    goto ok;
    end;
  $0b:begin;
    {$ifdef debug}writeln('or r16,rm');{$endif}
    if doANYrm(1,intSiz,false) then goto vege;
    goto ok;
    end;
  $e6:begin;
    {$ifdef debug}writeln('out num8,al');{$endif}
    i:=getCodeInt(1,false);
    writePort(i,1,readReg8(0,false));
    goto ok;
    end;
  $e7:begin;
    {$ifdef debug}writeln('out num8,ax');{$endif}
    i:=getCodeInt(1,false);
    writePort(i,intSiz,readReg16(0,false,extSiz));
    goto ok;
    end;
  $ee:begin;
    {$ifdef debug}writeln('out dx,al');{$endif}
    writePort(regs_d[reg_edx] and $ffff,1,readReg8(0,false));
    goto ok;
    end;
  $ef:begin;
    {$ifdef debug}writeln('out dx,ax');{$endif}
    writePort(regs_d[reg_edx] and $ffff,intSiz,readReg16(0,false,extSiz));
    goto ok;
    end;
  $6e:begin;
    {$ifdef debug}writeln('outsb');{$endif}
    if doString(5,1) then goto vege;
    goto ok;
    end;
  $6f:begin;
    {$ifdef debug}writeln('outsw');{$endif}
    if doString(5,intSiz) then goto vege;
    goto ok;
    end;
  $8f:begin;
    i:=getCodeInt(1,false);
    case (i shr 3) and 7 of
      0:begin;
        {$ifdef debug}writeln('pop rm16');{$endif}
        if pop4stack(o,intSiz) then goto vege;
        if doWrite(i,intSiz,o) then goto vege;
        goto ok;
        end;
      end;
    goto vege;
    end;
  $58..$5f:begin;
    {$ifdef debug}writeln('pop r16');{$endif}
    if pop4stack(o,intSiz) then goto vege;
    writeReg16(i and 7,o,extSiz);
    goto ok;
    end;
  $1f:begin;
    {$ifdef debug}writeln('pop ds');{$endif}
    if doPOPSR(reg_ds) then goto vege;
    goto ok;
    end;
  $07:begin;
    {$ifdef debug}writeln('pop es');{$endif}
    if doPOPSR(reg_es) then goto vege;
    goto ok;
    end;
  $17:begin;
    {$ifdef debug}writeln('pop ss');{$endif}
    if doPOPSR(reg_ss) then goto vege;
    goto ok;
    end;
  $61:begin;
    {$ifdef debug}writeln('popa');{$endif}
    for i:=7 downto 0 do begin;
      if pop4stack(o,intSiz) then goto vege;
      if (i=reg_esp) then continue;
      writeReg16(i,o,extSiz);
      end;
    goto ok;
    end;
  $9d:begin;
    {$ifdef debug}writeln('popf');{$endif}
    if pop4stack(o,intSiz) then goto vege;
    if (intSiz>2) then reg_flg:=0 else reg_flg:=reg_flg and $ffff0000;
    reg_flg:=reg_flg or o;
    goto ok;
    end;
  $50..$57:begin;
    {$ifdef debug}writeln('push r16');{$endif}
    if push2stack(regs_d[i and 7],intSiz) then goto vege;
    goto ok;
    end;
  $6a:begin;
    {$ifdef debug}writeln('push num8');{$endif}
    i:=getCodeInt(1,true);
    if push2stack(i,intSiz) then goto vege;
    goto ok;
    end;
  $68:begin;
    {$ifdef debug}writeln('push num16');{$endif}
    i:=getCodeInt(intSiz,true);
    if push2stack(i,intSiz) then goto vege;
    goto ok;
    end;
  $0e:begin;
    {$ifdef debug}writeln('push cs');{$endif}
    if doPUSHSR(reg_cs) then goto vege;
    goto ok;
    end;
  $16:begin;
    {$ifdef debug}writeln('push ss');{$endif}
    if doPUSHSR(reg_ss) then goto vege;
    goto ok;
    end;
  $1e:begin;
    {$ifdef debug}writeln('push ds');{$endif}
    if doPUSHSR(reg_ds) then goto vege;
    goto ok;
    end;
  $06:begin;
    {$ifdef debug}writeln('push es');{$endif}
    if doPUSHSR(reg_es) then goto vege;
    goto ok;
    end;
  $60:begin;
    {$ifdef debug}writeln('pusha');{$endif}
    for i:=0 to 7 do begin;
      o:=readReg16(i,false,extSiz);
      if push2stack(o,intSiz) then goto vege;
      end;
    goto ok;
    end;
  $9c:begin;
    {$ifdef debug}writeln('pushf');{$endif}
    if push2stack(reg_flg,intSiz) then goto vege;
    goto ok;
    end;
  $d0:begin;
    i:=getCodeInt(1,false);
    case (i shr 3) and 7 of
      2:begin;
        {$ifdef debug}writeln('rcl rm8,1');{$endif}
        if doANYmc($d002,1,false,i,1) then goto vege;
        goto ok;
        end;
      3:begin;
        {$ifdef debug}writeln('rcr rm8,1');{$endif}
        if doANYmc($d003,1,false,i,1) then goto vege;
        goto ok;
        end;
      0:begin;
        {$ifdef debug}writeln('rol rm8,1');{$endif}
        if doANYmc($d000,1,false,i,1) then goto vege;
        goto ok;
        end;
      1:begin;
        {$ifdef debug}writeln('ror rm8,1');{$endif}
        if doANYmc($d001,1,false,i,1) then goto vege;
        goto ok;
        end;
      4:begin;
        {$ifdef debug}writeln('shl rm8,1');{$endif}
        if doANYmc($d004,1,false,i,1) then goto vege;
        goto ok;
        end;
      5:begin;
        {$ifdef debug}writeln('shr rm8,1');{$endif}
        if doANYmc($d005,1,false,i,1) then goto vege;
        goto ok;
        end;
      7:begin;
        {$ifdef debug}writeln('sar rm8,1');{$endif}
        if doANYmc($d007,1,false,i,1) then goto vege;
        goto ok;
        end;
      end;
    goto vege;
    end;
  $d2:begin;
    i:=getCodeInt(1,false);
    case (i shr 3) and 7 of
      2:begin;
        {$ifdef debug}writeln('rcl rm8,cl');{$endif}
        if doANYmc($d002,1,false,i,regs_d[reg_ecx]) then goto vege;
        goto ok;
        end;
      3:begin;
        {$ifdef debug}writeln('rcr rm8,cl');{$endif}
        if doANYmc($d003,1,false,i,regs_d[reg_ecx]) then goto vege;
        goto ok;
        end;
      0:begin;
        {$ifdef debug}writeln('rol rm8,cl');{$endif}
        if doANYmc($d000,1,false,i,regs_d[reg_ecx]) then goto vege;
        goto ok;
        end;
      1:begin;
        {$ifdef debug}writeln('ror rm8,cl');{$endif}
        if doANYmc($d001,1,false,i,regs_d[reg_ecx]) then goto vege;
        goto ok;
        end;
      4:begin;
        {$ifdef debug}writeln('shl rm8,cl');{$endif}
        if doANYmc($d004,1,false,i,regs_d[reg_ecx]) then goto vege;
        goto ok;
        end;
      5:begin;
        {$ifdef debug}writeln('shr rm8,cl');{$endif}
        if doANYmc($d005,1,false,i,regs_d[reg_ecx]) then goto vege;
        goto ok;
        end;
      7:begin;
        {$ifdef debug}writeln('sar rm8,cl');{$endif}
        if doANYmc($d007,1,false,i,regs_d[reg_ecx]) then goto vege;
        goto ok;
        end;
      end;
    goto vege;
    end;
  $c0:begin;
    i:=getCodeInt(1,false);
    case (i shr 3) and 7 of
      2:begin;
        {$ifdef debug}writeln('rcl rm8,num8');{$endif}
        if doANYmi($d002,1,1,false,i) then goto vege;
        goto ok;
        end;
      3:begin;
        {$ifdef debug}writeln('rcr rm8,num8');{$endif}
        if doANYmi($d003,1,1,false,i) then goto vege;
        goto ok;
        end;
      0:begin;
        {$ifdef debug}writeln('rol rm8,num8');{$endif}
        if doANYmi($d000,1,1,false,i) then goto vege;
        goto ok;
        end;
      1:begin;
        {$ifdef debug}writeln('ror rm8,num8');{$endif}
        if doANYmi($d001,1,1,false,i) then goto vege;
        goto ok;
        end;
      4:begin;
        {$ifdef debug}writeln('shl rm8,num8');{$endif}
        if doANYmi($d004,1,1,false,i) then goto vege;
        goto ok;
        end;
      5:begin;
        {$ifdef debug}writeln('shr rm8,num8');{$endif}
        if doANYmi($d005,1,1,false,i) then goto vege;
        goto ok;
        end;
      7:begin;
        {$ifdef debug}writeln('sar rm8,num8');{$endif}
        if doANYmi($d007,1,1,false,i) then goto vege;
        goto ok;
        end;
      end;
    goto vege;
    end;
  $d1:begin;
    i:=getCodeInt(1,false);
    case (i shr 3) and 7 of
      2:begin;
        {$ifdef debug}writeln('rcl rm16,1');{$endif}
        if doANYmc($d002,intSiz,false,i,1) then goto vege;
        goto ok;
        end;
      3:begin;
        {$ifdef debug}writeln('rcr rm16,1');{$endif}
        if doANYmc($d003,intSiz,false,i,1) then goto vege;
        goto ok;
        end;
      0:begin;
        {$ifdef debug}writeln('rol rm16,1');{$endif}
        if doANYmc($d000,intSiz,false,i,1) then goto vege;
        goto ok;
        end;
      1:begin;
        {$ifdef debug}writeln('ror rm16,1');{$endif}
        if doANYmc($d001,intSiz,false,i,1) then goto vege;
        goto ok;
        end;
      4:begin;
        {$ifdef debug}writeln('shl rm16,1');{$endif}
        if doANYmc($d004,intSiz,false,i,1) then goto vege;
        goto ok;
        end;
      5:begin;
        {$ifdef debug}writeln('shr rm16,1');{$endif}
        if doANYmc($d005,intSiz,false,i,1) then goto vege;
        goto ok;
        end;
      7:begin;
        {$ifdef debug}writeln('sar rm16,1');{$endif}
        if doANYmc($d007,intSiz,false,i,1) then goto vege;
        goto ok;
        end;
      end;
    goto vege;
    end;
  $d3:begin;
    i:=getCodeInt(1,false);
    case (i shr 3) and 7 of
      2:begin;
        {$ifdef debug}writeln('rcl rm16,cl');{$endif}
        if doANYmc($d002,intSiz,false,i,regs_d[reg_ecx]) then goto vege;
        goto ok;
        end;
      3:begin;
        {$ifdef debug}writeln('rcr rm16,cl');{$endif}
        if doANYmc($d003,intSiz,false,i,regs_d[reg_ecx]) then goto vege;
        goto ok;
        end;
      0:begin;
        {$ifdef debug}writeln('rol rm16,cl');{$endif}
        if doANYmc($d000,intSiz,false,i,regs_d[reg_ecx]) then goto vege;
        goto ok;
        end;
      1:begin;
        {$ifdef debug}writeln('ror rm16,cl');{$endif}
        if doANYmc($d001,intSiz,false,i,regs_d[reg_ecx]) then goto vege;
        goto ok;
        end;
      4:begin;
        {$ifdef debug}writeln('shl rm16,cl');{$endif}
        if doANYmc($d004,intSiz,false,i,regs_d[reg_ecx]) then goto vege;
        goto ok;
        end;
      5:begin;
        {$ifdef debug}writeln('shr rm16,cl');{$endif}
        if doANYmc($d005,intSiz,false,i,regs_d[reg_ecx]) then goto vege;
        goto ok;
        end;
      7:begin;
        {$ifdef debug}writeln('sar rm16,cl');{$endif}
        if doANYmc($d007,intSiz,false,i,regs_d[reg_ecx]) then goto vege;
        goto ok;
        end;
      end;
    goto vege;
    end;
  $c1:begin;
    i:=getCodeInt(1,false);
    case (i shr 3) and 7 of
      2:begin;
        {$ifdef debug}writeln('rcl rm16,num8');{$endif}
        if doANYmi($d002,intSiz,1,false,i) then goto vege;
        goto ok;
        end;
      3:begin;
        {$ifdef debug}writeln('rcr rm16,num8');{$endif}
        if doANYmi($d003,intSiz,1,false,i) then goto vege;
        goto ok;
        end;
      0:begin;
        {$ifdef debug}writeln('rol rm16,num8');{$endif}
        if doANYmi($d000,intSiz,1,false,i) then goto vege;
        goto ok;
        end;
      1:begin;
        {$ifdef debug}writeln('ror rm16,num8');{$endif}
        if doANYmi($d001,intSiz,1,false,i) then goto vege;
        goto ok;
        end;
      4:begin;
        {$ifdef debug}writeln('shl rm16,num8');{$endif}
        if doANYmi($d004,intSiz,1,false,i) then goto vege;
        goto ok;
        end;
      5:begin;
        {$ifdef debug}writeln('shr rm16,num8');{$endif}
        if doANYmi($d005,intSiz,1,false,i) then goto vege;
        goto ok;
        end;
      7:begin;
        {$ifdef debug}writeln('sar rm16,num8');{$endif}
        if doANYmi($d007,intSiz,1,false,i) then goto vege;
        goto ok;
        end;
      end;
    goto vege;
    end;
  $c3:begin;
    {$ifdef debug}writeln('ret');{$endif}
    if pop4stack(i,intSiz) then goto vege;
    reg_eip:=i;
    goto ok;
    end;
  $cb:begin;
    {$ifdef debug}writeln('retf');{$endif}
    if pop4stack(i,intSiz) then goto vege;
    reg_eip:=i;
    if pop4stack(i,2) then goto vege;
    regs_s[reg_cs]:=i;
    goto ok;
    end;
  $c2:begin;
    {$ifdef debug}writeln('ret num16');{$endif}
    if pop4stack(i,intSiz) then goto vege;
    reg_eip:=i;
    inc(regs_d[reg_esp],getCodeInt(2,false));
    goto ok;
    end;
  $ca:begin;
    {$ifdef debug}writeln('retf num16');{$endif}
    if pop4stack(i,intSiz) then goto vege;
    reg_eip:=i;
    if pop4stack(i,2) then goto vege;
    regs_s[reg_cs]:=i;
    inc(regs_d[reg_esp],getCodeInt(2,false));
    goto ok;
    end;
  $9e:begin;
    {$ifdef debug}writeln('sahf');{$endif}
    i:=readReg8(reg_ah,false);
    i:=(i and $d5) or 2;
    reg_flg:=(reg_flg and $ffffff00) or i;
    goto ok;
    end;
  $1c:begin;
    {$ifdef debug}writeln('sbb al,num8');{$endif}
    if doANYmi(3,1,0,false,$c0) then goto vege;
    goto ok;
    end;
  $1d:begin;
    {$ifdef debug}writeln('sbb ax,num16');{$endif}
    if doANYmi(3,intSiz,0,false,$c0) then goto vege;
    goto ok;
    end;
  $18:begin;
    {$ifdef debug}writeln('sbb rm,r8');{$endif}
    if doANYmr(3,1,false) then goto vege;
    goto ok;
    end;
  $19:begin;
    {$ifdef debug}writeln('sbb rm,r16');{$endif}
    if doANYmr(3,intSiz,false) then goto vege;
    goto ok;
    end;
  $1a:begin;
    {$ifdef debug}writeln('sbb r8,rm');{$endif}
    if doANYrm(3,1,false) then goto vege;
    goto ok;
    end;
  $1b:begin;
    {$ifdef debug}writeln('sbb r16,rm');{$endif}
    if doANYrm(3,intSiz,false) then goto vege;
    goto ok;
    end;
  $ae:begin;
    {$ifdef debug}writeln('scasb');{$endif}
    if doString(6,1) then goto vege;
    goto ok;
    end;
  $af:begin;
    {$ifdef debug}writeln('scasw');{$endif}
    if doString(6,intSiz) then goto vege;
    goto ok;
    end;
  $f9:begin;
    {$ifdef debug}writeln('stc');{$endif}
    setFlagsC(true);
    goto ok;
    end;
  $fd:begin;
    {$ifdef debug}writeln('std');{$endif}
    setFlagsD(true);
    goto ok;
    end;
  $fb:begin;
    {$ifdef debug}writeln('sti');{$endif}
    setFlagsI(true);
    goto ok;
    end;
  $aa:begin;
    {$ifdef debug}writeln('stosb');{$endif}
    if doString(7,1) then goto vege;
    goto ok;
    end;
  $ab:begin;
    {$ifdef debug}writeln('stosw');{$endif}
    if doString(7,intSiz) then goto vege;
    goto ok;
    end;
  $2c:begin;
    {$ifdef debug}writeln('sub al,num8');{$endif}
    if doANYmi(3,1,0,false,$c0) then goto vege;
    goto ok;
    end;
  $2d:begin;
    {$ifdef debug}writeln('sub ax,num16');{$endif}
    if doANYmi(3,intSiz,0,false,$c0) then goto vege;
    goto ok;
    end;
  $28:begin;
    {$ifdef debug}writeln('sub rm,r8');{$endif}
    if doANYmr(3,1,false) then goto vege;
    goto ok;
    end;
  $29:begin;
    {$ifdef debug}writeln('sub rm,r16');{$endif}
    if doANYmr(3,intSiz,false) then goto vege;
    goto ok;
    end;
  $2a:begin;
    {$ifdef debug}writeln('sub r8,rm');{$endif}
    if doANYrm(3,1,false) then goto vege;
    goto ok;
    end;
  $2b:begin;
    {$ifdef debug}writeln('sub r16,rm');{$endif}
    if doANYrm(3,intSiz,false) then goto vege;
    goto ok;
    end;
  $a8:begin;
    {$ifdef debug}writeln('test al,num8');{$endif}
    if doANYmi($f600,1,0,false,$c0) then goto vege;
    goto ok;
    end;
  $a9:begin;
    {$ifdef debug}writeln('test ax,num16');{$endif}
    if doANYmi($f600,intSiz,0,false,$c0) then goto vege;
    goto ok;
    end;
  $84:begin;
    {$ifdef debug}writeln('test rm8,r8');{$endif}
    if doANYmr($f600,1,false) then goto vege;
    goto ok;
    end;
  $85:begin;
    {$ifdef debug}writeln('test rm16,r16');{$endif}
    if doANYmr($f600,intSiz,false) then goto vege;
    goto ok;
    end;
  $9b:begin;
    {$ifdef debug}writeln('wait');{$endif}
    goto ok;
    end;
  $91..$97:begin;
    {$ifdef debug}writeln('xchg ax,r16');{$endif}
    i:=((i and 7) shl 3) or $c0;
    if doXCHG(i,intSiz) then goto vege;
    goto ok;
    end;
  $86:begin;
    {$ifdef debug}writeln('xchg rm8,r8');{$endif}
    i:=getCodeInt(1,false);
    if doXCHG(i,1) then goto vege;
    goto ok;
    end;
  $87:begin;
    {$ifdef debug}writeln('xchg rm16,r16');{$endif}
    i:=getCodeInt(1,false);
    if doXCHG(i,intSiz) then goto vege;
    goto ok;
    end;
  $d7:begin;
    {$ifdef debug}writeln('xlat');{$endif}
    if (extSeg>=0) then i:=extSeg else i:=reg_ds;
    i:=regs_s[i] shl 4;
    inc(i,readReg16(reg_ebx,false,extAdr));
    inc(i,readReg8(reg_al,false));
    if ReadDecoded(typ_ad,i,1,false,o) then goto vege;
    writeReg8(reg_al,o);
    goto ok;
    end;
  $34:begin;
    {$ifdef debug}writeln('xor al,num8');{$endif}
    if doANYmi(6,1,0,false,$c0) then goto vege;
    goto ok;
    end;
  $35:begin;
    {$ifdef debug}writeln('xor ax,num16');{$endif}
    if doANYmi(6,intSiz,0,false,$c0) then goto vege;
    goto ok;
    end;
  $30:begin;
    {$ifdef debug}writeln('xor rm,r8');{$endif}
    if doANYmr(6,1,false) then goto vege;
    goto ok;
    end;
  $31:begin;
    {$ifdef debug}writeln('xor rm,r16');{$endif}
    if doANYmr(6,intSiz,false) then goto vege;
    goto ok;
    end;
  $32:begin;
    {$ifdef debug}writeln('xor r8,rm');{$endif}
    if doANYrm(6,1,false) then goto vege;
    goto ok;
    end;
  $33:begin;
    {$ifdef debug}writeln('xor r16,rm');{$endif}
    if doANYrm(6,intSiz,false) then goto vege;
    goto ok;
    end;
  end;
goto vege;



op0F:
i:=getCodeInt(1,false);
case i of
  $bc:begin;
    {$ifdef debug}writeln('bsf r16,rm');{$endif}
    if doANYrm($1001,intSiz,false) then goto vege;
    goto ok;
    end;
  $bd:begin;
    {$ifdef debug}writeln('bsr r16,rm');{$endif}
    if doANYrm($1002,intSiz,false) then goto vege;
    goto ok;
    end;
  $c8..$cf:begin;
    {$ifdef debug}writeln('bswap r16');{$endif}
    i:=i and 7;
    o:=readReg16(i,false,extSiz);
    if extSiz then begin; {32 bit}
      o:=((o and $ff00) shl 8) or ((o and $ff0000) shr 8) or (o shl 24) or (o shr 24);
      end else begin; {16 bit}
      o:=((o and $ff) shl 8) or ((o and $ff00) shr 8);
      end;
    writeReg16(i,o,extSiz);
    goto ok;
    end;
  $a3:begin;
    {$ifdef debug}writeln('bt rm,r16');{$endif}
    if doANYmr($0ba4,intSiz,false) then goto vege;
    goto ok;
    end;
  $bb:begin;
    {$ifdef debug}writeln('btc rm,r16');{$endif}
    if doANYmr($0ba7,intSiz,false) then goto vege;
    goto ok;
    end;
  $b3:begin;
    {$ifdef debug}writeln('btr rm,r16');{$endif}
    if doANYmr($0ba6,intSiz,false) then goto vege;
    goto ok;
    end;
  $ab:begin;
    {$ifdef debug}writeln('bts rm,r16');{$endif}
    if doANYmr($0ba5,intSiz,false) then goto vege;
    goto ok;
    end;
  $ba:begin;
    i:=getCodeInt(1,false);
    case (i shr 3) and 7 of
      4:begin;
        {$ifdef debug}writeln('bt rm,num8');{$endif}
        if doANYmi($0ba4,intSiz,1,false,i) then goto vege;
        goto ok;
        end;
      7:begin;
        {$ifdef debug}writeln('btc rm,num8');{$endif}
        if doANYmi($0ba7,intSiz,1,false,i) then goto vege;
        goto ok;
        end;
      6:begin;
        {$ifdef debug}writeln('btr rm,num8');{$endif}
        if doANYmi($0ba6,intSiz,1,false,i) then goto vege;
        goto ok;
        end;
      5:begin;
        {$ifdef debug}writeln('bts rm,num8');{$endif}
        if doANYmi($0ba5,intSiz,1,false,i) then goto vege;
        goto ok;
        end;
      end;
    goto vege;
    end;
  $40..$4f:begin;
    {$ifdef debug}writeln('cmovcc rm,r16');{$endif}
    o:=getCodeInt(1,false);
    if doRead(o,intSiz,false,p) then goto vege;
    if evalCond(i) then writeReg16((o shr 3) and 7,p,extSiz);
    goto ok;
    end;
  $b0:begin;
    {$ifdef debug}writeln('cmpxchg r8,rm');{$endif}
    i:=getCodeInt(1,false);
    if doCMPXCHG(i,1) then goto vege;
    goto ok;
    end;
  $b1:begin;
    {$ifdef debug}writeln('cmpxchg r16,rm');{$endif}
    i:=getCodeInt(1,false);
    if doCMPXCHG(i,intSiz) then goto vege;
    goto ok;
    end;
  $c7:begin;
    i:=getCodeInt(1,false);
    case (i shr 3) and 7 of
      1:begin;
        {$ifdef debug}writeln('cmpxchg8b m64');{$endif}
        decodeModeReg(i,o,p,false);
        if (o<>typ_ad) then goto vege;
        if ReadDecoded(typ_ad,p,4,false,i) then goto vege;
        if ReadDecoded(typ_ad,p+4,4,false,o) then goto vege;
        if (i<>regs_d[reg_eax]) or (o<>regs_d[reg_edx]) then begin;
          setFlagsZ(true);
          if WriteDecoded(typ_ad,p,4,regs_d[reg_ebx]) then goto vege;
          if WriteDecoded(typ_ad,p+4,4,regs_d[reg_ecx]) then goto vege;
          end else begin;
          setFlagsZ(false);
          regs_d[reg_eax]:=i;
          regs_d[reg_edx]:=o;
          end;
        goto ok;
        end;
      end;
    goto vege;
    end;
  $a2:begin;
    {$ifdef debug}writeln('cpuid');{$endif}
    case regs_d[reg_eax] of
      $00000000:begin;
        doCPUID('----i386atoremul');
        regs_d[reg_eax]:=1;
        end;
      $00000001:begin;
        doCPUID('');
        regs_d[reg_eax]:=$629;
        end;
      $80000000:begin;
        doCPUID('');
        regs_d[reg_eax]:=$80000004;
        end;
      $80000002:doCPUID('intel(r) 80386 c');
      $80000003:doCPUID('entral processin');
      $80000004:doCPUID('g unit emulator');
      else doCPUID('');
      end;
    goto ok;
    end;
  $af:begin;
    {$ifdef debug}writeln('imul r16,rm');{$endif}
    i:=getCodeInt(1,false);
    if doRead(i,intSiz,true,p) then goto vege;
    i:=(i shr 3) and 7;
    o:=readReg16(i,true,extSiz);
    doMulS32hl(p,o);
    setFlagsC(p<>0);
    setFlagsO(p<>0);
    writeReg16(i,o,extSiz);
    setFlagsZSP(o,intSiz);
    goto ok;
    end;
  $80..$8f:begin;
    {$ifdef debug}writeln('jcc num16');{$endif}
    o:=getCodeInt(intSiz,true);
    if evalCond(i) then inc(reg_eip,o);
    goto ok;
    end;
  $b2:begin;
    {$ifdef debug}writeln('lss reg16,m16');{$endif}
    if doLSS(reg_ss) then goto vege;
    goto ok;
    end;
  $b4:begin;
    {$ifdef debug}writeln('lfs reg16,m16');{$endif}
    if doLSS(reg_fs) then goto vege;
    goto ok;
    end;
  $b5:begin;
    {$ifdef debug}writeln('lgs reg16,m16');{$endif}
    if doLSS(reg_gs) then goto vege;
    goto ok;
    end;
  $01:begin;
    i:=getCodeInt(1,false);
    case (i shr 3) and 7 of
      6:begin;
        {$ifdef debug}writeln('lmsw rm16');{$endif}
        if doRead(i,2,false,p) then goto vege;
        reg_cr0:=(reg_cr0 and $ffff0000) or p;
        goto ok;
        end;
      4:begin;
        {$ifdef debug}writeln('smsw rm16');{$endif}
        if doWrite(i,intSiz,reg_cr0) then goto vege;
        goto ok;
        end;
      end;
    goto vege;
    end;
  $20:begin;
    i:=getCodeInt(1,false);
    case i and $f8 of
      $c0:begin;
        {$ifdef debug}writeln('mov r32,cr0');{$endif}
        regs_d[i and 7]:=reg_cr0;
        goto ok;
        end;
      end;
    goto vege;
    end;
  $22:begin;
    i:=getCodeInt(1,false);
    case i and $f8 of
      $c0:begin;
        {$ifdef debug}writeln('mov cr0,r32');{$endif}
        reg_cr0:=regs_d[i and 7];
        goto ok;
        end;
      end;
    goto vege;
    end;
  $be:begin;
    {$ifdef debug}writeln('movsx r16,rm8');{$endif}
    if doMOVSX(true,extSiz,1) then goto vege;
    goto ok;
    end;
  $bf:begin;
    {$ifdef debug}writeln('movsx r32,rm16');{$endif}
    if doMOVSX(true,true,2) then goto vege;
    goto ok;
    end;
  $b6:begin;
    {$ifdef debug}writeln('movzx r16,rm8');{$endif}
    if doMOVSX(false,extSiz,1) then goto vege;
    goto ok;
    end;
  $b7:begin;
    {$ifdef debug}writeln('movzx r32,rm16');{$endif}
    if doMOVSX(false,true,2) then goto vege;
    goto ok;
    end;
  $a1:begin;
    {$ifdef debug}writeln('pop fs');{$endif}
    if doPOPSR(reg_fs) then goto vege;
    goto ok;
    end;
  $a9:begin;
    {$ifdef debug}writeln('pop gs');{$endif}
    if doPOPSR(reg_gs) then goto vege;
    goto ok;
    end;
  $a0:begin;
    {$ifdef debug}writeln('push fs');{$endif}
    if doPUSHSR(reg_fs) then goto vege;
    goto ok;
    end;
  $a8:begin;
    {$ifdef debug}writeln('push gs');{$endif}
    if doPUSHSR(reg_gs) then goto vege;
    goto ok;
    end;
  $90..$9f:begin;
    {$ifdef debug}writeln('setcc rm8');{$endif}
    p:=getCodeInt(1,false);
    if evalCond(i) then o:=1 else o:=0;
    if doWrite(p,1,o) then goto vege;
    goto ok;
    end;
  $a4:begin;
    {$ifdef debug}writeln('shld rm16,r16,num8');{$endif}
    i:=getCodeInt(1,false);
    decodeModeReg(i,o,p,false);
    i:=(i shr 3) and 7;
    q:=getCodeInt(1,false);
    if doSHLD(true,o,p,i,q) then goto vege;
    goto ok;
    end;
  $a5:begin;
    {$ifdef debug}writeln('shld rm16,r16,cl');{$endif}
    i:=getCodeInt(1,false);
    decodeModeReg(i,o,p,false);
    i:=(i shr 3) and 7;
    if doSHLD(true,o,p,i,regs_d[reg_ecx]) then goto vege;
    goto ok;
    end;
  $ac:begin;
    {$ifdef debug}writeln('shrd rm16,r16,num8');{$endif}
    i:=getCodeInt(1,false);
    decodeModeReg(i,o,p,false);
    i:=(i shr 3) and 7;
    q:=getCodeInt(1,false);
    if doSHLD(false,o,p,i,q) then goto vege;
    goto ok;
    end;
  $ad:begin;
    {$ifdef debug}writeln('shrd rm16,r16,cl');{$endif}
    i:=getCodeInt(1,false);
    decodeModeReg(i,o,p,false);
    i:=(i shr 3) and 7;
    if doSHLD(false,o,p,i,regs_d[reg_ecx]) then goto vege;
    goto ok;
    end;
  $c0:begin;
    {$ifdef debug}writeln('xadd rm8,r8');{$endif}
    if doXADD(1) then goto vege;
    goto ok;
    end;
  $c1:begin;
    {$ifdef debug}writeln('xadd rm16,r16');{$endif}
    if doXADD(intSiz) then goto vege;
    goto ok;
    end;
  end;
goto vege;

ok:
result:=0;
vege:
emulateOneOpcode:=result;
End;
