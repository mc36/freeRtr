Type memoryType=array[0..1] of byte;
Var
  memoryData:^memoryType;
  memorySize:LongInt;
Var
  regs_d:array[0..7] of LongInt;
  regs_a:array[0..7] of LongInt;
  reg_pc,reg_ss,reg_sr:LongInt;

Procedure emulateRestart;
Var i:Longint;
Begin;
for i:=0 to 7 do begin;
  regs_a[i]:=0;
  regs_d[i]:=0;
  end;
reg_pc:=0;
reg_ss:=memorySize;
reg_sr:=$2000;
End;





Function emulateOneOpcode:LongInt;
Label ok,vege;
Const
  maskVal:array[-4..63] of LongInt=(0,0,0,0,
    $1,$3,$7,$f,$1f,$3f,$7f,$ff,$1ff,$3ff,$7ff,$fff,$1fff,$3fff,$7fff,$ffff,
    $1ffff,$3ffff,$7ffff,$fffff,$1fffff,$3fffff,$7fffff,$ffffff,
    $1ffffff,$3ffffff,$7ffffff,$fffffff,$1fffffff,$3fffffff,$7fffffff,$ffffffff,
    $ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,
    $ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,
    $ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,
    $ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff);
  bitVals:array[-4..63] of LongInt=(0,0,0,0,
    $1,$2,$4,$8,$10,$20,$40,$80,$100,$200,$400,$800,$1000,$2000,$4000,$8000,
    $10000,$20000,$40000,$80000,$100000,$200000,$400000,$800000,
    $1000000,$2000000,$4000000,$8000000,$10000000,$20000000,$40000000,$80000000,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  memSize1:array[0..3] of LongInt=(1,2,4,0);
  memSize2:array[0..3] of LongInt=(0,1,2,4);
  memSize3:array[0..3] of LongInt=(0,1,4,2);
  upd_nrm=0;
  upd_bef=1;
  upd_aft=2;
  typ_rd=1;
  typ_ra=2;
  typ_cn=3;
  typ_ad=4;
Var
  result:LongInt;
  orig_pc:LongInt;



Function checkRange(p:LongInt):Boolean;
Begin;
if (p>=memorySize) or (p<0) then begin;
  result:=2;
  checkRange:=True;
  exit;
  end;
checkRange:=False;
End;

Function getCodeInt(siz:LongInt;signed:Boolean):LongInt;
Var i:LongInt;
Begin;
case siz of
  1:begin;
    i:=memoryData^[reg_pc+1];
    if signed then i:=shortint(i);
    inc(reg_pc,2);
    end;
  2:begin;
    i:=ReadWordMSB(memoryData^[reg_pc]);
    if signed then i:=integer(i);
    inc(reg_pc,2);
    end;
  4:begin;
    i:=ReadLongMSB(memoryData^[reg_pc]);
    inc(reg_pc,4);
    end;
  else i:=0;
  end;
getCodeInt:=i;
End;

Procedure xchg(var a,b:LongInt);
Var i:LongInt;
Begin;
i:=a;
a:=b;
b:=i;
End;


Procedure decodeDataAddr(i:LongInt;var typ,val:LongInt);
Begin;
val:=i and 7;
if (i and 8<>0) then val:=regs_a[val] else val:=regs_d[val];
typ:=typ_ad;
End;

{$define debug_mr}
{$undef debug_mr}
Procedure decodeModeReg(mr,siz,upd:Longint;var typ,val:LongInt);
{typ: 1=reg_d, 2=reg_a, 3=const, 4=addr}
{upd: 0=normal, 1=before, 2=after}
Label f1,vege;
Var i,o,p,q:LongInt;
Begin;
q:=reg_pc;
typ:=0;
if not (siz in [1,2,4]) then siz:=2;
case (mr shr 3) and 7 of
  0:begin;
    {$ifdef debug_mr}writeln('Dn');{$endif}
    val:=mr and 7;
    typ:=typ_rd;
    goto vege;
    end;
  1:begin;
    {$ifdef debug_mr}writeln('An');{$endif}
    val:=mr and 7;
    typ:=typ_ra;
    goto vege;
    end;
  2:begin;
    {$ifdef debug_mr}writeln('(An)');{$endif}
    val:=regs_a[mr and 7];
    typ:=typ_ad;
    goto vege;
    end;
  3:begin;
    {$ifdef debug_mr}writeln('(An)+');{$endif}
    mr:=mr and 7;
    val:=regs_a[mr];
    if (upd<>upd_bef) then inc(regs_a[mr],siz);
    typ:=typ_ad;
    goto vege;
    end;
  4:begin;
    {$ifdef debug_mr}writeln('-(An)');{$endif}
    mr:=mr and 7;
    if (upd<>upd_aft) then dec(regs_a[mr],siz);
    val:=regs_a[mr];
    typ:=typ_ad;
    goto vege;
    end;
  5:begin;
    {$ifdef debug_mr}writeln('(num16,An)');{$endif}
    val:=getCodeInt(2,true);
    inc(val,regs_a[mr and 7]);
    typ:=typ_ad;
    goto vege;
    end;
  6:begin;
    {$ifdef debug_mr}write('An extended: ');{$endif}
    p:=regs_a[mr and 7];
    goto f1;
    end;
  7:case mr and 7 of
    0:begin;
      {$ifdef debug_mr}writeln('(num16)');{$endif}
      val:=getCodeInt(2,true);
      typ:=typ_ad;
      goto vege;
      end;
    1:begin;
      {$ifdef debug_mr}writeln('(num32)');{$endif}
      val:=getCodeInt(4,true);
      typ:=typ_ad;
      goto vege;
      end;
    4:begin;
      {$ifdef debug_mr}writeln('num16');{$endif}
      val:=getCodeInt(siz,false);
      typ:=typ_cn;
      goto vege;
      end;
    2:begin;
      {$ifdef debug_mr}writeln('(num16,pc)');{$endif}
      val:=getCodeInt(2,true);
      inc(val,orig_pc);
      typ:=typ_ad;
      goto vege;
      end;
    3:begin;
      {$ifdef debug_mr}write('pc extended: ');{$endif}
      p:=orig_pc;
      goto f1;
      end;
    end;
  end;
{$ifdef debug_mr}writeln('???');{$endif}
goto vege;
f1:
mr:=getCodeInt(2,false);
if (mr and $100=0) then begin;
  {$ifdef debug_mr}writeln('(num8,base,reg.size*scale)');{$endif}
  val:=shortint(mr);
  inc(val,p);
  p:=(mr shr 12) and 7;
  if (mr and $8000=0) then p:=regs_d[p] else p:=regs_a[p];
  if (mr and $800=0) then p:=integer(p);
  p:=p shl ((mr shr 9) and 3);
  inc(val,p);
  typ:=typ_ad;
  goto vege;
  end;
if (mr and $80=0) then val:=p else val:=0;
if (mr and $40=0) then begin;
  p:=(mr shr 12) and 7;
  if (mr and $8000=0) then p:=regs_d[p] else p:=regs_a[p];
  if (mr and $800=0) then p:=integer(p);
  o:=p shl ((mr shr 9) and 3);
  end else o:=0;
case (mr shr 4) and 3 of
  0,1:;
  2:inc(val,getCodeInt(2,true));
  3:inc(val,getCodeInt(4,true));
  end;
if (mr and 7=0) then begin;
  {$ifdef debug_mr}writeln('(num,Zbase,Zreg.size*scale)');{$endif}
  inc(val,o);
  typ:=typ_ad;
  goto vege;
  end;
case mr and 3 of
  0,1:;
  2:p:=getCodeInt(2,true);
  3:p:=getCodeInt(4,true);
  end;
if (mr and $40<>0) then begin;
  {$ifdef debug_mr}writeln('([Zbase,num],num)');{$endif}
  if checkRange(val) then goto vege;
  i:=ReadLongMSB(memoryData^[val]);
  val:=i+p;
  typ:=typ_ad;
  goto vege;
  end;
if (mr and 4=0) then begin;
  {$ifdef debug_mr}writeln('([Zbase,num,Zreg.size*scale],num)');{$endif}
  inc(val,o);
  end else begin;
  {$ifdef debug_mr}writeln('([Zbase,num],Zreg.size*scale,num)');{$endif}
  inc(p,o);
  end;
if checkRange(val) then goto vege;
i:=ReadLongMSB(memoryData^[val]);
val:=i+p;
typ:=typ_ad;
vege:
if (upd=upd_bef) then reg_pc:=q;
exit;
End;


Function ReadDecoded(typ,val,siz:LongInt;signed:Boolean;var data:LongInt):Boolean;
Begin;
ReadDecoded:=True;
case typ of
  1:data:=regs_d[val];
  2:data:=regs_a[val];
  3:data:=val;
  4:begin;
    if checkRange(val) then begin; result:=2;exit; end;
    case siz of
      1:data:=memoryData^[val];
      2:data:=ReadWordMSB(memoryData^[val]);
      4:data:=ReadLongMSB(memoryData^[val]);
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
    1:regs_d[val]:=(regs_d[val] and $ffffff00) or (data and $ff);
    2:regs_d[val]:=(regs_d[val] and $ffff0000) or (data and $ffff);
    4:regs_d[val]:=data;
    else begin; result:=1;exit; end;
    end;
  2:case siz of
    1:regs_a[val]:=(regs_a[val] and $ffffff00) or (data and $ff);
    2:regs_a[val]:=(regs_a[val] and $ffff0000) or (data and $ffff);
    4:regs_a[val]:=data;
    else begin; result:=1;exit; end;
    end;
  3:begin; result:=1;exit; end;
  4:begin;
    if checkRange(val) then begin; result:=2;exit; end;
    case siz of
      1:memoryData^[val]:=data;
      2:WriteWordMSB(memoryData^[val],data);
      4:WriteLongMSB(memoryData^[val],data);
      else begin; result:=1;exit; end;
      end;
    end;
  else begin; result:=1;exit; end;
  end;
WriteDecoded:=False;
End;


Function doRead(mr,siz,upd:LongInt;signed:Boolean;var data:LongInt):Boolean;
Var typ,val:LongInt;
Begin;
decodeModeReg(mr,siz,upd,typ,val);
doRead:=ReadDecoded(typ,val,siz,signed,data);
End;

Function doWrite(mr,siz,upd:LongInt;data:LongInt):Boolean;
Var typ,val:LongInt;
Begin;
decodeModeReg(mr,siz,upd,typ,val);
doWrite:=WriteDecoded(typ,val,siz,data);
End;


Procedure setFlagNZVC(a,b,siz:LongInt);
Var c,d:LongInt;
Begin;
c:=a+b;
case siz of
  1:begin;
    a:=(a shl 8) and $ff00;
    b:=(b shl 8) and $ff00;
    c:=c and $ff;
    end;
  2:begin;
    a:=a and $ffff;
    b:=b and $ffff;
    c:=c and $ffff;
    end;
  4:begin;
    a:=(((a and $ffff)+(b and $ffff)) shr 16)+(a shr 16);
    b:=(b shr 16);
    end;
  else exit;
  end;
d:=a+b;
reg_sr:=reg_sr and $fffffff0;
if (d>$ffff) then reg_sr:=reg_sr or 1;
if (a and $8000<>d and $8000) then reg_sr:=reg_sr or 2;
if (c=0) then reg_sr:=reg_sr or 4;
if (d and $8000<>0) then reg_sr:=reg_sr or 8;
End;

Procedure setFlagNZVB(a,b,siz:LongInt);
Var c,d:LongInt;
Begin;
c:=a-b;
case siz of
  1:begin;
    a:=(a shl 8) and $ff00;
    b:=(b shl 8) and $ff00;
    c:=c and $ff;
    end;
  2:begin;
    a:=a and $ffff;
    b:=b and $ffff;
    c:=c and $ffff;
    end;
  4:begin;
    a:=(a shr 16)+(((a and $ffff)-(b and $ffff)) shr 16);
    b:=(b shr 16);
    end;
  else exit;
  end;
d:=a-b;
reg_sr:=reg_sr and $fffffff0;
if (d<0) then reg_sr:=reg_sr or 1;
if ((a and $8000) xor (b and $8000)<>d and $8000) then reg_sr:=reg_sr or 2;
if (c=0) then reg_sr:=reg_sr or 4;
if (d and $8000<>0) then reg_sr:=reg_sr or 8;
End;

Procedure setFlagC(v:Boolean);
Begin;
if v then reg_sr:=reg_sr or 1 else reg_sr:=reg_sr and $fffffffe;
End;

Procedure setFlagV(v:Boolean);
Begin;
if v then reg_sr:=reg_sr or 2 else reg_sr:=reg_sr and $fffffffd;
End;

Procedure setFlagZ(v:Boolean);
Begin;
if v then reg_sr:=reg_sr or 4 else reg_sr:=reg_sr and $fffffffb;
End;

Procedure setFlagN(v:Boolean);
Begin;
if v then reg_sr:=reg_sr or 8 else reg_sr:=reg_sr and $fffffff7;
End;

Procedure setFlagX(v:Boolean);
Begin;
if v then reg_sr:=reg_sr or $10 else reg_sr:=reg_sr and $ffffffef;
End;

Procedure setFlagXC;
Begin;
if (reg_sr and 1<>0) then reg_sr:=reg_sr or $10 else reg_sr:=reg_sr and $ffffffef;
End;

Function getFlagX:LongInt;
Begin;
if (reg_sr and $10<>0) then getFlagX:=1 else getFlagX:=0;
End;


Function evalCond(i:Longint):Boolean;
Var b,n,z,v,c:Boolean;
Begin;
n:=(reg_sr and 8<>0);
z:=(reg_sr and 4<>0);
v:=(reg_sr and 2<>0);
c:=(reg_sr and 1<>0);
case i and $f of
  0:b:=true;                    {t}
  1:b:=false;                   {f}
  2:b:=(not c) and (not z);     {hi}
  3:b:=c or v;                  {ls}
  4:b:=not c;                   {cc}
  5:b:=c;                       {cs}
  6:b:=not z;                   {ne}
  7:b:=z;                       {eq}
  8:b:=not v;                   {vc}
  9:b:=v;                       {vs}
  10:b:=not n;                  {pl}
  11:b:=n;                      {mi}
  12:b:=z or (n and v) or ((not n) and (not v));                        {ge}
  13:b:=(n and (not v)) or ((not n) and v);                             {lt}
  14:b:=(n and v and (not v)) or ((not n) and (not v) and (not v));     {gt}
  15:b:=z or (n and (not v)) or ((not n) and v);                        {le}
  else b:=false;
  end;
evalCond:=b;
End;


function push2stack(dat,siz:LongInt):Boolean;
Begin;
push2stack:=True;
dec(reg_ss,siz);
if checkRange(reg_ss) then exit;
case siz of
  1:memoryData^[reg_ss]:=dat;
  2:WriteWordMSB(memoryData^[reg_ss],dat);
  4:WriteLongMSB(memoryData^[reg_ss],dat);
  else exit;
  end;
push2stack:=False;
End;

Function pop4stack(var dat:LongInt;siz:LongInt):Boolean;
Begin;
pop4stack:=True;
if checkRange(reg_ss) then exit;
case siz of
  1:dat:=memoryData^[reg_ss];
  2:dat:=ReadWordMSB(memoryData^[reg_ss]);
  4:dat:=ReadLongMSB(memoryData^[reg_ss]);
  else exit;
  end;
inc(reg_ss,siz);
pop4stack:=False;
End;


Procedure begSHT(var i,o,p,q:LongInt);
Begin;
if (i and $20=0) then begin;
  o:=(i shr 9) and 7;
  if (o=0) then o:=8;
  end else begin;
  o:=regs_d[(i shr 9) and 7];
  end;
p:=memSize1[(i shr 6) and 3];
q:=i and 7;
End;

Procedure doASR(var dat:LongInt;siz,cnt:LongInt);
Var
  old:Longint;
  i,o:LongInt;
Begin;
cnt:=cnt and $3f;
if (cnt=0) then begin; setFlagNZVC(dat,dat,siz);setFlagXC;exit; end;
old:=dat;
siz:=(siz*8)-1;
dat:=dat shr cnt;
if (old and BitVals[siz]<>0) then begin;
  o:=maskVal[siz-cnt];
  i:=maskVal[siz]-o;
  dat:=(dat and o) or i;
  end;
if (old and bitVals[cnt-1]<>0) then i:=$ff else i:=0;
setFlagNZVC(i,i,1);
setFlagXC;
End;

Procedure doASL(var dat:LongInt;siz,cnt:LongInt);
Var
  old:Longint;
  i,o:LongInt;
Begin;
cnt:=cnt and $3f;
if (cnt=0) then begin; setFlagNZVC(dat,dat,siz);setFlagXC;exit; end;
old:=dat;
siz:=(siz*8)-1;
dat:=dat shl cnt;
if (old and bitVals[siz-cnt+1]<>0) then i:=$ff else i:=0;
setFlagNZVC(i,i,1);
setFlagXC;
End;

Procedure doLSR(var dat:LongInt;siz,cnt:LongInt);
Var
  old:Longint;
  i,o:LongInt;
Begin;
cnt:=cnt and $3f;
if (cnt=0) then begin; setFlagNZVC(dat,dat,siz);setFlagXC;exit; end;
old:=dat;
dat:=dat shr cnt;
if (old and bitVals[cnt-1]<>0) then i:=$ff else i:=0;
setFlagNZVC(i,i,1);
setFlagXC;
End;

Procedure doLSL(var dat:LongInt;siz,cnt:LongInt);
Var
  old:Longint;
  i,o:LongInt;
Begin;
cnt:=cnt and $3f;
if (cnt=0) then begin; setFlagNZVC(dat,dat,siz);setFlagXC;exit; end;
old:=dat;
dat:=dat shl cnt;
if (old and bitVals[cnt-1]<>0) then i:=$ff else i:=0;
setFlagNZVC(i,i,1);
setFlagXC;
End;

Procedure doROR(var dat:LongInt;siz,cnt:LongInt);
Var
  old:Longint;
  i,o:LongInt;
Begin;
cnt:=cnt and $3f;
if (cnt=0) then begin; setFlagNZVC(dat,dat,siz);setFlagXC;exit; end;
old:=dat;
dat:=rotateRight(dat,siz*8,cnt);
if (old and bitVals[cnt-1]<>0) then i:=$ff else i:=0;
setFlagNZVC(i,i,1);
setFlagXC;
End;

Procedure doROL(var dat:LongInt;siz,cnt:LongInt);
Var
  old:Longint;
  i,o:LongInt;
Begin;
cnt:=cnt and $3f;
if (cnt=0) then begin; setFlagNZVC(dat,dat,siz);setFlagXC;exit; end;
old:=dat;
dat:=rotateLeft(dat,siz*8,cnt);
if (old and bitVals[cnt-1]<>0) then i:=$ff else i:=0;
setFlagNZVC(i,i,1);
setFlagXC;
End;

Procedure doROXR(var dat:LongInt;siz,cnt:LongInt);
Var
  old:Longint;
  i,o:LongInt;
Begin;
cnt:=cnt and $3f;
if (cnt=0) then begin; setFlagNZVC(dat,dat,siz);setFlagXC;exit; end;
old:=dat;
dat:=getFlagX+(dat shl 1);
dat:=rotateRight(dat,(siz*8)+1,cnt);
setFlagX(dat and 1<>0);
dat:=dat shr 1;
if (old and bitVals[cnt-1]<>0) then i:=$ff else i:=0;
setFlagNZVC(i,i,1);
End;

Procedure doROXL(var dat:LongInt;siz,cnt:LongInt);
Var
  old:Longint;
  i,o:LongInt;
Begin;
cnt:=cnt and $3f;
if (cnt=0) then begin; setFlagNZVC(dat,dat,siz);setFlagXC;exit; end;
old:=dat;
dat:=getFlagX+(dat shl 1);
dat:=rotateLeft(dat,(siz*8)+1,cnt);
setFlagX(dat and 1<>0);
dat:=dat shr 1;
if (old and bitVals[cnt-1]<>0) then i:=$ff else i:=0;
setFlagNZVC(i,i,1);
End;


Function doBCHG(bn,mr:LongInt):Boolean;
Var i,o:LongInt;
Begin;
doBCHG:=True;
if doRead(mr,4,upd_bef,false,i) then exit;
o:=bitVals[bn and $1f];
setFlagZ(i and o=0);
i:=i xor o;
if doWrite(mr,4,upd_aft,i) then exit;
doBCHG:=False;
End;

Function doBCLR(bn,mr:LongInt):Boolean;
Var i,o:LongInt;
Begin;
doBCLR:=True;
if doRead(mr,4,upd_bef,false,i) then exit;
o:=bitVals[bn and $1f];
setFlagZ(i and o=0);
i:=(not o) and i;
if doWrite(mr,4,upd_aft,i) then exit;
doBCLR:=False;
End;

Function doBSET(bn,mr:LongInt):Boolean;
Var i,o:LongInt;
Begin;
doBSET:=True;
if doRead(mr,4,upd_bef,false,i) then exit;
o:=bitVals[bn and $1f];
setFlagZ(i and o=0);
i:=i or o;
if doWrite(mr,4,upd_aft,i) then exit;
doBSET:=False;
End;

Function doBTST(bn,mr:LongInt):Boolean;
Var i,o:LongInt;
Begin;
doBTST:=True;
if doRead(mr,4,upd_nrm,false,i) then exit;
o:=bitVals[bn and $1f];
setFlagZ(i and o=0);
doBTST:=False;
End;

Procedure decodeBitField(var ofs,wdt,reg:LongInt);
Var i:LongInt;
Begin;
i:=getCodeInt(2,false);
reg:=(i shr 12) and 7;
if (i and $20=0) then wdt:=i and $1f else wdt:=regs_d[i and 7];
i:=i shr 6;
if (i and $20=0) then ofs:=i and $1f else ofs:=regs_d[i and 7];
End;

Function doBFCHG(mr,ofs,wdt:LongInt):Boolean;
Var i,o:LongInt;
Begin;
doBFCHG:=True;
ofs:=ofs and $1f;
wdt:=wdt and $1f;
if doRead(mr,4,upd_bef,false,i) then exit;
o:=bitVals[ofs+wdt-1];
setFlagN(i and o<>0);
o:=maskVal[ofs+wdt-1]-maskVal[ofs-1];
setFlagZ(i and o=0);
i:=i xor o;
setFlagV(false);
setFlagC(false);
if doWrite(mr,4,upd_aft,i) then exit;
doBFCHG:=False;
End;

Function doBFCLR(mr,ofs,wdt:LongInt):Boolean;
Var i,o:LongInt;
Begin;
doBFCLR:=True;
ofs:=ofs and $1f;
wdt:=wdt and $1f;
if doRead(mr,4,upd_bef,false,i) then exit;
o:=bitVals[ofs+wdt-1];
setFlagN(i and o<>0);
o:=maskVal[ofs+wdt-1]-maskVal[ofs-1];
setFlagZ(i and o=0);
i:=(not o) and i;
setFlagV(false);
setFlagC(false);
if doWrite(mr,4,upd_aft,i) then exit;
doBFCLR:=False;
End;

Function doBFSET(mr,ofs,wdt:LongInt):Boolean;
Var i,o:LongInt;
Begin;
doBFSET:=True;
ofs:=ofs and $1f;
wdt:=wdt and $1f;
if doRead(mr,4,upd_bef,false,i) then exit;
o:=bitVals[ofs+wdt-1];
setFlagN(i and o<>0);
o:=maskVal[ofs+wdt-1]-maskVal[ofs-1];
setFlagZ(i and o=0);
i:=i or o;
setFlagV(false);
setFlagC(false);
if doWrite(mr,4,upd_aft,i) then exit;
doBFSET:=False;
End;

Function doBFTST(mr,ofs,wdt:LongInt):Boolean;
Var i,o:LongInt;
Begin;
doBFTST:=True;
ofs:=ofs and $1f;
wdt:=wdt and $1f;
if doRead(mr,4,upd_nrm,false,i) then exit;
o:=bitVals[ofs+wdt-1];
setFlagN(i and o<>0);
o:=maskVal[ofs+wdt-1]-maskVal[ofs-1];
setFlagZ(i and o=0);
setFlagV(false);
setFlagC(false);
doBFTST:=False;
End;

Function doBFEXT(mr,ofs,wdt:LongInt;var dat:LongInt;sign:Boolean):Boolean;
Var i,o:LongInt;
Begin;
doBFEXT:=True;
ofs:=ofs and $1f;
wdt:=wdt and $1f;
if doRead(mr,4,upd_nrm,false,i) then exit;
o:=bitVals[ofs+wdt-1];
setFlagN(i and o<>0);
o:=maskVal[ofs+wdt-1]-maskVal[ofs-1];
setFlagZ(i and o=0);
setFlagV(false);
setFlagC(false);
dat:=(i and o) shr ofs;
i:=bitVals[wdt-1];
if sign then if (dat and i<>0) then dat:=(not maskVal[wdt-1]) or dat;
doBFEXT:=False;
End;

Function doBFFFO(mr,ofs,wdt:LongInt;var dat:LongInt;sign:Boolean):Boolean;
Label f1;
Var i,o:LongInt;
Begin;
doBFFFO:=True;
ofs:=ofs and $1f;
wdt:=wdt and $1f;
if doRead(mr,4,upd_nrm,false,i) then exit;
o:=bitVals[ofs+wdt-1];
setFlagN(i and o<>0);
o:=maskVal[ofs+wdt-1]-maskVal[ofs-1];
setFlagZ(i and o=0);
setFlagV(false);
setFlagC(false);
o:=(i and o) shr ofs;
for i:=ofs+wdt-1 downto ofs do if (bitVals[i] and o<>0) then goto f1;
i:=ofs+wdt;
f1:
dat:=i;
doBFFFO:=False;
End;

Function doBFINS(mr,ofs,wdt:LongInt;dat:LongInt):Boolean;
Var i,o:LongInt;
Begin;
doBFINS:=True;
ofs:=ofs and $1f;
wdt:=wdt and $1f;
if doRead(mr,4,upd_bef,false,i) then exit;
o:=bitVals[ofs+wdt-1];
setFlagN(i and o<>0);
o:=maskVal[ofs+wdt-1]-maskVal[ofs-1];
setFlagZ(i and o=0);
setFlagV(false);
setFlagC(false);
dat:=(dat shl ofs) and o;
i:=((not o) and i) or dat;
if doWrite(mr,4,upd_aft,i) then exit;
doBFINS:=False;
End;

Function doCAS(dc,du,ea,siz:LongInt):Boolean;
Var i,o,eav:LongInt;
Begin;
doCAS:=True;
dc:=dc and 7;
du:=du and 7;
if doRead(ea,siz,upd_bef,true,eav) then exit;
if (eav=regs_d[dc]) then eav:=regs_d[du] else if doWrite(dc,siz,upd_nrm,eav) then exit;
if doWrite(ea,siz,upd_aft,eav) then exit;
doCAS:=False;
End;

Function doCAS2(dc1,dc2,du1,du2,a1,a2,siz:LongInt):Boolean;
Var i,o,t1,t2,v1,v2:LongInt;
Begin;
doCAS2:=True;
dc1:=dc1 and 7;
du1:=du1 and 7;
dc2:=dc2 and 7;
du2:=du2 and 7;
decodeDataAddr(a1,t1,a1);
decodeDataAddr(a2,t2,a2);
if ReadDecoded(t1,a1,siz,true,v1) then exit;
if ReadDecoded(t2,a2,siz,true,v2) then exit;
if (regs_d[dc1]=v1) and (regs_d[dc2]=v2) then begin;
  if WriteDecoded(t1,a1,siz,regs_d[du1]) then exit;
  if WriteDecoded(t2,a2,siz,regs_d[du2]) then exit;
  end else begin;
  if doWrite(dc1,siz,upd_nrm,v1) then exit;
  if doWrite(dc2,siz,upd_nrm,v2) then exit;
  end;
doCAS2:=False;
End;

Function doCMP2(reg,ea,siz:LongInt):Boolean;
Var i,o,p:LongInt;
Begin;
doCMP2:=True;
decodeDataAddr(reg,i,o);
if ReadDecoded(i,o,siz,true,reg) then exit;
decodeModeReg(ea,siz,upd_nrm,i,ea);
if (i<>typ_ad) then exit;
ReadDecoded(typ_ad,ea,siz,true,i);
o:=siz;
if (o<2) then o:=2;
inc(ea,o);
ReadDecoded(typ_ad,ea,siz,true,o);
setFlagZ((reg=i) or (reg=o));
setFlagC((reg<i) or (reg>o));
doCMP2:=False;
End;

Function doLINK(reg,num:LongInt):Boolean;
Begin;
doLINK:=True;
reg:=reg and 7;
if push2stack(regs_a[reg],4) then exit;
inc(reg_ss,num);
doLINK:=False;
End;

Function doMOVE16(sr,tr,sa,ta:LongInt):Boolean;
Begin;
doMOVE16:=True;
if (sr>=0) then sa:=regs_a[sr];
if (tr>=0) then ta:=regs_a[tr];
if checkRange(sa) then exit;
if checkRange(ta) then exit;
move(memoryData^[sa],memoryData^[ta],16);
if (sr>=0) then inc(regs_a[sr],16);
if (tr>=0) then inc(regs_a[tr],16);
doMOVE16:=False;
End;

Function doMOVEM(ea,siz,list:LongInt;read:Boolean):Boolean;
Var i,o,r,q,p,s:LongInt;
Begin;
doMOVEM:=True;
decodeModeReg(ea,siz,upd_bef,i,p);
if (i<>typ_ad) then exit;
if checkRange(p) then exit;
s:=0;
for q:=0 to 15 do begin;
  if (list and 1=0) then continue;
  decodeDataAddr(q,i,o);
  if read then begin;
    if ReadDecoded(i,o,siz,false,r) then exit;
    if WriteDecoded(typ_ad,p,siz,r) then exit;
    end else begin;
    if ReadDecoded(typ_ad,p,siz,false,i) then exit;
    if WriteDecoded(i,o,siz,r) then exit;
    end;
  inc(p,siz);
  inc(s,siz);
  list:=list shr 1;
  end;
decodeModeReg(ea,s,upd_aft,i,p);
doMOVEM:=False;
End;

Procedure doMOVEP(adr,siz:LongInt;var dat:LongInt;read:Boolean);
Var i,o,p,q:LongInt;
Begin;
for q:=siz downto 0 do begin;
  p:=q*8;
  o:=$ff shl p;
  if read then begin;
    i:=memoryData^[adr];
    dat:=(not o) and dat;
    dat:=(i shl o) or dat;
    end else begin;
    i:=(dat shr p) and $ff;
    memoryData^[adr]:=i;
    end;
  inc(adr,2);
  end;
End;




Var i,o,p,q,r,s:LongInt;
Begin;
result:=1;

if checkRange(reg_pc) then goto vege;
i:=getCodeInt(2,false);
orig_pc:=reg_pc;

case i of
  $4e75:begin;
    {$ifdef debug}writeln('rts');{$endif}
    if pop4stack(o,4) then goto vege;
    reg_pc:=o;
    goto ok;
    end;
  $4e77:begin;
    {$ifdef debug}writeln('rtr');{$endif}
    if pop4stack(i,2) then goto vege;
    if pop4stack(o,4) then goto vege;
    reg_pc:=o;
    reg_sr:=(reg_sr and $ffffffe0) or (i and $1f);
    goto ok;
    end;
  $4e74:begin;
    {$ifdef debug}writeln('rtd num');{$endif}
    if pop4stack(o,4) then goto vege;
    i:=getCodeInt(2,false);
    reg_pc:=o;
    inc(reg_ss,i);
    goto ok;
    end;
  $003c:begin;
    {$ifdef debug}writeln('ori num8,ccr');{$endif}
    i:=getCodeInt(1,false);
    reg_sr:=(reg_sr or i);
    goto ok;
    end;
  $4e71:begin;
    {$ifdef debug}writeln('nop');{$endif}
    goto ok;
    end;
  $0a3c:begin;
    {$ifdef debug}writeln('eori num8,ccr');{$endif}
    i:=getCodeInt(1,false);
    reg_sr:=reg_sr xor i;
    goto ok;
    end;
  $023c:begin;
    {$ifdef debug}writeln('andi num8,ccr');{$endif}
    i:=getCodeInt(1,false);
    reg_sr:=(reg_sr and $ffffff00) or (reg_sr and i);
    goto ok;
    end;
  end;

if (i and $fdff=$0cfc) then begin;
  {$ifdef debug}writeln('cas2 Dc1:Dc2,Du1:Du2,Rn1:Rn2');{$endif}
  if (i and $200<>0) then p:=4 else p:=2;
  i:=getCodeInt(2,false);
  o:=getCodeInt(2,false);
  doCAS2(i,o,i shr 6,o shr 6,i shr 12,o shr 12,p);
  goto ok;
  end;

case i and $fffc of
  $4ac0:begin;
    {$ifdef debug}writeln('tas ea');{$endif}
    if doRead(i,1,upd_bef,true,o) then goto vege;
    setFlagNZVC(0,o,1);
    o:=o or $80;
    if doWrite(i,1,upd_aft,o) then goto vege;
    goto ok;
    end;
  $42c0:begin;
    {$ifdef debug}writeln('move ccr,ea');{$endif}
    if doWrite(i,2,upd_nrm,reg_sr and $1f) then goto vege;
    goto ok;
    end;
  $44c0:begin;
    {$ifdef debug}writeln('move ea,ccr');{$endif}
    if doRead(i,2,upd_nrm,false,p) then goto vege;
    reg_sr:=(reg_sr and $ffffffe0) or (p and $1f);
    goto ok;
    end;
  $40c0:begin;
    {$ifdef debug}writeln('move sr,ea');{$endif}
    if doWrite(i,2,upd_nrm,reg_sr) then goto vege;
    goto ok;
    end;
  end;

case i and $fff8 of
  $4e58:begin;
    {$ifdef debug}writeln('unlk An');{$endif}
    i:=i and 7;
    reg_ss:=regs_a[i];
    if pop4stack(o,4) then goto vege;
    regs_a[i]:=o;
    goto ok;
    end;
  $4840:begin;
    {$ifdef debug}writeln('swap Dn');{$endif}
    i:=i and 7;
    o:=regs_d[i];
    o:=(o shl 16) or (o shr 16);
    regs_d[i]:=o;
    goto ok;
    end;
  $f620:begin;
    {$ifdef debug}writeln('move16 (Ax)+,(Ay)+');{$endif}
    o:=getCodeInt(2,false);
    i:=i and 7;
    o:=(o shr 12) and 7;
    if doMOVE16(i,o,0,0) then goto vege;
    goto ok;
    end;
  $4e50:begin;
    {$ifdef debug}writeln('link num16,An');{$endif}
    if doLINK(i,getCodeInt(2,true)) then goto vege;
    goto ok;
    end;
  $4808:begin;
    {$ifdef debug}writeln('link num32,An');{$endif}
    if doLINK(i,getCodeInt(4,true)) then goto vege;
    goto ok;
    end;
  end;

if (i and $ffe0=$f600) then begin;
  {$ifdef debug}writeln('move16 (Ax),(num32)');{$endif}
  o:=getCodeInt(4,false);
  p:=i and 7;
  case (i shr 3) and 3 of
    0:if doMOVE16(p,-1,-1,o) then goto vege;
    1:if doMOVE16(-1,p,o,-1) then goto vege;
    2:if doMOVE16(-1,-1,regs_a[p],o) then goto vege;
    3:if doMOVE16(-1,-1,o,regs_a[p]) then goto vege;
    end;
  goto ok;
  end;

case i and $f1f0 of
  $8100:begin;
    if (i and 8=0) then begin
      {$ifdef debug}writeln('sbcd Dn,Dn');{$endif}
      o:=(i shr 9) and 7;
      i:=i and 7;
      end else begin;
      {$ifdef debug}writeln('sbcd -(An),-(An)');{$endif}
      o:=((i shr 9) and 7) or $20;
      i:=(i and 7) or $20
      end;
    if doRead(i,1,upd_nrm,false,i) then goto vege;
    if doRead(o,1,upd_bef,false,p) then goto vege;
    i:=((i shr 4)*10)+(i and $f);
    p:=((p shr 4)*10)+(p and $f)-getFlagX-i;
    setFlagNZVB(0,p,1);
    setFlagC(p<0);
    setFlagXC;
    i:=p mod 100;
    p:=((i div 10) shl 4) or (i mod 10);
    if doWrite(o,1,upd_aft,p) then goto vege;
    goto ok;
    end;
  $c100:begin;
    if (i and 8=0) then begin
      {$ifdef debug}writeln('abcd Dn,Dn');{$endif}
      o:=(i shr 9) and 7;
      i:=i and 7;
      end else begin;
      {$ifdef debug}writeln('abcd -(An),-(An)');{$endif}
      o:=((i shr 9) and 7) or $20;
      i:=(i and 7) or $20
      end;
    if doRead(i,1,upd_nrm,false,i) then goto vege;
    if doRead(o,1,upd_bef,false,p) then goto vege;
    i:=((i shr 4)*10)+(i and $f);
    p:=((p shr 4)*10)+(p and $f)+i;
    setFlagNZVC(0,p,1);
    setFlagC(p>99);
    setFlagXC;
    i:=p mod 100;
    p:=((i div 10) shl 4) or (i mod 10);
    if doWrite(o,1,upd_aft,p) then goto vege;
    goto ok;
    end;
  $8140:begin;
    if (i and 8=0) then begin
      {$ifdef debug}writeln('pack Dn,Dn,num');{$endif}
      o:=(i shr 9) and 7;
      i:=i and 7;
      end else begin;
      {$ifdef debug}writeln('pack -(An),-(An),num');{$endif}
      o:=((i shr 9) and 7) or $20;
      i:=(i and 7) or $20
      end;
    if doRead(i,2,upd_nrm,false,o) then goto vege;
    inc(o,getCodeInt(2,false));
    o:=((o and $f00) shr 4) or (o and $f);
    if doWrite(o,1,upd_nrm,o) then goto vege;
    goto ok;
    end;
  $8180:begin;
    if (i and 8=0) then begin
      {$ifdef debug}writeln('unpk Dn,Dn,num');{$endif}
      o:=(i shr 9) and 7;
      i:=i and 7;
      end else begin;
      {$ifdef debug}writeln('unpk -(An),-(An),num');{$endif}
      o:=((i shr 9) and 7) or $20;
      i:=(i and 7) or $20
      end;
    if doRead(i,2,upd_nrm,false,o) then goto vege;
    o:=((o and $f0) shl 4) or (o and $f);
    inc(o,getCodeInt(2,false));
    if doWrite(o,1,upd_nrm,o) then goto vege;
    goto ok;
    end;
  end;

case i and $ffc0 of
  $4840:begin;
    {$ifdef debug}writeln('pea ea');{$endif}
    decodeModeReg(i,4,upd_nrm,o,p);
    if (o<>typ_ad) then goto vege;
    if push2stack(p,4) then goto vege;
    goto ok;
    end;
  $4800:begin;
    {$ifdef debug}writeln('nbcd ea');{$endif}
    if doRead(i,1,upd_bef,false,o) then goto vege;
    o:=getFlagX+((o shr 4)*10)+(o and $f);
    setFlagNZVB(0,o,1);
    setFlagC(o>99);
    setFlagXC;
    o:=(100-o) mod 100;
    o:=((o div 10) shl 4) or (o mod 10);
    if doWrite(i,1,upd_aft,o) then goto vege;
    goto ok;
    end;
  $4c00:begin;
    {$ifdef debug}writeln('muls.l ea,Dh,Dl');{$endif}
    if doRead(i,4,upd_nrm,true,p) then goto vege;
    o:=getCodeInt(2,false);
    r:=(o shr 12) and 7;
    q:=regs_d[r];
    doMulS32hl(q,p);
    regs_d[r]:=p;
    if (o and $400<>0) then regs_d[o and 7]:=q;
    setFlagN(q<0);
    setFlagZ((q=0) and (p=0));
    setFlagV(q<>0);
    setFlagC(false);
    goto ok;
    end;
  $4e80:begin;
    {$ifdef debug}writeln('jsr ea');{$endif}
    decodeModeReg(i,4,upd_nrm,o,p);
    if (o<>typ_ad) then goto vege;
    if push2stack(reg_pc,4) then goto vege;
    reg_pc:=p;
    goto ok;
    end;
  $4ec0:begin;
    {$ifdef debug}writeln('jmp ea');{$endif}
    decodeModeReg(i,4,upd_nrm,o,p);
    if (o<>typ_ad) then goto vege;
    reg_pc:=p;
    goto ok;
    end;
  $4c40:begin;
    {$ifdef debug}writeln('divs.l ea,Dr:Dq');{$endif}
    setFlagC(false);
    if doRead(i,4,upd_nrm,true,o) then goto vege;
    if (o=0) then begin; result:=4;goto vege; end;
    i:=getCodeInt(2,false);
    q:=(i shr 12) and 7;
    r:=i and 7;
    p:=regs_d[r];
    i:=p div o;
    regs_d[q]:=i;
    if (q<>r) then regs_d[r]:=p mod o;
    setFlagNZVC(i,0,4);
    goto ok;
    end;
  $eac0:begin;
    {$ifdef debug}writeln('bfchg ea{ofs,width}');{$endif}
    decodeBitField(p,q,r);
    if doBFCHG(i,p,q) then goto vege;
    goto ok;
    end;
  $ecc0:begin;
    {$ifdef debug}writeln('bfclr ea{ofs,width}');{$endif}
    decodeBitField(p,q,r);
    if doBFCLR(i,p,q) then goto vege;
    goto ok;
    end;
  $ebc0:begin;
    {$ifdef debug}writeln('bfexts ea{ofs,width},Dn');{$endif}
    decodeBitField(p,q,r);
    if doBFEXT(i,p,q,i,true) then goto vege;
    regs_d[r]:=i;
    goto ok;
    end;
  $e9c0:begin;
    {$ifdef debug}writeln('bfextu ea{ofs,width},Dn');{$endif}
    decodeBitField(p,q,r);
    if doBFEXT(i,p,q,i,false) then goto vege;
    regs_d[r]:=i;
    goto ok;
    end;
  $edc0:begin;
    {$ifdef debug}writeln('bfffo ea{ofs,width},Dn');{$endif}
    decodeBitField(p,q,r);
    if doBFFFO(i,p,q,i,false) then goto vege;
    regs_d[r]:=i;
    goto ok;
    end;
  $efc0:begin;
    {$ifdef debug}writeln('bfins ea{ofs,width},Dn');{$endif}
    decodeBitField(p,q,r);
    if doBFINS(i,p,q,regs_d[r]) then goto vege;
    goto ok;
    end;
  $eec0:begin;
    {$ifdef debug}writeln('bfset ea{ofs,width}');{$endif}
    decodeBitField(p,q,r);
    if doBFSET(i,p,q) then goto vege;
    goto ok;
    end;
  $e8c0:begin;
    {$ifdef debug}writeln('bftst ea{ofs,width}');{$endif}
    decodeBitField(p,q,r);
    if doBFTST(i,p,q) then goto vege;
    goto ok;
    end;
  $0840:begin;
    {$ifdef debug}writeln('bchg num,ea');{$endif}
    if doBCHG(getCodeInt(2,false),i) then goto vege;
    goto ok;
    end;
  $0880:begin;
    {$ifdef debug}writeln('bclr num,ea');{$endif}
    if doBCLR(getCodeInt(2,false),i) then goto vege;
    goto ok;
    end;
  $08c0:begin;
    {$ifdef debug}writeln('bset num,ea');{$endif}
    if doBSET(getCodeInt(2,false),i) then goto vege;
    goto ok;
    end;
  $0800:begin;
    {$ifdef debug}writeln('btst num,ea');{$endif}
    if doBTST(getCodeInt(2,false),i) then goto vege;
    goto ok;
    end;
  end;

if (i and $fb80=$4880) then begin;
  {$ifdef debug}writeln('movem list,ea');{$endif}
  if (i and $40<>0) then p:=4 else p:=2;
  o:=getCodeInt(2,false);
  if doMOVEM(i,p,o,i and $400<>0) then goto vege;
  goto ok;
  end;

if (i and $fe38=$4800) then begin;
  {$ifdef debug}writeln('ext Dn');{$endif}
  case (i shr 6) and 7 of
    2:begin; p:=1;q:=2; end;
    3:begin; p:=2;q:=4; end;
    7:begin; p:=1;q:=4; end;
    else goto vege;
    end;
  i:=i and 7;
  if doRead(i,p,upd_bef,true,o) then goto vege;
  if doWrite(i,q,upd_aft,o) then goto vege;
  goto ok;
  end;

case i and $fec0 of
  $e4c0:begin;
    if doRead(i,2,upd_bef,true,r) then goto vege;
    if (i and $100=0) then begin;
      {$ifdef debug}writeln('roxr ae');{$endif}
      doROXR(r,2,1);
      end else begin;
      {$ifdef debug}writeln('roxl ae');{$endif}
      doROXL(r,2,1);
      end;
    if doWrite(i,2,upd_aft,r) then goto vege;
    goto ok;
    end;
  $e6c0:begin;
    if doRead(i,2,upd_bef,true,r) then goto vege;
    if (i and $100=0) then begin;
      {$ifdef debug}writeln('ror ae');{$endif}
      doROR(r,2,1);
      end else begin;
      {$ifdef debug}writeln('rol ae');{$endif}
      doROL(r,2,1);
      end;
    if doWrite(i,2,upd_aft,r) then goto vege;
    goto ok;
    end;
  $e2c0:begin;
    if doRead(i,2,upd_bef,true,r) then goto vege;
    if (i and $100=0) then begin;
      {$ifdef debug}writeln('lsr ae');{$endif}
      doLSR(r,2,1);
      end else begin;
      {$ifdef debug}writeln('lsl ae');{$endif}
      doLSL(r,2,1);
      end;
    if doWrite(i,2,upd_aft,r) then goto vege;
    goto ok;
    end;
  $e0c0:begin;
    if doRead(i,2,upd_bef,true,r) then goto vege;
    if (i and $100=0) then begin;
      {$ifdef debug}writeln('asr ae');{$endif}
      doASR(r,2,1);
      end else begin;
      {$ifdef debug}writeln('asl ae');{$endif}
      doASL(r,2,1);
      end;
    if doWrite(i,2,upd_aft,r) then goto vege;
    goto ok;
    end;
  end;

if (i and $f130=$c100) then begin;
  o:=(i shr 9) and 7;
  p:=i and 7;
  case (i shr 3) and $1f of
    8:begin;
      {$ifdef debug}writeln('exg Dn,Dn');{$endif}
      xchg(regs_d[o],regs_d[p]);
      goto ok;
      end;
    9:begin;
      {$ifdef debug}writeln('exg An,An');{$endif}
      xchg(regs_a[o],regs_a[p]);
      goto ok;
      end;
    17:begin;
      {$ifdef debug}writeln('exg Dn,Dn;exg An,An');{$endif}
      xchg(regs_d[o],regs_d[p]);
      xchg(regs_a[o],regs_a[p]);
      goto ok;
      end;
    end;
  end;

if (i and $f0f8=$50c8) then begin;
  {$ifdef debug}writeln('dbcc Dn,ofs');{$endif}
  o:=getCodeInt(2,true);
  if evalCond(i shr 8) then goto ok;
  i:=i and 7;
  if doRead(i,2,upd_nrm,true,r) then goto vege;
  dec(r);
  if doWrite(i,2,upd_nrm,r) then goto vege;
  if (r<>-1) then reg_pc:=orig_pc+o;
  goto ok;
  end;

case i and $ff00 of
  $4a00:begin;
    {$ifdef debug}writeln('tst ea');{$endif}
    p:=memSize1[(i shr 6) and 3];
    if doRead(i,p,upd_nrm,true,q) then goto vege;
    setFlagNZVC(q,0,p);
    goto ok;
    end;
  $0400:begin;
    {$ifdef debug}writeln('subi num,ea');{$endif}
    p:=memSize1[(i shr 6) and 3];
    o:=i and $3f;
    i:=getCodeInt(p,true);
    if doRead(o,p,upd_bef,true,q) then goto vege;
    setFlagNZVB(q,i,p);
    setFlagXC;
    dec(q,i);
    if doWrite(o,p,upd_aft,q) then goto vege;
    goto ok;
    end;
  $0000:begin;
    {$ifdef debug}writeln('ori num,ea');{$endif}
    p:=memSize1[(i shr 6) and 3];
    o:=i and $3f;
    i:=getCodeInt(p,false);
    if doRead(o,p,upd_bef,false,q) then goto vege;
    q:=q or i;
    setFlagNZVC(q,0,p);
    if doWrite(o,p,upd_aft,q) then goto vege;
    goto ok;
    end;
  $4600:begin;
    {$ifdef debug}writeln('not ea');{$endif}
    p:=memSize1[(i shr 6) and 3];
    if doRead(i,p,upd_bef,true,r) then goto vege;
    r:=not r;
    setFlagNZVC(r,0,p);
    if doWrite(i,p,upd_aft,r) then goto vege;
    goto ok;
    end;
  $4000:begin;
    {$ifdef debug}writeln('negx ea');{$endif}
    p:=memSize1[(i shr 6) and 3];
    if doRead(i,p,upd_bef,true,r) then goto vege;
    inc(r,getFlagX);
    setFlagNZVB(0,r,p);
    setFlagXC;
    r:=-r;
    if doWrite(i,p,upd_aft,r) then goto vege;
    goto ok;
    end;
  $4400:begin;
    {$ifdef debug}writeln('neg ea');{$endif}
    p:=memSize1[(i shr 6) and 3];
    if doRead(i,p,upd_bef,true,r) then goto vege;
    setFlagNZVB(0,r,p);
    setFlagXC;
    r:=-r;
    if doWrite(i,p,upd_aft,r) then goto vege;
    goto ok;
    end;
  $0a00:begin;
    {$ifdef debug}writeln('eori num,ea');{$endif}
    p:=memSize1[(i shr 6) and 3];
    o:=i and $3f;
    i:=getCodeInt(p,false);
    if doRead(o,p,upd_bef,false,q) then goto vege;
    q:=q xor i;
    setFlagNZVC(q,0,p);
    if doWrite(o,p,upd_aft,q) then goto vege;
    goto ok;
    end;
  $0c00:begin;
    {$ifdef debug}writeln('cmpi num,ea');{$endif}
    p:=memSize1[(i shr 6) and 3];
    o:=i and $3f;
    i:=getCodeInt(p,true);
    if doRead(o,p,upd_nrm,true,q) then goto vege;
    setFlagNZVB(q,i,p);
    goto ok;
    end;
  $4200:begin;
    {$ifdef debug}writeln('clr ea');{$endif}
    p:=memSize1[(i shr 6) and 3];
    if doWrite(i,p,upd_nrm,0) then goto vege;
    setFlagN(false);
    setFlagZ(true);
    setFlagV(false);
    setFlagC(false);
    goto ok;
    end;
  $6000:begin;
    {$ifdef debug}writeln('bra ofs');{$endif}
    o:=i and $ff;
    o:=shortint(o);
    if (o=0) then o:=getCodeInt(2,true) else if (o=-1) then o:=getCodeInt(4,true);
    reg_pc:=orig_pc+o;
    goto ok;
    end;
  $6100:begin;
    {$ifdef debug}writeln('bsr ofs');{$endif}
    o:=i and $ff;
    o:=shortint(o);
    if (o=0) then o:=getCodeInt(2,true) else if (o=-1) then o:=getCodeInt(4,true);
    if push2stack(reg_pc,4) then goto vege;
    reg_pc:=orig_pc+o;
    goto ok;
    end;
  $0600:begin;
    {$ifdef debug}writeln('addi num,ea');{$endif}
    p:=memSize1[(i shr 6) and 3];
    o:=i and $3f;
    i:=getCodeInt(p,true);
    if doRead(o,p,upd_bef,true,q) then goto vege;
    setFlagNZVC(i,q,p);
    setFlagXC;
    inc(q,i);
    if doWrite(o,p,upd_aft,q) then goto vege;
    goto ok;
    end;
  $0200:begin;
    {$ifdef debug}writeln('andi num,ea');{$endif}
    p:=memSize1[(i shr 6) and 3];
    o:=i and $3f;
    i:=getCodeInt(p,false);
    if doRead(o,p,upd_bef,false,q) then goto vege;
    q:=q and i;
    setFlagNZVC(q,0,p);
    if doWrite(o,p,upd_aft,q) then goto vege;
    goto ok;
    end;
  end;

case i and $f9c0 of
  $08c0:begin;
    {$ifdef debug}writeln('cas Dc,Du,ea');{$endif}
    p:=memSize2[(i shr 9) and 3];
    o:=getCodeInt(2,false);
    doCAS(o and 7,(o shr 6) and 7,i,p);
    goto ok;
    end;
  $00c0:begin;
    {$ifdef debug}writeln('cmp2 ea,Rn');{$endif}
    p:=memSize2[(i shr 9) and 3];
    o:=getCodeInt(2,false);
    doCMP2(o shr 12,i,p);
    goto ok;
    end;
  end;

if (i and $f138=$b108) then begin;
  {$ifdef debug}writeln('cmpm (ay)+,(ax)+');{$endif}
  p:=memSize1[(i shr 6) and 3];
  if doRead((i and 7) or $18,p,upd_nrm,true,o) then goto vege;
  if doRead(((i shr 9) and 7) or $18,p,upd_nrm,true,q) then goto vege;
  setFlagNZVB(q,o,p);
  goto ok;
  end;

if (i and $f1c8=$0108) then begin;
  {$ifdef debug}writeln('movep (num16,An),Dn');{$endif}
  o:=getCodeInt(2,true);
  inc(o,regs_a[i and 7]);
  if checkRange(o) then goto vege;
  p:=(i shr 9) and 7;
  case (i shr 6) and 3 of
    0:doMOVEP(o,1,regs_d[p],true);
    1:doMOVEP(o,3,regs_d[p],true);
    2:doMOVEP(o,1,regs_d[p],false);
    3:doMOVEP(o,3,regs_d[p],false);
    end;
  goto ok;
  end;

case i and $f018 of
  $e010:begin;
    begSHT(i,o,p,q);
    if doRead(q,p,upd_bef,false,r) then goto vege;
    if (i and $100=0) then begin;
      {$ifdef debug}writeln('roxr any,Dn');{$endif}
      doROXR(r,p,o);
      end else begin;
      {$ifdef debug}writeln('roxl any,Dn');{$endif}
      doROXL(r,p,o);
      end;
    if doWrite(q,p,upd_aft,r) then goto vege;
    goto ok;
    end;
  $e018:begin;
    begSHT(i,o,p,q);
    if doRead(q,p,upd_bef,false,r) then goto vege;
    if (i and $100=0) then begin;
      {$ifdef debug}writeln('ror any,Dn');{$endif}
      doROR(r,p,o);
      end else begin;
      {$ifdef debug}writeln('rol any,Dn');{$endif}
      doROL(r,p,o);
      end;
    if doWrite(q,p,upd_aft,r) then goto vege;
    goto ok;
    end;
  $e008:begin;
    begSHT(i,o,p,q);
    if doRead(q,p,upd_bef,false,r) then goto vege;
    if (i and $100=0) then begin;
      {$ifdef debug}writeln('lsr any,Dn');{$endif}
      doLSR(r,p,o);
      end else begin;
      {$ifdef debug}writeln('lsl any,Dn');{$endif}
      doLSL(r,p,o);
      end;
    if doWrite(q,p,upd_aft,r) then goto vege;
    goto ok;
    end;
  $e000:begin;
    begSHT(i,o,p,q);
    if doRead(q,p,upd_bef,true,r) then goto vege;
    if (i and $100=0) then begin;
      {$ifdef debug}writeln('asr any,Dn');{$endif}
      doASR(r,p,o);
      end else begin;
      {$ifdef debug}writeln('asl any,Dn');{$endif}
      doASL(r,p,o);
      end;
    if doWrite(q,p,upd_aft,r) then goto vege;
    goto ok;
    end;
  end;

case i and $f1c0 of
  $c0c0:begin;
    {$ifdef debug}writeln('mulu.w ea,Dn');{$endif}
    if doRead(i,2,upd_nrm,false,o) then goto vege;
    i:=(i shr 9) and 7;
    p:=regs_d[i] and $ffff;
    o:=p*o;
    regs_d[i]:=o;
    setFlagNZVC(o,0,4);
    goto ok;
    end;
  $c1c0:begin;
    {$ifdef debug}writeln('muls.w ea,Dn');{$endif}
    if doRead(i,2,upd_nrm,true,o) then goto vege;
    i:=(i shr 9) and 7;
    p:=regs_d[i];
    p:=integer(p);
    o:=p*o;
    regs_d[i]:=o;
    setFlagNZVC(o,0,4);
    goto ok;
    end;
  $41c0:begin;
    {$ifdef debug}writeln('lea ea,An');{$endif}
    decodeModeReg(i,4,upd_nrm,o,p);
    if (o<>typ_ad) then goto vege;
    regs_a[(i shr 9) and 7]:=p;
    goto ok;
    end;
  $80c0:begin;
    {$ifdef debug}writeln('divu.w ea,Dn');{$endif}
    setFlagC(false);
    if doRead(i,2,upd_nrm,false,o) then goto vege;
    q:=(i shr 9) and 7;
    p:=regs_d[q];
    if (o=0) then begin; result:=4;goto vege; end;
    i:=p div o;
    if (i and $ffff0000<>0) then begin; setFlagV(true);goto vege; end;
    o:=p mod o;
    p:=(o shl 16) or (i and $ffff);
    regs_d[q]:=p;
    setFlagNZVC(i,0,2);
    goto ok;
    end;
  $81c0:begin;
    {$ifdef debug}writeln('divs.w ea,Dn');{$endif}
    setFlagC(false);
    if doRead(i,2,upd_nrm,true,o) then goto vege;
    q:=(i shr 9) and 7;
    p:=regs_d[q];
    if (o=0) then begin; result:=4;goto vege; end;
    i:=p div o;
    if (i<-32768) or (i>=32768) then begin; setFlagV(true);goto vege; end;
    o:=p mod o;
    p:=(o shl 16) or (i and $ffff);
    regs_d[q]:=p;
    setFlagNZVC(i,0,2);
    goto ok;
    end;
  $0140:begin;
    {$ifdef debug}writeln('bchg Dn,ea');{$endif}
    if doBCHG(regs_d[(i shr 9) and 7],i) then goto vege;
    goto ok;
    end;
  $0180:begin;
    {$ifdef debug}writeln('bclr Dn,ea');{$endif}
    if doBCLR(regs_d[(i shr 9) and 7],i) then goto vege;
    goto ok;
    end;
  $01c0:begin;
    {$ifdef debug}writeln('bset Dn,ea');{$endif}
    if doBSET(regs_d[(i shr 9) and 7],i) then goto vege;
    goto ok;
    end;
  $0100:begin;
    {$ifdef debug}writeln('btst Dn,ea');{$endif}
    if doBTST(regs_d[(i shr 9) and 7],i) then goto vege;
    goto ok;
    end;
  end;

if (i and $e1c0=$2040) then begin;
  {$ifdef debug}writeln('movea ea,An');{$endif}
  if (i and $1000<>0) then p:=2 else p:=4;
  if doRead(i,p,upd_nrm,true,o) then goto vege;
  regs_a[(i shr 9) and 7]:=o;
  goto ok;
  end;

case i and $f0c0 of
  $90c0:begin;
    {$ifdef debug}writeln('suba ea,An');{$endif}
    if (i and $100=0) then p:=2 else p:=4;
    if doRead(i,p,upd_nrm,true,o) then goto vege;
    i:=(i shr 9) and 7;
    setFlagNZVB(regs_a[i],o,4);
    setFlagXC;
    dec(regs_a[i],o);
    goto ok;
    end;
  $50c0:begin;
    {$ifdef debug}writeln('scc ea');{$endif}
    if evalCond(i shr 8) then o:=$ff else o:=0;
    if doWrite(i,1,upd_nrm,o) then goto vege;
    goto ok;
    end;
  $d0c0:begin;
    {$ifdef debug}writeln('adda ea,An');{$endif}
    if (i and $100=0) then p:=2 else p:=4;
    if doRead(i,p,upd_nrm,true,o) then goto vege;
    i:=(i shr 9) and 7;
    setFlagNZVC(regs_a[i],o,4);
    setFlagXC;
    inc(regs_a[i],o);
    goto ok;
    end;
  $b0c0:begin;
    {$ifdef debug}writeln('cmpa ea,Dn');{$endif}
    if (i and $100=0) then p:=2 else p:=4;
    if doRead(i,p,upd_nrm,true,o) then goto vege;
    q:=regs_a[(i shr 9) and 7];
    setFlagNZVB(q,o,4);
    goto ok;
    end;
  end;

case i and $f130 of
  $9100:begin;
    p:=memSize1[(i shr 6) and 3];
    if (i and 8=0) then begin
      {$ifdef debug}writeln('subx Dn,Dn');{$endif}
      o:=(i shr 9) and 7;
      i:=i and 7;
      end else begin;
      {$ifdef debug}writeln('subx -(An),-(An)');{$endif}
      o:=((i shr 9) and 7) or $20;
      i:=(i and 7) or $20
      end;
    if doRead(i,p,upd_nrm,true,i) then goto vege;
    inc(i,getFlagX);
    if doRead(o,p,upd_bef,true,q) then goto vege;
    setFlagNZVB(q,i,p);
    setFlagXC;
    dec(q,i);
    if doWrite(o,p,upd_aft,q) then goto vege;
    goto ok;
    end;
  $d100:begin;
    p:=memSize1[(i shr 6) and 3];
    if (i and 8=0) then begin
      {$ifdef debug}writeln('addx Dn,Dn');{$endif}
      o:=(i shr 9) and 7;
      i:=i and 7;
      end else begin;
      {$ifdef debug}writeln('addx -(An),-(An)');{$endif}
      o:=((i shr 9) and 7) or $20;
      i:=(i and 7) or $20
      end;
    if doRead(i,p,upd_nrm,true,i) then goto vege;
    inc(i,getFlagX);
    if doRead(o,p,upd_bef,true,q) then goto vege;
    setFlagNZVC(q,i,p);
    setFlagXC;
    inc(q,i);
    if doWrite(o,p,upd_aft,q) then goto vege;
    goto ok;
    end;
  end;

if (i and $f100=$7000) then begin;
  {$ifdef debug}writeln('moveq num,Dn');{$endif}
  o:=(i shr 9) and 7;
  i:=shortint(i);
  regs_d[o]:=i;
  setFlagNZVC(i,0,4);
  goto ok;
  end;

case i and $f100 of
  $5100:begin;
    {$ifdef debug}writeln('subq num,ea');{$endif}
    p:=memSize1[(i shr 6) and 3];
    o:=i and $3f;
    i:=(i shr 9) and 7;
    if (i=0) then i:=8;
    if doRead(o,p,upd_bef,true,q) then goto vege;
    setFlagNZVB(q,i,p);
    setFlagXC;
    dec(q,i);
    if doWrite(o,p,upd_aft,q) then goto vege;
    goto ok;
    end;
  $5000:begin;
    {$ifdef debug}writeln('addq num,ea');{$endif}
    p:=memSize1[(i shr 6) and 3];
    o:=i and $3f;
    i:=(i shr 9) and 7;
    if (i=0) then i:=8;
    if doRead(o,p,upd_bef,true,q) then goto vege;
    setFlagNZVC(i,q,p);
    setFlagXC;
    inc(q,i);
    if doWrite(o,p,upd_aft,q) then goto vege;
    goto ok;
    end;
  $b100:begin;
    {$ifdef debug}writeln('eor Dn,ea');{$endif}
    p:=memSize1[(i shr 6) and 3];
    if doRead(i,p,upd_bef,true,o) then goto vege;
    o:=o xor regs_d[(i shr 9) and 7];
    setFlagNZVC(o,0,p);
    if doWrite(i,p,upd_aft,o) then goto vege;
    goto ok;
    end;
  end;

case i and $f000 of
  $9000:begin;
    p:=memSize1[(i shr 6) and 3];
    if (i and $100=0) then begin;
      {$ifdef debug}writeln('sub ea,Dn');{$endif}
      o:=(i shr 9) and 7;
      i:=i and $3f;
      end else begin;
      {$ifdef debug}writeln('sub Dn,ea');{$endif}
      o:=i and $3f;
      i:=(i shr 9) and 7;
      end;
    if doRead(i,p,upd_nrm,true,i) then goto vege;
    if doRead(o,p,upd_bef,true,q) then goto vege;
    setFlagNZVB(q,i,p);
    setFlagXC;
    dec(q,i);
    if doWrite(o,p,upd_aft,q) then goto vege;
    goto ok;
    end;
  $8000:begin;
    p:=memSize1[(i shr 6) and 3];
    if (i and $100=0) then begin;
      {$ifdef debug}writeln('or ea,Dn');{$endif}
      o:=(i shr 9) and 7;
      i:=i and $3f;
      end else begin;
      {$ifdef debug}writeln('or Dn,ea');{$endif}
      o:=i and $3f;
      i:=(i shr 9) and 7;
      end;
    if doRead(i,p,upd_nrm,false,i) then goto vege;
    if doRead(o,p,upd_bef,false,q) then goto vege;
    q:=q or i;
    setFlagNZVC(q,0,p);
    if doWrite(o,p,upd_aft,q) then goto vege;
    goto ok;
    end;
  $b000:begin;
    {$ifdef debug}writeln('cmp ea,Dn');{$endif}
    p:=memSize1[(i shr 6) and 3];
    if doRead(i,p,upd_nrm,true,o) then goto vege;
    q:=regs_d[(i shr 9) and 7];
    setFlagNZVB(q,o,p);
    goto ok;
    end;
  $6000:begin;
    {$ifdef debug}writeln('bcc ofs');{$endif}
    o:=i and $ff;
    o:=shortint(o);
    if (o=0) then o:=getCodeInt(2,true) else if (o=-1) then o:=getCodeInt(4,true);
    if evalCond(i shr 8) then reg_pc:=orig_pc+o;
    goto ok;
    end;
  $d000:begin;
    p:=memSize1[(i shr 6) and 3];
    if (i and $100=0) then begin;
      {$ifdef debug}writeln('add ea,Dn');{$endif}
      o:=(i shr 9) and 7;
      i:=i and $3f;
      end else begin;
      {$ifdef debug}writeln('add Dn,ea');{$endif}
      o:=i and $3f;
      i:=(i shr 9) and 7;
      end;
    if doRead(i,p,upd_nrm,true,i) then goto vege;
    if doRead(o,p,upd_bef,true,q) then goto vege;
    setFlagNZVC(i,q,p);
    setFlagXC;
    inc(q,i);
    if doWrite(o,p,upd_aft,q) then goto vege;
    goto ok;
    end;
  $c000:begin;
    p:=memSize1[(i shr 6) and 3];
    if (i and $100=0) then begin;
      {$ifdef debug}writeln('and ea,Dn');{$endif}
      o:=(i shr 9) and 7;
      i:=i and $3f;
      end else begin;
      {$ifdef debug}writeln('and Dn,ea');{$endif}
      o:=i and $3f;
      i:=(i shr 9) and 7;
      end;
    if doRead(i,p,upd_nrm,false,i) then goto vege;
    if doRead(o,p,upd_bef,false,q) then goto vege;
    q:=q and i;
    setFlagNZVC(q,0,p);
    if doWrite(o,p,upd_aft,q) then goto vege;
    goto ok;
    end;
  end;

if (i and $c000=$0000) then begin;
  {$ifdef debug}writeln('move ea,ea');{$endif}
  p:=memSize3[(i shr 12) and 3];
  if doRead(i,p,upd_nrm,false,o) then goto vege;
  i:=(i shr 6) and $3f;
  i:=(i shl 3) or (i shr 3);
  if doWrite(i,p,upd_nrm,o) then goto vege;
  setFlagNZVC(o,0,p);
  goto ok;
  end;

goto vege;

ok:
result:=0;
vege:
emulateOneOpcode:=result;
End;
