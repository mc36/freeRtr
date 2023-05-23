Type memoryType=array[0..1] of byte;
Var
  memoryData:^memoryType;
  memorySize:LongInt;
Var
  regs:array[0..31] of LongInt;
  reg_msr,reg_cr,reg_xer,reg_lr,reg_ctr,reg_iar,reg_res:LongInt;

Procedure emulateRestart;
Var i:Longint;
Begin;
for i:=0 to 31 do regs[i]:=0;
reg_msr:=$40;
reg_res:=-1;
reg_cr:=0;
reg_xer:=0;
reg_lr:=0;
reg_ctr:=0;
reg_iar:=0;
End;



Function emulateOneOpcode:LongInt;
Label ok,vege;
Const
  bits:array[0..31] of LongInt=($80000000,$40000000,$20000000,$10000000,
    $8000000,$4000000,$2000000,$1000000,$800000,$400000,$200000,$100000,
    $80000,$40000,$20000,$10000,$8000,$4000,$2000,$1000,
    $800,$400,$200,$100,$80,$40,$20,$10,$8,$4,$2,$1);
Var result:LongInt;

Function checkRange(p:LongInt):Boolean;
Begin;
if (p>=memorySize) or (p<0) then begin;
  result:=2;
  checkRange:=True;
  exit;
  end;
checkRange:=False;
End;

Procedure updateLGES(r:LongInt);
Begin;
reg_cr:=reg_cr and $0fffffff;
if (r>0) then reg_cr:=reg_cr or $40000000 else
 if (r<0) then reg_cr:=reg_cr or $80000000 else reg_cr:=reg_cr or $20000000;
if (reg_xer and $80000000<>0) then reg_cr:=reg_cr or $10000000;
End;

Procedure updateSO(a,b:LongInt);
Begin;
a:=(((a and $ffff)+(b and $ffff)) shr 16)+(a shr 16)+(b shr 16);
if ((a and $8000) shr 15<>a shr 16) then reg_xer:=reg_xer or $c0000000 else reg_xer:=reg_xer and $b0000000;
End;

Procedure updateC(a,b:LongInt);
Begin;
a:=(((a and $ffff)+(b and $ffff)) shr 16)+(a shr 16)+(b shr 16);
if (a and $f0000<>0) then reg_xer:=reg_xer or $20000000 else reg_xer:=reg_xer and $d0000000;
End;

Function evalCond(bo,bi:LongInt):Boolean;
Var c1,c2:Boolean;
Begin;
bi:=bi and $1f;
if (bo and 4=0) then begin;
  dec(reg_ctr);
  c1:=(reg_ctr<>0) xor (bo and 2<>0);
  end else c1:=true;
if (bo and 16=0) then begin;
  c2:=(reg_cr and bits[bi]<>0) xor (bo and 8=0);
  end else c2:=true;
evalCond:=c1 and c2;
End;

Procedure doComp(a,b,c:LongInt);
Var i:LongInt;
Begin;
c:=(c*(-4))+28;
if (a>b) then i:=4 else if (a<b) then i:=8 else i:=2;
if (reg_xer and $80000000<>0) then i:=i or 1;
reg_cr:=(not ($f shl c)) and reg_cr;
reg_cr:=(i shl c) or reg_cr;
End;

Function doLWS(r,a,n:LongInt):Boolean;
Var i:LongInt;
Begin;
doLWS:=True;
while (n>0) do begin;
  if checkRange(a) then exit;
  i:=ReadLongMSB(memoryData^[a]);
  case n of
    0:;
    1:regs[r]:=i and $ff000000;
    2:regs[r]:=i and $ffff0000;
    3:regs[r]:=i and $ffffff00;
    else regs[r]:=i;
    end;
  r:=(r+1) and $1f;
  dec(n,4);
  inc(a,4);
  end;
doLWS:=False;
End;

Function doSTWS(r,a,n:LongInt):Boolean;
Var i:LongInt;
Begin;
doSTWS:=True;
while (n>0) do begin;
  if checkRange(a) then exit;
  i:=ReadLongMSB(memoryData^[a]);
  case n of
    0:;
    1:memoryData^[a]:=regs[r] shr 24;
    2:WriteWordMSB(memoryData^[a],regs[r] shr 16);
    3:WriteLongMSB(memoryData^[a],(regs[r] shr 8) or (memoryData^[a+3] shl 24));
    else WriteLongMSB(memoryData^[a],regs[r]);
    end;
  r:=(r+1) and $1f;
  dec(n,4);
  inc(a,4);
  end;
doSTWS:=False;
End;

Function genMask(b,e:LongInt):LongInt;
Var i,o:LongInt;
Begin;
o:=0;
if (b>e) then begin;
  for i:=b to 31 do o:=bits[i] or o;
  for i:=0 to e do o:=bits[i] or o;
  end else begin;
  for i:=b to e do o:=bits[i] or o;
  end;
genMask:=o;
End;

Procedure doSRAW(var d:LongInt;r:LongInt);
Var i:LongInt;
Begin;
if (r and $1f<>r) then begin;
  if (d<0) then d:=$ffffffff else d:=0;
  updateC(0,0);
  end else begin;
  i:=genMask(r+1,31);
  if (d and i=0) then updateC(0,0) else updateC($ffffffff,$ffffffff);
  d:=d shr r;
  if (d<0) then d:=d or (not i);
  end;
End;



Var i,o,p,q,r,s:LongInt;
Begin;
result:=1;

if checkRange(reg_iar) then goto vege;
i:=ReadLongMSB(memoryData^[reg_iar]);
inc(reg_iar,4);
case i shr 26 of
  $1f:begin;
    case (i shr 1) and $3ff of
      $01c:begin;
        {$ifdef debug}writeln('and[.] rs,ra,rb');{$endif}
        o:=regs[(i shr 11) and $1f];
        p:=regs[(i shr 21) and $1f];
        o:=o and p;
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 16) and $1f]:=o;
        goto ok;
        end;
      $03c:begin;
        {$ifdef debug}writeln('andc[.] rs,ra,rb');{$endif}
        o:=not regs[(i shr 11) and $1f];
        p:=regs[(i shr 21) and $1f];
        o:=o and p;
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 16) and $1f]:=o;
        goto ok;
        end;
      $000:begin;
        {$ifdef debug}writeln('cmp cond,ra,rb');{$endif}
        o:=regs[(i shr 16) and $1f];
        p:=regs[(i shr 11) and $1f];
        doComp(o,p,(i shr 23) and 7);
        goto ok;
        end;
      $020:begin;
        {$ifdef debug}writeln('cmpl cond,ra,rb');{$endif}
        o:=regs[(i shr 16) and $1f];
        p:=regs[(i shr 11) and $1f];
        if (o and $ffff0000=p and $ffff0000) then begin;
          o:=o and $ffff;
          p:=p and $ffff;
          end else begin;
          o:=o shr 16;
          p:=p shr 16;
          end;
        doComp(o,p,(i shr 23) and 7);
        goto ok;
        end;
      $01a:begin;
        {$ifdef debug}writeln('cntlzw[.] rs,ra');{$endif}
        p:=regs[(i shr 21) and $1f];
        o:=0;
        while (o<32) do begin;
          if (bits[o] and p<>0) then break;
          inc(o);
          end;
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 16) and $1f]:=o;
        goto ok;
        end;
      $356:begin;
        {$ifdef debug}writeln('eieio');{$endif}
        goto ok;
        end;
      $11c:begin;
        {$ifdef debug}writeln('eqv[.] rs,ra,rb');{$endif}
        o:=regs[(i shr 11) and $1f];
        p:=regs[(i shr 21) and $1f];
        o:=not (o xor p);
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 16) and $1f]:=o;
        goto ok;
        end;
      $3ba:begin;
        {$ifdef debug}writeln('extsb[.] rs,ra');{$endif}
        o:=regs[(i shr 21) and $1f];
        o:=shortint(o);
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 16) and $1f]:=o;
        goto ok;
        end;
      $39a:begin;
        {$ifdef debug}writeln('extsh[.] rs,ra');{$endif}
        o:=regs[(i shr 21) and $1f];
        o:=integer(o);
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 16) and $1f]:=o;
        goto ok;
        end;
      $077:begin;
        {$ifdef debug}writeln('lbzux rd,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f]+regs[o];
        if checkRange(p) then goto vege;
        regs[(i shr 21) and $1f]:=memoryData^[p];
        regs[o]:=p;
        goto ok;
        end;
      $057:begin;
        {$ifdef debug}writeln('lbzx rd,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f];
        if (o<>0) then p:=regs[o]+p;
        if checkRange(p) then goto vege;
        regs[(i shr 21) and $1f]:=memoryData^[p];
        goto ok;
        end;
      $177:begin;
        {$ifdef debug}writeln('lhaux rd,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f]+regs[o];
        if checkRange(p) then goto vege;
        q:=ReadWordMSB(memoryData^[p]);
        regs[(i shr 21) and $1f]:=integer(q);
        regs[o]:=p;
        goto ok;
        end;
      $157:begin;
        {$ifdef debug}writeln('lhax rd,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f];
        if (o<>0) then p:=regs[o]+p;
        if checkRange(p) then goto vege;
        q:=ReadWordMSB(memoryData^[p]);
        regs[(i shr 21) and $1f]:=integer(q);
        goto ok;
        end;
      $316:begin;
        {$ifdef debug}writeln('lhbrx rd,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f];
        if (o<>0) then p:=regs[o]+p;
        if checkRange(p) then goto vege;
        regs[(i shr 21) and $1f]:=ReadWordLSB(memoryData^[p]);
        goto ok;
        end;
      $137:begin;
        {$ifdef debug}writeln('lhzux rd,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f]+regs[o];
        if checkRange(p) then goto vege;
        regs[(i shr 21) and $1f]:=ReadWordMSB(memoryData^[p]);
        regs[o]:=p;
        goto ok;
        end;
      $117:begin;
        {$ifdef debug}writeln('lhzx rd,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f];
        if (o<>0) then p:=regs[o]+p;
        if checkRange(p) then goto vege;
        regs[(i shr 21) and $1f]:=ReadWordMSB(memoryData^[p]);
        goto ok;
        end;
      $255:begin;
        {$ifdef debug}writeln('lswi rd,ra,nb');{$endif}
        o:=(i shr 16) and $1f;
        if (o<>0) then p:=regs[o] else p:=0;
        o:=(i shr 11) and $1f;
        if (o<1) then o:=32;
        if doLWS((i shr 21) and $1f,p,o) then goto vege;
        goto ok;
        end;
      $215:begin;
        {$ifdef debug}writeln('lswx rd,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f];
        if (o<>0) then p:=regs[o]+p;
        o:=reg_xer and $7f;
        if (o<1) then o:=128;
        if doLWS((i shr 21) and $1f,p,o) then goto vege;
        goto ok;
        end;
      $014:begin;
        {$ifdef debug}writeln('lwarx rd,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f];
        if (o<>0) then p:=regs[o]+p;
        if checkRange(p) then goto vege;
        regs[(i shr 21) and $1f]:=ReadLongMSB(memoryData^[p]);
        reg_res:=p;
        goto ok;
        end;
      $216:begin;
        {$ifdef debug}writeln('lwbrx rd,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f];
        if (o<>0) then p:=regs[o]+p;
        if checkRange(p) then goto vege;
        regs[(i shr 21) and $1f]:=ReadLongLSB(memoryData^[p]);
        goto ok;
        end;
      $037:begin;
        {$ifdef debug}writeln('lwzux rd,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f]+regs[o];
        if checkRange(p) then goto vege;
        regs[(i shr 21) and $1f]:=ReadLongMSB(memoryData^[p]);
        regs[o]:=p;
        goto ok;
        end;
      $017:begin;
        {$ifdef debug}writeln('lwzx rd,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f];
        if (o<>0) then p:=regs[o]+p;
        if checkRange(p) then goto vege;
        regs[(i shr 21) and $1f]:=ReadLongMSB(memoryData^[p]);
        goto ok;
        end;
      $200:begin;
        {$ifdef debug}writeln('mcrxr crfd');{$endif}
        o:=((i shr 23) and 7)*(-4)+28;
        i:=reg_xer shr 28;
        reg_cr:=(not ($f shl o)) and reg_cr;
        reg_cr:=(i shl o) or reg_cr;
        reg_xer:=reg_xer and $0fffffff;
        goto ok;
        end;
      $013:begin;
        {$ifdef debug}writeln('mfcr rd');{$endif}
        regs[(i shr 21) and 7]:=reg_cr;
        goto ok;
        end;
      $053:begin;
        {$ifdef debug}writeln('mfmsr rd');{$endif}
        regs[(i shr 21) and 7]:=reg_msr;
        goto ok;
        end;
      $153:begin;
        {$ifdef debug}writeln('mfspr rd,spr');{$endif}
        o:=(i shr 11) and $3ff;
        o:=((o shr 5) or (o shl 5)) and $3ff;
        case o of
          1:o:=reg_xer;
          8:o:=reg_lr;
          9:o:=reg_ctr;
          else goto vege;
          end;
        regs[(i shr 21) and 7]:=o;
        goto ok;
        end;
      $90:begin;
        {$ifdef debug}writeln('mtcrf crm,rs');{$endif}
        p:=(i shr 12) and $ff;
        q:=0;
        r:=$f;
        s:=1;
        for o:=0 to 7 do begin;
          if (p and s<>0) then q:=q or r;
          r:=r shl 4;
          s:=s shl 1;
          end;
        reg_cr:=(reg_cr and (not q)) or (regs[(i shr 21) and $1f] and q);
        goto ok;
        end;
      $092:begin;
        {$ifdef debug}writeln('mtmsr rs');{$endif}
        reg_msr:=regs[(i shr 21) and 7];
        goto ok;
        end;
      $1d3:begin;
        {$ifdef debug}writeln('mtspr spr,rs');{$endif}
        o:=(i shr 11) and $3ff;
        o:=((o shr 5) or (o shl 5)) and $3ff;
        p:=regs[(i shr 21) and 7];
        case o of
          1:reg_xer:=p;
          8:reg_lr:=p;
          9:reg_ctr:=p;
          else goto vege;
          end;
        goto ok;
        end;
      $04b:begin;
        {$ifdef debug}writeln('mulhw[.] rd,ra,rb');{$endif}
        o:=regs[(i shr 16) and $1f];
        p:=regs[(i shr 11) and $1f];
        doMulS32h(o,p);
        regs[(i shr 21) and $1f]:=o;
        if (i and 1<>0) then updateLGES(o);
        goto ok;
        end;
      $00b:begin;
        {$ifdef debug}writeln('mulhwu[.] rd,ra,rb');{$endif}
        o:=regs[(i shr 16) and $1f];
        p:=regs[(i shr 11) and $1f];
        doMulU32h(o,p);
        regs[(i shr 21) and $1f]:=o;
        if (i and 1<>0) then updateLGES(o);
        goto ok;
        end;
      $1dc:begin;
        {$ifdef debug}writeln('nand[.] rs,ra,rb');{$endif}
        o:=regs[(i shr 11) and $1f];
        p:=regs[(i shr 21) and $1f];
        o:=not (o and p);
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 16) and $1f]:=o;
        goto ok;
        end;
      $07c:begin;
        {$ifdef debug}writeln('nor[.] rs,ra,rb');{$endif}
        o:=regs[(i shr 11) and $1f];
        p:=regs[(i shr 21) and $1f];
        o:=not (o or p);
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 16) and $1f]:=o;
        goto ok;
        end;
      $1bc:begin;
        {$ifdef debug}writeln('or[.] rs,ra,rb');{$endif}
        o:=regs[(i shr 11) and $1f];
        p:=regs[(i shr 21) and $1f];
        o:=o or p;
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 16) and $1f]:=o;
        goto ok;
        end;
      $19c:begin;
        {$ifdef debug}writeln('orc[.] rs,ra,rb');{$endif}
        o:=not regs[(i shr 11) and $1f];
        p:=regs[(i shr 21) and $1f];
        o:=o or p;
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 16) and $1f]:=o;
        goto ok;
        end;
      $018:begin;
        {$ifdef debug}writeln('slw[.] rs,ra,rb');{$endif}
        q:=regs[(i shr 11) and $1f];
        p:=regs[(i shr 21) and $1f];
        if (q and $1f<>q) then o:=0 else o:=p shl q;
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 16) and $1f]:=o;
        goto ok;
        end;
      $318:begin;
        {$ifdef debug}writeln('sraw[.] rs,ra,rb');{$endif}
        o:=regs[(i shr 21) and $1f];
        doSRAW(o,regs[(i shr 11) and $1f]);
        regs[(i shr 16) and $1f]:=o;
        if (i and 1<>0) then updateLGES(o);
        goto ok;
        end;
      $338:begin;
        {$ifdef debug}writeln('srawi[.] rs,ra,sh');{$endif}
        o:=regs[(i shr 21) and $1f];
        doSRAW(o,(i shr 11) and $1f);
        regs[(i shr 16) and $1f]:=o;
        if (i and 1<>0) then updateLGES(o);
        goto ok;
        end;
      $218:begin;
        {$ifdef debug}writeln('srw[.] rs,ra,rb');{$endif}
        o:=regs[(i shr 21) and $1f];
        p:=regs[(i shr 11) and $1f];
        if (p>$1f) then o:=0 else o:=o shr p;
        regs[(i shr 16) and $1f]:=o;
        if (i and 1<>0) then updateLGES(o);
        goto ok;
        end;
      $0f7:begin;
        {$ifdef debug}writeln('stbux rs,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f]+regs[o];
        if checkRange(p) then goto vege;
        memoryData^[p]:=regs[(i shr 21) and $1f];
        regs[o]:=p;
        goto ok;
        end;
      $0d7:begin;
        {$ifdef debug}writeln('stbx rs,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f];
        if (o<>0) then p:=regs[o]+p;
        if checkRange(p) then goto vege;
        memoryData^[p]:=regs[(i shr 21) and $1f];
        goto ok;
        end;
      $1b7:begin;
        {$ifdef debug}writeln('sthux rs,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f]+regs[o];
        if checkRange(p) then goto vege;
        WriteWordMSB(memoryData^[p],regs[(i shr 21) and $1f]);
        regs[o]:=p;
        goto ok;
        end;
      $197:begin;
        {$ifdef debug}writeln('sthx rs,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f];
        if (o<>0) then p:=regs[o]+p;
        if checkRange(p) then goto vege;
        WriteWordMSB(memoryData^[p],regs[(i shr 21) and $1f]);
        goto ok;
        end;
      $396:begin;
        {$ifdef debug}writeln('sthbrx rs,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f]+regs[o];
        if checkRange(p) then goto vege;
        WriteWordLSB(memoryData^[p],regs[(i shr 21) and $1f]);
        regs[o]:=p;
        goto ok;
        end;
      $205:begin;
        {$ifdef debug}writeln('stswi rd,ra,nb');{$endif}
        o:=(i shr 16) and $1f;
        if (o<>0) then p:=regs[o] else p:=0;
        o:=(i shr 11) and $1f;
        if (o<1) then o:=32;
        if doLWS((i shr 21) and $1f,p,o) then goto vege;
        goto ok;
        end;
      $295:begin;
        {$ifdef debug}writeln('stswx rd,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f];
        if (o<>0) then p:=regs[o]+p;
        o:=reg_xer and $7f;
        if (o<1) then o:=128;
        if doLWS((i shr 21) and $1f,p,o) then goto vege;
        goto ok;
        end;
      $296:begin;
        {$ifdef debug}writeln('stwbrx rs,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f]+regs[o];
        if checkRange(p) then goto vege;
        WriteLongLSB(memoryData^[p],regs[(i shr 21) and $1f]);
        regs[o]:=p;
        goto ok;
        end;
      $096:begin;
        {$ifdef debug}writeln('stwcx. rs,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f];
        if (o<>0) then inc(p,regs[o]);
        updateLGES(0);
        if (reg_res=-1) then begin;
          reg_cr:=reg_cr and $0fffffff;
          end else begin;
          if checkRange(p) then goto vege;
          WriteLongLSB(memoryData^[p],regs[(i shr 21) and $1f]);
          regs[o]:=p;
          end;
        reg_res:=-1;
        goto ok;
        end;
      $0b7:begin;
        {$ifdef debug}writeln('stwux rs,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f]+regs[o];
        if checkRange(p) then goto vege;
        WriteLongMSB(memoryData^[p],regs[(i shr 21) and $1f]);
        regs[o]:=p;
        goto ok;
        end;
      $097:begin;
        {$ifdef debug}writeln('stwx rs,ra,rb');{$endif}
        o:=(i shr 16) and $1f;
        p:=regs[(i shr 11) and $1f];
        if (o<>0) then p:=regs[o]+p;
        if checkRange(p) then goto vege;
        WriteLongMSB(memoryData^[p],regs[(i shr 21) and $1f]);
        goto ok;
        end;
      $13c:begin;
        {$ifdef debug}writeln('xor[.] rs,ra,rb');{$endif}
        o:=regs[(i shr 11) and $1f];
        p:=regs[(i shr 21) and $1f];
        o:=o xor p;
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 16) and $1f]:=o;
        goto ok;
        end;
      end;
    case (i shr 1) and $1ff of
      $10a:begin;
        {$ifdef debug}writeln('add[o][.] rd,ra,rb');{$endif}
        o:=regs[(i shr 11) and $1f];
        p:=regs[(i shr 16) and $1f];
        if (i and $400<>0) then updateSO(o,p);
        inc(o,p);
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 21) and $1f]:=o;
        goto ok;
        end;
      $00a:begin;
        {$ifdef debug}writeln('addc[o][.] rd,ra,rb');{$endif}
        o:=regs[(i shr 11) and $1f];
        p:=regs[(i shr 16) and $1f];
        if (i and $400<>0) then updateSO(o,p);
        updateC(o,p);
        inc(o,p);
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 21) and $1f]:=o;
        goto ok;
        end;
      $08a:begin;
        {$ifdef debug}writeln('adde[o][.] rd,ra,rb');{$endif}
        o:=regs[(i shr 11) and $1f];
        p:=regs[(i shr 16) and $1f];
        if (reg_xer and $20000000<>0) then inc(o);
        if (i and $400<>0) then updateSO(o,p);
        updateC(o,p);
        inc(o,p);
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 21) and $1f]:=o;
        goto ok;
        end;
      $0ea:begin;
        {$ifdef debug}writeln('addme[o][.] rd,ra');{$endif}
        if (reg_xer and $20000000<>0) then o:=0 else o:=-1;
        p:=regs[(i shr 16) and $1f];
        if (i and $400<>0) then updateSO(o,p);
        updateC(o,p);
        inc(o,p);
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 21) and $1f]:=o;
        goto ok;
        end;
      $0ca:begin;
        {$ifdef debug}writeln('addze[o][.] rd,ra');{$endif}
        if (reg_xer and $20000000<>0) then o:=1 else o:=0;
        p:=regs[(i shr 16) and $1f];
        if (i and $400<>0) then updateSO(o,p);
        updateC(o,p);
        inc(o,p);
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 21) and $1f]:=o;
        goto ok;
        end;
      $1eb:begin;
        {$ifdef debug}writeln('divw[o][.] rd,ra,rb');{$endif}
        o:=regs[(i shr 11) and $1f];
        p:=regs[(i shr 16) and $1f];
        doDivS32(p,o);
        if (i and $400<>0) then updateSO(0,o);
        if (i and 1<>0) then updateLGES(p);
        regs[(i shr 21) and $1f]:=p;
        goto ok;
        end;
      $1cb:begin;
        {$ifdef debug}writeln('divwu[o][.] rd,ra,rb');{$endif}
        o:=regs[(i shr 11) and $1f];
        p:=regs[(i shr 16) and $1f];
        doDivU32(p,o);
        if (i and $400<>0) then updateSO(0,o);
        if (i and 1<>0) then updateLGES(p);
        regs[(i shr 21) and $1f]:=p;
        goto ok;
        end;
      $0eb:begin;
        {$ifdef debug}writeln('mullw[o][.] rd,ra,rb');{$endif}
        o:=regs[(i shr 16) and $1f];
        p:=regs[(i shr 11) and $1f];
        doMul32l(o,p);
        regs[(i shr 21) and $1f]:=o;
        if (i and $400<>0) then updateSO(0,p);
        if (i and 1<>0) then updateLGES(o);
        goto ok;
        end;
      $068:begin;
        {$ifdef debug}writeln('neg[o][.] rd,ra');{$endif}
        o:=regs[(i shr 16) and $1f];
        if (o=$80000000) then p:=-1 else p:=0;
        o:=-o;
        regs[(i shr 21) and $1f]:=o;
        if (i and $400<>0) then updateSO(0,p);
        if (i and 1<>0) then updateLGES(o);
        goto ok;
        end;
      $028:begin;
        {$ifdef debug}writeln('subf[o][.] rd,ra,rb');{$endif}
        o:=regs[(i shr 11) and $1f];
        p:=regs[(i shr 16) and $1f];
        if (i and $400<>0) then updateSO(-o,p);
        dec(o,p);
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 21) and $1f]:=o;
        goto ok;
        end;
      $008:begin;
        {$ifdef debug}writeln('subfc[o][.] rd,ra,rb');{$endif}
        o:=regs[(i shr 11) and $1f];
        p:=regs[(i shr 16) and $1f];
        if (i and $400<>0) then updateSO(-o,p);
        updateC(-o,p);
        dec(o,p);
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 21) and $1f]:=o;
        goto ok;
        end;
      $088:begin;
        {$ifdef debug}writeln('subfe[o][.] rd,ra,rb');{$endif}
        o:=regs[(i shr 11) and $1f];
        p:=regs[(i shr 16) and $1f];
        if (reg_xer and $20000000=0) then dec(o);
        if (i and $400<>0) then updateSO(-o,p);
        updateC(-o,p);
        dec(o,p);
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 21) and $1f]:=o;
        goto ok;
        end;
      $0e8:begin;
        {$ifdef debug}writeln('subfme[o][.] rd,ra');{$endif}
        if (reg_xer and $20000000=0) then o:=-1 else o:=0;
        p:=-regs[(i shr 16) and $1f];
        if (i and $400<>0) then updateSO(o,p);
        updateC(o,p);
        inc(o,p);
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 21) and $1f]:=o;
        goto ok;
        end;
      $0c8:begin;
        {$ifdef debug}writeln('subfze[o][.] rd,ra');{$endif}
        if (reg_xer and $20000000=0) then o:=0 else o:=1;
        p:=-regs[(i shr 16) and $1f];
        if (i and $400<>0) then updateSO(o,p);
        updateC(o,p);
        inc(o,p);
        if (i and 1<>0) then updateLGES(o);
        regs[(i shr 21) and $1f]:=o;
        goto ok;
        end;
      end;
    end;
  $13:begin;
    case (i shr 1) and $3ff of
      $210:begin;
        {$ifdef debug}writeln('bcctr[l] bo,bi');{$endif}
        if (i and 1<>0) then reg_lr:=reg_iar;
        if evalCond(i shr 21,i shr 16) then reg_iar:=reg_ctr and $fffffffc;
        goto ok;
        end;
      $010:begin;
        {$ifdef debug}writeln('bclr[l] bo,bi');{$endif}
        o:=reg_iar;
        if evalCond(i shr 21,i shr 16) then reg_iar:=reg_lr and $fffffffc;
        if (i and 1<>0) then reg_lr:=o;
        goto ok;
        end;
      $101:begin;
        {$ifdef debug}writeln('crand cd,ca,cb');{$endif}
        o:=bits[(i shr 21) and $1f];
        p:=bits[(i shr 16) and $1f];
        q:=bits[(i shr 11) and $1f];
        if (reg_cr and p<>0) and (reg_cr and q<>0) then reg_cr:=o or reg_cr else reg_cr:=(not o) and reg_cr;
        goto ok;
        end;
      $081:begin;
        {$ifdef debug}writeln('crandc cd,ca,cb');{$endif}
        o:=bits[(i shr 21) and $1f];
        p:=bits[(i shr 16) and $1f];
        q:=bits[(i shr 11) and $1f];
        if (reg_cr and p<>0) and (reg_cr and q=0) then reg_cr:=o or reg_cr else reg_cr:=(not o) and reg_cr;
        goto ok;
        end;
      $121:begin;
        {$ifdef debug}writeln('creqv cd,ca,cb');{$endif}
        o:=bits[(i shr 21) and $1f];
        p:=bits[(i shr 16) and $1f];
        q:=bits[(i shr 11) and $1f];
        if (reg_cr and p<>0) xor (reg_cr and q=0) then reg_cr:=o or reg_cr else reg_cr:=(not o) and reg_cr;
        goto ok;
        end;
      $0e1:begin;
        {$ifdef debug}writeln('crnand cd,ca,cb');{$endif}
        o:=bits[(i shr 21) and $1f];
        p:=bits[(i shr 16) and $1f];
        q:=bits[(i shr 11) and $1f];
        if (reg_cr and p<>0) and (reg_cr and q<>0) then reg_cr:=(not o) and reg_cr else reg_cr:=o or reg_cr;
        goto ok;
        end;
      $021:begin;
        {$ifdef debug}writeln('crnor cd,ca,cb');{$endif}
        o:=bits[(i shr 21) and $1f];
        p:=bits[(i shr 16) and $1f];
        q:=bits[(i shr 11) and $1f];
        if (reg_cr and p<>0) or (reg_cr and q<>0) then reg_cr:=(not o) and reg_cr else reg_cr:=o or reg_cr;
        goto ok;
        end;
      $1c1:begin;
        {$ifdef debug}writeln('cror cd,ca,cb');{$endif}
        o:=bits[(i shr 21) and $1f];
        p:=bits[(i shr 16) and $1f];
        q:=bits[(i shr 11) and $1f];
        if (reg_cr and p<>0) or (reg_cr and q<>0) then reg_cr:=o or reg_cr else reg_cr:=(not o) and reg_cr;
        goto ok;
        end;
      $1a1:begin;
        {$ifdef debug}writeln('crorc cd,ca,cb');{$endif}
        o:=bits[(i shr 21) and $1f];
        p:=bits[(i shr 16) and $1f];
        q:=bits[(i shr 11) and $1f];
        if (reg_cr and p<>0) or (reg_cr and q=0) then reg_cr:=o or reg_cr else reg_cr:=(not o) and reg_cr;
        goto ok;
        end;
      $0c1:begin;
        {$ifdef debug}writeln('crxor cd,ca,cb');{$endif}
        o:=bits[(i shr 21) and $1f];
        p:=bits[(i shr 16) and $1f];
        q:=bits[(i shr 11) and $1f];
        if (reg_cr and p<>0) xor (reg_cr and q<>0) then reg_cr:=o or reg_cr else reg_cr:=(not o) and reg_cr;
        goto ok;
        end;
      $000:begin;
        {$ifdef debug}writeln('mcrf crfd,crfs');{$endif}
        o:=((i shr 23) and 7)*(-4)+28;
        p:=((i shr 18) and 7)*(-4)+28;
        i:=(reg_cr shr p) and $f;
        reg_cr:=(not ($f shl o)) and reg_cr;
        reg_cr:=(i shl o) or reg_cr;
        goto ok;
        end;
      end;
    end;
  $0e:begin;
    {$ifdef debug}writeln('addi rd,ra,simm');{$endif}
    p:=(i shr 16) and $1f;
    if (p=0) then o:=0 else o:=regs[p];
    p:=integer(i);
    inc(o,p);
    regs[(i shr 21) and $1f]:=o;
    goto ok;
    end;
  $0c:begin;
    {$ifdef debug}writeln('addic rd,ra,simm');{$endif}
    o:=regs[(i shr 16) and $1f];
    p:=integer(i);
    updateC(o,p);
    inc(o,p);
    regs[(i shr 21) and $1f]:=o;
    goto ok;
    end;
  $0d:begin;
    {$ifdef debug}writeln('addic. rd,ra,simm');{$endif}
    o:=regs[(i shr 16) and $1f];
    p:=integer(i);
    updateC(o,p);
    inc(o,p);
    updateLGES(o);
    regs[(i shr 21) and $1f]:=o;
    goto ok;
    end;
  $0f:begin;
    {$ifdef debug}writeln('addis rd,ra,simm');{$endif}
    p:=(i shr 16) and $1f;
    if (p=0) then o:=0 else o:=regs[p];
    p:=i shl 16;
    inc(o,p);
    regs[(i shr 21) and $1f]:=o;
    goto ok;
    end;
  $1c:begin;
    {$ifdef debug}writeln('andi. rs,ra,simm');{$endif}
    o:=regs[(i shr 21) and $1f];
    p:=integer(i);
    o:=o and p;
    updateLGES(o);
    regs[(i shr 16) and $1f]:=o;
    goto ok;
    end;
  $1d:begin;
    {$ifdef debug}writeln('andis. rs,ra,simm');{$endif}
    o:=regs[(i shr 21) and $1f];
    p:=i shl 16;
    o:=o and p;
    updateLGES(o);
    regs[(i shr 16) and $1f]:=o;
    goto ok;
    end;
  $12:begin;
    {$ifdef debug}writeln('b[l][a] simm');{$endif}
    if (i and 1<>0) then reg_lr:=reg_iar;
    o:=i and $3fffffc;
    if (o and $2000000<>0) then o:=o or $fc000000;
    if (i and 2=0) then inc(o,reg_iar-4);
    reg_iar:=o;
    goto ok;
    end;
  $10:begin;
    {$ifdef debug}writeln('bc[l][a] bo,bi,simm');{$endif}
    if (i and 1<>0) then reg_lr:=reg_iar;
    o:=integer(i);
    o:=o and $fffffffc;
    if (i and 2=0) then inc(o,reg_iar-4);
    if evalCond(i shr 21,i shr 16) then reg_iar:=o;
    goto ok;
    end;
  $0b:begin;
    {$ifdef debug}writeln('cmpi cond,ra,simm');{$endif}
    o:=regs[(i shr 16) and $1f];
    p:=integer(i);
    doComp(o,p,(i shr 23) and 7);
    goto ok;
    end;
  $0a:begin;
    {$ifdef debug}writeln('cmpli cond,ra,simm');{$endif}
    o:=regs[(i shr 16) and $1f];
    p:=i and $ffff;
    if (o and $ffff0000=p and $ffff0000) then begin;
      o:=o and $ffff;
      p:=p and $ffff;
      end else begin;
      o:=o shr 16;
      p:=p shr 16;
      end;
    doComp(o,p,(i shr 23) and 7);
    goto ok;
    end;
  $22:begin;
    {$ifdef debug}writeln('lbz rd,simm(ra)');{$endif}
    o:=(i shr 16) and $1f;
    p:=integer(i);
    if (o<>0) then p:=regs[o]+p;
    if checkRange(p) then goto vege;
    regs[(i shr 21) and $1f]:=memoryData^[p];
    goto ok;
    end;
  $23:begin;
    {$ifdef debug}writeln('lbzu rd,simm(ra)');{$endif}
    o:=(i shr 16) and $1f;
    p:=integer(i);
    p:=regs[o]+p;
    if checkRange(p) then goto vege;
    regs[(i shr 21) and $1f]:=memoryData^[p];
    regs[o]:=p;
    goto ok;
    end;
  $2a:begin;
    {$ifdef debug}writeln('lha rd,simm(ra)');{$endif}
    o:=(i shr 16) and $1f;
    p:=integer(i);
    if (o<>0) then p:=regs[o]+p;
    if checkRange(p) then goto vege;
    q:=ReadWordMSB(memoryData^[p]);
    regs[(i shr 21) and $1f]:=integer(q);
    goto ok;
    end;
  $2b:begin;
    {$ifdef debug}writeln('lhau rd,simm(ra)');{$endif}
    o:=(i shr 16) and $1f;
    p:=integer(i);
    p:=regs[o]+p;
    if checkRange(p) then goto vege;
    q:=ReadWordMSB(memoryData^[p]);
    regs[(i shr 21) and $1f]:=integer(q);
    regs[o]:=p;
    goto ok;
    end;
  $28:begin;
    {$ifdef debug}writeln('lhz rd,simm(ra)');{$endif}
    o:=(i shr 16) and $1f;
    p:=integer(i);
    if (o<>0) then p:=regs[o]+p;
    if checkRange(p) then goto vege;
    regs[(i shr 21) and $1f]:=ReadWordMSB(memoryData^[p]);
    goto ok;
    end;
  $29:begin;
    {$ifdef debug}writeln('lhzu rd,simm(ra)');{$endif}
    o:=(i shr 16) and $1f;
    p:=integer(i);
    p:=regs[o]+p;
    if checkRange(p) then goto vege;
    regs[(i shr 21) and $1f]:=ReadWordMSB(memoryData^[p]);
    regs[o]:=p;
    goto ok;
    end;
  $2e:begin;
    {$ifdef debug}writeln('lmw rd,simm(ra)');{$endif}
    o:=(i shr 16) and $1f;
    p:=integer(i);
    if (o<>0) then p:=regs[o]+p;
    for o:=(i shr 21) and $1f to 31 do begin;
      if checkRange(p) then goto vege;
      regs[o]:=ReadLongMSB(memoryData^[p]);
      inc(p,4);
      end;
    goto ok;
    end;
  $20:begin;
    {$ifdef debug}writeln('lwz rd,simm(ra)');{$endif}
    o:=(i shr 16) and $1f;
    p:=integer(i);
    if (o<>0) then p:=regs[o]+p;
    if checkRange(p) then goto vege;
    regs[(i shr 21) and $1f]:=ReadLongMSB(memoryData^[p]);
    goto ok;
    end;
  $21:begin;
    {$ifdef debug}writeln('lwzu rd,simm(ra)');{$endif}
    o:=(i shr 16) and $1f;
    p:=integer(i);
    p:=regs[o]+p;
    if checkRange(p) then goto vege;
    regs[(i shr 21) and $1f]:=ReadLongMSB(memoryData^[p]);
    regs[o]:=p;
    goto ok;
    end;
  $07:begin;
    {$ifdef debug}writeln('mulli rd,ra,simm');{$endif}
    o:=regs[(i shr 16) and $1f];
    p:=integer(i);
    doMul32l(o,p);
    regs[(i shr 21) and $1f]:=o;
    goto ok;
    end;
  $18:begin;
    {$ifdef debug}writeln('ori rs,ra,uimm');{$endif}
    o:=regs[(i shr 21) and $1f];
    p:=i and $ffff;
    o:=o or p;
    regs[(i shr 16) and $1f]:=o;
    goto ok;
    end;
  $19:begin;
    {$ifdef debug}writeln('oris rs,ra,uimm');{$endif}
    o:=regs[(i shr 21) and $1f];
    p:=i shl 16;
    o:=o or p;
    regs[(i shr 16) and $1f]:=o;
    goto ok;
    end;
  $14:begin;
    {$ifdef debug}writeln('rlwimi[.] ra,rs,sh,mb,me');{$endif}
    o:=rotateLeft(regs[(i shr 21) and $1f],32,(i shr 11) and $1f);
    p:=genMask((i shr 6) and $1f,(i shr 1) and $1f);
    q:=(i shr 16) and $1f;
    o:=(regs[q] and (not p)) or (o and p);
    if (i and 1<>0) then updateLGES(o);
    regs[q]:=o;
    goto ok;
    end;
  $15:begin;
    {$ifdef debug}writeln('rlwinm[.] ra,rs,sh,mb,me');{$endif}
    o:=rotateLeft(regs[(i shr 21) and $1f],32,(i shr 11) and $1f);
    p:=genMask((i shr 6) and $1f,(i shr 1) and $1f);
    o:=o and p;
    if (i and 1<>0) then updateLGES(o);
    regs[(i shr 16) and $1f]:=o;
    goto ok;
    end;
  $17:begin;
    {$ifdef debug}writeln('rlwnm[.] ra,rs,rb,mb,me');{$endif}
    o:=rotateLeft(regs[(i shr 21) and $1f],32,regs[(i shr 11) and $1f] and $1f);
    p:=genMask((i shr 6) and $1f,(i shr 1) and $1f);
    o:=o and p;
    if (i and 1<>0) then updateLGES(o);
    regs[(i shr 16) and $1f]:=o;
    goto ok;
    end;
  $26:begin;
    {$ifdef debug}writeln('stb rs,simm(ra)');{$endif}
    o:=(i shr 16) and $1f;
    p:=integer(i);
    if (o<>0) then p:=regs[o]+p;
    if checkRange(p) then goto vege;
    memoryData^[p]:=regs[(i shr 21) and $1f];
    goto ok;
    end;
  $27:begin;
    {$ifdef debug}writeln('stbu rs,simm(ra)');{$endif}
    o:=(i shr 16) and $1f;
    p:=integer(i);
    if (o<>0) then p:=regs[o]+p;
    if checkRange(p) then goto vege;
    memoryData^[p]:=regs[(i shr 21) and $1f];
    regs[o]:=p;
    goto ok;
    end;
  $2c:begin;
    {$ifdef debug}writeln('sth rs,simm(ra)');{$endif}
    o:=(i shr 16) and $1f;
    p:=integer(i);
    if (o<>0) then p:=regs[o]+p;
    if checkRange(p) then goto vege;
    WriteWordMSB(memoryData^[p],regs[(i shr 21) and $1f]);
    goto ok;
    end;
  $2d:begin;
    {$ifdef debug}writeln('sthu rs,simm(ra)');{$endif}
    o:=(i shr 16) and $1f;
    p:=integer(i);
    if (o<>0) then p:=regs[o]+p;
    if checkRange(p) then goto vege;
    WriteWordMSB(memoryData^[p],regs[(i shr 21) and $1f]);
    regs[o]:=p;
    goto ok;
    end;
  $2f:begin;
    {$ifdef debug}writeln('stmw rs,simm(ra)');{$endif}
    o:=(i shr 16) and $1f;
    p:=integer(i);
    if (o<>0) then p:=regs[o]+p;
    for o:=(i shr 21) and $1f to 31 do begin;
      if checkRange(p) then goto vege;
      WriteLongMSB(memoryData^[p],regs[o]);
      inc(p,4);
      end;
    goto ok;
    end;
  $24:begin;
    {$ifdef debug}writeln('stw rs,simm(ra)');{$endif}
    o:=(i shr 16) and $1f;
    p:=integer(i);
    if (o<>0) then p:=regs[o]+p;
    if checkRange(p) then goto vege;
    WriteLongMSB(memoryData^[p],regs[(i shr 21) and $1f]);
    goto ok;
    end;
  $25:begin;
    {$ifdef debug}writeln('stwu rs,simm(ra)');{$endif}
    o:=(i shr 16) and $1f;
    p:=integer(i);
    if (o<>0) then p:=regs[o]+p;
    if checkRange(p) then goto vege;
    WriteLongMSB(memoryData^[p],regs[(i shr 21) and $1f]);
    regs[o]:=p;
    goto ok;
    end;
  $08:begin;
    {$ifdef debug}writeln('subfic rd,ra,simm');{$endif}
    o:=regs[(i shr 16) and $1f];
    p:=integer(i);
    updateC(o,-p);
    dec(o,p);
    regs[(i shr 21) and $1f]:=o;
    goto ok;
    end;
  $1a:begin;
    {$ifdef debug}writeln('xori rs,ra,uimm');{$endif}
    o:=regs[(i shr 21) and $1f];
    p:=i and $ffff;
    o:=o xor p;
    regs[(i shr 16) and $1f]:=o;
    goto ok;
    end;
  $1b:begin;
    {$ifdef debug}writeln('xoris rs,ra,uimm');{$endif}
    o:=regs[(i shr 21) and $1f];
    p:=i shl 16;
    o:=o xor p;
    regs[(i shr 16) and $1f]:=o;
    goto ok;
    end;
  end;
goto vege;

ok:
result:=0;
vege:
emulateOneOpcode:=result;
End;
