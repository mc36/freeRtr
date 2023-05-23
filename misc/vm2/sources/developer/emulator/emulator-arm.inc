Type memoryType=array[0..1] of byte;
Var
  memoryData:^memoryType;
  memorySize:LongInt;
Var
  regs:array[0..15] of LongInt;
  reg_cpsr:LongInt;
Const
  reg_pc=15;
  reg_lr=14;
  reg_sp=13;
  flg_N=$80000000;
  flg_Z=$40000000;
  flg_C=$20000000;
  flg_V=$10000000;
  flg_G0=$10000;
  flg_G1=$20000;
  flg_G2=$40000;
  flg_G3=$80000;

Procedure emulateRestart;
Var i:Longint;
Begin;
for i:=0 to 15 do regs[i]:=0;
regs[reg_sp]:=memorySize-4;
reg_cpsr:=0;
End;

Function emulateOneOpcode:LongInt;
Label ok,vege;
Const
  bits:array[0..31] of LongInt=($80000000,$40000000,$20000000,$10000000,
    $8000000,$4000000,$2000000,$1000000,$800000,$400000,$200000,$100000,
    $80000,$40000,$20000,$10000,$8000,$4000,$2000,$1000,
    $800,$400,$200,$100,$80,$40,$20,$10,$8,$4,$2,$1);
Var result,op:LongInt;

Function checkRange(p:LongInt):Boolean;
Begin;
if (p>=memorySize) or (p<0) then begin;
  result:=2;
  checkRange:=True;
  exit;
  end;
checkRange:=False;
End;

Function findBitHi(a:LongInt):LongInt;
Var i:LongInt;
Begin;
findBitHi:=-1;
if (a=0) then exit;
for i:=0 to 32 do begin;
  a:=a shr 1;
  if (a<>0) then continue;
  findBitHi:=i;
  exit;
  end;
End;

Function findBitLo(a:LongInt):LongInt;
Var i,o:LongInt;
Begin;
findBitLo:=-1;
if (a=0) then exit;
for i:=0 to 32 do begin;
  o:=a and 1;
  a:=a shr 1;
  if (o=0) then continue;
  findBitLo:=i;
  exit;
  end;
End;

Function countBits(a:LongInt):LongInt;
Var i,o:LongInt;
Begin;
o:=0;
for i:=0 to 32 do begin;
  if (a and 1<>0) then inc(o);
  a:=a shr 1;
  end;
countBits:=o;
End;

Procedure setFlagN(b:boolean);
Begin;
if b then reg_cpsr:=reg_cpsr or flg_N else reg_cpsr:=reg_cpsr and (not flg_N);
End;

Procedure setFlagZ(b:boolean);
Begin;
if b then reg_cpsr:=reg_cpsr or flg_Z else reg_cpsr:=reg_cpsr and (not flg_Z);
End;

Procedure setFlagC(b:boolean);
Begin;
if b then reg_cpsr:=reg_cpsr or flg_C else reg_cpsr:=reg_cpsr and (not flg_C);
End;

Procedure setFlagV(b:boolean);
Begin;
if b then reg_cpsr:=reg_cpsr or flg_V else reg_cpsr:=reg_cpsr and (not flg_V);
End;

Procedure setFlagNZ(i:LongInt);
Begin;
setFlagN(i<0);
setFlagZ(i=0);
End;

Procedure setFlagCV(a,b:LongInt);
Var i,o,p,r:LongInt;
Begin;
r:=(a+b) shr 24;
a:=a shr 24;
b:=b shr 24;
i:=a and $80;
o:=b and $80;
p:=(r and $80) xor $80;
setFlagC((i and o) or (i and p) or (o and p)<>0);
setFlagV(((i xor $80) and (o xor $80) and (p xor $80)) or (i and o and p)<>0);
End;

Function testCnd(i:LongInt):Boolean;
Var fz,fc,fn,fv:boolean;
Begin;
testCnd:=false;
fz:=(reg_cpsr and flg_Z<>0);
fc:=(reg_cpsr and flg_C<>0);
fn:=(reg_cpsr and flg_N<>0);
fv:=(reg_cpsr and flg_V<>0);
case (i shr 28) and $f of
  0:testCnd:=fz;                        {eq}
  1:testCnd:=not fz;                    {ne}
  2:testCnd:=fc;                        {cs}
  3:testCnd:=not fc;                    {cc}
  4:testCnd:=fn;                        {mi}
  5:testCnd:=not fn;                    {pl}
  6:testCnd:=fv;                        {vs}
  7:testCnd:=not fv;                    {vc}
  8:testCnd:=fc and (not fz);           {hi}
  9:testCnd:=(not fc) or fz;            {ls}
  10:testCnd:=(fn=fv);                  {ge}
  11:testCnd:=(fn<>fv);                 {lt}
  12:testCnd:=(not fz) and (fn=fv);     {gt}
  13:testCnd:=fz or (fn<>fv);           {le}
  14:testCnd:=true;                     {al}
  else exit;
  end;
End;

Function doRot(m,v,r:LongInt):LongInt;
Begin;
r:=r and $3f;
case m and 3 of
  0:v:=v shl r; {lsl}
  1:v:=v shr r; {lsr}
  2:v:=v shr r; {asr}
  3:v:=rotateRight(v,32,r); {ror}
  end;
doRot:=v;
End;

Function doSsat(i,b:LongInt):LongInt;
Label f1;
Begin;
if (b>=32) then goto f1;
b:=(1 shl b)-1;
if (i>b) then i:=b;
b:=-b+1;
if (i<b) then i:=b;
f1:
doSsat:=i;
End;

Function doUsat(i,b:LongInt):LongInt;
Label f1;
Begin;
if (b>=32) then goto f1;
b:=1 shl b;
if (i>b) then i:=b;
if (i<0) then i:=0;
f1:
doUsat:=i;
End;

Function getData(i:LongInt):LongInt;
Begin;
if (i and $02000000<>0) then begin; {immed}
  getData:=rotateRight(i and $ff,32,((i shr 8) and $f) shl 1);
  exit;
  end;
if (i and $10<>0) then begin; {reg shift reg}
  getData:=doRot(i shr 5,regs[i and $f],regs[(i shr 8) and $f]);
  exit;
  end;
if (i and $ff0=0) then begin; {reg}
  getData:=regs[i and $f];
  exit;
  end;
{reg shift num}
getData:=doRot(i shr 5,regs[i and $f],(i shr 7) and $1f);
End;

Function getWord(i,s:LongInt):LongInt;
Label f1;
Var o,p:LongInt;
Begin;
if (i and $02000000=0) then begin; {reg+-immed}
  o:=i and $fff;
  goto f1;
  end;
if (i and $ff0=0) then begin; {reg+-reg}
  o:=regs[i and $f];
  goto f1;
  end;
{reg+-reg shift num}
o:=doRot(i shr 5,regs[i and $f],(i shr 7) and $1f);
f1:
if (i and $00800000=0) then o:=-o;
p:=regs[(i shr 16) and $f];
inc(o,p);
if (s>0) then if (i and $00200000<>0) then regs[(i shr 16) and $f]:=o;
if (i and $01000000=0) then o:=p;
getWord:=o;
End;

Function getMisc(i,s:LongInt):LongInt;
Label f1;
Var o,p:LongInt;
Begin;
if (i and $00400000=0) then begin; {reg+-reg}
  o:=regs[i and $f];
  goto f1;
  end;
{reg+-num}
o:=((i and $f00) shr 4) or (i and $f);
f1:
if (i and $00800000=0) then o:=-o;
p:=regs[(i shr 16) and $f];
inc(o,p);
if (s>0) then if (i and $00200000<>0) then regs[(i shr 16) and $f]:=o;
if (i and $01000000=0) then o:=p;
getMisc:=o;
End;




Procedure doCndRdRm(md:LongInt);
Var i,o,p:LongInt;
Begin;
if not testCnd(op) then exit;
o:=regs[op and $f];
p:=regs[(op shr 12) and $f];
i:=0;
case md of
  1:begin; {clz}
    p:=findBitHi(o);
    if (p<0) then p:=32 else p:=31-p;
    end;
  2:begin; {rev}
    p:=((o and $ff) shl 24) or ((o and $ff00) shl 8) or
      ((o and $ff0000) shr 8) or ((o and $ff000000) shr 24);
    end;
  3:begin; {rev16}
    p:=((o and $ff) shl 8) or ((o and $ff00) shr 8) or
      ((o and $ff0000) shl 8) or ((o and $ff000000) shr 8);
    end;
  4:begin; {revsh}
    p:=((o and $ff) shl 8) or ((o and $ff00) shr 8);
    p:=shortint(p);
    end;
  end;
regs[(op shr 12) and $f]:=p;
if (i and 1<>0) then setFlagNZ(o+p);
if (i and 2<>0) then setFlagCV(o,p);
End;



Procedure doCndRdRn(md:LongInt);
Var i,o,p:LongInt;
Begin;
if not testCnd(op) then exit;
o:=regs[(op shr 12) and $f];
p:=regs[(op shr 16) and $f];
i:=0;
case md of
  1:begin; {ldrex}
    if checkRange(o) then exit;
    p:=ReadLongLSB(memoryData^[o]);
    end;
  2:begin; {strex}
    if checkRange(o) then exit;
    WriteLongLSB(memoryData^[o],p);
    end;
  end;
regs[(op shr 16) and $f]:=p;
if (i and 1<>0) then setFlagNZ(o+p);
if (i and 2<>0) then setFlagCV(o,p);
End;



Procedure doCndRdRmRn(md:LongInt);
Var i,o,p,q,r,s:LongInt;
Begin;
if not testCnd(op) then exit;
i:=regs[(op shr 12) and $f];
o:=regs[op and $f];
p:=regs[(op shr 16) and $f];
case md of
  1:begin; {qadd}
    i:=o+p;
    end;
  2:begin; {qadd16}
    r:=o and $ffff;s:=p and $ffff;i:=doSsat(integer(r)+integer(s),16);
    r:=o shr 16;s:=p shr 16;o:=doSsat(integer(r)+integer(s),16);
    inc(i,o shl 16);
    end;
  3:begin; {qadd8}
    r:=o and $ff;s:=p and $ff;i:=doSsat(shortint(r)+shortint(s),8);
    r:=(o shr 8) and $ff;s:=(p shr 8) and $ff;inc(i,doSsat(shortint(r)+shortint(s),8) shl 8);
    r:=(o shr 16) and $ff;s:=(p shr 16) and $ff;inc(i,doSsat(shortint(r)+shortint(s),8) shl 16);
    r:=(o shr 24) and $ff;s:=(p shr 24) and $ff;inc(i,doSsat(shortint(r)+shortint(s),8) shl 24);
    end;
  4:begin; {qaddsubx}
    r:=o and $ffff;s:=p shr 16;i:=doSsat(integer(r)+integer(s),16);
    r:=p and $ffff;s:=o shr 16;o:=doSsat(integer(r)-integer(s),16);
    i:=(i shl 16)+(o and $ffff);
    end;
  5:begin; {qdadd}
    i:=o+(p*2);
    end;
  6:begin; {qdsub}
    i:=o-(p*2);
    end;
  7:begin; {qsub}
    i:=o-p;
    end;
  8:begin; {qsub16}
    r:=p and $ffff;s:=o and $ffff;i:=doSsat(integer(r)-integer(s),16);
    r:=p shr 16;s:=o shr 16;o:=doSsat(integer(r)-integer(s),16);
    inc(i,o shl 16);
    end;
  9:begin; {qsub8}
    r:=p and $ff;s:=o and $ff;i:=doSsat(shortint(r)-shortint(s),8);
    r:=(p shr 8) and $ff;s:=(o shr 8) and $ff;inc(i,doSsat(shortint(r)-shortint(s),8) shl 8);
    r:=(p shr 16) and $ff;s:=(o shr 16) and $ff;inc(i,doSsat(shortint(r)-shortint(s),8) shl 16);
    r:=(p shr 24) and $ff;s:=(o shr 24) and $ff;inc(i,doSsat(shortint(r)-shortint(s),8) shl 24);
    end;
  10:begin; {qsubaddx}
    r:=p shr 16;s:=o and $ffff;i:=doSsat(integer(r)-integer(s),16);
    r:=o shr 16;s:=p and $ffff;o:=doSsat(integer(r)+integer(s),16);
    i:=(i shl 16)+(o and $ffff);
    end;
  11:begin; {sadd16}
    i:=(o+p) and $ffff;
    o:=((o shr 16)+(p shr 16)) and $ffff;
    i:=(o shl 16)+(i and $ffff);
    end;
  12:begin; {sadd8}
    i:=(o+p) and $ff;
    inc(i,((o shr 8)+((p shr 8)) and $ff) shl 8);
    inc(i,((o shr 16)+((p shr 16)) and $ff) shl 16);
    inc(i,((o shr 24)+((p shr 24)) and $ff) shl 24);
    end;
  13:begin; {saddsubx}
    i:=(p and $ffff)-(o shr 16);
    o:=(p shr 16)+(o and $ffff);
    i:=(i shl 16)+(o and $ffff);
    end;
  14:begin; {sel}
    if (reg_cpsr and flg_G0<>0) then i:=o and $ff else i:=p and $ff;
    if (reg_cpsr and flg_G1<>0) then inc(i,o and $ff00) else inc(i,p and $ff00);
    if (reg_cpsr and flg_G2<>0) then inc(i,o and $ff0000) else inc(i,p and $ff0000);
    if (reg_cpsr and flg_G3<>0) then inc(i,o and $ff000000) else inc(i,p and $ff000000);
    end;
  15:begin; {shadd16}
    r:=o+p;i:=integer(r) shr 1;
    s:=(o shr 16)+(p shr 16);o:=integer(s) shr 1;
    i:=(o shl 16)+(i and $ffff);
    end;
  16:begin; {shadd8}
    r:=o+p;i:=(shortint(r) shr 1) and $ff;
    r:=(o shr 8)+(p shr 8);inc(i,((shortint(r) shr 1) and $ff) shl 8);
    r:=(o shr 16)+(p shr 16);inc(i,((shortint(r) shr 1) and $ff) shl 16);
    r:=(o shr 24)+(p shr 24);inc(i,((shortint(r) shr 1) and $ff) shl 24);
    end;
  17:begin; {shaddsubx}
    r:=(o and $ffff)+(p shr 16);i:=integer(r) shr 1;
    r:=(p and $ffff)-(o shr 16);o:=integer(r) shr 1;
    i:=(i shl 16)+(o and $ffff);
    end;
  18:begin; {shsub16}
    r:=p-o;i:=integer(r) shr 1;
    r:=(p shr 16)-(o shr 16);o:=integer(r) shr 1;
    i:=(o shl 16)+(i and $ffff);
    end;
  19:begin; {shsub8}
    r:=p-o;i:=(shortint(r) shr 1) and $ff;
    r:=(p shr 8)-(o shr 8);inc(i,((shortint(r) shr 1) and $ff) shl 8);
    r:=(p shr 16)-(o shr 16);inc(i,((shortint(r) shr 1) and $ff) shl 16);
    r:=(p shr 24)-(o shr 24);inc(i,((shortint(r) shr 1) and $ff) shl 24);
    end;
  20:begin; {shsubaddx}
    r:=(p shr 16)-(o and $ffff);i:=integer(r) shr 1;
    r:=(o shr 16)+(p and $ffff);o:=integer(r) shr 1;
    i:=(i shl 16)+(o and $ffff);
    end;
  21:begin; {ssub16}
    i:=integer(p)-integer(o);
    r:=p shr 16;s:=o shr 16;o:=integer(r)-integer(s);
    i:=(o shl 16)+(i and $ffff);
    end;
  22:begin; {ssub8}
    r:=p-o;i:=(shortint(r) shr 1) and $ff;
    r:=(p shr 8)-(o shr 8);inc(i,(shortint(r) and $ff) shl 8);
    r:=(p shr 16)-(o shr 16);inc(i,(shortint(r) and $ff) shl 16);
    r:=(p shr 24)-(o shr 24);inc(i,(shortint(r) and $ff) shl 24);
    end;
  23:begin; {ssubaddx}
    r:=(p shr 16)-(o and $ffff);i:=integer(r);
    r:=(o shr 16)+(p and $ffff);o:=integer(r);
    i:=(i shl 16)+(o and $ffff);
    end;
  24:begin; {swp}
    if checkRange(p) then exit;
    i:=ReadLongLSB(memoryData^[p]);
    WriteLongLSB(memoryData^[p],o);
    end;
  25:begin; {swpb}
    if checkRange(p) then exit;
    i:=memoryData^[p];
    memoryData^[p]:=o;
    end;
  26:begin; {sxtab}
    i:=((op shr 10) and 3)*8;
    r:=o shr i;i:=shortint(r)+p;
    end;
  27:begin; {sxtab16}
    i:=((op shr 10) and 3)*8;
    o:=rotateRight(o,32,i);
    i:=(shortint(o)+p) and $ffff;
    r:=o shr 16;inc(i,(shortint(r)+(p shr 16)) shl 16);
    end;
  28:begin; {sxtah}
    i:=((op shr 10) and 3)*8;
    o:=rotateRight(o,32,i);
    i:=integer(o)+p;
    end;
  29:begin; {sxtb}
    i:=((op shr 10) and 3)*8;
    r:=o shr i;i:=shortint(r);
    end;
  30:begin; {sxtb16}
    i:=((op shr 10) and 3)*8;
    o:=rotateRight(o,32,i);
    i:=shortint(o);
    r:=o shr 16;inc(i,shortint(r) shl 16);
    end;
  31:begin; {sxth}
    i:=((op shr 10) and 3)*8;
    r:=o shr i;i:=integer(r);
    end;
  32:begin; {uadd16}
    i:=(o+p) and $ffff;
    inc(i,((o shr 16)+(p shr 16)) shl 16);
    end;
  33:begin; {uadd8}
    i:=(o+p) and $ff;
    inc(i,(((o shr 8)+(p shr 8)) and $ff) shl 8);
    inc(i,(((o shr 16)+(p shr 16)) and $ff) shl 16);
    inc(i,(((o shr 24)+(p shr 24)) and $ff) shl 24);
    end;
  34:begin; {uaddsubx}
    i:=((p shr 16)+o) and $ffff;
    inc(i,((p and $ffff)-(o shr 16)) shl 16);
    end;
  35:begin; {uhsub16}
    i:=(p-o) and $ffff;
    inc(i,((p shr 16)-(o shr 16)) shl 16);
    end;
  36:begin; {uhsub8}
    i:=(p-o) and $ff;
    inc(i,(((p shr 8)-(o shr 8)) and $ff) shl 8);
    inc(i,(((p shr 16)-(o shr 16)) and $ff) shl 16);
    inc(i,(((p shr 24)-(o shr 24)) and $ff) shl 24);
    end;
  37:begin; {uhsubaddx}
    i:=((p shr 16)-o) and $ffff;
    inc(i,((p and $ffff)+(o shr 16)) shl 16);
    end;
  38:begin; {uqadd16}
    i:=doUsat((o and $ffff)+(p and $ffff),16);
    inc(i,doUsat((o shr 16)+(p shr 16),16) shl 16);
    end;
  39:begin; {uqadd8}
    i:=doUsat((o and $ff)+(p and $ff),8);
    inc(i,doUsat(((o shr 8) and $ff)+((p shr 8) and $ff),8) shl 8);
    inc(i,doUsat(((o shr 16) and $ff)+((p shr 16) and $ff),8) shl 16);
    inc(i,doUsat(((o shr 24) and $ff)+((p shr 24) and $ff),8) shl 24);
    end;
  40:begin; {uqaddsubx}
    i:=doUsat((p and $ffff)-(o shr 16),16);
    inc(i,doUsat((p shr 16)+(o and $ffff),16) shl 16);
    end;
  41:begin; {uqsub16}
    i:=doUsat((p and $ffff)-(o and $ffff),16);
    inc(i,doUsat((p shr 16)-(o shr 16),16) shl 16);
    end;
  42:begin; {uqsub8}
    i:=doUsat((p and $ff)-(o and $ff),8);
    inc(i,doUsat(((o shr 8) and $ff)-((o shr 8) and $ff),8) shl 8);
    inc(i,doUsat(((o shr 16) and $ff)-((o shr 16) and $ff),8) shl 16);
    inc(i,doUsat(((o shr 24) and $ff)-((o shr 24) and $ff),8) shl 24);
    end;
  43:begin; {uqsubaddx}
    i:=doUsat((p and $ffff)+(o shr 16),16);
    inc(i,doUsat((p shr 16)-(o and $ffff),16) shl 16);
    end;
  44:begin; {usub16}
    i:=((p and $ffff)-(o and $ffff)) and $ffff;
    inc(i,((p shr 16)-(o shr 16)) shl 16);
    end;
  45:begin; {usub8}
    i:=((p and $ff)-(o and $ff)) and $ff;
    inc(i,(((p shr 8) and $ff)-((o shr 8) and $ff)) shl 8);
    inc(i,(((p shr 16) and $ff)-((o shr 16) and $ff)) shl 16);
    inc(i,(((p shr 24) and $ff)-((o shr 24) and $ff)) shl 24);
    end;
  46:begin; {usubaddx}
    i:=((p shr 16)-(o and $ffff)) shl 16;
    inc(i,((p and $ffff)+(o shr 16)) and $ff);
    end;
  47:begin; {uxtab}
    i:=((op shr 10) and 3)*8;
    i:=((o shr i) and $ff)+p;
    end;
  48:begin; {uxtab16}
    i:=((op shr 10) and 3)*8;
    o:=rotateRight(o,32,i);
    i:=((o and $ff)+p) and $ffff;
    inc(i,(((o shr 16) and $ff)+(p shr 16)) shl 16);
    end;
  49:begin; {uxtah}
    i:=((op shr 10) and 3)*8;
    o:=rotateRight(o,32,i);
    i:=(o and $ffff)+p;
    end;
  50:begin; {uxtb}
    i:=((op shr 10) and 3)*8;
    i:=(o shr i) and $ff;
    end;
  51:begin; {uxtb16}
    i:=((op shr 10) and 3)*8;
    o:=rotateRight(o,32,i);
    i:=o and $ff;
    inc(i,((o shr 16) and $ff) shl 16);
    end;
  52:begin; {uxth}
    i:=((op shr 10) and 3)*8;
    i:=(o shr i) and $ffff;
    end;
  end;
regs[(op shr 12) and $f]:=i;
End;


Procedure doCndRnDat(md:LongInt);
Var i,o,p:LongInt;
Begin;
if not testCnd(op) then exit;
o:=getData(op);
p:=regs[(op shr 16) and $f];
i:=0;
case md of
  1:begin; {cmn}
    i:=3;
    end;
  2:begin; {cmp}
    o:=-o;
    i:=3;
    end;
  3:begin; {teq}
    p:=p xor o;
    o:=0;
    i:=1;
    end;
  4:begin; {tst}
    p:=p and o;
    o:=0;
    i:=1;
    end;
  end;
if (i and 1<>0) then setFlagNZ(o+p);
if (i and 2<>0) then setFlagCV(o,p);
End;



Procedure doCndSrDrNdat(md:LongInt);
Var i,o,p:LongInt;
Begin;
if not testCnd(op) then exit;
i:=(op shr 12) and $f;
o:=regs[(op shr 16) and $f];
p:=getData(op);
case md of
  1:begin; {adc}
    if (reg_cpsr and flg_C<>0) then inc(p);
    regs[i]:=o+p;
    i:=3;
    end;
  2:begin; {add}
    regs[i]:=o+p;
    i:=3;
    end;
  3:begin; {and}
    o:=o and p;p:=0;
    regs[i]:=o;
    i:=1;
    end;
  4:begin; {bic}
    o:=o and (not p);p:=0;
    regs[i]:=o;
    i:=1;
    end;
  5:begin; {eor}
    o:=o xor p;p:=0;
    regs[i]:=o;
    i:=1;
    end;
  6:begin; {mov}
    regs[i]:=p;
    o:=0;
    i:=1;
    end;
  7:begin; {mvn}
    p:=not p;
    regs[i]:=p;
    o:=0;
    i:=1;
    end;
  8:begin; {orr}
    o:=o or p;p:=0;
    regs[i]:=o;
    i:=3;
    end;
  9:begin; {rsb}
    regs[i]:=p-o;
    o:=-o;
    i:=3;
    end;
  10:begin; {rsc}
    if (reg_cpsr and flg_C=0) then inc(o);
    regs[i]:=p-o;
    o:=-o;
    i:=3;
    end;
  11:begin; {sbc}
    if (reg_cpsr and flg_C=0) then inc(p);
    regs[i]:=o-p;
    p:=-p;
    i:=3;
    end;
  12:begin; {sub}
    regs[i]:=o-p;
    p:=-p;
    i:=3;
    end;
  end;
if (op and $00100000=0) then exit;
if (i and 1<>0) then setFlagNZ(o+p);
if (i and 2<>0) then setFlagCV(o,p);
End;



Procedure doCndMor(md:LongInt);
Var i,o,p:LongInt;
Begin;
if not testCnd(op) then exit;
p:=regs[(op shr 16) and $f];
if checkRange(p) then exit;
i:=countBits(op and $ffff);
for o:=0 to 15 do begin;
  if ((1 shl o) and op=0) then continue;
  if (op and $01000000<>0) then begin;
    if (op and $00800000<>0) then inc(p,4) else dec(p,4);
    end;
  case md of
    1:begin; {ldm}
      regs[o]:=ReadLongLSB(memoryData^[p]);
      end;
    2:begin; {stm}
      WriteLongLSB(memoryData^[p],regs[o]);
      end;
    end;
  if (op and $01000000=0) then begin;
    if (op and $00800000<>0) then inc(p,4) else dec(p,4);
    end;
  end;
if (op and $00200000<>0) then regs[(op shr 16) and $f]:=p;
End;



Procedure doCndRdAdr(md:LongInt);
Var i,o,p:LongInt;
Begin;
if not testCnd(op) then exit;
p:=getWord(op,4);
if checkRange(p) then exit;
o:=regs[(op shr 12) and $f];
case md of
  1:begin; {ldr}
    o:=ReadLongLSB(memoryData^[p]);
    end;
  2:begin; {ldrb}
    o:=memoryData^[p];
    end;
  3:begin; {str}
    WriteLongLSB(memoryData^[p],o);
    end;
  4:begin; {strb}
    memoryData^[p]:=o;
    end;
  end;
regs[(op shr 12) and $f]:=o;
End;



Procedure doCndRdMsc(md:LongInt);
Var i,o,p:LongInt;
Begin;
if not testCnd(op) then exit;
p:=getMisc(op,4);
if checkRange(p) then exit;
o:=(op shr 12) and $f;
case md of
  1:begin; {ldrd}
    regs[o]:=ReadLongLSB(memoryData^[p]);
    regs[o+1]:=ReadLongLSB(memoryData^[p+4]);
    end;
  2:begin; {ldrh}
    regs[o]:=ReadWordLSB(memoryData^[p]);
    end;
  3:begin; {ldrsb}
    i:=memoryData^[p];
    regs[o]:=shortint(i);
    end;
  4:begin; {ldrsh}
    i:=ReadWordLSB(memoryData^[p]);
    regs[o]:=integer(i);
    end;
  5:begin; {strd}
    WriteLongLSB(memoryData^[p],regs[o]);
    WriteLongLSB(memoryData^[p+4],regs[o+1]);
    end;
  6:begin; {strh}
    WriteWordLSB(memoryData^[p],regs[o]);
    end;
  end;
End;



Procedure doCndSrDrMrSrN(md:LongInt);
Var i,o,p,q:LongInt;
Begin;
if not testCnd(op) then exit;
i:=regs[(op shr 16) and $f];
o:=regs[op and $f];
p:=regs[(op shr 8) and $f];
q:=regs[(op shr 12) and $f];
case md of
  1:begin; {mla}
    i:=(o*p)+q;
    end;
  2:begin; {smlal}
    doMulS32hl(o,p);
    inc(regs[(op shr 12) and $f],p);
    inc(i,o);
    end;
  3:begin; {smull}
    doMulS32hl(o,p);
    inc(regs[(op shr 12) and $f],p);
    i:=o;
    end;
  4:begin; {umlal}
    doMulU32hl(o,p);
    inc(p,i);
    inc(p,q);
    regs[(op shr 12) and $f]:=p;
    i:=o;
    end;
  5:begin; {umull}
    doMulU32hl(o,p);
    regs[(op shr 12) and $f]:=p;
    i:=o;
    end;
  end;
if (op and $00100000<>0) then setFlagNZ(i);
regs[(op shr 16) and $f]:=i;
End;



Procedure doCndSrDrMrS(md:LongInt);
Var i,o,p:LongInt;
Begin;
if not testCnd(op) then exit;
i:=regs[(op shr 16) and $f];
o:=regs[op and $f];
p:=regs[(op shr 8) and $f];
case md of
  1:begin; {mla}
    i:=o*p;
    end;
  end;
if (op and $00100000<>0) then setFlagNZ(i);
regs[(op shr 16) and $f]:=i;
End;



Procedure doMsrCnd(i:LongInt);
Var o,p:LongInt;
Begin;
if not testCnd(op) then exit;
o:=0;
for p:=0 to 3 do if ((1 shl (p+16)) and op<>0) then inc(o,$ff shl (p*8));
reg_cpsr:=((reg_cpsr or o) xor o) or i;
End;



Function doUsad(o,p:LongInt):LongInt;
Var i,q,r:LongInt;
Begin;
q:=o and $ff;
r:=p and $ff;
if (q>r) then i:=q-r else i:=r-q;
q:=(o shr 8) and $ff;
r:=(p shr 8) and $ff;
if (q>r) then inc(i,(q-r) shl 8) else inc(i,(r-q) shl 8);
q:=(o shr 16) and $ff;
r:=(p shr 16) and $ff;
if (q>r) then inc(i,(q-r) shl 16) else inc(i,(r-q) shl 16);
q:=(o shr 24) and $ff;
r:=(p shr 24) and $ff;
if (q>r) then inc(i,(q-r) shl 24) else inc(i,(r-q) shl 24);
doUsad:=i;
End;


Procedure doCndRdRmRsRn(md:LongInt);
Var i,o,p,q,r,s:LongInt;
Begin;
if not testCnd(op) then exit;
i:=regs[(op shr 16) and $f];
o:=regs[op and $f];
p:=regs[(op shr 8) and $f];
q:=regs[(op shr 12) and $f];
case md of
  0..3:begin; {smla}
    if (md and 1<>0) then o:=o shr 16;
    if (md and 2<>0) then p:=p shr 16;
    i:=(integer(o)*integer(p))+q;
    end;
  4..5:begin; {smlad}
    if (md and 1<>0) then p:=rotateLeft(p,32,16);
    i:=integer(o)*integer(p);
    r:=o shr 16;s:=p shr 16;inc(i,integer(r)*integer(s));
    inc(i,q);
    end;
  8..11:begin; {smlal}
    if (md and 1<>0) then o:=o shr 16;
    if (md and 2<>0) then p:=p shr 16;
    doMulS32hl(o,p);
    inc(regs[(op shr 12) and $f],p);
    inc(i,o);
    end;
  6..7:begin; {smlald}
    if (md and 1<>0) then q:=rotateLeft(q,32,16);
    inc(i,integer(q)*integer(p));
    r:=q shr 16;s:=p shr 16;inc(i,integer(r)*integer(s));
    end;
  12..13:begin; {smlaw}
    if (md and 1<>0) then p:=p shr 16;
    i:=integer(p)*integer(o);
    inc(i,q);
    end;
  14..15:begin; {smlsd}
    if (md and 1<>0) then p:=rotateLeft(p,32,16);
    i:=integer(o)*integer(p);
    r:=o shr 16;s:=p shr 16;p:=integer(r)*integer(s);
    inc(i,q-p);
    end;
  16..17:begin; {smlsld}
    if (md and 1<>0) then p:=rotateLeft(p,32,16);
    q:=integer(o)*integer(p);
    r:=o shr 16;s:=p shr 16;p:=integer(r)*integer(s);
    inc(i,q-p);
    end;
  18..19:begin; {smmla}
    doMulS32hl(o,p);
    i:=q+o;
    end;
  20..21:begin; {smmls}
    doMulS32hl(o,p);
    i:=q-o;
    end;
  22..23:begin; {smmul}
    doMulS32hl(o,p);
    i:=o;
    end;
  24..25:begin; {smuad}
    if (md and 1<>0) then p:=rotateLeft(p,32,16);
    i:=integer(o)*integer(p);
    r:=o shr 16;s:=p shr 16;inc(i,integer(r)*integer(s));
    end;
  28..31:begin; {smul}
    if (md and 1<>0) then o:=o shr 16;
    if (md and 2<>0) then p:=p shr 16;
    i:=integer(o)*integer(p);
    end;
  26..27:begin; {smulw}
    if (md and 1<>0) then p:=p shr 16;
    p:=integer(p)*o;
    end;
  32..33:begin; {smusd}
    if (md and 1<>0) then p:=rotateLeft(p,32,16);
    i:=integer(o)*integer(p);
    r:=o shr 16;s:=p shr 16;
    dec(i,integer(r)*integer(s));
    end;
  34:begin; {umaal}
    doMulU32hl(o,p);
    inc(p,i);
    inc(p,q);
    regs[(op shr 12) and $f]:=p;
    i:=o;
    end;
  35:begin; {usad8}
    i:=doUsad(o,p);
    end;
  36:begin; {usada8}
    i:=doUsad(o,p)+q;
    end;
  end;
regs[(op shr 16) and $f]:=i;
End;

Procedure doCndRdImmRmSft(md:LongInt);
Var i,o,p,q:LongInt;
Begin;
i:=regs[(op shr 12) and $f];
o:=(op shr 16) and $1f;
p:=regs[op and $f];
q:=(op shr 7) and $1f;
case md of
  1:begin; {ssat}
    if (op and $40=0) then p:=p shl q else p:=p shr q;
    i:=doSsat(p,o+q);
    end;
  2:begin; {ssat16}
    o:=(o and $f)+1;
    q:=doSsat(p and $ffff,o);
    p:=doSsat(p shr 16,o);
    i:=(p shl 16)+(q and $ffff);
    end;
  3:begin; {usat}
    if (op and $40=0) then p:=p shl q else p:=p shr q;
    i:=doUsat(p,o+q);
    end;
  4:begin; {usat16}
    o:=(o and $f)+1;
    q:=doUsat(p and $ffff,o);
    p:=doUsat(p shr 16,o);
    i:=(p shl 16)+(q and $ffff);
    end;
  end;
regs[(op shr 12) and $f]:=i;
End;


Var i,o:LongInt;
Begin;
result:=1;
if checkRange(regs[reg_pc]) then goto vege;
op:=ReadLongLSB(memoryData^[regs[reg_pc]]);
inc(regs[reg_pc],4);

case op and $0ff0f0f0 of
  $0780f010:begin; {usad8}
    {$ifdef debug}writeln('usad8Cnd rd,rm,rs');{$endif}
    doCndRdRmRsRn(35);
    goto ok;
    end;
  end;

case op and $0ff0f0d0 of
  $0750f010:begin;
    {$ifdef debug}writeln('smmulCnd rd,rm,rs');{$endif}
    doCndRdRmRsRn(((op shr 5) and 1)+22);
    goto ok;
    end;
  $0700f010:begin;
    {$ifdef debug}writeln('smuadCnd rd,rm,rs');{$endif}
    doCndRdRmRsRn(((op shr 5) and 1)+24);
    goto ok;
    end;
  $0700f050:begin;
    {$ifdef debug}writeln('smusdCnd rd,rm,rs');{$endif}
    doCndRdRmRsRn(((op shr 5) and 1)+32);
    goto ok;
    end;
  end;

case op and $0fff00f0 of
  $06af0070:begin;
    {$ifdef debug}writeln('sxtb rd,rm');{$endif}
    doCndRdRmRn(29);
    goto ok;
    end;
  $068f0070:begin;
    {$ifdef debug}writeln('sxtb16 rd,rm');{$endif}
    doCndRdRmRn(30);
    goto ok;
    end;
  $06bf0070:begin;
    {$ifdef debug}writeln('sxth rd,rm');{$endif}
    doCndRdRmRn(31);
    goto ok;
    end;
  $06ef0070:begin;
    {$ifdef debug}writeln('uxtb rd,rm');{$endif}
    doCndRdRmRn(50);
    goto ok;
    end;
  $06cf0070:begin;
    {$ifdef debug}writeln('uxtb16 rd,rm');{$endif}
    doCndRdRmRn(51);
    goto ok;
    end;
  $06ff0070:begin;
    {$ifdef debug}writeln('uxth rd,rm');{$endif}
    doCndRdRmRn(52);
    goto ok;
    end;
  end;

case op and $0ff000f0 of
  $01200010:begin;
    {$ifdef debug}writeln('bxCnd reg');{$endif}
    if not testCnd(op) then goto ok;
    regs[reg_pc]:=regs[op and $f];
    goto ok;
    end;
  $01200020:begin;
    {$ifdef debug}writeln('bxjCnd reg');{$endif}
    if not testCnd(op) then goto ok;
    regs[reg_pc]:=regs[op and $f];
    goto ok;
    end;
  $01200030:begin;
    {$ifdef debug}writeln('blxCnd reg');{$endif}
    if not testCnd(op) then goto ok;
    regs[reg_lr]:=regs[reg_pc];
    regs[reg_pc]:=regs[op and $f];
    goto ok;
    end;
  $01600010:begin;
    {$ifdef debug}writeln('clzCnd rd,rm');{$endif}
    doCndRdRm(1);
    goto ok;
    end;
  $06b00030:begin;
    {$ifdef debug}writeln('revCnd rd,rm');{$endif}
    doCndRdRm(2);
    goto ok;
    end;
  $06b000b0:begin;
    {$ifdef debug}writeln('rev16Cnd rd,rm');{$endif}
    doCndRdRm(3);
    goto ok;
    end;
  $06f000b0:begin;
    {$ifdef debug}writeln('revshCnd rd,rm');{$endif}
    doCndRdRm(4);
    goto ok;
    end;
  $01900090:begin;
    {$ifdef debug}writeln('ldrex rd,rn');{$endif}
    doCndRdRn(1);
    goto ok;
    end;
  $01000050:begin;
    {$ifdef debug}writeln('qadd rd,rm,rn');{$endif}
    doCndRdRmRn(1);
    goto ok;
    end;
  $06200010:begin;
    {$ifdef debug}writeln('qadd16 rd,rm,rn');{$endif}
    doCndRdRmRn(2);
    goto ok;
    end;
  $06200090:begin;
    {$ifdef debug}writeln('qadd8 rd,rm,rn');{$endif}
    doCndRdRmRn(3);
    goto ok;
    end;
  $06200030:begin;
    {$ifdef debug}writeln('qaddsubx rd,rm,rn');{$endif}
    doCndRdRmRn(4);
    goto ok;
    end;
  $01400050:begin;
    {$ifdef debug}writeln('qdadd rd,rm,rn');{$endif}
    doCndRdRmRn(5);
    goto ok;
    end;
  $01600050:begin;
    {$ifdef debug}writeln('qdsub rd,rm,rn');{$endif}
    doCndRdRmRn(6);
    goto ok;
    end;
  $01200050:begin;
    {$ifdef debug}writeln('qsub rd,rm,rn');{$endif}
    doCndRdRmRn(7);
    goto ok;
    end;
  $06200070:begin;
    {$ifdef debug}writeln('qsub16 rd,rm,rn');{$endif}
    doCndRdRmRn(8);
    goto ok;
    end;
  $062000f0:begin;
    {$ifdef debug}writeln('qsub8 rd,rm,rn');{$endif}
    doCndRdRmRn(9);
    goto ok;
    end;
  $06200050:begin;
    {$ifdef debug}writeln('qsubaddx rd,rm,rn');{$endif}
    doCndRdRmRn(10);
    goto ok;
    end;
  $06100010:begin;
    {$ifdef debug}writeln('sadd16 rd,rm,rn');{$endif}
    doCndRdRmRn(11);
    goto ok;
    end;
  $06100090:begin;
    {$ifdef debug}writeln('sadd8 rd,rm,rn');{$endif}
    doCndRdRmRn(12);
    goto ok;
    end;
  $06100030:begin;
    {$ifdef debug}writeln('saddsubx rd,rm,rn');{$endif}
    doCndRdRmRn(13);
    goto ok;
    end;
  $068000b0:begin;
    {$ifdef debug}writeln('sel rd,rm,rn');{$endif}
    doCndRdRmRn(14);
    goto ok;
    end;
  $06300010:begin;
    {$ifdef debug}writeln('shadd16 rd,rm,rn');{$endif}
    doCndRdRmRn(15);
    goto ok;
    end;
  $06300090:begin;
    {$ifdef debug}writeln('shadd8 rd,rm,rn');{$endif}
    doCndRdRmRn(16);
    goto ok;
    end;
  $06300030:begin;
    {$ifdef debug}writeln('shaddsubx rd,rm,rn');{$endif}
    doCndRdRmRn(17);
    goto ok;
    end;
  $06300070:begin;
    {$ifdef debug}writeln('shsub16 rd,rm,rn');{$endif}
    doCndRdRmRn(18);
    goto ok;
    end;
  $063000f0:begin;
    {$ifdef debug}writeln('shsub8 rd,rm,rn');{$endif}
    doCndRdRmRn(19);
    goto ok;
    end;
  $06300050:begin;
    {$ifdef debug}writeln('shsubaddx rd,rm,rn');{$endif}
    doCndRdRmRn(20);
    goto ok;
    end;
  $06a00030:begin;
    {$ifdef debug}writeln('ssat16Cnd rd,imm,rm');{$endif}
    doCndRdImmRmSft(2);
    goto ok;
    end;
  $06100070:begin;
    {$ifdef debug}writeln('ssub16 rd,rm,rn');{$endif}
    doCndRdRmRn(21);
    goto ok;
    end;
  $061000f0:begin;
    {$ifdef debug}writeln('ssub8 rd,rm,rn');{$endif}
    doCndRdRmRn(22);
    goto ok;
    end;
  $06100050:begin;
    {$ifdef debug}writeln('ssubaddx rd,rm,rn');{$endif}
    doCndRdRmRn(23);
    goto ok;
    end;
  $01800090:begin;
    {$ifdef debug}writeln('strex rd,rn');{$endif}
    doCndRdRn(2);
    goto ok;
    end;
  $01000090:begin;
    {$ifdef debug}writeln('swp rd,rn,rm');{$endif}
    doCndRdRmRn(24);
    goto ok;
    end;
  $01400090:begin;
    {$ifdef debug}writeln('swpb rd,rn,rm');{$endif}
    doCndRdRmRn(25);
    goto ok;
    end;
  $06a00070:begin;
    {$ifdef debug}writeln('sxtab rd,rn,rm');{$endif}
    doCndRdRmRn(26);
    goto ok;
    end;
  $06900070:begin;
    {$ifdef debug}writeln('sxtab16 rd,rn,rm');{$endif}
    doCndRdRmRn(27);
    goto ok;
    end;
  $06b00070:begin;
    {$ifdef debug}writeln('sxtah rd,rn,rm');{$endif}
    doCndRdRmRn(28);
    goto ok;
    end;
  $06500010:begin;
    {$ifdef debug}writeln('uadd16 rd,rn,rm');{$endif}
    doCndRdRmRn(32);
    goto ok;
    end;
  $06500090:begin;
    {$ifdef debug}writeln('uadd8 rd,rn,rm');{$endif}
    doCndRdRmRn(33);
    goto ok;
    end;
  $06500030:begin;
    {$ifdef debug}writeln('uaddsubx rd,rn,rm');{$endif}
    doCndRdRmRn(34);
    goto ok;
    end;
  $06700070:begin;
    {$ifdef debug}writeln('uhsub16 rd,rn,rm');{$endif}
    doCndRdRmRn(35);
    goto ok;
    end;
  $067000f0:begin;
    {$ifdef debug}writeln('uhsub8 rd,rn,rm');{$endif}
    doCndRdRmRn(36);
    goto ok;
    end;
  $06700050:begin;
    {$ifdef debug}writeln('uhsubaddx rd,rn,rm');{$endif}
    doCndRdRmRn(37);
    goto ok;
    end;
  $00400090:begin;
    {$ifdef debug}writeln('umaal rd,rn,rm');{$endif}
    doCndRdRmRsRn(34);
    goto ok;
    end;
  $06600010:begin;
    {$ifdef debug}writeln('uqadd16 rd,rn,rm');{$endif}
    doCndRdRmRn(38);
    goto ok;
    end;
  $06600090:begin;
    {$ifdef debug}writeln('uqadd8 rd,rn,rm');{$endif}
    doCndRdRmRn(39);
    goto ok;
    end;
  $06600030:begin;
    {$ifdef debug}writeln('uqaddsubx rd,rn,rm');{$endif}
    doCndRdRmRn(40);
    goto ok;
    end;
  $06600070:begin;
    {$ifdef debug}writeln('uqsub16 rd,rn,rm');{$endif}
    doCndRdRmRn(41);
    goto ok;
    end;
  $066000f0:begin;
    {$ifdef debug}writeln('uqsub8 rd,rn,rm');{$endif}
    doCndRdRmRn(42);
    goto ok;
    end;
  $06600050:begin;
    {$ifdef debug}writeln('uqsubaddx rd,rn,rm');{$endif}
    doCndRdRmRn(43);
    goto ok;
    end;
  $07800010:begin; {usada8}
    {$ifdef debug}writeln('usada8Cnd rd,rm,rn,rs');{$endif}
    doCndRdRmRsRn(36);
    goto ok;
    end;
  $06e00030:begin;
    {$ifdef debug}writeln('usat16Cnd rd,imm,rm');{$endif}
    doCndRdImmRmSft(4);
    goto ok;
    end;
  $06500070:begin;
    {$ifdef debug}writeln('usub16 rd,rn,rm');{$endif}
    doCndRdRmRn(44);
    goto ok;
    end;
  $065000f0:begin;
    {$ifdef debug}writeln('usub8 rd,rn,rm');{$endif}
    doCndRdRmRn(45);
    goto ok;
    end;
  $06500050:begin;
    {$ifdef debug}writeln('usubaddx rd,rn,rm');{$endif}
    doCndRdRmRn(46);
    goto ok;
    end;
  $06e00070:begin;
    {$ifdef debug}writeln('uxtab rd,rn,rm');{$endif}
    doCndRdRmRn(47);
    goto ok;
    end;
  $06c00070:begin;
    {$ifdef debug}writeln('uxtab16 rd,rn,rm');{$endif}
    doCndRdRmRn(48);
    goto ok;
    end;
  $06f00070:begin;
    {$ifdef debug}writeln('uxtah rd,rn,rm');{$endif}
    doCndRdRmRn(49);
    goto ok;
    end;
  end;

case op and $0fe000f0 of
  $00200090:begin;
    {$ifdef debug}writeln('mlaCndS rd,rm,rs,rn');{$endif}
    doCndSrDrMrSrN(1);
    goto ok;
    end;
  $00000090:begin;
    {$ifdef debug}writeln('mulCndS rd,rm,rs');{$endif}
    doCndSrDrMrS(1);
    goto ok;
    end;
  $00e00090:begin;
    {$ifdef debug}writeln('smlalCndS rd,rm,rs,rn');{$endif}
    doCndSrDrMrSrN(2);
    goto ok;
    end;
  $00c00090:begin;
    {$ifdef debug}writeln('smullCndS rd,rm,rs,rn');{$endif}
    doCndSrDrMrSrN(3);
    goto ok;
    end;
  $00a00090:begin;
    {$ifdef debug}writeln('umlal rd,rn,rs,rm');{$endif}
    doCndSrDrMrSrN(4);
    goto ok;
    end;
  $00800090:begin;
    {$ifdef debug}writeln('umull rd,rn,rs,rm');{$endif}
    doCndSrDrMrSrN(5);
    goto ok;
    end;
  end;

case op and $0ff00070 of
  $06800010:begin;
    {$ifdef debug}writeln('pkhbt rd,rn,rm');{$endif}
    if not testCnd(op) then goto ok;
    i:=regs[(op shr 16) and $f];
    o:=regs[op and $f] shl ((op shr 7) and $1f);
    regs[(op shr 12) and $f]:=(i and $ffff) or (o and $ffff0000);
    goto ok;
    end;
  $06800050:begin;
    {$ifdef debug}writeln('pkhtb rd,rn,rm');{$endif}
    if not testCnd(op) then goto ok;
    i:=regs[(op shr 16) and $f];
    o:=regs[op and $f] shr ((op shr 7) and $1f);
    regs[(op shr 12) and $f]:=(i and $ffff0000) or (o and $ffff);
    goto ok;
    end;
  end;

case op and $0ff000b0 of
  $01200080:begin;
    {$ifdef debug}writeln('smlawCnd rd,rm,rs,rn');{$endif}
    doCndRdRmRsRn(((op shr 6) and 1)+12);
    goto ok;
    end;
  $012000a0:begin;
    {$ifdef debug}writeln('smulwCnd rd,rm,rs');{$endif}
    doCndRdRmRsRn(((op shr 6) and 1)+26);
    goto ok;
    end;
  end;

case op and $0ff000d0 of
  $07000010:begin;
    {$ifdef debug}writeln('smladCnd rd,rm,rs,rn');{$endif}
    doCndRdRmRsRn(((op shr 5) and 1)+4);
    goto ok;
    end;
  $07400010:begin;
    {$ifdef debug}writeln('smlaldCnd rd,rm,rs,rn');{$endif}
    doCndRdRmRsRn(((op shr 5) and 1)+6);
    goto ok;
    end;
  $07000050:begin;
    {$ifdef debug}writeln('smlsdCnd rd,rm,rs,rn');{$endif}
    doCndRdRmRsRn(((op shr 5) and 1)+14);
    goto ok;
    end;
  $07400050:begin;
    {$ifdef debug}writeln('smlsldCnd rd,rm,rs,rn');{$endif}
    doCndRdRmRsRn(((op shr 5) and 1)+16);
    goto ok;
    end;
  $07500010:begin;
    {$ifdef debug}writeln('smmlaCnd rd,rm,rs,rn');{$endif}
    doCndRdRmRsRn(((op shr 5) and 1)+18);
    goto ok;
    end;
  $075000d0:begin;
    {$ifdef debug}writeln('smmlsCnd rd,rm,rs,rn');{$endif}
    doCndRdRmRsRn(((op shr 5) and 1)+20);
    goto ok;
    end;
  end;

case op and $0ff00090 of
  $01000080:begin;
    {$ifdef debug}writeln('smlaCnd rd,rm,rs,rn');{$endif}
    doCndRdRmRsRn(((op shr 5) and 3)+0);
    goto ok;
    end;
  $01400080:begin;
    {$ifdef debug}writeln('smlaCnd rd,rm,rs,rn');{$endif}
    doCndRdRmRsRn(((op shr 5) and 3)+8);
    goto ok;
    end;
  $01600080:begin;
    {$ifdef debug}writeln('smulCnd rd,rm,rs,rn');{$endif}
    doCndRdRmRsRn(((op shr 5) and 3)+28);
    goto ok;
    end;
  end;

case op and $0fe00030 of
  $06a00010:begin;
    {$ifdef debug}writeln('ssatCnd rd,imm,rm,shft');{$endif}
    doCndRdImmRmSft(1);
    goto ok;
    end;
  $06e00010:begin;
    {$ifdef debug}writeln('usatCnd rd,imm,rm,shft');{$endif}
    doCndRdImmRmSft(3);
    goto ok;
    end;
  end;

case (op shr 16) and $0ff0 of
  $0100,$0140:begin;
    {$ifdef debug}writeln('msrCnd rn,cpsr');{$endif}
    if not testCnd(op) then goto ok;
    regs[(op shr 12) and $f]:=reg_cpsr;
    goto ok;
    end;
  $0320,$0360:begin;
    {$ifdef debug}writeln('msrCnd cpsr,imm');{$endif}
    doMsrCnd((op and $ff) shl (((op shr 8) and $f) shl 1));
    goto ok;
    end;
  $0120,$0160:begin;
    {$ifdef debug}writeln('msrCnd cpsr,reg');{$endif}
    doMsrCnd(regs[op and $f]);
    goto ok;
    end;
  end;

case (op shr 16) and $0df0 of
  $0170:begin;
    {$ifdef debug}writeln('cmnCnd rn,dat');{$endif}
    doCndRnDat(1);
    goto ok;
    end;
  $0150:begin;
    {$ifdef debug}writeln('cmpCnd rn,dat');{$endif}
    doCndRnDat(2);
    goto ok;
    end;
  $0130:begin;
    {$ifdef debug}writeln('teqCnd rn,dat');{$endif}
    doCndRnDat(3);
    goto ok;
    end;
  $0110:begin;
    {$ifdef debug}writeln('tstCnd rn,dat');{$endif}
    doCndRnDat(4);
    goto ok;
    end;
  end;

case (op shr 16) and $0de0 of
  $00a0:begin;
    {$ifdef debug}writeln('adcCndS rd,rn,dat');{$endif}
    doCndSrDrNdat(1);
    goto ok;
    end;
  $0080:begin;
    {$ifdef debug}writeln('addCndS rd,rn,dat');{$endif}
    doCndSrDrNdat(2);
    goto ok;
    end;
  $0000:begin;
    {$ifdef debug}writeln('andCndS rd,rn,dat');{$endif}
    doCndSrDrNdat(3);
    goto ok;
    end;
  $01c0:begin;
    {$ifdef debug}writeln('bicCndS rd,rn,dat');{$endif}
    doCndSrDrNdat(4);
    goto ok;
    end;
  $0020:begin;
    {$ifdef debug}writeln('eorCndS rd,rn,dat');{$endif}
    doCndSrDrNdat(5);
    goto ok;
    end;
  $01a0:begin;
    {$ifdef debug}writeln('movCndS rd,dat');{$endif}
    doCndSrDrNdat(6);
    goto ok;
    end;
  $01e0:begin;
    {$ifdef debug}writeln('mvnCndS rd,dat');{$endif}
    doCndSrDrNdat(7);
    goto ok;
    end;
  $0180:begin;
    {$ifdef debug}writeln('orrCndS rd,rn,dat');{$endif}
    doCndSrDrNdat(8);
    goto ok;
    end;
  $0060:begin;
    {$ifdef debug}writeln('rsbCndS rd,rn,dat');{$endif}
    doCndSrDrNdat(9);
    goto ok;
    end;
  $00e0:begin;
    {$ifdef debug}writeln('rscCndS rd,rn,dat');{$endif}
    doCndSrDrNdat(10);
    goto ok;
    end;
  $00c0:begin;
    {$ifdef debug}writeln('sbcCndS rd,rn,dat');{$endif}
    doCndSrDrNdat(11);
    goto ok;
    end;
  $0040:begin;
    {$ifdef debug}writeln('subCndS rd,rn,dat');{$endif}
    doCndSrDrNdat(12);
    goto ok;
    end;
  end;

case op and $0e1000f0 of
  $000000d0:begin;
    {$ifdef debug}writeln('ldrd reg');{$endif}
    doCndRdMsc(1);
    goto ok;
    end;
  $001000b0:begin;
    {$ifdef debug}writeln('ldrh reg');{$endif}
    doCndRdMsc(2);
    goto ok;
    end;
  $001000d0:begin;
    {$ifdef debug}writeln('ldrsb reg');{$endif}
    doCndRdMsc(3);
    goto ok;
    end;
  $001000f0:begin;
    {$ifdef debug}writeln('ldrsh reg');{$endif}
    doCndRdMsc(4);
    goto ok;
    end;
  $000000f0:begin;
    {$ifdef debug}writeln('strd reg');{$endif}
    doCndRdMsc(5);
    goto ok;
    end;
  $000000b0:begin;
    {$ifdef debug}writeln('strh reg');{$endif}
    doCndRdMsc(6);
    goto ok;
    end;
  end;

case (op shr 16) and $0e50 of
  $0810:begin;
    {$ifdef debug}writeln('ldm reg');{$endif}
    doCndMor(1);
    goto ok;
    end;
  $0800:begin;
    {$ifdef debug}writeln('stm reg');{$endif}
    doCndMor(2);
    goto ok;
    end;
  end;

case (op shr 16) and $0e00 of
  $0a00:begin;
    {$ifdef debug}writeln('bL ofs');{$endif}
    if not testCnd(op) then goto ok;
    if (op and $01000000<>0) then regs[reg_lr]:=regs[reg_pc];
    i:=(op and $ffffff) shl 8;
    i:=(i div 64)+4;
    inc(regs[reg_pc],i);
    goto ok;
    end;
  end;

case (op shr 16) and $0c50 of
  $0410:begin;
    {$ifdef debug}writeln('ldr rd,[adr]');{$endif}
    doCndRdAdr(1);
    goto ok;
    end;
  $0450:begin;
    {$ifdef debug}writeln('ldrb rd,[adr]');{$endif}
    doCndRdAdr(2);
    goto ok;
    end;
  $0400:begin;
    {$ifdef debug}writeln('str rd,[adr]');{$endif}
    doCndRdAdr(3);
    goto ok;
    end;
  $0440:begin;
    {$ifdef debug}writeln('strb rd,[adr]');{$endif}
    doCndRdAdr(4);
    goto ok;
    end;
  end;

goto vege;
ok:
result:=0;
vege:
emulateOneOpcode:=result;
exit;
End;
