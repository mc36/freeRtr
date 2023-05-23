Type memoryType=array[0..1] of byte;
Var
  memoryData:^memoryType;
  memorySize:LongInt;
Var
  regs:array[0..31] of LongInt;
  reg_pc,reg_npc,reg_psr,reg_y:LongInt;
Const
  reg_sp=14;
  reg_lr=15;
  reg_fp=30;
  flg_n=$800000;
  flg_z=$400000;
  flg_v=$200000;
  flg_c=$100000;


Procedure emulateRestart;
Var i:Longint;
Begin;
for i:=0 to 31 do regs[i]:=0;
regs[reg_sp]:=memorySize-4;
regs[reg_fp]:=regs[reg_sp];
reg_pc:=0;
reg_npc:=-1;
reg_y:=0;
reg_psr:=0;
End;

Function emulateOneOpcode:LongInt;
Label ok,vege;
Var op,result,nextpc:LongInt;

Function checkRange(p:LongInt):Boolean;
Begin;
if (p>=memorySize) or (p<0) then begin;
  result:=2;
  checkRange:=True;
  exit;
  end;
checkRange:=False;
End;

Procedure setR25(i:LongInt);
Begin;
regs[(op shr 25) and $1f]:=i;
End;

Procedure setFlag(i:LongInt;b:Boolean);
Begin;
if b then reg_psr:=reg_psr or i else reg_psr:=reg_psr and (not i);
End;

Procedure setNZ(i:LongInt);
Begin;
setFlag(flg_n,i<0);
setFlag(flg_z,i=0);
End;

Procedure setVCa(a,b,c:LongInt);
Begin;
a:=a shr 31;
b:=b shr 31;
c:=c shr 31;
setFlag(flg_v,(a and b and c) or (not a and not b and c)<>0);
setFlag(flg_c,(a and b) or (not c and (a or b))<>0);
End;

Procedure setVCs(a,b,c:LongInt);
Begin;
a:=a shr 31;
b:=b shr 31;
c:=c shr 31;
setFlag(flg_v,(a and not b and not c) or (not a and b and c)<>0);
setFlag(flg_c,(not a and b) or (c and (not a or b))<>0);
End;

Function testCond:Boolean;
Var z,n,c,v,r:Boolean;
Begin;
z:=(reg_psr and flg_z<>0);
n:=(reg_psr and flg_n<>0);
c:=(reg_psr and flg_c<>0);
v:=(reg_psr and flg_v<>0);
case (op shr 25) and 7 of
  0:r:=false;
  1:r:=z;
  2:r:=z or (n xor v);
  3:r:=n xor v;
  4:r:=c or z;
  5:r:=c;
  6:r:=n;
  7:r:=v;
  else r:=false;
  end;
if (op and $10000000<>0) then r:=not r;
testCond:=r;
End;

Var
  r0,r14,r25,r0im13,i22:LongInt;
  i,o:LongInt;
Begin;
result:=1;
if checkRange(reg_pc) then goto vege;
op:=ReadLongMSB(memoryData^[reg_pc]);
inc(reg_pc,4);
nextpc:=reg_npc;
reg_npc:=-1;

r0:=regs[op and $1f];
r14:=regs[(op shr 14) and $1f];
r25:=regs[(op shr 25) and $1f];
if (op and $2000=0) then r0im13:=r0 else begin;
  r0im13:=op and $1fff;
  if (op and $1000<>0) then r0im13:=r0im13 or $fffff000;
  end;
i22:=op and $3fffff;
if (op and $200000<>0) then i22:=i22 or $ffc00000;

if (op shr 30=1) then begin;
  {$ifdef debug}writeln('call i30');{$endif}
  i:=(op and $3fffffff)*4;
  regs[reg_lr]:=reg_pc+4;
  reg_npc:=i+reg_pc-4;
  goto ok;
  end;

case ((op shr 22) and 7) or ((op shr 24) and $c0) of
  $02:begin;
    {$ifdef debug}writeln('bc<,a> i22');{$endif}
    if testCond then reg_npc:=(i22*4)+reg_pc-4 else
    if (op and $20000000<>0) then inc(reg_pc,4);
    goto ok;
    end;
  $04:begin;
    {$ifdef debug}writeln('sethi rd,i22');{$endif}
    setR25(op shl 10);
    goto ok;
    end;
  end;

case ((op shr 19) and $3f) or ((op shr 24) and $c0) of
  $80:begin;
    {$ifdef debug}writeln('add rs,r/i,rd');{$endif}
    i:=r14+r0im13;
    setR25(i);
    goto ok;
    end;
  $90:begin;
    {$ifdef debug}writeln('addcc rs,r/i,rd');{$endif}
    i:=r14+r0im13;
    setR25(i);
    setNZ(i);
    setVCa(r14,r0im13,i);
    goto ok;
    end;
  $88:begin;
    {$ifdef debug}writeln('addx rs,r/i,rd');{$endif}
    if (reg_psr and flg_c<>0) then inc(r0im13);
    i:=r14+r0im13;
    setR25(i);
    goto ok;
    end;
  $98:begin;
    {$ifdef debug}writeln('addxcc rs,r/i,rd');{$endif}
    if (reg_psr and flg_c<>0) then inc(r0im13);
    i:=r14+r0im13;
    setR25(i);
    setNZ(i);
    setVCa(r14,r0im13,i);
    goto ok;
    end;
  $81:begin;
    {$ifdef debug}writeln('and rs,r/i,rd');{$endif}
    i:=r14 and r0im13;
    setR25(i);
    goto ok;
    end;
  $91:begin;
    {$ifdef debug}writeln('andcc rs,r/i,rd');{$endif}
    i:=r14 and r0im13;
    setR25(i);
    setNZ(i);
    setVCa(0,0,0);
    goto ok;
    end;
  $85:begin;
    {$ifdef debug}writeln('andn rs,r/i,rd');{$endif}
    i:=r14 and not r0im13;
    setR25(i);
    goto ok;
    end;
  $95:begin;
    {$ifdef debug}writeln('andnc rs,r/i,rd');{$endif}
    i:=r14 and not r0im13;
    setR25(i);
    setNZ(i);
    setVCa(0,0,0);
    goto ok;
    end;
  $b8:begin;
    {$ifdef debug}writeln('jmpl rs,r/i,rd');{$endif}
    setR25(reg_pc+4);
    reg_npc:=r14+r0im13;
    goto ok;
    end;
  $c0:begin;
    {$ifdef debug}writeln('ld rs,r/i,rd');{$endif}
    i:=r14+r0im13;
    if checkRange(i) then goto vege;
    setR25(ReadLongMSB(memoryData^[i]));
    goto ok;
    end;
  $c3:begin;
    {$ifdef debug}writeln('ldd rs,r/i,rd');{$endif}
    i:=r14+r0im13;
    if checkRange(i) then goto vege;
    setR25(ReadLongMSB(memoryData^[i]));
    regs[((op shr 25)+1) and $1f]:=ReadLongMSB(memoryData^[i+4]);
    goto ok;
    end;
  $c9:begin;
    {$ifdef debug}writeln('ldsb rs,r/i,rd');{$endif}
    i:=r14+r0im13;
    if checkRange(i) then goto vege;
    o:=memoryData^[i];
    o:=shortint(o);
    setR25(o);
    goto ok;
    end;
  $ca:begin;
    {$ifdef debug}writeln('ldsh rs,r/i,rd');{$endif}
    i:=r14+r0im13;
    if checkRange(i) then goto vege;
    o:=ReadWordMSB(memoryData^[i]);
    o:=integer(o);
    setR25(o);
    goto ok;
    end;
  $cd:begin;
    {$ifdef debug}writeln('ldstub rs,r/i,rd');{$endif}
    i:=r14+r0im13;
    if checkRange(i) then goto vege;
    setR25(memoryData^[i]);
    memoryData^[i]:=$ff;
    goto ok;
    end;
  $c1:begin;
    {$ifdef debug}writeln('ldub rs,r/i,rd');{$endif}
    i:=r14+r0im13;
    if checkRange(i) then goto vege;
    setR25(memoryData^[i]);
    goto ok;
    end;
  $c2:begin;
    {$ifdef debug}writeln('lduh rs,r/i,rd');{$endif}
    i:=r14+r0im13;
    if checkRange(i) then goto vege;
    setR25(ReadWordMSB(memoryData^[i]));
    goto ok;
    end;
  $a4:begin;
    {$ifdef debug}writeln('mulscc rs,r/i,rd');{$endif}
    r14:=r14 shr 1;
    if (reg_psr and flg_n<>0) xor (reg_psr and flg_v<>0) then r14:=r14 or $80000000;
    if (reg_y and 1=0) then r0im13:=0;
    setR25(r0im13+r14);
    goto ok;
    end;
  $82:begin;
    {$ifdef debug}writeln('or rs,r/i,rd');{$endif}
    i:=r14 or r0im13;
    setR25(i);
    goto ok;
    end;
  $92:begin;
    {$ifdef debug}writeln('orcc rs,r/i,rd');{$endif}
    i:=r14 or r0im13;
    setR25(i);
    setNZ(i);
    setVCa(0,0,0);
    goto ok;
    end;
  $86:begin;
    {$ifdef debug}writeln('orn rs,r/i,rd');{$endif}
    i:=r14 or not r0im13;
    setR25(i);
    goto ok;
    end;
  $96:begin;
    {$ifdef debug}writeln('orncc rs,r/i,rd');{$endif}
    i:=r14 or not r0im13;
    setR25(i);
    setNZ(i);
    setVCa(0,0,0);
    goto ok;
    end;
  $a9:begin;
    {$ifdef debug}writeln('rd psr,rd');{$endif}
    setR25(reg_psr);
    goto ok;
    end;
  $a8:begin;
    {$ifdef debug}writeln('rd y,rd');{$endif}
    setR25(reg_y);
    goto ok;
    end;
  $bd:begin;
    {$ifdef debug}writeln('restore rs,r/i,rd');{$endif}
    i:=r14+r0im13;
    setR25(i);
    goto ok;
    end;
  $bc:begin;
    {$ifdef debug}writeln('save rs,r/i,rd');{$endif}
    i:=r14+r0im13;
    setR25(i);
    goto ok;
    end;
  $a5:begin;
    {$ifdef debug}writeln('sll rs,r/i,rd');{$endif}
    setR25(r14 shl r0im13);
    goto ok;
    end;
  $a7:begin;
    {$ifdef debug}writeln('sra rs,r/i,rd');{$endif}
    setR25(r14 div (1 shl r0im13));
    goto ok;
    end;
  $a6:begin;
    {$ifdef debug}writeln('srl rs,r/i,rd');{$endif}
    setR25(r14 shr r0im13);
    goto ok;
    end;
  $c4:begin;
    {$ifdef debug}writeln('st rd,rs,r/i');{$endif}
    i:=r14+r0im13;
    if checkRange(i) then goto vege;
    WriteLongMSB(memoryData^[i],r25);
    goto ok;
    end;
  $c5:begin;
    {$ifdef debug}writeln('stb rd,rs,r/i');{$endif}
    i:=r14+r0im13;
    if checkRange(i) then goto vege;
    memoryData^[i]:=r25;
    goto ok;
    end;
  $c7:begin;
    {$ifdef debug}writeln('std rd,rs,r/i');{$endif}
    i:=r14+r0im13;
    if checkRange(i) then goto vege;
    WriteLongMSB(memoryData^[i],r25);
    WriteLongMSB(memoryData^[i+4],regs[((op shr 25)+1) and $1f]);
    goto ok;
    end;
  $c6:begin;
    {$ifdef debug}writeln('sth rd,rs,r/i');{$endif}
    i:=r14+r0im13;
    if checkRange(i) then goto vege;
    WriteWordMSB(memoryData^[i],r25);
    goto ok;
    end;
  $84:begin;
    {$ifdef debug}writeln('sub rd,rs,r/i');{$endif}
    i:=r14-r0im13;
    setR25(i);
    goto ok;
    end;
  $94:begin;
    {$ifdef debug}writeln('subcc rd,rs,r/i');{$endif}
    i:=r14-r0im13;
    setR25(i);
    setNZ(i);
    setVCs(r14,r0im13,i);
    goto ok;
    end;
  $8c:begin;
    {$ifdef debug}writeln('subx rd,rs,r/i');{$endif}
    if (reg_psr and flg_c<>0) then inc(r0im13);
    i:=r14-r0im13;
    setR25(i);
    goto ok;
    end;
  $9c:begin;
    {$ifdef debug}writeln('subxcc rd,rs,r/i');{$endif}
    if (reg_psr and flg_c<>0) then inc(r0im13);
    i:=r14-r0im13;
    setR25(i);
    setNZ(i);
    setVCs(r14,r0im13,i);
    goto ok;
    end;
  $cf:begin;
    {$ifdef debug}writeln('swap rd,rs,r/i');{$endif}
    i:=r14+r0im13;
    if checkRange(i) then goto vege;
    setR25(ReadLongMSB(memoryData^[i]));
    WriteLongMSB(memoryData^[i],r25);
    goto ok;
    end;
  $a0:begin;
    {$ifdef debug}writeln('taddcc rs,r/i,rd');{$endif}
    i:=r14+r0im13;
    setR25(i);
    setNZ(i);
    setVCa(r14,r0im13,i);
    goto ok;
    end;
  $a2:begin;
    {$ifdef debug}writeln('taddcctv rs,r/i,rd');{$endif}
    i:=r14+r0im13;
    setR25(i);
    setNZ(i);
    setVCa(r14,r0im13,i);
    goto ok;
    end;
  $ba:begin;
    {$ifdef debug}writeln('t<c> rs,r/i,rd');{$endif}
    goto ok;
    end;
  $a1:begin;
    {$ifdef debug}writeln('tsubcc rd,rs,r/i');{$endif}
    i:=r14-r0im13;
    setR25(i);
    setNZ(i);
    setVCs(r14,r0im13,i);
    goto ok;
    end;
  $a3:begin;
    {$ifdef debug}writeln('tsubcctv rd,rs,r/i');{$endif}
    i:=r14-r0im13;
    setR25(i);
    setNZ(i);
    setVCs(r14,r0im13,i);
    goto ok;
    end;
  $b1:begin;
    {$ifdef debug}writeln('wr rs,r/i,psr');{$endif}
    reg_psr:=r14 xor r0im13;
    goto ok;
    end;
  $b0:begin;
    {$ifdef debug}writeln('wr rs,r/i,y');{$endif}
    reg_y:=r14 xor r0im13;
    goto ok;
    end;
  $87:begin;
    {$ifdef debug}writeln('xnor rs,r/i,rd');{$endif}
    i:=r14 xor not r0im13;
    setR25(i);
    goto ok;
    end;
  $97:begin;
    {$ifdef debug}writeln('xnorcc rs,r/i,rd');{$endif}
    i:=r14 xor not r0im13;
    setR25(i);
    setNZ(i);
    setVCs(0,0,0);
    goto ok;
    end;
  $83:begin;
    {$ifdef debug}writeln('xor rs,r/i,rd');{$endif}
    i:=r14 xor r0im13;
    setR25(i);
    goto ok;
    end;
  $93:begin;
    {$ifdef debug}writeln('xorcc rs,r/i,rd');{$endif}
    i:=r14 xor r0im13;
    setR25(i);
    setNZ(i);
    setVCs(0,0,0);
    goto ok;
    end;
  end;

goto vege;
ok:
result:=0;
vege:
regs[0]:=0;
if (nextpc<>-1) then reg_pc:=nextpc;
emulateOneOpcode:=result;
exit;
End;
