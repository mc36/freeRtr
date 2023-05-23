Type memoryType=array[0..1] of byte;
Var
  memoryData:^memoryType;
  memorySize:LongInt;
Var
  regs:array[0..31] of LongInt;
  reg_pc,reg_npc,reg_hi,reg_lo:LongInt;

Procedure emulateRestart;
Var i:Longint;
Begin;
for i:=0 to 31 do regs[i]:=0;
regs[29]:=memorySize-4;
reg_pc:=0;
reg_npc:=-1;
reg_hi:=0;
reg_lo:=0;
End;



Function emulateOneOpcode:LongInt;
Label ok,vege;
Const
  bits:array[0..31] of LongInt=($80000000,$40000000,$20000000,$10000000,
    $8000000,$4000000,$2000000,$1000000,$800000,$400000,$200000,$100000,
    $80000,$40000,$20000,$10000,$8000,$4000,$2000,$1000,
    $800,$400,$200,$100,$80,$40,$20,$10,$8,$4,$2,$1);
Var result,op,nextpc:LongInt;

Function checkRange(p:LongInt):Boolean;
Begin;
if (p>=memorySize) or (p<0) then begin;
  result:=2;
  checkRange:=True;
  exit;
  end;
checkRange:=False;
End;

Function doOneComp3(o,a,b:LongInt;var c:LongInt):Boolean;
Label ok;
Var i,p:LongInt;
Begin;
doOneComp3:=true;
case o of
  1:c:=a+b;           {add}
  2:c:=a+b;           {addu}
  3:c:=a and b;       {and}
  4:begin; {div}
    i:=a;o:=b;
    doDivS32(i,b);
    reg_lo:=i;
    doMulS32hl(i,o);
    reg_hi:=a-i;
    end;
  5:begin; {divu}
    i:=a;o:=b;
    doDivU32(i,b);
    reg_lo:=i;
    doMulU32hl(i,o);
    reg_hi:=a-i;
    end;
  6:begin; {jalr}
    c:=reg_pc+4;
    reg_npc:=a;
    end;
  7:reg_npc:=a;       {jr}
  8:c:=reg_hi;        {mfhi}
  9:c:=reg_lo;        {mflo}
  10:reg_hi:=a;       {mthi}
  11:reg_lo:=a;       {mtlo}
  12:begin; {mult}
    doMulS32hl(a,b);
    reg_hi:=a;
    reg_lo:=b;
    end;
  13:begin; {multu}
    doMulU32hl(a,b);
    reg_hi:=a;
    reg_lo:=b;
    end;
  14:c:=a or b;       {or}
  15:c:=not (a or b); {nor}
  16:c:=a shl b;      {sll}
  17:c:=b shl a;      {sllv}
  18:if (a<b) then c:=1 else c:=0; {slt}
  19:if (longword(a)<longword(b)) then c:=1 else c:=0; {sltu}
  20:c:=a shr b;      {sra}
  21:c:=b shr a;      {srav}
  22:c:=a-b;          {sub}
  23:c:=a xor b;      {xor}
  else exit;
  end;
ok:
doOneComp3:=false;
End;

Function doOneComp2(o,a,b:LongInt;var c:LongInt):Boolean;
Label ok;
Begin;
doOneComp2:=true;
case o of
  1:c:=a+b;     {addi}
  2:c:=a+b;     {addiu}
  3:c:=a and b; {andi}
  4:if (b=c) then reg_npc:=(a*4)+reg_pc; {beq}
  5:if (b<>c) then reg_npc:=(a*4)+reg_pc; {bne}
  6:begin; {lb}
    inc(a,b);
    if checkRange(a) then exit;
    c:=memoryData^[a];
    c:=shortint(c);
    end;
  7:begin; {lbu}
    inc(a,b);
    if checkRange(a) then exit;
    c:=memoryData^[a];
    end;
  8:begin; {lh}
    inc(a,b);
    if checkRange(a) then exit;
    if (a and 1<>0) then begin; result:=2;exit; end;
    c:=ReadWordMSB(memoryData^[a]);
    c:=integer(c);
    end;
  9:begin; {lhu}
    inc(a,b);
    if checkRange(a) then exit;
    if (a and 1<>0) then begin; result:=2;exit; end;
    c:=ReadWordMSB(memoryData^[a]);
    end;
  10:begin; {lw}
    inc(a,b);
    if checkRange(a) then exit;
    if (a and 3<>0) then begin; result:=2;exit; end;
    c:=ReadLongMSB(memoryData^[a]);
    end;
  11:begin; {lwl}
    inc(a,b);
    if checkRange(a) then exit;
    c:=(ReadWordMSB(memoryData^[a]) shl 16) or (c and $ffff);
    end;
  12:begin; {lwr}
    inc(a,b);
    inc(a,2);
    if checkRange(a) then exit;
    c:=ReadWordMSB(memoryData^[a]) or (c and $ffff0000);
    end;
  13:c:=a shl 16;     {lui}
  14:c:=a or b;       {ori}
  15:begin; {sb}
    inc(a,b);
    if checkRange(a) then exit;
    memoryData^[a]:=c;
    end;
  16:begin; {sh}
    inc(a,b);
    if checkRange(a) then exit;
    if (a and 1<>0) then begin; result:=2;exit; end;
    WriteWordMSB(memoryData^[a],c);
    end;
  17:if (a<b) then c:=1 else c:=0; {slti}
  18:begin; {sw}
    inc(a,b);
    if checkRange(a) then exit;
    if (a and 3<>0) then begin; result:=2;exit; end;
    WriteLongMSB(memoryData^[a],c);
    end;
  19:begin; {swl}
    inc(a,b);
    if checkRange(a) then exit;
    WriteWordMSB(memoryData^[a],c shr 16);
    end;
  20:begin; {swr}
    inc(a,b);
    inc(a,2);
    if checkRange(a) then exit;
    WriteWordMSB(memoryData^[a],c);
    end;
  21:c:=a xor b;      {xori}
  22:if (b>0) then reg_npc:=(a*4)+reg_pc; {bgtz}
  23:if (b<=0) then reg_npc:=(a*4)+reg_pc; {blez}
  24:if (b<0) then reg_npc:=(a*4)+reg_pc; {bltz}
  25:if (b<0) then begin; regs[31]:=reg_pc+4;reg_npc:=(a*4)+reg_pc; end; {bltzal}
  26:if (b>=0) then reg_npc:=(a*4)+reg_pc; {bgez}
  27:if (b>=0) then begin; regs[31]:=reg_pc+4;reg_npc:=(a*4)+reg_pc; end; {bgezal}
  else exit;
  end;
ok:
doOneComp2:=false;
End;

Function doOneSTSI(p:LongInt):Boolean;
Var i,o:LongInt;
Begin;
doOneSTSI:=true;
i:=integer(op);
o:=regs[(op shr 21) and 31];
if doOneComp2(p,i,o,regs[(op shr 16) and 31]) then exit;
doOneSTSI:=false;
End;

Function doOneSTUI(p:LongInt):Boolean;
Var i,o:LongInt;
Begin;
doOneSTUI:=true;
i:=op and $ffff;
o:=regs[(op shr 21) and 31];
if doOneComp2(p,i,o,regs[(op shr 16) and 31]) then exit;
doOneSTUI:=false;
End;

Function doOneSTD(p:LongInt):Boolean;
Var i,o:LongInt;
Begin;
doOneSTD:=true;
i:=regs[(op shr 21) and 31];
o:=regs[(op shr 16) and 31];
if doOneComp3(p,i,o,regs[(op shr 11) and 31]) then exit;
doOneSTD:=false;
End;

Function doOneDD(p:LongInt):Boolean;
Var i,o:LongInt;
Begin;
doOneDD:=true;
if doOneComp3(p,0,0,regs[(op shr 11) and 31]) then exit;
doOneDD:=false;
End;

Function doOneTDN(p:LongInt):Boolean;
Var i,o:LongInt;
Begin;
doOneTDN:=true;
i:=regs[(op shr 16) and 31];
o:=(op shr 6) and 31;
if doOneComp3(p,i,o,regs[(op shr 11) and 31]) then exit;
doOneTDN:=false;
End;


Var i,o:LongInt;
Begin;
result:=1;

if checkRange(reg_pc) then goto vege;
op:=ReadLongMSB(memoryData^[reg_pc]);
inc(reg_pc,4);
nextpc:=reg_npc;
reg_npc:=-1;

i:=op shr 26;
case i of
  $08:begin;
    {$ifdef debug}writeln('addi rt,ts,imm');{$endif}
    if doOneSTSI(1) then goto vege;
    goto ok;
    end;
  $09:begin;
    {$ifdef debug}writeln('addiu rt,ts,imm');{$endif}
    if doOneSTSI(2) then goto vege;
    goto ok;
    end;
  $0c:begin;
    {$ifdef debug}writeln('andi rt,ts,imm');{$endif}
    if doOneSTUI(3) then goto vege;
    goto ok;
    end;
  $04:begin;
    {$ifdef debug}writeln('beq rt,ts,imm');{$endif}
    if doOneSTSI(4) then goto vege;
    goto ok;
    end;
  $05:begin;
    {$ifdef debug}writeln('bne rt,ts,imm');{$endif}
    if doOneSTSI(5) then goto vege;
    goto ok;
    end;
  $02:begin;
    {$ifdef debug}writeln('j imm');{$endif}
    reg_npc:=(op and $03ffffff) shl 2;
    goto ok;
    end;
  $03:begin;
    {$ifdef debug}writeln('jal imm');{$endif}
    regs[31]:=reg_pc+4;
    reg_npc:=(op and $03ffffff) shl 2;
    goto ok;
    end;
  $20:begin;
    {$ifdef debug}writeln('lb rt,ofs,ts');{$endif}
    if doOneSTSI(6) then goto vege;
    goto ok;
    end;
  $24:begin;
    {$ifdef debug}writeln('lbu rt,ofs,ts');{$endif}
    if doOneSTSI(7) then goto vege;
    goto ok;
    end;
  $21:begin;
    {$ifdef debug}writeln('lh rt,ofs,ts');{$endif}
    if doOneSTSI(8) then goto vege;
    goto ok;
    end;
  $25:begin;
    {$ifdef debug}writeln('lhu rt,ofs,ts');{$endif}
    if doOneSTSI(9) then goto vege;
    goto ok;
    end;
  $23:begin;
    {$ifdef debug}writeln('lw rt,ofs,ts');{$endif}
    if doOneSTSI(10) then goto vege;
    goto ok;
    end;
  $22:begin;
    {$ifdef debug}writeln('lwl rt,ofs,ts');{$endif}
    if doOneSTSI(11) then goto vege;
    goto ok;
    end;
  $26:begin;
    {$ifdef debug}writeln('lwr rt,ofs,ts');{$endif}
    if doOneSTSI(12) then goto vege;
    goto ok;
    end;
  $0f:begin;
    {$ifdef debug}writeln('lui rt,ts,imm');{$endif}
    if doOneSTSI(13) then goto vege;
    goto ok;
    end;
  $0d:begin;
    {$ifdef debug}writeln('ori rt,ts,imm');{$endif}
    if doOneSTUI(14) then goto vege;
    goto ok;
    end;
  $28:begin;
    {$ifdef debug}writeln('sb rt,ofs,ts');{$endif}
    if doOneSTSI(15) then goto vege;
    goto ok;
    end;
  $29:begin;
    {$ifdef debug}writeln('sh rt,ofs,ts');{$endif}
    if doOneSTSI(16) then goto vege;
    goto ok;
    end;
  $0a:begin;
    {$ifdef debug}writeln('slti rt,ts,imm');{$endif}
    if doOneSTSI(17) then goto vege;
    goto ok;
    end;
  $0b:begin;
    {$ifdef debug}writeln('sltiu rt,ts,imm');{$endif}
    if doOneSTUI(17) then goto vege;
    goto ok;
    end;
  $2b:begin;
    {$ifdef debug}writeln('sw rt,ofs,ts');{$endif}
    if doOneSTSI(18) then goto vege;
    goto ok;
    end;
  $2a:begin;
    {$ifdef debug}writeln('swl rt,ofs,ts');{$endif}
    if doOneSTSI(19) then goto vege;
    goto ok;
    end;
  $2e:begin;
    {$ifdef debug}writeln('swr rt,ofs,ts');{$endif}
    if doOneSTSI(20) then goto vege;
    goto ok;
    end;
  $0e:begin;
    {$ifdef debug}writeln('xori rt,ts,imm');{$endif}
    if doOneSTUI(21) then goto vege;
    goto ok;
    end;
  $07:begin;
    {$ifdef debug}writeln('bgtz ts,ofs');{$endif}
    if doOneSTSI(22) then goto vege;
    goto ok;
    end;
  $06:begin;
    {$ifdef debug}writeln('blez ts,ofs');{$endif}
    if doOneSTSI(23) then goto vege;
    goto ok;
    end;
  $01:begin;
    i:=(op shr 16) and 31;
    case i of
      $00:begin;
        {$ifdef debug}writeln('bltz ts,ofs');{$endif}
        if doOneSTSI(24) then goto vege;
        goto ok;
        end;
      $10:begin;
        {$ifdef debug}writeln('bltzal');{$endif}
        if doOneSTSI(25) then goto vege;
        goto ok;
        end;
      $01:begin;
        {$ifdef debug}writeln('bgez');{$endif}
        if doOneSTSI(26) then goto vege;
        goto ok;
        end;
      $11:begin;
        {$ifdef debug}writeln('bgezal');{$endif}
        if doOneSTSI(27) then goto vege;
        goto ok;
        end;
      end;
    end;
  $00:begin;
    i:=op and $3f;
    case i of
      $20:begin;
        {$ifdef deb\ug}writeln('add rd,rs,tr');{$endif}
        if doOneSTD(1) then goto vege;
        goto ok;
        end;
      $21:begin;
        {$ifdef debug}writeln('addu rd,rs,tr');{$endif}
        if doOneSTD(2) then goto vege;
        goto ok;
        end;
      $24:begin;
        {$ifdef debug}writeln('and rd,rs,tr');{$endif}
        if doOneSTD(3) then goto vege;
        goto ok;
        end;
      $1a:begin;
        {$ifdef debug}writeln('div');{$endif}
        if doOneSTD(4) then goto vege;
        goto ok;
        end;
      $1b:begin;
        {$ifdef debug}writeln('divu rd,rs,tr');{$endif}
        if doOneSTD(5) then goto vege;
        goto ok;
        end;
      $09:begin;
        {$ifdef debug}writeln('jalr rd,rs');{$endif}
        if doOneSTD(6) then goto vege;
        goto ok;
        end;
      $08:begin;
        {$ifdef debug}writeln('jr rd');{$endif}
        if doOneSTD(7) then goto vege;
        goto ok;
        end;
      $10:begin;
        {$ifdef debug}writeln('mfhi rd');{$endif}
        if doOneDD(8) then goto vege;
        goto ok;
        end;
      $12:begin;
        {$ifdef debug}writeln('mflo rd');{$endif}
        if doOneDD(9) then goto vege;
        goto ok;
        end;
      $11:begin;
        {$ifdef debug}writeln('mthi rd');{$endif}
        if doOneSTD(10) then goto vege;
        goto ok;
        end;
      $13:begin;
        {$ifdef debug}writeln('mtlo rd');{$endif}
        if doOneSTD(11) then goto vege;
        goto ok;
        end;
      $18:begin;
        {$ifdef debug}writeln('mult rd,rs,tr');{$endif}
        if doOneSTD(12) then goto vege;
        goto ok;
        end;
      $19:begin;
        {$ifdef debug}writeln('multu rd,rs,tr');{$endif}
        if doOneSTD(13) then goto vege;
        goto ok;
        end;
      $25:begin;
        {$ifdef debug}writeln('or rd,rs,tr');{$endif}
        if doOneSTD(14) then goto vege;
        goto ok;
        end;
      $27:begin;
        {$ifdef debug}writeln('nor rd,rs,tr');{$endif}
        if doOneSTD(15) then goto vege;
        goto ok;
        end;
      $00:begin;
        {$ifdef debug}writeln('sll rd,rs,num');{$endif}
        if doOneTDN(16) then goto vege;
        goto ok;
        end;
      $04:begin;
        {$ifdef debug}writeln('sllv rd,rs,tr');{$endif}
        if doOneSTD(17) then goto vege;
        goto ok;
        end;
      $2a:begin;
        {$ifdef debug}writeln('slt rd,rs,tr');{$endif}
        if doOneSTD(18) then goto vege;
        goto ok;
        end;
      $2b:begin;
        {$ifdef debug}writeln('sltu rd,rs,tr');{$endif}
        if doOneSTD(19) then goto vege;
        goto ok;
        end;
      $03:begin;
        {$ifdef debug}writeln('sra rd,rs,num');{$endif}
        if doOneSTD(20) then goto vege;
        goto ok;
        end;
      $07:begin;
        {$ifdef debug}writeln('srav rd,rs,tr');{$endif}
        if doOneSTD(21) then goto vege;
        goto ok;
        end;
      $02:begin;
        {$ifdef debug}writeln('srl rd,rs,num');{$endif}
        if doOneSTD(20) then goto vege;
        goto ok;
        end;
      $06:begin;
        {$ifdef debug}writeln('srlv rd,rs,tr');{$endif}
        if doOneSTD(21) then goto vege;
        goto ok;
        end;
      $22:begin;
        {$ifdef debug}writeln('sub rd,rs,tr');{$endif}
        if doOneSTD(22) then goto vege;
        goto ok;
        end;
      $23:begin;
        {$ifdef debug}writeln('subu rd,rs,tr');{$endif}
        if doOneSTD(22) then goto vege;
        goto ok;
        end;
      $26:begin;
        {$ifdef debug}writeln('xor rd,rs,tr');{$endif}
        if doOneSTD(23) then goto vege;
        goto ok;
        end;
      end;
    end;
  end;
goto vege;

ok:
result:=0;
vege:
regs[0]:=0;
if (nextpc<>-1) then reg_pc:=nextpc;
emulateOneOpcode:=result;
End;
