Type memoryType=array[0..1] of byte;
Var
  memoryData:^memoryType;
  memorySize:LongInt;
Var
  regs:array[0..7] of byte;
  regs_pc,regs_sp:Word;
  flg_c,flg_v,flg_p:Boolean;
  regs_a:Byte;
Const
  regBofs=$f0;
  regDptr=$82; {lsb}

Procedure emulateRestart;
Var i:Longint;
Begin;
for i:=0 to 7 do regs[i]:=0;
regs_pc:=0;
regs_sp:=0;
flg_c:=false;
flg_v:=false;
flg_p:=false;
regs_a:=0;
End;




Function emulateOneOpcode:LongInt;
Label ok,vege;
Var result:LongInt;

Procedure setFlags(o,n:LongInt);
Begin;
flg_c:=(n>=$100);
flg_v:=(o and $80<>n and $80);
flg_p:=(n and 1=0);
End;

Function getByte:LongInt;
Begin;
if (regs_pc>=memorySize) then begin;
  result:=2;
  getByte:=-1;
  exit;
  end;
getByte:=memoryData^[regs_pc];
inc(regs_pc);
End;

Function getWord:LongInt;
Begin;
if (regs_pc+1>=memorySize) then begin;
  result:=2;
  getWord:=-1;
  exit;
  end;
getWord:=ReadWordMSB(memoryData^[regs_pc]);
inc(regs_pc,2);
End;

Function checkRange(p:LongInt):Boolean;
Begin;
if (p>=memorySize) or (p<0) then begin;
  result:=2;
  checkRange:=True;
  exit;
  end;
checkRange:=False;
End;

Function pushByte(val:LongInt):Boolean;
Begin;
pushByte:=True;
inc(regs_sp);
if checkRange(regs_sp) then exit;
memoryData^[regs_sp]:=val;
pushByte:=False;
End;

Function pushWord(val:LongInt):Boolean;
Begin;
pushWord:=True;
if pushByte(val) then exit;
if pushByte(val shr 8) then exit;
pushWord:=False;
End;

Function popByte:LongInt;
Begin;
popByte:=-1;
if checkRange(regs_sp) then exit;
popByte:=memoryData^[regs_sp];
dec(regs_sp);
End;

Function popWord:LongInt;
Var i,o:LongInt;
Begin;
popWord:=-1;
i:=popByte;
if (i<0) then exit;
o:=popByte;
if (o<0) then exit;
popWord:=(i shl 8)+o;
End;

Function getRnDirRiNum(i:LongInt):LongInt;
Begin;
getRnDirRiNum:=-1;
case i and $f of
  $8..$f:begin; {Rn}
    getRnDirRiNum:=regs[i and 7];
    end;
  $4:begin; {num8}
    i:=getByte;
    if (i<0) then exit;
    getRnDirRiNum:=i;
    end;
  $5:begin; {adr8}
    i:=getByte;
    if (i<0) then exit;
    getRnDirRiNum:=memoryData^[i];
    end;
  $6:begin; {@r0}
    getRnDirRiNum:=memoryData^[regs[0]];
    end;
  $7:begin; {@r1}
    getRnDirRiNum:=memoryData^[regs[1]];
    end;
  end;
End;


Function doCJNE(a,d:LongInt):Boolean;
Var p:LongInt;
Begin;
doCJNE:=True;
p:=getByte;
if (p<0) then exit;
p:=shortint(p);
flg_c:=(a<d);
if (a<>d) then inc(regs_pc,p);
doCJNE:=False;
End;

Function doDJNZ(var d:byte):Boolean;
Var p:LongInt;
Begin;
doDJNZ:=True;
p:=getByte;
if (p<0) then exit;
p:=shortint(p);
dec(d);
if (d>0) then inc(regs_pc,p);
doDJNZ:=False;
End;

Procedure doXchg(var a,b:Byte);
Var t:Byte;
Begin;
t:=a;
a:=b;
b:=t;
End;

Procedure doXchgD(var a,b:Byte);
Var t:Byte;
Begin;
t:=a;
a:=(a and $f0) or (b and $f);
b:=(b and $f0) or (t and $f);
End;


Var i,o,p:LongInt;
Begin;
result:=1;

i:=getByte;
if (i<0) then goto vege;
case i of
  $00:begin;
    {$ifdef debug}writeln('nop');{$endif}
    goto ok;
    end;
  $11,$31,$51,$71,$91,$b1,$d1,$f1:begin;
    {$ifdef debug}writeln('acall abs11');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    i:=((i shr 5) shl 8) or o;
    if pushWord(regs_pc) then goto vege;
    regs_pc:=i;
    goto ok;
    end;
  $01,$21,$41,$61,$81,$a1,$c1,$e1:begin;
    {$ifdef debug}writeln('ajmp abs11');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    i:=((i shr 5) shl 8) or o;
    regs_pc:=i;
    goto ok;
    end;
  $12:begin;
    {$ifdef debug}writeln('lcall abs16');{$endif}
    o:=getWord;
    if (o<0) then goto vege;
    if pushWord(regs_pc) then goto vege;
    regs_pc:=o;
    goto ok;
    end;
  $02:begin;
    {$ifdef debug}writeln('ljmp abs16');{$endif}
    o:=getWord;
    if (o<0) then goto vege;
    regs_pc:=o;
    goto ok;
    end;
  $24..$2f:begin;
    {$ifdef debug}writeln('add a,any');{$endif}
    i:=getRnDirRiNum(i);
    if (i<0) then goto vege;
    setFlags(regs_a,regs_a+i);
    inc(regs_a,i);
    goto ok;
    end;
  $34..$3f:begin;
    {$ifdef debug}writeln('addc a,any');{$endif}
    i:=getRnDirRiNum(i);
    if (i<0) then goto vege;
    if flg_c then inc(i);
    setFlags(regs_a,regs_a+i);
    inc(regs_a,i);
    goto ok;
    end;
  $54..$5f:begin;
    {$ifdef debug}writeln('anl a,any');{$endif}
    i:=getRnDirRiNum(i);
    if (i<0) then goto vege;
    regs_a:=regs_a and i;
    goto ok;
    end;
  $52,$53:begin;
    {$ifdef debug}writeln('anl (dir),any');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    if (i and 1<>0) then i:=getByte else i:=regs_a;
    if (i<0) then goto vege;
    memoryData^[o]:=memoryData^[o] and i;
    goto ok;
    end;
  $82,$b0:begin;
    {$ifdef debug}writeln('anl c,[/]bit');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    p:=memoryData^[o shr 3];
    if (i and $f=0) then p:=not p;
    p:=(1 shl (o and 7)) and p;
    if (p=0) then flg_c:=false;
    goto ok;
    end;
  $b5:begin;
    {$ifdef debug}writeln('cjne a,(adr8),rel8');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    if doCJNE(regs_a,memoryData^[o]) then goto vege;
    goto ok;
    end;
  $b4:begin;
    {$ifdef debug}writeln('cjne a,num8,rel8');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    if doCJNE(regs_a,o) then goto vege;
    goto ok;
    end;
  $b8..$bf:begin;
    {$ifdef debug}writeln('cjne Rn,num8,rel8');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    if doCJNE(regs[i and 7],o) then goto vege;
    goto ok;
    end;
  $b6,$b7:begin;
    {$ifdef debug}writeln('cjne @Ri,num8,rel8');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    if doCJNE(memoryData^[regs[i and 1]],o) then goto vege;
    goto ok;
    end;
  $e4:begin;
    {$ifdef debug}writeln('clr a');{$endif}
    regs_a:=0;
    goto ok;
    end;
  $c3:begin;
    {$ifdef debug}writeln('clr c');{$endif}
    flg_c:=false;
    goto ok;
    end;
  $c2:begin;
    {$ifdef debug}writeln('clr bit');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    i:=o shr 3;
    p:=(1 shl (o and 7));
    memoryData^[i]:=memoryData^[i] and (not p);
    goto ok;
    end;
  $f4:begin;
    {$ifdef debug}writeln('cpl a');{$endif}
    regs_a:=not regs_a;
    goto ok;
    end;
  $b3:begin;
    {$ifdef debug}writeln('cpl c');{$endif}
    flg_c:=not flg_c;
    goto ok;
    end;
  $b2:begin;
    {$ifdef debug}writeln('cpl bit');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    i:=o shr 3;
    p:=(1 shl (o and 7));
    memoryData^[i]:=memoryData^[i] xor p;
    goto ok;
    end;
  $d4:begin;
    {$ifdef debug}writeln('da a');{$endif}
    if (regs_a and $0f>$09) then regs_a:=((regs_a+$06) and $f)+(regs_a and $f0);
    if (regs_a and $f0>$90) then regs_a:=((regs_a+$60) and $f0)+(regs_a and $f);
    goto ok;
    end;
  $14:begin;
    {$ifdef debug}writeln('dec a');{$endif}
    dec(regs_a);
    goto ok;
    end;
  $18..$1f:begin;
    {$ifdef debug}writeln('dec Rn');{$endif}
    dec(regs[i and 7]);
    goto ok;
    end;
  $15:begin;
    {$ifdef debug}writeln('dec (adr8)');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    dec(memoryData^[o]);
    goto ok;
    end;
  $16,$17:begin;
    {$ifdef debug}writeln('dec @Ri');{$endif}
    dec(memoryData^[regs[i and 1]]);
    goto ok;
    end;
  $84:begin;
    {$ifdef debug}writeln('div ab');{$endif}
    o:=memoryData^[regBofs];
    memoryData^[regBofs]:=regs_a mod o;
    regs_a:=regs_a div o;
    goto ok;
    end;
  $a4:begin;
    {$ifdef debug}writeln('mul ab');{$endif}
    o:=memoryData^[regBofs];
    i:=regs_a;
    i:=i*o;
    memoryData^[regBofs]:=i shr 8;
    regs_a:=i;
    goto ok;
    end;
  $d8..$df:begin;
    {$ifdef debug}writeln('djnz Rn,rel8');{$endif}
    if doDJNZ(regs[i and 7]) then goto vege;
    goto ok;
    end;
  $d5:begin;
    {$ifdef debug}writeln('djnz (adr8),rel8');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    if doDJNZ(memoryData^[o]) then goto vege;
    goto ok;
    end;
  $04:begin;
    {$ifdef debug}writeln('inc a');{$endif}
    inc(regs_a);
    goto ok;
    end;
  $08..$0f:begin;
    {$ifdef debug}writeln('inc Rn');{$endif}
    inc(regs[i and 7]);
    goto ok;
    end;
  $05:begin;
    {$ifdef debug}writeln('inc (adr8)');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    inc(memoryData^[o]);
    goto ok;
    end;
  $06,$07:begin;
    {$ifdef debug}writeln('inc @Ri');{$endif}
    inc(memoryData^[regs[i and 1]]);
    goto ok;
    end;
  $a3:begin;
    {$ifdef debug}writeln('inc dptr');{$endif}
    WriteWordLSB(memoryData^[regDptr],ReadWordLSB(memoryData^[regDptr])+1);
    goto ok;
    end;
  $20:begin;
    {$ifdef debug}writeln('jb bit,rel8');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    if (memoryData^[o shr 3] and (1 shl (o and 7))<>0) then inc(regs_pc,i);
    goto ok;
    end;
  $30:begin;
    {$ifdef debug}writeln('jnb bit,rel8');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    if (memoryData^[o shr 3] and (1 shl (o and 7))=0) then inc(regs_pc,i);
    goto ok;
    end;
  $10:begin;
    {$ifdef debug}writeln('jbc bit,rel8');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    p:=o shr 3;
    o:=1 shl (o and 7);
    if (memoryData^[p] and o<>0) then inc(regs_pc,i);
    memoryData^[p]:=memoryData^[p] and (not o);
    goto ok;
    end;
  $40:begin;
    {$ifdef debug}writeln('jc rel8');{$endif}
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    if flg_c then inc(regs_pc,i);
    goto ok;
    end;
  $50:begin;
    {$ifdef debug}writeln('jnc rel8');{$endif}
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    if not flg_c then inc(regs_pc,i);
    goto ok;
    end;
  $60:begin;
    {$ifdef debug}writeln('jz rel8');{$endif}
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    if (regs_a=0) then inc(regs_pc,i);
    goto ok;
    end;
  $70:begin;
    {$ifdef debug}writeln('jnz rel8');{$endif}
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    if (regs_a<>0) then inc(regs_pc,i);
    goto ok;
    end;
  $73:begin;
    {$ifdef debug}writeln('jmp @a+dptr');{$endif}
    regs_pc:=ReadWordLSB(memoryData^[regDptr])+regs_a;
    goto ok;
    end;
  $74,$e5..$ef:begin;
    {$ifdef debug}writeln('mov a,any');{$endif}
    i:=getRnDirRiNum(i);
    if (i<0) then goto vege;
    regs_a:=i;
    goto ok;
    end;
  $f8..$ff:begin;
    {$ifdef debug}writeln('mov Rn,a');{$endif}
    regs[i and 7]:=regs_a;
    goto ok;
    end;
  $a8..$af:begin;
    {$ifdef debug}writeln('mov Rn,(adr8)');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    regs[i and 7]:=memoryData^[o];
    goto ok;
    end;
  $78..$7f:begin;
    {$ifdef debug}writeln('mov Rn,num8');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    regs[i and 7]:=o;
    goto ok;
    end;
  $f5:begin;
    {$ifdef debug}writeln('mov (adr8),a');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    memoryData^[o]:=regs_a;
    goto ok;
    end;
  $88..$8f:begin;
    {$ifdef debug}writeln('mov (adr8),Rn');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    memoryData^[o]:=regs[i and 7];
    goto ok;
    end;
  $85:begin;
    {$ifdef debug}writeln('mov (adr8),(adr8)');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    i:=memoryData^[o];
    o:=getByte;
    if (o<0) then goto vege;
    memoryData^[o]:=i;
    goto ok;
    end;
  $86,$87:begin;
    {$ifdef debug}writeln('mov (adr8),@Ri');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    memoryData^[o]:=memoryData^[regs[i and 1]];
    goto ok;
    end;
  $75:begin;
    {$ifdef debug}writeln('mov (adr8),num8');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    i:=getByte;
    if (i<0) then goto vege;
    memoryData^[o]:=i;
    goto ok;
    end;
  $f6,$f7:begin;
    {$ifdef debug}writeln('mov @Ri,a');{$endif}
    memoryData^[regs[i and 1]]:=regs_a;
    goto ok;
    end;
  $a6,$a7:begin;
    {$ifdef debug}writeln('mov @Ri,(adr8)');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    memoryData^[regs[i and 1]]:=memoryData^[o];
    goto ok;
    end;
  $76,$77:begin;
    {$ifdef debug}writeln('mov @Ri,num8');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    memoryData^[regs[i and 1]]:=o;
    goto ok;
    end;
  $a2:begin;
    {$ifdef debug}writeln('mov c,bit');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    flg_c:=(memoryData^[o shr 3] and (1 shl (o and 7))<>0);
    goto ok;
    end;
  $92:begin;
    {$ifdef debug}writeln('mov bit,c');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    p:=1 shl (o and 7);
    o:=o shr 3;
    i:=memoryData^[o];
    if flg_c then i:=i or p else i:=i and (not p);
    memoryData^[o]:=i;
    goto ok;
    end;
  $90:begin;
    {$ifdef debug}writeln('mov dptr,num16');{$endif}
    o:=getWord;
    if (o<0) then goto vege;
    WriteWordLSB(memoryData^[regDptr],o);
    goto ok;
    end;
  $93:begin;
    {$ifdef debug}writeln('movc a,@a+dptr');{$endif}
    regs_a:=memoryData^[ReadWordLSB(memoryData^[regDptr])+regs_a];
    goto ok;
    end;
  $83:begin;
    {$ifdef debug}writeln('movc a,@a+pc');{$endif}
    regs_a:=memoryData^[regs_pc+regs_a];
    goto ok;
    end;
  $e2,$e3:begin;
    {$ifdef debug}writeln('movx a,@Ri');{$endif}
    regs_a:=memoryData^[regs[i and 1]];
    goto ok;
    end;
  $e0:begin;
    {$ifdef debug}writeln('movx a,dptr');{$endif}
    regs_a:=memoryData^[ReadWordLSB(memoryData^[regDptr])];
    goto ok;
    end;
  $f2,$f3:begin;
    {$ifdef debug}writeln('movx a,@Ri');{$endif}
    memoryData^[regs[i and 1]]:=regs_a;
    goto ok;
    end;
  $f0:begin;
    {$ifdef debug}writeln('movx a,dptr');{$endif}
    memoryData^[ReadWordLSB(memoryData^[regDptr])]:=regs_a;
    goto ok;
    end;
  $44..$4f:begin;
    {$ifdef debug}writeln('orl a,any');{$endif}
    i:=getRnDirRiNum(i);
    if (i<0) then goto vege;
    regs_a:=regs_a or i;
    goto ok;
    end;
  $42,$43:begin;
    {$ifdef debug}writeln('orl (dir),any');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    if (i and 1<>0) then i:=getByte else i:=regs_a;
    if (i<0) then goto vege;
    memoryData^[o]:=memoryData^[o] or i;
    goto ok;
    end;
  $72,$a0:begin;
    {$ifdef debug}writeln('orl c,[/]bit');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    p:=memoryData^[o shr 3];
    if (i and $f=0) then p:=not p;
    p:=(1 shl (o and 7)) and p;
    if (p<>0) then flg_c:=true;
    goto ok;
    end;
  $d0:begin;
    {$ifdef debug}writeln('pop (adr8)');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    i:=popByte;
    memoryData^[o]:=i;
    goto ok;
    end;
  $c0:begin;
    {$ifdef debug}writeln('push (adr8)');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    if pushByte(memoryData^[o]) then goto vege;
    goto ok;
    end;
  $32,$22:begin;
    {$ifdef debug}writeln('ret[i]');{$endif}
    o:=popWord;
    if (o<0) then goto vege;
    regs_pc:=o;
    goto ok;
    end;
  $23:begin;
    {$ifdef debug}writeln('rl a');{$endif}
    i:=regs_a shl 1;
    if (i and $100<>0) then inc(i);
    regs_a:=i;
    goto ok;
    end;
  $33:begin;
    {$ifdef debug}writeln('rlc a');{$endif}
    i:=regs_a shl 1;
    if flg_c then inc(i);
    regs_a:=i;
    flg_c:=(i and $100<>0);
    goto ok;
    end;
  $03:begin;
    {$ifdef debug}writeln('rr a');{$endif}
    i:=regs_a;
    if (i and 1<>0) then inc(i,$100);
    regs_a:=i shr 1;
    goto ok;
    end;
  $13:begin;
    {$ifdef debug}writeln('rrc a');{$endif}
    i:=regs_a;
    if flg_c then inc(i,$100);
    flg_c:=(i and 1<>0);
    regs_a:=i shr 1;
    goto ok;
    end;
  $d3:begin;
    {$ifdef debug}writeln('set c');{$endif}
    flg_c:=true;
    goto ok;
    end;
  $d2:begin;
    {$ifdef debug}writeln('set bit');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    i:=o shr 3;
    p:=(1 shl (o and 7));
    memoryData^[i]:=memoryData^[i] or p;
    goto ok;
    end;
  $80:begin;
    {$ifdef debug}writeln('sjmp (rel8)');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    inc(regs_pc,o);
    goto ok;
    end;
  $94..$9f:begin;
    {$ifdef debug}writeln('subb a,any');{$endif}
    i:=getRnDirRiNum(i);
    if (i<0) then goto vege;
    if flg_c then inc(i);
    setFlags(regs_a,regs_a-i);
    dec(regs_a,i);
    goto ok;
    end;
  $c4:begin;
    {$ifdef debug}writeln('swap a');{$endif}
    regs_a:=(regs_a shr 4) or (regs_a shl 4);
    goto ok;
    end;
  $c8..$cf:begin;
    {$ifdef debug}writeln('xch a,Rn');{$endif}
    doXchg(regs_a,regs[i and 7]);
    goto ok;
    end;
  $c5:begin;
    {$ifdef debug}writeln('xch a,(adr8)');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    doXchg(regs_a,memoryData^[o]);
    goto ok;
    end;
  $c6,$c7:begin;
    {$ifdef debug}writeln('xch a,@Ri');{$endif}
    doXchg(regs_a,memoryData^[regs[i and 1]]);
    goto ok;
    end;
  $d6,$d7:begin;
    {$ifdef debug}writeln('xchd a,@Ri');{$endif}
    doXchgD(regs_a,memoryData^[regs[i and 1]]);
    goto ok;
    end;
  $64..$6f:begin;
    {$ifdef debug}writeln('xrl a,any');{$endif}
    i:=getRnDirRiNum(i);
    if (i<0) then goto vege;
    regs_a:=regs_a xor i;
    goto ok;
    end;
  $62,$63:begin;
    {$ifdef debug}writeln('xrl (dir),any');{$endif}
    o:=getByte;
    if (o<0) then goto vege;
    if (i and 1<>0) then i:=getByte else i:=regs_a;
    if (i<0) then goto vege;
    memoryData^[o]:=memoryData^[o] xor i;
    goto ok;
    end;
  end;
goto vege;

ok:
result:=0;
vege:
emulateOneOpcode:=result;
End;
