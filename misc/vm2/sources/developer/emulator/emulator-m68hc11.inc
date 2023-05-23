Type memoryType=array[0..1] of byte;
Var
  memoryData:^memoryType;
  memorySize:LongInt;
Var
  regs:array[0..7] of byte;
  regs2:array[0..7] of byte;
  flg_s,flg_x,flg_h,flg_i,flg_n,flg_z,flg_v,flg_c:Boolean;
  regs_x,regs_y,regs_pc,regs_sp:Word;
  regs_a,regs_b:Byte;
Const
  interruptBase=$10;
  reg_a=0;
  reg_b=1;
  reg_x=2;
  reg_y=3;
  reg_d=4;

Procedure emulateRestart;
Var i:Longint;
Begin;
for i:=0 to 7 do regs[i]:=0;
for i:=0 to 7 do regs2[i]:=0;
flg_s:=false;
flg_x:=false;
flg_h:=false;
flg_i:=false;
flg_n:=false;
flg_z:=false;
flg_v:=false;
flg_c:=false;
regs_x:=0;
regs_y:=0;
regs_pc:=0;
regs_sp:=memorySize-1;
regs_a:=0;
regs_b:=0;
End;


Function getFlags:LongInt;
Var i:LongInt;
Begin;
i:=0;
if flg_s then i:=i or $80;
if flg_x then i:=i or $40;
if flg_h then i:=i or $20;
if flg_i then i:=i or $10;
if flg_n then i:=i or $08;
if flg_z then i:=i or $04;
if flg_v then i:=i or $02;
if flg_c then i:=i or $01;
getFlags:=i;
End;

Procedure putFlags(i:LongInt);
Begin;
flg_s:=(i and $80<>0);
flg_x:=(i and $40<>0);
flg_h:=(i and $20<>0);
flg_i:=(i and $10<>0);
flg_n:=(i and $08<>0);
flg_z:=(i and $04<>0);
flg_v:=(i and $02<>0);
flg_c:=(i and $01<>0);
End;




Function emulateOneOpcode:LongInt;
Label ok,vege;
Var result,prefix:LongInt;

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

Function readReg(r:LongInt):LongInt;
Begin;
case r of
  reg_a:readReg:=regs_a;
  reg_b:readReg:=regs_b;
  reg_x:readReg:=regs_x;
  reg_y:readReg:=regs_y;
  reg_d:readReg:=(regs_a shl 8) or regs_b;
  else readReg:=-1;
  end;
End;

Procedure writeReg(r,v:LongInt);
Begin;
case r of
  reg_a:regs_a:=v;
  reg_b:regs_b:=v;
  reg_x:regs_x:=v;
  reg_y:regs_y:=v;
  reg_d:begin;
    regs_a:=v shr 8;
    regs_b:=v;
    end;
  end;
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

Procedure setFlagsNZ8(i:LongInt);
Begin;
flg_z:=(i=0);
flg_n:=(i and $80<>0);
End;

Procedure setFlagsNZ16(i:LongInt);
Begin;
flg_z:=(i=0);
flg_n:=(i and $8000<>0);
End;

Procedure setFlagsHVC(a,b:LongInt);
Var i,o,p:LongInt;
Begin;
i:=a and 8;
o:=b and 8;
p:=(not (a+b)) and 8;
flg_h:=((i and o) or (i and p) or (o and p)<>0);
i:=a and $80;
o:=b and $80;
p:=(not (a+b)) and $80;
flg_c:=((i and o) or (i and p) or (o and p)<>0);
flg_v:=((i and o and p) or ((i xor $80) and (o xor $80) and (p xor $80))<>0);
End;

Function pushByte(val:LongInt):Boolean;
Begin;
pushByte:=True;
if checkRange(regs_sp) then exit;
memoryData^[regs_sp]:=val;
dec(regs_sp);
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
inc(regs_sp);
if checkRange(regs_sp) then exit;
popByte:=memoryData^[regs_sp];
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

Function getAddrDir:LongInt;
Var i:Longint;
Begin;
getAddrDir:=-1;
i:=getByte;
if (i<0) then exit;
if checkRange(i) then exit;
getAddrDir:=i;
End;

Function getAddrExt:LongInt;
Var i:Longint;
Begin;
getAddrExt:=-1;
i:=getWord;
if (i<0) then exit;
if checkRange(i) then exit;
getAddrExt:=i;
End;

Function getAddrInd:LongInt;
Var i:Longint;
Begin;
getAddrInd:=-1;
i:=getByte;
if (prefix and 1=0) then inc(i,regs_x) else inc(i,regs_y);
if checkRange(i) then exit;
getAddrInd:=i;
End;

Function getAddress(typ:LongInt):LongInt;
Begin;
case typ of
  $10:typ:=getAddrDir; {direct}
  $20:typ:=getAddrInd; {indirect}
  $30:typ:=getAddrExt; {extended}
  else typ:=-1;
  end;
getAddress:=typ;
End;

Function getAddrVal(typ,siz:LongInt):LongInt;
Label err,vege;
Begin;
case typ of
  $00:begin; {imm}
    if (siz=1) then typ:=getByte else typ:=getWord;
    if (typ<0) then goto err;
    goto vege;
    end;
  $10,$20,$30:begin; {memory}
    typ:=getAddress(typ);
    if (typ<0) then goto err;
    end;
  else begin;
    err:
    typ:=-1;
    goto vege;
    end;
  end;
if (siz=1) then typ:=memoryData^[typ] else typ:=ReadWordMSB(memoryData^[typ]);
vege:
getAddrVal:=typ;
End;

Procedure doASL(var val:Byte);
Begin;
flg_c:=(val and $80<>0);
val:=val shl 1;
setFlagsNZ8(val);
flg_v:=flg_c xor flg_n;
End;

Procedure doASR(var val:Byte);
Begin;
flg_c:=(val and 1<>0);
val:=val shr 1;
if (val and $40<>0) then val:=val or $80;
setFlagsNZ8(val);
flg_v:=flg_c xor flg_n;
End;

Function doBCLR(ps:LongInt):Boolean;
Var i:LongInt;
Begin;
doBCLR:=True;
if (ps<0) then exit;
i:=getByte;
if (i<0) then exit;
memoryData^[ps]:=memoryData^[ps] and (not i);
setFlagsNZ8(memoryData^[ps]);
flg_v:=false;
doBCLR:=false;
End;

Function doBSET(ps:LongInt):Boolean;
Var i:LongInt;
Begin;
doBSET:=True;
if (ps<0) then exit;
i:=getByte;
if (i<0) then exit;
memoryData^[ps]:=memoryData^[ps] or i;
setFlagsNZ8(memoryData^[ps]);
flg_v:=false;
doBSET:=false;
End;

Function doBRCLR(ps:LongInt):Boolean;
Var i,o:LongInt;
Begin;
doBRCLR:=True;
if (ps<0) then exit;
i:=getByte;
if (i<0) then exit;
o:=getByte;
if (o<0) then exit;
o:=shortint(o);
if (memoryData^[ps] and i=0) then inc(regs_pc,o);
doBRCLR:=False;
End;

Function doBRSET(ps:LongInt):Boolean;
Var i,o:LongInt;
Begin;
doBRSET:=True;
if (ps<0) then exit;
i:=getByte;
if (i<0) then exit;
o:=getByte;
if (o<0) then exit;
o:=shortint(o);
if (memoryData^[ps] and (not i)=0) then inc(regs_pc,o);
doBRSET:=False;
End;

Procedure doCLR(var val:Byte);
Begin;
val:=0;
flg_c:=false;
flg_v:=false;
flg_n:=false;
flg_z:=true;
End;

Procedure doCOM(var val:Byte);
Begin;
val:=$ff-val;
setFlagsNZ8(val);
flg_c:=true;
flg_v:=false;
End;

Procedure doDEC(var val:Byte);
Begin;
flg_v:=(val=$80);
dec(val);
setFlagsNZ8(val);
End;

Procedure doINC(var val:Byte);
Begin;
flg_v:=(val=$7f);
inc(val);
setFlagsNZ8(val);
End;

Procedure doLSR(var val:Byte);
Begin;
flg_c:=(val and 1<>0);
val:=val shr 1;
setFlagsNZ8(val);
flg_v:=flg_c;
End;

Procedure doNEG(var val:Byte);
Begin;
flg_v:=(val=$80);
flg_c:=(val=$00);
val:=-val;
setFlagsNZ8(val);
End;

Procedure doROL(var val:Byte);
Var i:LongInt;
Begin;
if flg_c then i:=1 else i:=0;
flg_c:=(val and $80<>0);
val:=(val shl 1)+i;
setFlagsNZ8(val);
flg_v:=flg_n xor flg_c;
End;

Procedure doROR(var val:Byte);
Var i:LongInt;
Begin;
if flg_c then i:=$80 else i:=0;
flg_c:=(val and 1<>0);
val:=(val shr 1)+i;
setFlagsNZ8(val);
flg_v:=flg_n xor flg_c;
End;

Procedure doTST(var val:Byte);
Var i:LongInt;
Begin;
setFlagsNZ8(val);
flg_v:=false;
flg_c:=false;
End;





Var i,o,p:LongInt;
Begin;
result:=1;

prefix:=0;
i:=getByte;
if (i<0) then goto vege;
case i of
  $18:prefix:=1;
  $1a:prefix:=2;
  $cd:prefix:=3;
  end;
if (prefix<>0) then begin;
  i:=getByte;
  if (i<0) then goto vege;
  end;

case i of
  $1b:begin;
    {$ifdef debug}writeln('aba');{$endif}
    setFlagsHVC(regs_a,regs_b);
    inc(regs_a,regs_b);
    setFlagsNZ8(regs_a);
    goto ok;
    end;
  $3a:begin;
    {$ifdef debug}writeln('abxy');{$endif}
    prefix:=(prefix and 1)+reg_x;
    writeReg(prefix,readReg(prefix)+regs_b);
    goto ok;
    end;
  $89,$99,$b9,$a9:begin;
    {$ifdef debug}writeln('adca');{$endif}
    i:=getAddrVal(i-$89,1);
    if (i<0) then goto vege;
    if flg_c then inc(i);
    setFlagsHVC(regs_a,i);
    inc(regs_a,i);
    setFlagsNZ8(regs_a);
    goto ok;
    end;
  $c9,$d9,$f9,$e9:begin;
    {$ifdef debug}writeln('adcb');{$endif}
    i:=getAddrVal(i-$c9,1);
    if (i<0) then goto vege;
    if flg_c then inc(i);
    setFlagsHVC(regs_b,i);
    inc(regs_b,i);
    setFlagsNZ8(regs_b);
    goto ok;
    end;
  $8b,$9b,$bb,$ab:begin;
    {$ifdef debug}writeln('adda');{$endif}
    i:=getAddrVal(i-$8b,1);
    if (i<0) then goto vege;
    setFlagsHVC(regs_a,i);
    inc(regs_a,i);
    setFlagsNZ8(regs_a);
    goto ok;
    end;
  $cb,$db,$fb,$eb:begin;
    {$ifdef debug}writeln('addb');{$endif}
    i:=getAddrVal(i-$cb,1);
    if (i<0) then goto vege;
    setFlagsHVC(regs_b,i);
    inc(regs_b,i);
    setFlagsNZ8(regs_b);
    goto ok;
    end;
  $c3,$d3,$f3,$e3:begin;
    {$ifdef debug}writeln('addd');{$endif}
    i:=getAddrVal(i-$c3,2);
    if (i<0) then goto vege;
    o:=readReg(reg_d);
    setFlagsHVC(o,i);
    inc(o,i);
    setFlagsNZ8(o);
    writeReg(reg_d,o);
    goto ok;
    end;
  $84,$94,$b4,$a4:begin;
    {$ifdef debug}writeln('anda');{$endif}
    i:=getAddrVal(i-$84,1);
    if (i<0) then goto vege;
    regs_a:=regs_a and i;
    setFlagsNZ8(regs_a);
    flg_v:=false;
    goto ok;
    end;
  $c4,$d4,$f4,$e4:begin;
    {$ifdef debug}writeln('andb');{$endif}
    i:=getAddrVal(i-$c4,1);
    if (i<0) then goto vege;
    regs_b:=regs_b and i;
    setFlagsNZ8(regs_b);
    flg_v:=false;
    goto ok;
    end;
  $48:begin;
    {$ifdef debug}writeln('asla');{$endif}
    doASL(regs_a);
    goto ok;
    end;
  $58:begin;
    {$ifdef debug}writeln('aslb');{$endif}
    doASL(regs_b);
    goto ok;
    end;
  $68,$78:begin;
    {$ifdef debug}writeln('asl');{$endif}
    i:=getAddress(i-$48);
    if (i<0) then goto vege;
    doASL(memoryData^[i]);
    goto ok;
    end;
  $05:begin;
    {$ifdef debug}writeln('asld');{$endif}
    i:=readReg(reg_d);
    flg_c:=(i and $8000<>0);
    i:=(i shl 1) and $ffff;
    setFlagsNZ16(i);
    flg_v:=flg_c xor flg_n;
    writeReg(reg_d,i);
    goto ok;
    end;
  $47:begin;
    {$ifdef debug}writeln('asra');{$endif}
    doASR(regs_a);
    goto ok;
    end;
  $57:begin;
    {$ifdef debug}writeln('asrb');{$endif}
    doASR(regs_b);
    goto ok;
    end;
  $67,$77:begin;
    {$ifdef debug}writeln('asr');{$endif}
    i:=getAddress(i-$47);
    if (i<0) then goto vege;
    doASR(memoryData^[i]);
    goto ok;
    end;
  $20:begin;
    {$ifdef debug}writeln('bra');{$endif}
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    inc(regs_pc,i);
    goto ok;
    end;
  $21:begin;
    {$ifdef debug}writeln('brn');{$endif}
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    goto ok;
    end;
  $22:begin;
    {$ifdef debug}writeln('bhi');{$endif}
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    if not (flg_c or flg_z) then inc(regs_pc,i);
    goto ok;
    end;
  $23:begin;
    {$ifdef debug}writeln('bls');{$endif}
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    if (flg_c or flg_z) then inc(regs_pc,i);
    goto ok;
    end;
  $24:begin;
    {$ifdef debug}writeln('bcc');{$endif}
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    if not flg_c then inc(regs_pc,i);
    goto ok;
    end;
  $25:begin;
    {$ifdef debug}writeln('bcs');{$endif}
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    if flg_c then inc(regs_pc,i);
    goto ok;
    end;
  $26:begin;
    {$ifdef debug}writeln('bne');{$endif}
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    if not flg_z then inc(regs_pc,i);
    goto ok;
    end;
  $27:begin;
    {$ifdef debug}writeln('beq');{$endif}
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    if flg_z then inc(regs_pc,i);
    goto ok;
    end;
  $28:begin;
    {$ifdef debug}writeln('bvc');{$endif}
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    if not flg_v then inc(regs_pc,i);
    goto ok;
    end;
  $29:begin;
    {$ifdef debug}writeln('bvs');{$endif}
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    if flg_v then inc(regs_pc,i);
    goto ok;
    end;
  $2a:begin;
    {$ifdef debug}writeln('bpl');{$endif}
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    if not flg_n then inc(regs_pc,i);
    goto ok;
    end;
  $2b:begin;
    {$ifdef debug}writeln('bmi');{$endif}
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    if flg_n then inc(regs_pc,i);
    goto ok;
    end;
  $2c:begin;
    {$ifdef debug}writeln('bge');{$endif}
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    if not (flg_n xor flg_v) then inc(regs_pc,i);
    goto ok;
    end;
  $2d:begin;
    {$ifdef debug}writeln('blt');{$endif}
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    if (flg_n xor flg_v) then inc(regs_pc,i);
    goto ok;
    end;
  $2e:begin;
    {$ifdef debug}writeln('bgt');{$endif}
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    if not (flg_z or (flg_n xor flg_v)) then inc(regs_pc,i);
    goto ok;
    end;
  $2f:begin;
    {$ifdef debug}writeln('ble');{$endif}
    i:=getByte;
    if (i<0) then goto vege;
    i:=shortint(i);
    if (flg_z or (flg_n xor flg_v)) then inc(regs_pc,i);
    goto ok;
    end;
  $15:begin;
    {$ifdef debug}writeln('bclr');{$endif}
    if doBCLR(getAddrDir) then goto vege;
    goto ok;
    end;
  $1d:begin;
    {$ifdef debug}writeln('bclr');{$endif}
    if doBCLR(getAddrInd) then goto vege;
    goto ok;
    end;
  $85,$95,$b5,$a5:begin;
    {$ifdef debug}writeln('bita');{$endif}
    i:=getAddrVal(i-$85,1);
    if (i<0) then goto vege;
    setFlagsNZ8(regs_a and i);
    flg_v:=false;
    goto ok;
    end;
  $c5,$d5,$f5,$e5:begin;
    {$ifdef debug}writeln('bitb');{$endif}
    i:=getAddrVal(i-$c5,1);
    if (i<0) then goto vege;
    setFlagsNZ8(regs_b and i);
    flg_v:=false;
    goto ok;
    end;
  $13:begin;
    {$ifdef debug}writeln('brclr');{$endif}
    if doBRCLR(getAddrDir) then goto vege;
    goto ok;
    end;
  $1f:begin;
    {$ifdef debug}writeln('brclr');{$endif}
    if doBRCLR(getAddrInd) then goto vege;
    goto ok;
    end;
  $12:begin;
    {$ifdef debug}writeln('brset');{$endif}
    if doBRSET(getAddrDir) then goto vege;
    goto ok;
    end;
  $1e:begin;
    {$ifdef debug}writeln('brset');{$endif}
    if doBRSET(getAddrInd) then goto vege;
    goto ok;
    end;
  $14:begin;
    {$ifdef debug}writeln('bset');{$endif}
    if doBSET(getAddrDir) then goto vege;
    goto ok;
    end;
  $1c:begin;
    {$ifdef debug}writeln('bset');{$endif}
    if doBSET(getAddrInd) then goto vege;
    goto ok;
    end;
  $11:begin;
    {$ifdef debug}writeln('cba');{$endif}
    setFlagsHVC(not regs_a,regs_b);
    setFlagsNZ8(regs_a-regs_b);
    goto ok;
    end;
  $0c:begin;
    {$ifdef debug}writeln('clc');{$endif}
    flg_c:=false;
    goto ok;
    end;
  $0e:begin;
    {$ifdef debug}writeln('cli');{$endif}
    flg_i:=false;
    goto ok;
    end;
  $4f:begin;
    {$ifdef debug}writeln('clra');{$endif}
    doCLR(regs_a);
    goto ok;
    end;
  $5f:begin;
    {$ifdef debug}writeln('clrb');{$endif}
    doCLR(regs_b);
    goto ok;
    end;
  $6f,$7f:begin;
    {$ifdef debug}writeln('clr');{$endif}
    i:=getAddress(i-$4f);
    if (i<0) then goto vege;
    doCLR(memoryData^[i]);
    goto ok;
    end;
  $0a:begin;
    {$ifdef debug}writeln('clv');{$endif}
    flg_i:=false;
    goto ok;
    end;
  $81,$91,$b1,$a1:begin;
    {$ifdef debug}writeln('cmpa');{$endif}
    i:=getAddrVal(i-$81,1);
    if (i<0) then goto vege;
    setFlagsHVC(not regs_a,i);
    setFlagsNZ8(regs_a-i);
    goto ok;
    end;
  $c1,$d1,$f1,$e1:begin;
    {$ifdef debug}writeln('cmpb');{$endif}
    i:=getAddrVal(i-$c1,1);
    if (i<0) then goto vege;
    setFlagsHVC(not regs_b,i);
    setFlagsNZ8(regs_b-i);
    goto ok;
    end;
  $43:begin;
    {$ifdef debug}writeln('coma');{$endif}
    doCOM(regs_a);
    goto ok;
    end;
  $53:begin;
    {$ifdef debug}writeln('comb');{$endif}
    doCOM(regs_b);
    goto ok;
    end;
  $63,$73:begin;
    {$ifdef debug}writeln('com');{$endif}
    i:=getAddress(i-$43);
    if (i<0) then goto vege;
    doCOM(memoryData^[i]);
    goto ok;
    end;
  $83,$93,$b3,$a3:case prefix of
    0,1:begin;
      {$ifdef debug}writeln('subd');{$endif}
      i:=getAddrVal(i-$83,2);
      if (i<0) then goto vege;
      o:=readReg(reg_d);
      setFlagsHVC(not o,i);
      dec(o,i);
      setFlagsNZ8(o);
      writeReg(reg_d,o);
      goto ok;
      end;
    2,3:begin;
      {$ifdef debug}writeln('cpd');{$endif}
      i:=getAddrVal(i-$83,2);
      o:=readReg(reg_d);
      setFlagsHVC(not (o shr 8),(i shr 8));
      setFlagsNZ16(o-i);
      goto ok;
      end;
    else goto vege;
    end;
  $8c,$9c,$bc,$ac:case prefix of
    0,3:begin;
      {$ifdef debug}writeln('cpx');{$endif}
      i:=getAddrVal(i-$8c,2);
      setFlagsHVC(not (regs_x shr 8),(i shr 8));
      setFlagsNZ16(regs_x-i);
      goto ok;
      end;
    1,2:begin;
      {$ifdef debug}writeln('cpy');{$endif}
      i:=getAddrVal(i-$8c,2);
      setFlagsHVC(not (regs_y shr 8),(i shr 8));
      setFlagsNZ16(regs_y-i);
      goto ok;
      end;
    else goto vege;
    end;
  $19:begin;
    {$ifdef debug}writeln('daa');{$endif}
    o:=0;
    if flg_h then begin;
      if (regs_a and $f in [$0..$3]) then o:=1;
      end else begin;
      if (regs_a and $f in [$a..$f]) then o:=1;
      end;
    if (o<>0) then inc(regs_a,$6);
    o:=0;
    if flg_c then begin;
      if (regs_a and $f0 in [$00..$30]) then o:=1;
      end else begin;
      if (regs_a and $f0 in [$a0..$f0]) then o:=1;
      end;
    if (o<>0) then inc(regs_a,$60);
    flg_c:=(o<>0);
    setFlagsNZ8(regs_a);
    goto ok;
    end;
  $4a:begin;
    {$ifdef debug}writeln('deca');{$endif}
    doDEC(regs_a);
    goto ok;
    end;
  $5a:begin;
    {$ifdef debug}writeln('decb');{$endif}
    doDEC(regs_b);
    goto ok;
    end;
  $6a,$7a:begin;
    {$ifdef debug}writeln('dec');{$endif}
    i:=getAddress(i-$4a);
    if (i<0) then goto vege;
    doDEC(memoryData^[i]);
    goto ok;
    end;
  $34:begin;
    {$ifdef debug}writeln('des');{$endif}
    dec(regs_sp);
    goto ok;
    end;
  $09:begin;
    {$ifdef debug}writeln('dexy');{$endif}
    prefix:=(prefix and 1)+reg_x;
    i:=readReg(prefix)-1;
    flg_z:=(i=0);
    writeReg(prefix,i);
    goto ok;
    end;
  $88,$98,$b8,$a8:begin;
    {$ifdef debug}writeln('eora');{$endif}
    i:=getAddrVal(i-$88,1);
    if (i<0) then goto vege;
    regs_a:=regs_a xor i;
    setFlagsNZ8(regs_a);
    flg_v:=false;
    goto ok;
    end;
  $c8,$d8,$f8,$e8:begin;
    {$ifdef debug}writeln('eorb');{$endif}
    i:=getAddrVal(i-$c8,1);
    if (i<0) then goto vege;
    regs_b:=regs_b xor i;
    setFlagsNZ8(regs_b);
    flg_v:=false;
    goto ok;
    end;
  $03:begin;
    {$ifdef debug}writeln('fdiv');{$endif}
    i:=readReg(reg_d);
    flg_c:=(regs_x=0);
    if flg_c then goto ok;
    flg_v:=(regs_x<i);
    i:=i shl 16;
    writeReg(reg_d,i mod regs_x);
    regs_x:=i div regs_x;
    flg_z:=(regs_x=0);
    goto ok;
    end;
  $02:begin;
    {$ifdef debug}writeln('idiv');{$endif}
    i:=readReg(reg_d);
    flg_c:=(regs_x=0);
    if flg_c then goto ok;
    writeReg(reg_d,i mod regs_x);
    regs_x:=i div regs_x;
    flg_z:=(regs_x=0);
    flg_v:=false;
    goto ok;
    end;
  $4c:begin;
    {$ifdef debug}writeln('inca');{$endif}
    doINC(regs_a);
    goto ok;
    end;
  $5c:begin;
    {$ifdef debug}writeln('incb');{$endif}
    doINC(regs_b);
    goto ok;
    end;
  $6c,$7c:begin;
    {$ifdef debug}writeln('inc');{$endif}
    i:=getAddress(i-$4c);
    if (i<0) then goto vege;
    doINC(memoryData^[i]);
    goto ok;
    end;
  $31:begin;
    {$ifdef debug}writeln('ins');{$endif}
    inc(regs_sp);
    goto ok;
    end;
  $08:begin;
    {$ifdef debug}writeln('inxy');{$endif}
    prefix:=(prefix and 1)+reg_x;
    i:=readReg(prefix)+1;
    flg_z:=(i=0);
    writeReg(prefix,i);
    goto ok;
    end;
  $6e,$7e:begin;
    {$ifdef debug}writeln('jmp');{$endif}
    i:=getAddress(i-$4e);
    if (i<0) then goto vege;
    regs_pc:=i;
    goto ok;
    end;
  $9d,$bd,$ad:begin;
    {$ifdef debug}writeln('jsr');{$endif}
    i:=getAddress(i-$8d);
    if (i<0) then goto vege;
    if pushWord(regs_pc) then goto vege;
    regs_pc:=i;
    goto ok;
    end;
  $86,$96,$b6,$a6:begin;
    {$ifdef debug}writeln('ldaa');{$endif}
    i:=getAddrVal(i-$86,1);
    if (i<0) then goto vege;
    regs_a:=i;
    setFlagsNZ8(regs_a);
    flg_v:=false;
    goto ok;
    end;
  $c6,$d6,$f6,$e6:begin;
    {$ifdef debug}writeln('ldab');{$endif}
    i:=getAddrVal(i-$c6,1);
    if (i<0) then goto vege;
    regs_b:=i;
    setFlagsNZ8(regs_b);
    flg_v:=false;
    goto ok;
    end;
  $cc,$dc,$fc,$ec:begin;
    {$ifdef debug}writeln('ldd');{$endif}
    i:=getAddrVal(i-$cc,2);
    if (i<0) then goto vege;
    setFlagsNZ16(i);
    writeReg(reg_d,i);
    flg_v:=false;
    goto ok;
    end;
  $8e,$9e,$be,$ae:begin;
    {$ifdef debug}writeln('lds');{$endif}
    i:=getAddrVal(i-$8e,2);
    if (i<0) then goto vege;
    setFlagsNZ16(i);
    regs_sp:=i;
    flg_v:=false;
    goto ok;
    end;
  $ce,$de,$fe,$ee:case prefix of
    0,3:begin;
      {$ifdef debug}writeln('ldx');{$endif}
      i:=getAddrVal(i-$ce,2);
      if (i<0) then goto vege;
      setFlagsNZ16(i);
      regs_x:=i;
      flg_v:=false;
      goto ok;
      end;
    1,2:begin;
      {$ifdef debug}writeln('ldy');{$endif}
      i:=getAddrVal(i-$ce,2);
      if (i<0) then goto vege;
      setFlagsNZ16(i);
      regs_y:=i;
      flg_v:=false;
      goto ok;
      end;
    end;
  $44:begin;
    {$ifdef debug}writeln('lsra');{$endif}
    doLSR(regs_a);
    goto ok;
    end;
  $54:begin;
    {$ifdef debug}writeln('lsrb');{$endif}
    doLSR(regs_b);
    goto ok;
    end;
  $64,$74:begin;
    {$ifdef debug}writeln('lsr');{$endif}
    i:=getAddress(i-$4c);
    if (i<0) then goto vege;
    doLSR(memoryData^[i]);
    goto ok;
    end;
  $3d:begin;
    {$ifdef debug}writeln('mul');{$endif}
    i:=regs_a*regs_b;
    writeReg(reg_d,i);
    flg_c:=(i and $80<>0);
    goto ok;
    end;
  $40:begin;
    {$ifdef debug}writeln('nega');{$endif}
    doNEG(regs_a);
    goto ok;
    end;
  $50:begin;
    {$ifdef debug}writeln('negb');{$endif}
    doNEG(regs_b);
    goto ok;
    end;
  $60,$70:begin;
    {$ifdef debug}writeln('neg');{$endif}
    i:=getAddress(i-$40);
    if (i<0) then goto vege;
    doNEG(memoryData^[i]);
    goto ok;
    end;
  $01:begin;
    {$ifdef debug}writeln('nop');{$endif}
    goto ok;
    end;
  $8a,$9a,$ba,$aa:begin;
    {$ifdef debug}writeln('oraa');{$endif}
    i:=getAddrVal(i-$8a,1);
    if (i<0) then goto vege;
    regs_a:=regs_a or i;
    setFlagsNZ8(regs_a);
    flg_v:=false;
    goto ok;
    end;
  $ca,$da,$fa,$ea:begin;
    {$ifdef debug}writeln('orab');{$endif}
    i:=getAddrVal(i-$ca,1);
    if (i<0) then goto vege;
    regs_b:=regs_b or i;
    setFlagsNZ8(regs_b);
    flg_v:=false;
    goto ok;
    end;
  $36:begin;
    {$ifdef debug}writeln('psha');{$endif}
    if pushByte(regs_a) then goto vege;
    goto ok;
    end;
  $37:begin;
    {$ifdef debug}writeln('pshb');{$endif}
    if pushByte(regs_b) then goto vege;
    goto ok;
    end;
  $3c:begin;
    {$ifdef debug}writeln('pshxy');{$endif}
    prefix:=(prefix and 1)+reg_x;
    i:=readReg(prefix);
    if pushWord(i) then goto vege;
    goto ok;
    end;
  $32:begin;
    {$ifdef debug}writeln('pula');{$endif}
    i:=popByte;
    if (i<0) then goto vege;
    regs_a:=i;
    goto ok;
    end;
  $33:begin;
    {$ifdef debug}writeln('pulb');{$endif}
    i:=popByte;
    if (i<0) then goto vege;
    regs_b:=i;
    goto ok;
    end;
  $38:begin;
    {$ifdef debug}writeln('pulxy');{$endif}
    prefix:=(prefix and 1)+reg_x;
    o:=popWord;
    if (o<0) then goto vege;
    writeReg(prefix,i);
    goto ok;
    end;
  $49:begin;
    {$ifdef debug}writeln('rola');{$endif}
    doROL(regs_a);
    goto ok;
    end;
  $59:begin;
    {$ifdef debug}writeln('rolb');{$endif}
    doROL(regs_b);
    goto ok;
    end;
  $69,$79:begin;
    {$ifdef debug}writeln('rol');{$endif}
    i:=getAddress(i-$49);
    if (i<0) then goto vege;
    doROL(memoryData^[i]);
    goto ok;
    end;
  $46:begin;
    {$ifdef debug}writeln('rora');{$endif}
    doROR(regs_a);
    goto ok;
    end;
  $56:begin;
    {$ifdef debug}writeln('rorb');{$endif}
    doROR(regs_b);
    goto ok;
    end;
  $66,$76:begin;
    {$ifdef debug}writeln('ror');{$endif}
    i:=getAddress(i-$46);
    if (i<0) then goto vege;
    doROR(memoryData^[i]);
    goto ok;
    end;
  $3b:begin;
    {$ifdef debug}writeln('rti');{$endif}
    i:=popByte;if (i<0) then goto vege;
    putFlags(i);
    i:=popByte;if (i<0) then goto vege;
    regs_b:=i;
    i:=popByte;if (i<0) then goto vege;
    regs_a:=i;
    i:=popWord;if (i<0) then goto vege;
    regs_x:=i;
    i:=popWord;if (i<0) then goto vege;
    regs_y:=i;
    i:=popWord;if (i<0) then goto vege;
    regs_pc:=i;
    goto ok;
    end;
  $39:begin;
    {$ifdef debug}writeln('rts');{$endif}
    i:=popWord;
    if (i<0) then goto vege;
    regs_pc:=i;
    goto ok;
    end;
  $10:begin;
    {$ifdef debug}writeln('sba');{$endif}
    setFlagsHVC(not regs_a,regs_b);
    dec(regs_a,regs_b);
    setFlagsNZ8(regs_a);
    goto ok;
    end;
  $82,$92,$b2,$a2:begin;
    {$ifdef debug}writeln('sbca');{$endif}
    i:=getAddrVal(i-$82,1);
    if (i<0) then goto vege;
    if flg_c then inc(i);
    setFlagsHVC(not regs_a,i);
    dec(regs_a,i);
    setFlagsNZ8(regs_a);
    goto ok;
    end;
  $c2,$d2,$f2,$e2:begin;
    {$ifdef debug}writeln('sbcb');{$endif}
    i:=getAddrVal(i-$c2,1);
    if (i<0) then goto vege;
    if flg_c then inc(i);
    setFlagsHVC(not regs_b,i);
    dec(regs_b,i);
    setFlagsNZ8(regs_b);
    goto ok;
    end;
  $0d:begin;
    {$ifdef debug}writeln('sec');{$endif}
    flg_c:=true;
    goto ok;
    end;
  $0f:begin;
    {$ifdef debug}writeln('sei');{$endif}
    flg_i:=true;
    goto ok;
    end;
  $0b:begin;
    {$ifdef debug}writeln('sev');{$endif}
    flg_v:=true;
    goto ok;
    end;
  $97,$b7,$a7:begin;
    {$ifdef debug}writeln('staa');{$endif}
    i:=getAddress(i-$87);
    if (i<0) then goto vege;
    memoryData^[i]:=regs_a;
    goto ok;
    end;
  $d7,$f7,$e7:begin;
    {$ifdef debug}writeln('stab');{$endif}
    i:=getAddress(i-$c7);
    if (i<0) then goto vege;
    memoryData^[i]:=regs_b;
    goto ok;
    end;
  $dd,$fd,$ed:begin;
    {$ifdef debug}writeln('std');{$endif}
    i:=getAddress(i-$cd);
    if (i<0) then goto vege;
    WriteWordMSB(memoryData^[i],readReg(reg_d));
    goto ok;
    end;
  $cf:begin;
    {$ifdef debug}writeln('stop');{$endif}
    goto ok;
    end;
  $9f,$bf,$af:begin;
    {$ifdef debug}writeln('sts');{$endif}
    i:=getAddress(i-$8f);
    if (i<0) then goto vege;
    WriteWordMSB(memoryData^[i],regs_sp);
    goto ok;
    end;
  $df,$ff,$ef:case prefix of
    0,3:begin;
      {$ifdef debug}writeln('stx');{$endif}
      i:=getAddress(i-$cf);
      if (i<0) then goto vege;
      WriteWordMSB(memoryData^[i],regs_x);
      goto ok;
      end;
    1,2:begin;
      {$ifdef debug}writeln('sty');{$endif}
      i:=getAddress(i-$cf);
      if (i<0) then goto vege;
      WriteWordMSB(memoryData^[i],regs_y);
      goto ok;
      end;
    end;
  $80,$90,$b0,$a0:begin;
    {$ifdef debug}writeln('suba');{$endif}
    i:=getAddrVal(i-$80,1);
    if (i<0) then goto vege;
    setFlagsHVC(not regs_a,i);
    dec(regs_a,i);
    setFlagsNZ8(regs_a);
    goto ok;
    end;
  $c0,$d0,$f0,$e0:begin;
    {$ifdef debug}writeln('subb');{$endif}
    i:=getAddrVal(i-$c0,1);
    if (i<0) then goto vege;
    setFlagsHVC(not regs_b,i);
    dec(regs_b,i);
    setFlagsNZ8(regs_b);
    goto ok;
    end;
  $3f:begin;
    {$ifdef debug}writeln('swi');{$endif}
    if pushWord(regs_pc) then goto vege;
    if pushWord(regs_y) then goto vege;
    if pushWord(regs_x) then goto vege;
    if pushByte(regs_a) then goto vege;
    if pushByte(regs_b) then goto vege;
    if pushByte(getFlags) then goto vege;
    regs_pc:=interruptBase;
    goto ok;
    end;
  $3e:begin;
    {$ifdef debug}writeln('wai');{$endif}
    if pushWord(regs_pc) then goto vege;
    if pushWord(regs_y) then goto vege;
    if pushWord(regs_x) then goto vege;
    if pushByte(regs_a) then goto vege;
    if pushByte(regs_b) then goto vege;
    if pushByte(getFlags) then goto vege;
    regs_pc:=interruptBase;
    goto ok;
    end;
  $16:begin;
    {$ifdef debug}writeln('tab');{$endif}
    regs_b:=regs_a;
    setFlagsNZ8(regs_b);
    flg_v:=false;
    goto ok;
    end;
  $06:begin;
    {$ifdef debug}writeln('tap');{$endif}
    putFlags(regs_a);
    goto ok;
    end;
  $17:begin;
    {$ifdef debug}writeln('tba');{$endif}
    regs_a:=regs_b;
    setFlagsNZ8(regs_a);
    flg_v:=false;
    goto ok;
    end;
  $00:begin;
    {$ifdef debug}writeln('test');{$endif}
    goto ok;
    end;
  $07:begin;
    {$ifdef debug}writeln('tpa');{$endif}
    regs_a:=getFlags;
    goto ok;
    end;
  $4d:begin;
    {$ifdef debug}writeln('tsta');{$endif}
    doTST(regs_a);
    goto ok;
    end;
  $5d:begin;
    {$ifdef debug}writeln('tstb');{$endif}
    doTST(regs_b);
    goto ok;
    end;
  $6d,$7d:begin;
    {$ifdef debug}writeln('tst');{$endif}
    i:=getAddress(i-$4d);
    if (i<0) then goto vege;
    doTST(memoryData^[i]);
    goto ok;
    end;
  $30:begin;
    {$ifdef debug}writeln('tsxy');{$endif}
    prefix:=(prefix and 1)+reg_x;
    writeReg(prefix,regs_sp);
    goto ok;
    end;
  $35:begin;
    {$ifdef debug}writeln('txys');{$endif}
    prefix:=(prefix and 1)+reg_x;
    regs_sp:=readReg(prefix);
    goto ok;
    end;
  $8f:begin;
    {$ifdef debug}writeln('xgdxy');{$endif}
    prefix:=(prefix and 1)+reg_x;
    i:=readReg(prefix);
    writeReg(prefix,readReg(reg_d));
    writeReg(reg_d,i);
    goto ok;
    end;
  end;
goto vege;

ok:
result:=0;
vege:
emulateOneOpcode:=result;
End;
