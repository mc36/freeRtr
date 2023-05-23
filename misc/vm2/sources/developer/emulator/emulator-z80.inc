Type memoryType=array[0..1] of byte;
Var
  memoryData:^memoryType;
  memorySize:LongInt;
Var
  regs:array[0..7] of byte;
  regs2:array[0..7] of byte;
  flg_i1,flg_i2,flg_s,flg_z,flg_h,flg_p,flg_n,flg_c:Boolean;
  regs_ix,regs_iy,regs_pc,regs_sp:Word;
  flg2,regs_i,regs_r:Byte;
Const
  reg_b=0;
  reg_c=1;
  reg_d=2;
  reg_e=3;
  reg_h=4;
  reg_l=5;
  reg_a=7;
  reg_bc=0;
  reg_de=1;
  reg_hl=2;
  reg_af=3;

Procedure emulateRestart;
Var i:Longint;
Begin;
for i:=0 to 7 do regs[i]:=0;
for i:=0 to 7 do regs2[i]:=0;
flg_i1:=true;
flg_i2:=true;
flg_s:=false;
flg_z:=false;
flg_h:=false;
flg_p:=false;
flg_n:=false;
flg_c:=false;
flg2:=0;
regs_ix:=0;
regs_iy:=0;
regs_pc:=0;
regs_sp:=memorySize;
regs_i:=$5a;
regs_r:=$19;
End;


Function getFlags:LongInt;
Var i:LongInt;
Begin;
i:=0;
if flg_s then i:=i or $80;
if flg_z then i:=i or $40;
if flg_h then i:=i or $10;
if flg_p then i:=i or $04;
if flg_n then i:=i or $02;
if flg_c then i:=i or $01;
getFlags:=i;
End;

Procedure putFlags(i:LongInt);
Begin;
flg_s:=(i and $80<>0);
flg_z:=(i and $40<>0);
flg_h:=(i and $10<>0);
flg_p:=(i and $04<>0);
flg_n:=(i and $02<>0);
flg_c:=(i and $01<>0);
End;

Procedure WriteReg16(n,v:LongInt);
Var h,l:LongInt;
Begin;
v:=v and $ffff;
h:=v shr 8;
l:=v and $ff;
case n of
  0:begin; regs[reg_b]:=h;regs[reg_c]:=l; end;
  1:begin; regs[reg_d]:=h;regs[reg_e]:=l; end;
  2,16:begin; regs[reg_h]:=h;regs[reg_l]:=l; end;
  3:regs_sp:=v;
  17:regs_ix:=v;
  18:regs_iy:=v;
  end;
End;

Function ReadReg16(n:LongInt):LongInt;
Var h,l:LongInt;
Begin;
case n of
  0:begin; h:=regs[reg_b];l:=regs[reg_c];inc(l,h*256); end;
  1:begin; h:=regs[reg_d];l:=regs[reg_e];inc(l,h*256); end;
  2,16:begin; h:=regs[reg_h];l:=regs[reg_l];inc(l,h*256); end;
  3:l:=regs_sp;
  17:l:=regs_ix;
  18:l:=regs_iy;
  else l:=-1;
  end;
ReadReg16:=l;
End;



Function emulateOneOpcode:LongInt;
Label ok,vege,hlixiy,ed,cb,hxycb;
Const bits:array[0..7] of byte=($01,$02,$04,$08,$10,$20,$40,$80);
Var result:LongInt;

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
getWord:=ReadWordLSB(memoryData^[regs_pc]);
inc(regs_pc,2);
End;

Function getHLIXIYptr(which:Byte):LongInt;
Var i,o:LongInt;
Begin;
getHLIXIYptr:=-1;
case which of
  0:begin; {hl}
    i:=regs[reg_h];
    o:=regs[reg_l];
    i:=(i*256)+o;
    end;
  1:begin; {ix}
    o:=regs_ix;
    i:=getByte;
    i:=shortint(i);
    inc(i,o);
    end;
  2:begin; {iy}
    o:=regs_iy;
    i:=shortint(i);
    i:=getByte;
    inc(i,o);
    end;
  3:begin; {bc}
    i:=regs[reg_b];
    o:=regs[reg_c];
    i:=(i*256)+o;
    end;
  4:begin; {de}
    i:=regs[reg_d];
    o:=regs[reg_e];
    i:=(i*256)+o;
    end;
  8:begin; {num16}
    i:=getWord;
    end;
  else exit;
  end;
getHLIXIYptr:=i;
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

Procedure setFlagsSZH(o,n,b:LongInt);
Var i:LongInt;
Begin;
if (b=8) then begin;
  n:=n and $ff;
  b:=$80;
  i:=$f0;
  end else begin;
  n:=n and $ffff;
  b:=$8000;
  i:=$ff00;
  end;
flg_s:=(n and b<>0);
flg_z:=(n=0);
flg_h:=(n and i<>o and i);
End;

Procedure setFlagsVC(o,n,b:LongInt);
Begin;
if (b=8) then b:=$80 else b:=$8000;
flg_c:=(n>=b+b);
flg_p:=(o and b<>n and b);
End;

Procedure setFlagsP(o,n,b:LongInt);
Begin;
flg_p:=(n and 1=0);
End;

Function testCond(c:byte):Boolean;
Var b:Boolean;
Begin;
case c and 7 of
  0:b:=not flg_z;
  1:b:=flg_z;
  2:b:=not flg_c;
  3:b:=flg_c;
  4:b:=not flg_p;
  5:b:=flg_p;
  6:b:=not flg_s;
  7:b:=flg_s;
  end;
testCond:=b;
End;

Procedure WriteReg16q(n,v:LongInt);
Begin;
if (n<>3) then begin;
  WriteReg16(n,v);
  exit;
  end;
regs[reg_a]:=(v shr 8) and $ff;
putFlags(v);
End;

Function ReadReg16q(n:LongInt):LongInt;
Begin;
if (n<>3) then begin;
  ReadReg16q:=ReadReg16(n);
  exit;
  end;
n:=regs[reg_a];
ReadReg16q:=(n*256)+getFlags;
End;

Procedure doExx(o:LongInt);
Var i:LongInt;
Begin;
i:=regs[o];
regs[o]:=regs2[o];
regs2[o]:=i;
End;



Procedure doLD(m:LongInt);
Var i,o:LongInt;
Begin;
i:=ReadReg16(reg_de);
if checkRange(i) then exit;
o:=memoryData^[i];
WriteReg16(reg_de,i+m);
i:=ReadReg16(reg_hl);
if checkRange(i) then exit;
memoryData^[i]:=o;
WriteReg16(reg_hl,i+m);
i:=ReadReg16(reg_bc)-1;
WriteReg16(reg_bc,i);
flg_p:=(i<>0);
flg_n:=false;
flg_h:=false;
End;

Procedure doLDR(m:LongInt);
Var i,o,p:LongInt;
Begin;
i:=ReadReg16(reg_de);
o:=ReadReg16(reg_hl);
p:=ReadReg16(reg_bc);
repeat
  if checkRange(i) then exit;
  if checkRange(o) then exit;
  memoryData^[o]:=memoryData^[i];
  i:=(i+1) and $ffff;
  o:=(o+1) and $ffff;
  p:=(p-1) and $ffff;
  until (p=0);
WriteReg16(reg_de,i);
WriteReg16(reg_hl,o);
WriteReg16(reg_bc,p);
flg_p:=false;
flg_n:=false;
flg_h:=false;
End;

Procedure doCP(m:LongInt);
Var i,o,p:LongInt;
Begin;
p:=regs[reg_a];
i:=ReadReg16(reg_hl);
if checkRange(i) then exit;
o:=p-memoryData^[i];
WriteReg16(reg_hl,i+m);
i:=ReadReg16(reg_bc)-1;
WriteReg16(reg_bc,i);
setFlagsSZH(p,o,8);
flg_p:=(i<>0);
flg_n:=true;
End;

Procedure doCPR(m:LongInt);
Var i,o,p,q:LongInt;
Begin;
i:=ReadReg16(reg_hl);
p:=ReadReg16(reg_bc);
q:=regs[reg_a];
repeat
  if checkRange(i) then exit;
  o:=q-memoryData^[i];
  i:=(i+1) and $ffff;
  p:=(p-1) and $ffff;
  until (p=0) or (o=0);
WriteReg16(reg_hl,i);
WriteReg16(reg_bc,p);
setFlagsSZH(q,o,8);
flg_p:=(i<>0);
flg_n:=true;
End;

Procedure doRLC(var b:Byte);
Begin;
b:=rotateLeft(b,8,1);
flg_c:=(b and 1<>0);
setFlagsP(b,b,8);
flg_n:=false;
flg_h:=false;
End;

Procedure doRL(var b:Byte);
Var i:LongInt;
Begin;
i:=b;
if flg_c then inc(i,$100);
i:=rotateLeft(i,9,1);
b:=i and $ff;
flg_c:=(i and $100<>0);
setFlagsP(b,b,8);
flg_n:=false;
flg_h:=false;
End;

Procedure doRRC(var b:Byte);
Begin;
b:=rotateLeft(b,8,1);
flg_c:=(b and $80<>0);
setFlagsP(b,b,8);
flg_n:=false;
flg_h:=false;
End;

Procedure doRR(var b:Byte);
Var i:LongInt;
Begin;
i:=b;
i:=i shl 1;
if flg_c then inc(i);
i:=rotateLeft(i,9,1);
b:=i shr 1;
flg_c:=(i and 1<>0);
setFlagsP(b,b,8);
flg_n:=false;
flg_h:=false;
End;

Procedure doSLA(var b:Byte);
Var i:LongInt;
Begin;
i:=b;
i:=i shl 1;
b:=i and $ff;
flg_c:=(i and $100<>0);
setFlagsP(b,b,8);
flg_n:=false;
flg_h:=false;
End;

Procedure doSRA(var b:byte);
Begin;
b:=b shr 1;
if (b and $40<>0) then inc(b,$80);
setFlagsP(b,b,8);
flg_n:=false;
flg_h:=false;
End;

Procedure doSRL(var b:byte);
Begin;
b:=b shr 1;
setFlagsP(b,b,8);
flg_n:=false;
flg_h:=false;
End;

Function doPush(i:LongInt):Boolean;
Begin;
doPush:=True;
dec(regs_sp,2);
if checkRange(regs_sp) then exit;
WriteWordLSB(memoryData^[regs_sp],i);
doPush:=False;
End;

Function doPop:LongInt;
Begin;
if checkRange(regs_sp) then exit;
doPop:=ReadWordLSB(memoryData^[regs_sp]);
inc(regs_sp,2);
End;




Var i,o,p:LongInt;
Begin;
result:=1;

p:=0;
i:=getByte;
if (i<0) then goto vege;
case i of
  $dd:begin;
    i:=getByte;
    p:=1;
    goto hlixiy;
    end;
  $fd:begin;
    i:=getByte;
    p:=2;
    goto hlixiy;
    end;
  $ed:goto ed;
  $cb:goto cb;
  $36:goto hlixiy;
  $2a:goto hlixiy;
  $22:goto hlixiy;
  $f9:goto hlixiy;
  $e3:goto hlixiy;
  $86:goto hlixiy;
  $86:goto hlixiy;
  $8e:goto hlixiy;
  $96:goto hlixiy;
  $9e:goto hlixiy;
  $b6:goto hlixiy;
  $ae:goto hlixiy;
  $a6:goto hlixiy;
  $be:goto hlixiy;
  $34:goto hlixiy;
  $35:goto hlixiy;
  $e9:goto hlixiy;
  $db:begin;
    {$ifdef debug}writeln('in a,(num8)');{$endif}
    regs[reg_a]:=readPort(getByte,1);
    goto ok;
    end;
  $d3:begin;
    {$ifdef debug}writeln('out (num8),a');{$endif}
    writePort(getByte,1,regs[reg_a]);
    goto ok;
    end;
  $c3:begin;
    {$ifdef debug}writeln('jp num16');{$endif}
    regs_pc:=getWord;
    goto ok;
    end;
  $cd:begin;
    {$ifdef debug}writeln('call num16');{$endif}
    o:=getWord;
    if doPush(regs_pc) then goto vege;
    regs_pc:=o;
    goto ok;
    end;
  $c9:begin;
    {$ifdef debug}writeln('ret');{$endif}
    i:=doPop;
    if (i<0) then goto vege;
    regs_pc:=i;
    goto ok;
    end;
  $18:begin;
    {$ifdef debug}writeln('jr num8');{$endif}
    i:=getByte;
    i:=shortint(i);
    inc(regs_pc,i);
    goto ok;
    end;
  $10:begin;
    {$ifdef debug}writeln('djnz num8');{$endif}
    i:=getByte;
    i:=shortint(i);
    dec(regs[reg_b]);
    if (regs[reg_b]<>0) then inc(regs_pc,i);
    goto ok;
    end;
  $0a:begin;
    {$ifdef debug}writeln('ld a,(bc)');{$endif}
    i:=getHLIXIYptr(3);
    if checkRange(i) then goto vege;
    regs[reg_a]:=memoryData^[i];
    goto ok;
    end;
  $1a:begin;
    {$ifdef debug}writeln('ld a,(de)');{$endif}
    i:=getHLIXIYptr(4);
    if checkRange(i) then goto vege;
    regs[reg_a]:=memoryData^[i];
    goto ok;
    end;
  $3a:begin;
    {$ifdef debug}writeln('ld a,(num16)');{$endif}
    i:=getHLIXIYptr(8);
    if checkRange(i) then goto vege;
    regs[reg_a]:=memoryData^[i];
    goto ok;
    end;
  $02:begin;
    {$ifdef debug}writeln('ld (bc),a');{$endif}
    i:=getHLIXIYptr(3);
    if checkRange(i) then goto vege;
    memoryData^[i]:=regs[reg_a];
    goto ok;
    end;
  $12:begin;
    {$ifdef debug}writeln('ld (de),a');{$endif}
    i:=getHLIXIYptr(4);
    if checkRange(i) then goto vege;
    memoryData^[i]:=regs[reg_a];
    goto ok;
    end;
  $32:begin;
    {$ifdef debug}writeln('ld (num16),a');{$endif}
    i:=getHLIXIYptr(8);
    if checkRange(i) then goto vege;
    memoryData^[i]:=regs[reg_a];
    goto ok;
    end;
  $eb:begin;
    {$ifdef debug}writeln('ex de,hl');{$endif}
    i:=ReadReg16(reg_de);
    WriteReg16(reg_de,ReadReg16(2));
    WriteReg16(reg_hl,i);
    goto ok;
    end;
  $08:begin;
    {$ifdef debug}writeln('ex af,af');{$endif}
    doExx(reg_a);
    i:=getFlags;
    putFlags(flg2);
    flg2:=i;
    goto ok;
    end;
  $d8:begin;
    {$ifdef debug}writeln('exx');{$endif}
    doExx(reg_b);
    doExx(reg_c);
    doExx(reg_d);
    doExx(reg_e);
    doExx(reg_h);
    doExx(reg_l);
    goto ok;
    end;
  $c6:begin;
    {$ifdef debug}writeln('add a,num8');{$endif}
    p:=regs[reg_a];
    o:=p+getByte;
    setFlagsSZH(p,o,8);
    setFlagsVC(p,o,8);
    flg_n:=false;
    regs[reg_a]:=o;
    goto ok;
    end;
  $ce:begin;
    {$ifdef debug}writeln('adc a,num8');{$endif}
    p:=regs[reg_a];
    o:=p+getByte;
    if flg_c then inc(o);
    setFlagsSZH(p,o,8);
    setFlagsVC(p,o,8);
    flg_n:=false;
    regs[reg_a]:=o;
    goto ok;
    end;
  $d6:begin;
    {$ifdef debug}writeln('sub a,num8');{$endif}
    p:=regs[reg_a];
    o:=p-getByte;
    setFlagsSZH(p,o,8);
    setFlagsVC(p,o,8);
    flg_n:=true;
    regs[reg_a]:=o;
    goto ok;
    end;
  $de:begin;
    {$ifdef debug}writeln('sbc a,num8');{$endif}
    p:=regs[reg_a];
    o:=p-getByte;
    if flg_c then dec(o);
    setFlagsSZH(p,o,8);
    setFlagsVC(p,o,8);
    flg_n:=true;
    regs[reg_a]:=o;
    goto ok;
    end;
  $e6:begin;
    {$ifdef debug}writeln('and a,num8');{$endif}
    p:=regs[reg_a];
    o:=p and getByte;
    setFlagsSZH(p,o,8);
    setFlagsP(p,o,8);
    flg_c:=false;
    flg_n:=true;
    regs[reg_a]:=o;
    goto ok;
    end;
  $f6:begin;
    {$ifdef debug}writeln('or a,num8');{$endif}
    p:=regs[reg_a];
    o:=p or getByte;
    setFlagsSZH(p,o,8);
    setFlagsP(p,o,8);
    flg_c:=false;
    flg_n:=true;
    regs[reg_a]:=o;
    goto ok;
    end;
  $ee:begin;
    {$ifdef debug}writeln('xor a,num8');{$endif}
    p:=regs[reg_a];
    o:=p xor getByte;
    setFlagsSZH(p,o,8);
    setFlagsP(p,o,8);
    flg_c:=false;
    flg_n:=true;
    regs[reg_a]:=o;
    goto ok;
    end;
  $fe:begin;
    {$ifdef debug}writeln('cp a,num8');{$endif}
    p:=regs[reg_a];
    o:=p-getByte;
    setFlagsSZH(p,o,8);
    setFlagsVC(p,o,8);
    flg_n:=true;
    goto ok;
    end;
  $27:begin;
    {$ifdef debug}writeln('daa');{$endif}
    p:=regs[reg_a];
    o:=((p div 10) shl 4) or (p mod 10);
    setFlagsSZH(p,o,8);
    setFlagsVC(p,o,8);
    setFlagsP(p,o,8);
    regs[reg_a]:=o;
    goto ok;
    end;
  $2f:begin;
    {$ifdef debug}writeln('cpl');{$endif}
    o:=not regs[reg_a];
    flg_n:=true;
    flg_h:=true;
    regs[reg_a]:=o;
    goto ok;
    end;
  $3f:begin;
    {$ifdef debug}writeln('ccf');{$endif}
    flg_c:=not flg_c;
    goto ok;
    end;
  $37:begin;
    {$ifdef debug}writeln('scf');{$endif}
    flg_c:=true;
    goto ok;
    end;
  $00:begin;
    {$ifdef debug}writeln('nop');{$endif}
    goto ok;
    end;
  $76:begin;
    {$ifdef debug}writeln('halt');{$endif}
    goto ok;
    end;
  $f3:begin;
    {$ifdef debug}writeln('di');{$endif}
    flg_i1:=false;
    goto ok;
    end;
  $fb:begin;
    {$ifdef debug}writeln('ei');{$endif}
    flg_i1:=true;
    goto ok;
    end;
  $07:begin;
    {$ifdef debug}writeln('rlca');{$endif}
    doRLC(regs[reg_a]);
    goto ok;
    end;
  $17:begin;
    {$ifdef debug}writeln('rla');{$endif}
    doRL(regs[reg_a]);
    goto ok;
    end;
  $0f:begin;
    {$ifdef debug}writeln('rrca');{$endif}
    doRRC(regs[reg_a]);
    goto ok;
    end;
  $1f:begin;
    {$ifdef debug}writeln('rra');{$endif}
    doRR(regs[reg_a]);
    goto ok;
    end;
  end;
case i and $cf of
  $01:begin;
    {$ifdef debug}writeln('ld reg16,num16');{$endif}
    WriteReg16((i shr 4) and 3,getWord);
    goto ok;
    end;
  $c5:begin;
    {$ifdef debug}writeln('push reg16');{$endif}
    if doPush(ReadReg16q((i shr 4) and 3)) then goto vege;
    goto ok;
    end;
  $c1:begin;
    {$ifdef debug}writeln('pop reg16');{$endif}
    o:=doPop;
    if (o<0) then goto vege;
    WriteReg16q((i shr 4) and 3,o);
    goto ok;
    end;
  $09:goto hlixiy;
  $03:begin;
    {$ifdef debug}writeln('inc reg16');{$endif}
    i:=(i shr 4) and 3;
    WriteReg16(i,ReadReg16(i)+1);
    goto ok;
    end;
  $0b:begin;
    {$ifdef debug}writeln('dec reg16');{$endif}
    i:=(i shr 4) and 3;
    WriteReg16(i,ReadReg16(i)-1);
    goto ok;
    end;
  end;
if (i and $e7=$20) then begin;
  {$ifdef debug}writeln('jr cond,num16');{$endif}
  o:=getByte;
  o:=shortint(o);
  if testCond((i shr 3) and 3) then inc(regs_pc,o);
  goto ok;
  end;
case i and $c7 of
  $c2:begin;
    {$ifdef debug}writeln('jp cond,num16');{$endif}
    o:=getWord;
    if testCond(i shr 3) then regs_pc:=o;
    goto ok;
    end;
  $c4:begin;
    {$ifdef debug}writeln('call cond,num16');{$endif}
    o:=getWord;
    if not testCond(i shr 3) then goto ok;
    if doPush(regs_pc) then goto vege;
    regs_pc:=o;
    goto ok;
    end;
  $c0:begin;
    {$ifdef debug}writeln('ret cond');{$endif}
    if not testCond(i shr 3) then goto ok;
    i:=doPop;
    if (i<0) then goto vege;
    regs_pc:=i;
    goto ok;
    end;
  $c7:begin;
    {$ifdef debug}writeln('rst num8');{$endif}
    if doPush(regs_pc) then goto vege;
    regs_pc:=i and $38;
    goto ok;
    end;
  $06:begin;
    {$ifdef debug}writeln('ld reg8,num8');{$endif}
    regs[(i shr 3) and 7]:=getByte;
    goto ok;
    end;
  $04:begin;
    {$ifdef debug}writeln('inc reg8');{$endif}
    p:=(i shr 3) and 7;
    o:=regs[p]+1;
    setFlagsSZH(o-1,o,8);
    setFlagsVC(o-1,o,8);
    flg_n:=false;
    regs[p]:=o;
    goto ok;
    end;
  $05:begin;
    {$ifdef debug}writeln('dec reg8');{$endif}
    p:=(i shr 3) and 7;
    o:=regs[p]-1;
    setFlagsSZH(o+1,o,8);
    setFlagsVC(o+1,o,8);
    flg_n:=true;
    regs[p]:=o;
    goto ok;
    end;
  $46:goto hlixiy;
  end;
case i and $f8 of
  $70:goto hlixiy;
  $80:begin;
    {$ifdef debug}writeln('add a,reg8');{$endif}
    p:=regs[reg_a];
    o:=p+regs[i and 7];
    setFlagsSZH(p,o,8);
    setFlagsVC(p,o,8);
    flg_n:=false;
    regs[reg_a]:=o;
    goto ok;
    end;
  $88:begin;
    {$ifdef debug}writeln('adc a,reg8');{$endif}
    p:=regs[reg_a];
    o:=p+regs[i and 7];
    if flg_c then inc(o);
    setFlagsSZH(p,o,8);
    setFlagsVC(p,o,8);
    flg_n:=false;
    regs[reg_a]:=o;
    goto ok;
    end;
  $90:begin;
    {$ifdef debug}writeln('sub a,reg8');{$endif}
    p:=regs[reg_a];
    o:=p-regs[i and 7];
    setFlagsSZH(p,o,8);
    setFlagsVC(p,o,8);
    flg_n:=true;
    regs[reg_a]:=o;
    goto ok;
    end;
  $98:begin;
    {$ifdef debug}writeln('sbc a,reg8');{$endif}
    p:=regs[reg_a];
    o:=p-regs[i and 7];
    if flg_c then dec(o);
    setFlagsSZH(p,o,8);
    setFlagsVC(p,o,8);
    flg_n:=true;
    regs[reg_a]:=o;
    goto ok;
    end;
  $a0:begin;
    {$ifdef debug}writeln('and a,reg8');{$endif}
    p:=regs[reg_a];
    o:=p and regs[i and 7];
    setFlagsSZH(p,o,8);
    setFlagsP(p,o,8);
    flg_n:=false;
    flg_c:=false;
    regs[reg_a]:=o;
    goto ok;
    end;
  $b0:begin;
    {$ifdef debug}writeln('or a,reg8');{$endif}
    p:=regs[reg_a];
    o:=p or regs[i and 7];
    setFlagsSZH(p,o,8);
    setFlagsP(p,o,8);
    flg_n:=false;
    flg_c:=false;
    regs[reg_a]:=o;
    goto ok;
    end;
  $a8:begin;
    {$ifdef debug}writeln('xor a,reg8');{$endif}
    p:=regs[reg_a];
    o:=p xor regs[i and 7];
    setFlagsSZH(p,o,8);
    setFlagsP(p,o,8);
    flg_n:=false;
    flg_c:=false;
    regs[reg_a]:=o;
    goto ok;
    end;
  $b8:begin;
    {$ifdef debug}writeln('cp a,reg8');{$endif}
    p:=regs[reg_a];
    o:=p-regs[i and 7];
    setFlagsSZH(p,o,8);
    setFlagsVC(p,o,8);
    flg_n:=true;
    goto ok;
    end;
  end;
if (i and $c0=$40) then begin;
  {$ifdef debug}writeln('ld reg8,reg8');{$endif}
  regs[(i shr 3) and 7]:=regs[i and 7];
  goto ok;
  end;
goto vege;



hlixiy:
case i of
  $24:begin;
    {$ifdef debug}writeln('inc reg8');{$endif}
    WriteReg16(p+16,ReadReg16(p+16)+$100);
    goto ok;
    end;
  $25:begin;
    {$ifdef debug}writeln('dec reg8');{$endif}
    WriteReg16(p+16,ReadReg16(p+16)-$100);
    goto ok;
    end;
  $2c:begin;
    {$ifdef debug}writeln('inc reg8');{$endif}
    i:=ReadReg16(p+16);
    i:=(i and $ff00) or ((i+1) and $ff);
    WriteReg16(p+16,i);
    goto ok;
    end;
  $2d:begin;
    {$ifdef debug}writeln('dec reg8');{$endif}
    i:=ReadReg16(p+16);
    i:=(i and $ff00) or ((i-1) and $ff);
    WriteReg16(p+16,i);
    goto ok;
    end;
  $e9:begin;
    {$ifdef debug}writeln('jp reg16');{$endif}
    regs_pc:=ReadReg16(p+16);
    goto ok;
    end;
  $cb:begin;
    p:=getHLIXIYptr(p);
    if checkRange(p) then goto vege;
    i:=getByte;
    goto hxycb;
    end;
  $36:begin;
    {$ifdef debug}writeln('ld (mem),num8');{$endif}
    i:=getHLIXIYptr(p);
    if checkRange(i) then goto vege;
    memoryData^[i]:=getByte;
    goto ok;
    end;
  $21:begin;
    {$ifdef debug}writeln('ld reg16,num16');{$endif}
    WriteReg16(p+16,getWord);
    goto ok;
    end;
  $2a:begin;
    {$ifdef debug}writeln('ld reg16,(num16)');{$endif}
    i:=getHLIXIYptr(8);
    if checkRange(i) then goto vege;
    WriteReg16(p+16,ReadWordLSB(memoryData^[i]));
    goto ok;
    end;
  $22:begin;
    {$ifdef debug}writeln('ld (num16),reg16');{$endif}
    i:=getHLIXIYptr(8);
    if checkRange(i) then goto vege;
    WriteWordLSB(memoryData^[i],ReadReg16(p+16));
    goto ok;
    end;
  $f9:begin;
    {$ifdef debug}writeln('ld sp,reg16');{$endif}
    regs_sp:=ReadReg16(p+16);
    goto ok;
    end;
  $e5:begin;
    {$ifdef debug}writeln('push reg16');{$endif}
    if doPush(ReadReg16(p+16)) then goto vege;
    goto ok;
    end;
  $e1:begin;
    {$ifdef debug}writeln('pop reg16');{$endif}
    o:=doPop;
    if (o<0) then goto vege;
    WriteReg16(p+16,o);
    goto ok;
    end;
  $e3:begin;
    {$ifdef debug}writeln('ex (sp),reg16');{$endif}
    if checkRange(regs_sp) then goto vege;
    i:=ReadReg16(p+16);
    WriteReg16(p+16,ReadWordLSB(memoryData^[regs_sp]));
    WriteWordLSB(memoryData^[regs_sp],i);
    goto ok;
    end;
  $86:begin;
    {$ifdef debug}writeln('add a,(mem)');{$endif}
    i:=getHLIXIYptr(p);
    if checkRange(i) then goto vege;
    p:=regs[reg_a];
    o:=p+memoryData^[i];
    setFlagsSZH(p,o,8);
    setFlagsVC(p,o,8);
    flg_n:=false;
    regs[reg_a]:=o;
    goto ok;
    end;
  $8e:begin;
    {$ifdef debug}writeln('adc a,(mem)');{$endif}
    i:=getHLIXIYptr(p);
    if checkRange(i) then goto vege;
    p:=regs[reg_a];
    o:=p+memoryData^[i];
    if flg_c then inc(o);
    setFlagsSZH(p,o,8);
    setFlagsVC(p,o,8);
    flg_n:=false;
    regs[reg_a]:=o;
    goto ok;
    end;
  $96:begin;
    {$ifdef debug}writeln('sub a,(mem)');{$endif}
    i:=getHLIXIYptr(p);
    if checkRange(i) then goto vege;
    p:=regs[reg_a];
    o:=p-memoryData^[i];
    setFlagsSZH(p,o,8);
    setFlagsVC(p,o,8);
    flg_n:=true;
    regs[reg_a]:=o;
    goto ok;
    end;
  $9e:begin;
    {$ifdef debug}writeln('sbc a,(mem)');{$endif}
    i:=getHLIXIYptr(p);
    if checkRange(i) then goto vege;
    p:=regs[reg_a];
    o:=p-memoryData^[i];
    if flg_c then dec(o);
    setFlagsSZH(p,o,8);
    setFlagsVC(p,o,8);
    flg_n:=true;
    regs[reg_a]:=o;
    goto ok;
    end;
  $b6:begin;
    {$ifdef debug}writeln('or a,(mem)');{$endif}
    i:=getHLIXIYptr(p);
    if checkRange(i) then goto vege;
    p:=regs[reg_a];
    o:=p or memoryData^[i];
    setFlagsSZH(p,o,8);
    setFlagsP(p,o,8);
    flg_c:=false;
    flg_n:=false;
    regs[reg_a]:=o;
    goto ok;
    end;
  $ae:begin;
    {$ifdef debug}writeln('xor a,(mem)');{$endif}
    i:=getHLIXIYptr(p);
    if checkRange(i) then goto vege;
    p:=regs[reg_a];
    o:=p xor memoryData^[i];
    setFlagsSZH(p,o,8);
    setFlagsP(p,o,8);
    flg_c:=false;
    flg_n:=false;
    regs[reg_a]:=o;
    goto ok;
    end;
  $a6:begin;
    {$ifdef debug}writeln('and a,(mem)');{$endif}
    i:=getHLIXIYptr(p);
    if checkRange(i) then goto vege;
    p:=regs[reg_a];
    o:=p and memoryData^[i];
    setFlagsSZH(p,o,8);
    setFlagsP(p,o,8);
    flg_c:=false;
    flg_n:=false;
    regs[reg_a]:=o;
    goto ok;
    end;
  $be:begin;
    {$ifdef debug}writeln('cp a,(mem)');{$endif}
    i:=getHLIXIYptr(p);
    if checkRange(i) then goto vege;
    p:=regs[reg_a];
    o:=p-memoryData^[i];
    setFlagsSZH(p,o,8);
    setFlagsVC(p,o,8);
    flg_n:=true;
    goto ok;
    end;
  $34:begin;
    {$ifdef debug}writeln('inc (mem)');{$endif}
    i:=getHLIXIYptr(p);
    if checkRange(i) then goto vege;
    p:=memoryData^[i];
    o:=p+1;
    setFlagsSZH(p,o,8);
    setFlagsVC(p,o,8);
    flg_n:=false;
    memoryData^[i]:=o;
    goto ok;
    end;
  $35:begin;
    {$ifdef debug}writeln('dec (mem)');{$endif}
    i:=getHLIXIYptr(p);
    if checkRange(i) then goto vege;
    p:=memoryData^[i];
    o:=p-1;
    setFlagsSZH(p,o,8);
    setFlagsVC(p,o,8);
    flg_n:=true;
    memoryData^[i]:=o;
    goto ok;
    end;
  $23:begin;
    {$ifdef debug}writeln('inc reg16');{$endif}
    WriteReg16(p+16,ReadReg16(p+16)+1);
    goto ok;
    end;
  $2b:begin;
    {$ifdef debug}writeln('dec reg16');{$endif}
    WriteReg16(p+16,ReadReg16(p+16)-1);
    goto ok;
    end;
  end;
if (i and $cf=$09) then begin;
  {$ifdef debug}writeln('add reg16,reg16');{$endif}
  o:=(i shr 4) and 3;
  if (o<>2) then i:=ReadReg16(o) else i:=ReadReg16(p+16);
  o:=ReadReg16(p+16)+i;
  WriteReg16(p+16,o);
  flg_n:=false;
  flg_c:=(o>$ffff);
  goto ok;
  end;
if (i and $c7=$46) then begin;
  {$ifdef debug}writeln('ld reg8,(mem)');{$endif}
  o:=getHLIXIYptr(p);
  if checkRange(o) then goto vege;
  regs[(i shr 3) and 7]:=memoryData^[o];
  goto ok;
  end;
if (i and $f8=$70) then begin;
  {$ifdef debug}writeln('ld (mem),reg8');{$endif}
  o:=getHLIXIYptr(p);
  if checkRange(o) then goto vege;
  memoryData^[o]:=regs[i and 7];
  goto ok;
  end;
goto vege;



ed:
i:=getByte;
case i of
  $4d:begin;
    {$ifdef debug}writeln('reti');{$endif}
    i:=doPop;
    if (i<0) then goto vege;
    regs_pc:=i;
    goto ok;
    end;
  $45:begin;
    {$ifdef debug}writeln('retn');{$endif}
    i:=doPop;
    if (i<0) then goto vege;
    regs_pc:=i;
    goto ok;
    end;
  $57:begin;
    {$ifdef debug}writeln('ld a,i');{$endif}
    regs[reg_a]:=regs_i;
    setFlagsSZH(regs_i,regs_i,8);
    flg_p:=flg_i1;
    flg_n:=false;
    goto ok;
    end;
  $5f:begin;
    {$ifdef debug}writeln('ld a,r');{$endif}
    regs[reg_a]:=regs_r;
    setFlagsSZH(regs_r,regs_r,8);
    flg_p:=flg_i1;
    flg_n:=false;
    goto ok;
    end;
  $47:begin;
    {$ifdef debug}writeln('ld i,a');{$endif}
    regs_i:=regs[reg_a];
    goto ok;
    end;
  $4f:begin;
    {$ifdef debug}writeln('ld r,a');{$endif}
    regs_r:=regs[reg_a];
    goto ok;
    end;
  $a0:begin;
    {$ifdef debug}writeln('ldi');{$endif}
    doLD(+1);
    goto ok;
    end;
  $b0:begin;
    {$ifdef debug}writeln('ldir');{$endif}
    doLDR(+1);
    goto ok;
    end;
  $a8:begin;
    {$ifdef debug}writeln('ldd');{$endif}
    doLD(-1);
    goto ok;
    end;
  $b8:begin;
    {$ifdef debug}writeln('lddr');{$endif}
    doLDR(-1);
    goto ok;
    end;
  $a1:begin;
    {$ifdef debug}writeln('cpi');{$endif}
    doCP(+1);
    goto ok;
    end;
  $b1:begin;
    {$ifdef debug}writeln('cpir');{$endif}
    doCPR(+1);
    goto ok;
    end;
  $a9:begin;
    {$ifdef debug}writeln('cpd');{$endif}
    doCP(-1);
    goto ok;
    end;
  $b9:begin;
    {$ifdef debug}writeln('cpdr');{$endif}
    doCPR(-1);
    goto ok;
    end;
  $44:begin;
    {$ifdef debug}writeln('neg');{$endif}
    p:=regs[reg_a];
    o:=-p;
    setFlagsSZH(p,o,8);
    setFlagsVC(p,o,8);
    flg_n:=true;
    regs[reg_a]:=o;
    goto ok;
    end;
  $46:begin;
    {$ifdef debug}writeln('im 0');{$endif}
    goto ok;
    end;
  $56:begin;
    {$ifdef debug}writeln('im 1');{$endif}
    goto ok;
    end;
  $5e:begin;
    {$ifdef debug}writeln('im 2');{$endif}
    goto ok;
    end;
  end;
case i and $cf of
  $4b:begin;
    {$ifdef debug}writeln('ld reg16,(num16)');{$endif}
    o:=getHLIXIYptr(8);
    if checkRange(o) then goto vege;
    WriteReg16((i shr 4) and 3,ReadWordLSB(memoryData^[o]));
    goto ok;
    end;
  $43:begin;
    {$ifdef debug}writeln('ld (num16),reg16');{$endif}
    o:=getHLIXIYptr(8);
    if checkRange(o) then goto vege;
    WriteWordLSB(memoryData^[o],ReadReg16((i shr 4) and 3));
    goto ok;
    end;
  $4a:begin;
    {$ifdef debug}writeln('adc hl,reg16');{$endif}
    p:=ReadReg16(reg_hl);
    o:=p+ReadReg16((i shr 4) and 3);
    if flg_c then inc(p);
    setFlagsSZH(p,o,16);
    setFlagsVC(p,o,16);
    flg_n:=false;
    WriteReg16(reg_hl,o);
    goto ok;
    end;
  $42:begin;
    {$ifdef debug}writeln('sbc hl,reg16');{$endif}
    p:=ReadReg16(reg_hl);
    o:=p-ReadReg16((i shr 4) and 3);
    if flg_c then dec(p);
    setFlagsSZH(p,o,16);
    setFlagsVC(p,o,16);
    flg_n:=false;
    WriteReg16(reg_hl,o);
    goto ok;
    end;
  $a2:begin;
    {$ifdef debug}writeln('ini');{$endif}
    p:=ReadReg16(reg_hl);
    if checkRange(p) then goto vege;
    memoryData^[p]:=readPort(regs[reg_c],1);
    dec(regs[reg_b]);
    WriteReg16(reg_hl,p+1);
    flg_z:=(regs[reg_b]=0);
    flg_n:=true;
    goto ok;
    end;
  $aa:begin;
    {$ifdef debug}writeln('ind');{$endif}
    p:=ReadReg16(reg_hl);
    if checkRange(p) then goto vege;
    memoryData^[p]:=readPort(regs[reg_c],1);
    dec(regs[reg_b]);
    WriteReg16(reg_hl,p-1);
    flg_z:=(regs[reg_b]=0);
    flg_n:=true;
    goto ok;
    end;
  $b2:begin;
    {$ifdef debug}writeln('inir');{$endif}
    p:=ReadReg16(reg_hl);
    o:=regs[reg_b];
    repeat
      if checkRange(p) then goto vege;
      memoryData^[p]:=readPort(regs[reg_c],1);
      o:=(o-1) and $ff;
      p:=(p+1) and $ffff;
      until (o=0);
    regs[reg_b]:=o;
    WriteReg16(reg_hl,p);
    flg_z:=true;
    flg_n:=true;
    goto ok;
    end;
  $ba:begin;
    {$ifdef debug}writeln('indr');{$endif}
    p:=ReadReg16(reg_hl);
    o:=regs[reg_b];
    repeat
      if checkRange(p) then goto vege;
      memoryData^[p]:=readPort(regs[reg_c],1);
      o:=(o-1) and $ff;
      p:=(p-1) and $ffff;
      until (o=0);
    regs[reg_b]:=o;
    WriteReg16(reg_hl,p);
    flg_z:=true;
    flg_n:=true;
    goto ok;
    end;
  $a3:begin;
    {$ifdef debug}writeln('outi');{$endif}
    p:=ReadReg16(reg_hl);
    if checkRange(p) then goto vege;
    writePort(regs[reg_c],1,memoryData^[p]);
    dec(regs[reg_b]);
    WriteReg16(reg_hl,p+1);
    flg_z:=(regs[reg_b]=0);
    flg_n:=true;
    goto ok;
    end;
  $ab:begin;
    {$ifdef debug}writeln('outd');{$endif}
    p:=ReadReg16(reg_hl);
    if checkRange(p) then goto vege;
    writePort(regs[reg_c],1,memoryData^[p]);
    dec(regs[reg_b]);
    WriteReg16(reg_hl,p-1);
    flg_z:=(regs[reg_b]=0);
    flg_n:=true;
    goto ok;
    end;
  $b3:begin;
    {$ifdef debug}writeln('outir');{$endif}
    p:=ReadReg16(reg_hl);
    o:=regs[reg_b];
    repeat
      if checkRange(p) then goto vege;
      writePort(regs[reg_c],1,memoryData^[p]);
      o:=(o-1) and $ff;
      p:=(p+1) and $ffff;
      until (o=0);
    regs[reg_b]:=o;
    WriteReg16(reg_hl,p);
    flg_z:=true;
    flg_n:=true;
    goto ok;
    end;
  $bb:begin;
    {$ifdef debug}writeln('outdr');{$endif}
    p:=ReadReg16(reg_hl);
    o:=regs[reg_b];
    repeat
      if checkRange(p) then goto vege;
      writePort(regs[reg_c],1,memoryData^[p]);
      o:=(o-1) and $ff;
      p:=(p-1) and $ffff;
      until (o=0);
    regs[reg_b]:=o;
    WriteReg16(reg_hl,p);
    flg_z:=true;
    flg_n:=true;
    goto ok;
    end;
  end;
case i and $c7 of
  $40:begin;
    {$ifdef debug}writeln('in reg8,(c)');{$endif}
    p:=regs[(i shr 3) and 7];
    o:=readPort(regs[reg_c],1);
    regs[(i shr 3) and 7]:=o;
    setFlagsSZH(p,o,8);
    flg_n:=false;
    goto ok;
    end;
  $41:begin;
    {$ifdef debug}writeln('out (c),reg8');{$endif}
    writePort(regs[reg_c],1,regs[(i shr 3) and 7]);
    goto ok;
    end;
  end;
goto vege;



cb:
i:=getByte;
if (i and 7=6) then begin;
  p:=getHLIXIYptr(0);
  if checkRange(p) then goto vege;
  goto hxycb;
  end;
case i and $f8 of
  $00:begin;
    {$ifdef debug}writeln('rlc reg8');{$endif}
    doRLC(regs[i and 7]);
    goto ok;
    end;
  $10:begin;
    {$ifdef debug}writeln('rl reg8');{$endif}
    doRL(regs[i and 7]);
    goto ok;
    end;
  $08:begin;
    {$ifdef debug}writeln('rrc reg8');{$endif}
    doRRC(regs[i and 7]);
    goto ok;
    end;
  $18:begin;
    {$ifdef debug}writeln('rr reg8');{$endif}
    doRR(regs[i and 7]);
    goto ok;
    end;
  $20:begin;
    {$ifdef debug}writeln('sla reg8');{$endif}
    doSLA(regs[i and 7]);
    goto ok;
    end;
  $30:begin;
    {$ifdef debug}writeln('sll reg8');{$endif}
    doSLA(regs[i and 7]);
    goto ok;
    end;
  $28:begin;
    {$ifdef debug}writeln('sra reg8');{$endif}
    doSRA(regs[i and 7]);
    goto ok;
    end;
  $38:begin;
    {$ifdef debug}writeln('srl reg8');{$endif}
    doSRL(regs[i and 7]);
    goto ok;
    end;
  $6f:begin;
    {$ifdef debug}writeln('rld');{$endif}
    p:=getHLIXIYptr(0);
    if checkRange(p) then goto vege;
    o:=memoryData^[p];
    i:=regs[reg_a];
    inc(o,i shl 8);
    o:=rotateLeft(o,12,4);
    regs[reg_a]:=(regs[reg_a] and $f0) or (o shr 8);
    memoryData^[p]:=o and $ff;
    goto ok;
    end;
  $67:begin;
    {$ifdef debug}writeln('rrd');{$endif}
    p:=getHLIXIYptr(0);
    if checkRange(p) then goto vege;
    o:=memoryData^[p];
    i:=regs[reg_a];
    inc(o,i shl 8);
    o:=rotateRight(o,12,4);
    regs[reg_a]:=(regs[reg_a] and $f0) or (o shr 8);
    memoryData^[p]:=o and $ff;
    goto ok;
    end;
  end;
case i and $c0 of
  $40:begin;
    {$ifdef debug}writeln('bit num8,reg8');{$endif}
    flg_z:=(bits[(i shr 3) and 7] and regs[i and 7]<>0);
    goto ok;
    end;
  $c0:begin;
    {$ifdef debug}writeln('set num8,reg8');{$endif}
    regs[i and 7]:=regs[i and 7] or bits[(i shr 3) and 7];
    goto ok;
    end;
  $80:begin;
    {$ifdef debug}writeln('res num8,reg8');{$endif}
    regs[i and 7]:=regs[i and 7] and (not bits[(i shr 3) and 7]);
    goto ok;
    end;
  end;
goto vege;



hxycb:
case i of
  $06:begin;
    {$ifdef debug}writeln('rlc (mem)');{$endif}
    doRLC(memoryData^[p]);
    goto ok;
    end;
  $16:begin;
    {$ifdef debug}writeln('rl (mem)');{$endif}
    doRL(memoryData^[p]);
    goto ok;
    end;
  $0e:begin;
    {$ifdef debug}writeln('rrc (mem)');{$endif}
    doRRC(memoryData^[p]);
    goto ok;
    end;
  $1e:begin;
    {$ifdef debug}writeln('rr (mem)');{$endif}
    doRR(memoryData^[p]);
    goto ok;
    end;
  $26:begin;
    {$ifdef debug}writeln('sla (mem)');{$endif}
    doSLA(memoryData^[p]);
    goto ok;
    end;
  $36:begin;
    {$ifdef debug}writeln('sll (mem)');{$endif}
    doSLA(memoryData^[p]);
    goto ok;
    end;
  $2e:begin;
    {$ifdef debug}writeln('sra (mem)');{$endif}
    doSRA(memoryData^[p]);
    goto ok;
    end;
  $3e:begin;
    {$ifdef debug}writeln('srl (mem)');{$endif}
    doSRL(memoryData^[p]);
    goto ok;
    end;
  end;
case i and $c7 of
  $46:begin;
    {$ifdef debug}writeln('bit num8,(mem)');{$endif}
    flg_z:=(bits[(i shr 3) and 7] and memoryData^[p]<>0);
    goto ok;
    end;
  $c6:begin;
    {$ifdef debug}writeln('set num8,(mem)');{$endif}
    memoryData^[p]:=memoryData^[p] or bits[(i shr 3) and 7];
    goto ok;
    end;
  $86:begin;
    {$ifdef debug}writeln('res num8,(mem)');{$endif}
    memoryData^[p]:=memoryData^[p] and (not bits[(i shr 3) and 7]);
    goto ok;
    end;
  end;
goto vege;


ok:
result:=0;
vege:
emulateOneOpcode:=result;
End;
