Const
  bitMaskValues:array[0..24] of LongInt=($0,$1,$3,$7,$f,$1f,$3f,
    $7f,$ff,$1ff,$3ff,$7ff,$fff,$1fff,$3fff,$7fff,$ffff,$1ffff,
    $3ffff,$7ffff,$fffff,$1fffff,$3fffff,$7fffff,$ffffff);
Var
  intermediateSped:LongInt;
  intermediateRate:LongInt;  {8,16,32,64}
  intermediateMeth:LongInt;  {1,2,3,4,5,6}
  dataBits:LongInt;
  parityBit:LongInt;
  decodeBitD:LongInt;
  decodeBitS:LongInt;
  decodeBufD:array[1..4*1024] of byte;
  decodeBufS:LongInt;
  decodeRowD:array[1..10] of byte;
  decodeRowS:LongInt;
  decodeBytD:LongInt;
  decodeBytS:LongInt;
  decodePrty:LongInt;
  decodeFrmg:LongInt;
  encodeBufD:array[1..4*1024] of byte;
  encodeBufS:LongInt;
  encodeBytD:array[1..1024] of byte;
  encodeBytS:LongInt;
  encodeQueu:String;
  encodeRowD:array[1..10] of byte;
  encodeRowB:LongInt;
  encodeRowP:LongInt;
  encodeBitD:LongInt;
  encodeBitS:LongInt;
  encodeGetD:LongInt;
  encodeGetS:LongInt;




Function decodeGetBit:LongInt;
Begin;
if (decodeBitS<1) then begin; decodeGetBit:=-1;exit; end;
decodeGetBit:=decodeBitD and 1;
decodeBitD:=decodeBitD shr 1;
dec(decodeBitS);
End;


Procedure decodeAsyncBits(p,q:Longint);
Var i,o:LongInt;
Begin;
p:=p and bitMaskValues[q];
inc(decodeBitD,p shl decodeBitS);
inc(decodeBitS,q);
while (decodeBitS>0) and (decodeBitD and 1=1) do decodeGetBit;
if (parityBit>0) then i:=1 else i:=0;
if (decodeBitS<i+dataBits+2) then exit;
if (decodeGetBit<>0) then begin; inc(decodeFrmg);exit; end;
q:=0;p:=0;
for o:=0 to dataBits-1 do begin;
  i:=decodeGetBit;
  inc(q,i shl o);
  inc(p,i);
  end;
case parityBit of
  0:;
  1:if (p and 1=decodeGetBit) then begin; inc(decodePrty);exit; end;
  2:if (p and 1<>decodeGetBit) then begin; inc(decodePrty);exit; end;
  3:if (decodeGetBit<>0) then begin; inc(decodePrty);exit; end;
  4:if (decodeGetBit<>1) then begin; inc(decodePrty);exit; end;
  else begin; inc(decodePrty);exit; end;
  end;
if (decodeGetBit<>1) then begin; inc(decodeFrmg);exit; end;
inc(decodeBufS);
decodeBufD[decodeBufS]:=q;
End;


Procedure decodeOneRow;
Label f1;
Var i,o,p:LongInt;
Begin;
p:=0;
for i:=sizeof(decodeRowD) downto 1 do if (decodeRowD[i]=0) then p:=i;
if (p<1) then begin;
  decodeRowS:=0;
  exit;
  end;
if (p>1) then begin;
  f1:
  dec(decodeRowS,p-1);
  move(decodeRowD[p],decodeRowD,decodeRowS);
  exit;
  end;
for i:=2 to sizeof(decodeRowD) do if (decodeRowD[i] and 1=0) then begin;
  p:=2;
  goto f1;
  end;
case intermediateMeth of
  1:begin; {600}
    decodeAsyncBits(decodeRowD[3] shr 1,1);
    decodeAsyncBits(decodeRowD[4] shr 1,1);
    decodeAsyncBits(decodeRowD[5] shr 1,1);
    decodeAsyncBits(decodeRowD[8] shr 1,1);
    decodeAsyncBits(decodeRowD[9] shr 1,1);
    decodeAsyncBits(decodeRowD[10] shr 1,1);
    end;
  2:begin; {1200}
    decodeAsyncBits(decodeRowD[2] shr 1,1);
    decodeAsyncBits(decodeRowD[3] shr 1,1);
    decodeAsyncBits(decodeRowD[3] shr 4,1);
    decodeAsyncBits(decodeRowD[4] shr 1,1);
    decodeAsyncBits(decodeRowD[5] shr 1,1);
    decodeAsyncBits(decodeRowD[5] shr 4,1);
    decodeAsyncBits(decodeRowD[7] shr 1,1);
    decodeAsyncBits(decodeRowD[8] shr 1,1);
    decodeAsyncBits(decodeRowD[8] shr 4,1);
    decodeAsyncBits(decodeRowD[9] shr 1,1);
    decodeAsyncBits(decodeRowD[10] shr 1,1);
    decodeAsyncBits(decodeRowD[10] shr 4,1);
    end;
  3:begin; {2400}
    decodeAsyncBits(decodeRowD[2] shr 1,1);
    decodeAsyncBits(decodeRowD[2] shr 3,1);
    decodeAsyncBits(decodeRowD[2] shr 5,1);
    decodeAsyncBits(decodeRowD[3] shr 1,1);
    decodeAsyncBits(decodeRowD[3] shr 3,1);
    decodeAsyncBits(decodeRowD[3] shr 5,1);
    decodeAsyncBits(decodeRowD[4] shr 1,1);
    decodeAsyncBits(decodeRowD[4] shr 3,1);
    decodeAsyncBits(decodeRowD[4] shr 5,1);
    decodeAsyncBits(decodeRowD[5] shr 1,1);
    decodeAsyncBits(decodeRowD[5] shr 3,1);
    decodeAsyncBits(decodeRowD[5] shr 5,1);
    decodeAsyncBits(decodeRowD[7] shr 1,1);
    decodeAsyncBits(decodeRowD[7] shr 3,1);
    decodeAsyncBits(decodeRowD[7] shr 5,1);
    decodeAsyncBits(decodeRowD[8] shr 1,1);
    decodeAsyncBits(decodeRowD[8] shr 3,1);
    decodeAsyncBits(decodeRowD[8] shr 5,1);
    decodeAsyncBits(decodeRowD[9] shr 1,1);
    decodeAsyncBits(decodeRowD[9] shr 3,1);
    decodeAsyncBits(decodeRowD[9] shr 5,1);
    decodeAsyncBits(decodeRowD[10] shr 1,1);
    decodeAsyncBits(decodeRowD[10] shr 3,1);
    decodeAsyncBits(decodeRowD[10] shr 5,1);
    end;
  4:begin; {3600}
    decodeAsyncBits(decodeRowD[2] shr 1,6);
    decodeAsyncBits(decodeRowD[3] shr 1,4);
    decodeAsyncBits(decodeRowD[4] shr 1,2);
    decodeAsyncBits(decodeRowD[4] shr 5,2);
    decodeAsyncBits(decodeRowD[5] shr 3,4);
    decodeAsyncBits(decodeRowD[7] shr 1,6);
    decodeAsyncBits(decodeRowD[8] shr 1,4);
    decodeAsyncBits(decodeRowD[9] shr 1,2);
    decodeAsyncBits(decodeRowD[9] shr 5,2);
    decodeAsyncBits(decodeRowD[10] shr 3,4);
    end;
  5:begin; {4800}
    decodeAsyncBits(decodeRowD[2] shr 1,6);
    decodeAsyncBits(decodeRowD[3] shr 1,6);
    decodeAsyncBits(decodeRowD[4] shr 1,6);
    decodeAsyncBits(decodeRowD[5] shr 1,6);
    decodeAsyncBits(decodeRowD[7] shr 1,6);
    decodeAsyncBits(decodeRowD[8] shr 1,6);
    decodeAsyncBits(decodeRowD[9] shr 1,6);
    decodeAsyncBits(decodeRowD[10] shr 1,6);
    end;
  6:begin; {3000}
    decodeAsyncBits(decodeRowD[2] shr 1,6);
    decodeAsyncBits(decodeRowD[3] shr 1,4);
    decodeAsyncBits(decodeRowD[4] shr 1,2);
    decodeAsyncBits(decodeRowD[4] shr 5,2);
    decodeAsyncBits(decodeRowD[5] shr 3,1);
    decodeAsyncBits(decodeRowD[7] shr 1,6);
    decodeAsyncBits(decodeRowD[8] shr 1,4);
    decodeAsyncBits(decodeRowD[9] shr 1,2);
    decodeAsyncBits(decodeRowD[9] shr 5,2);
    decodeAsyncBits(decodeRowD[10] shr 3,1);
    end;
  end;
decodeRowS:=0;
End;


Procedure decodeOneInterByte(i:LongInt);
Var o:LongInt;
Begin;
case intermediateRate of
  8:begin; i:=i and $01;o:=1; end;
  16:begin; i:=i and $03;o:=2; end;
  32:begin; i:=i and $0f;o:=4; end;
  64:begin; i:=i and $ff;o:=8; end;
  else exit;
  end;
inc(decodeBytD,i shl decodeBytS);
inc(decodeBytS,o);
if (decodeBytS<8) then exit;
inc(decodeRowS);
decodeRowD[decodeRowS]:=decodeBytD and $ff;
dec(decodeBytS,8);
decodeBytD:=decodeBytD shr 8;
if (decodeRowS>=sizeof(decodeRowD)) then decodeOneRow;
End;


Procedure decodeInterBytes(var buffer;siz:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  i,o,p:LongInt;
Begin;
for i:=1 to siz do decodeOneInterByte(buf[i]);
End;



Procedure encodeRowByte(v:LongInt);
Var i:LongInt;
Begin;
case intermediateRate of
  8:begin;
    for i:=1 to 8 do begin;
      inc(encodeBufS);
      encodeBufD[encodeBufS]:=(v and 1) or $fe;
      v:=v shr 1;
      end;
    end;
  16:begin;
    for i:=1 to 4 do begin;
      inc(encodeBufS);
      encodeBufD[encodeBufS]:=(v and 3) or $fc;
      v:=v shr 2;
      end;
    end;
  32:begin;
    inc(encodeBufS);
    encodeBufD[encodeBufS]:=(v and $f) or $f0;
    inc(encodeBufS);
    encodeBufD[encodeBufS]:=(v shr 4) or $f0;
    end;
  64:begin;
    inc(encodeBufS);
    encodeBufD[encodeBufS]:=v;
    end;
  else exit;
  end;
End;

Procedure encodeAsyncBit(p:LongInt);
Begin;
p:=p xor 1;
inc(encodeBitD,p shl encodeBitS);
inc(encodeBitS);
if (encodeBitS<8) then exit;
inc(encodeBytS);
encodeBytD[encodeBytS]:=encodeBitD and $ff;
encodeBitD:=encodeBitD shr 8;
dec(encodeBitS,8);
End;

Procedure encodeAsyncByte(p:LongInt);
Var i,o,q:LongInt;
Begin;
q:=0;
encodeAsyncBit(1);
for o:=0 to dataBits-1 do begin;
  i:=p and 1;
  p:=p shr 1;
  encodeAsyncBit(i xor 1);
  inc(q,i);
  end;
q:=q and 1;
case parityBit of
  1:encodeAsyncBit(q);
  2:encodeAsyncBit(q xor 1);
  3:encodeAsyncBit(1);
  4:encodeAsyncBit(0);
  end;
encodeAsyncBit(0);
encodeAsyncBit(0);
End;

Procedure encodeAsyncNone(free:Boolean);
Var i:LongInt;
Begin;
while (encodeBitS<>0) do encodeAsyncBit(0);
if free then for i:=1 to 8 do encodeAsyncBit(0);
End;

Function encodeRowGetBit:LongInt;
Begin;
while (encodeGetS<1) do begin;
  while (encodeBytS<1) do encodeAsyncNone(true);
  inc(encodeGetD,encodeBytD[1] shl encodeGetS);
  inc(encodeGetS,8);
  dec(encodeBytS);
  move(encodeBytD[2],encodeBytD,encodeBytS);
  end;
encodeRowGetBit:=encodeGetD and 1;
encodeGetD:=encodeGetD shr 1;
dec(encodeGetS);
End;

Procedure encodeRowAddBit(v:LongInt);
Begin;
v:=v and 1;
inc(encodeRowP);
if (encodeRowP>6) then begin;
  inc(encodeRowB);
  encodeRowP:=1;
  end;
inc(encodeRowD[encodeRowB],v shl encodeRowP);
End;

Procedure encodeRowBitMore(v,n:LongInt);
Var i:LongInt;
Begin;
for i:=1 to n do encodeRowAddBit(v);
End;

Procedure encodeRowMoreBit(n:LongInt);
Var i:LongInt;
Begin;
for i:=1 to n do encodeRowAddBit(encodeRowGetBit);
End;

Procedure encodeOneRow;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
Begin;
if (encodeBytS<sizeof(encodeBytD) shr 1) then begin;
  a:=encodeQueu;
  for i:=1 to ab0 do encodeAsyncByte(ab[i]);
  encodeAsyncNone(false);
  encodeQueu:='';
  end;
fillchar(encodeRowD,sizeof(encodeRowD),0);
encodeRowB:=2;
encodeRowP:=0;
case intermediateMeth of
  1:begin; {600}
    for i:=1 to 3 do encodeRowBitMore(encodeRowGetBit,8);
    encodeRowBitMore(1,1);
    encodeRowBitMore(0,5);
    for i:=1 to 3 do encodeRowBitMore(encodeRowGetBit,8);
    end;
  2:begin; {1200}
    for i:=1 to 6 do encodeRowBitMore(encodeRowGetBit,4);
    encodeRowBitMore(0,1);
    encodeRowBitMore(1,1);
    encodeRowBitMore(0,4);
    for i:=1 to 6 do encodeRowBitMore(encodeRowGetBit,4);
    end;
  3:begin; {2400}
    for i:=1 to 12 do encodeRowBitMore(encodeRowGetBit,2);
    encodeRowBitMore(1,2);
    encodeRowBitMore(0,4);
    for i:=1 to 12 do encodeRowBitMore(encodeRowGetBit,2);
    end;
  4:begin; {3600}
    encodeRowMoreBit(10);
    encodeRowBitMore(0,2);
    encodeRowMoreBit(2);
    encodeRowBitMore(0,2);
    encodeRowMoreBit(2);
    encodeRowBitMore(0,2);
    encodeRowMoreBit(4);
    encodeRowBitMore(1,1);
    encodeRowBitMore(0,1);
    encodeRowBitMore(1,1);
    encodeRowBitMore(0,3);
    encodeRowMoreBit(10);
    encodeRowBitMore(0,2);
    encodeRowMoreBit(2);
    encodeRowBitMore(0,2);
    encodeRowMoreBit(2);
    encodeRowBitMore(0,2);
    encodeRowMoreBit(4);
    end;
  5:begin; {4800}
    encodeRowMoreBit(24);
    encodeRowBitMore(0,1);
    encodeRowBitMore(1,2);
    encodeRowBitMore(0,3);
    encodeRowMoreBit(24);
    end;
  6:begin; {3000}
    encodeRowMoreBit(10);
    encodeRowBitMore(0,2);
    encodeRowMoreBit(2);
    encodeRowBitMore(0,2);
    encodeRowMoreBit(2);
    encodeRowBitMore(0,2);
    encodeRowMoreBit(1);
    encodeRowBitMore(0,3);
    encodeRowBitMore(0,2);
    encodeRowBitMore(1,1);
    encodeRowBitMore(0,3);
    encodeRowMoreBit(10);
    encodeRowBitMore(0,2);
    encodeRowMoreBit(2);
    encodeRowBitMore(0,2);
    encodeRowMoreBit(2);
    encodeRowBitMore(0,2);
    encodeRowMoreBit(1);
    encodeRowBitMore(0,3);
    end;
  else exit;
  end;
for i:=2 to sizeof(encodeRowD) do inc(encodeRowD[i]);
for i:=1 to sizeof(encodeRowD) do encodeRowByte(encodeRowD[i]);
End;




Procedure initializeRate(s:LongInt);

Procedure test(d,i,m:LongInt);
Begin;
if (intermediateMeth<>0) then exit;
if (s mod d<>0) then exit;
intermediateRate:=(s div d)*i;
if not (intermediateRate in [8,16,32,64]) then exit;
intermediateMeth:=m;
End;

Begin;
decodeBitD:=0;
decodeBitS:=0;
decodeBufS:=0;
decodeRowS:=0;
decodeBytD:=0;
decodeBytS:=0;
decodePrty:=0;
decodeFrmg:=0;
encodeBufS:=0;
encodeBytS:=0;
encodeQueu:='';
encodeRowB:=0;
encodeRowP:=0;
encodeBitD:=0;
encodeBitS:=0;
encodeGetD:=0;
encodeGetS:=0;
dataBits:=8;
parityBit:=0;
intermediateSped:=s;
intermediateRate:=0;
intermediateMeth:=0;
if (s<1) or (s>64000) then exit;
test(4800,8,5);
test(3600,8,4);
test(3000,8,6);
test(2400,8,3);
test(1200,8,2);
test(600,8,1);
End;
