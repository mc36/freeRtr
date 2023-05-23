Var
  dataBits:LongInt;
  parityBit:LongInt;
  encodeBitD:LongInt;
  encodeBitS:LongInt;
  encodeBufD:array[1..4*1024] of byte;
  encodeBufS:LongInt;
  decodeBitD:LongInt;
  decodeBitS:LongInt;
  decodeBufD:array[1..4*1024] of byte;
  decodeBufS:LongInt;
  decodePrty:LongInt;
  decodeFrmg:LongInt;


Procedure encodeOneBit(p:LongInt);
Begin;
p:=p xor 1;
inc(encodeBitD,p shl encodeBitS);
inc(encodeBitS);
if (encodeBitS<8) then exit;
inc(encodeBufS);
encodeBufD[encodeBufS]:=encodeBitD and $ff;
encodeBitD:=encodeBitD shr 8;
dec(encodeBitS,8);
End;

Procedure encodeOneByte(p:LongInt);
Var i,o,q:LongInt;
Begin;
q:=0;
encodeOneBit(1);
for o:=0 to dataBits-1 do begin;
  i:=p and 1;
  p:=p shr 1;
  encodeOneBit(i xor 1);
  inc(q,i);
  end;
q:=q and 1;
case parityBit of
  1:encodeOneBit(q);
  2:encodeOneBit(q xor 1);
  3:encodeOneBit(1);
  4:encodeOneBit(0);
  end;
encodeOneBit(0);
encodeOneBit(0);
End;

Procedure encodeBytes(var buffer;siz:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  i,o,p:LongInt;
Begin;
for i:=1 to siz do encodeOneByte(buf[i]);
End;

Procedure encodeFlush;
Var i:LongInt;
Begin;
while (encodeBitS<>0) do encodeOneBit(0);
for i:=1 to 8 do encodeOneBit(0);
End;



Function decodeGetBit:LongInt;
Begin;
if (decodeBitS<1) then begin; decodeGetBit:=-1;exit; end;
decodeGetBit:=decodeBitD and 1;
decodeBitD:=decodeBitD shr 1;
dec(decodeBitS);
End;

Procedure decodeOneByte(p:LongInt);
Var i,o,q:LongInt;
Begin;
inc(decodeBitD,p shl decodeBitS);
inc(decodeBitS,8);
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

Procedure decodeBytes(var buffer;siz:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  i,o,p:LongInt;
Begin;
for i:=1 to siz do decodeOneByte(buf[i]);
End;
