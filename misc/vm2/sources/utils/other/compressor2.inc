Var
  windowBits:LongInt;
  windowMask:LongInt;
  matchMask:LongInt;
  matchBits:LongInt;
  storeSize:LongInt;
  storeBits:LongInt;
  minMatchLen:LongInt;
  windowBuf:array[0..32*1024-1] of LongInt;
  windowPos:LongInt;
  streamBuf:LongInt;
  streamSiz:LongInt;



Function GetBits(b:LongInt):LongInt;
Begin;
while (streamSiz<24) do begin;
  streamBuf:=(getByte shl streamSiz) or streamBuf;
  inc(streamSiz,8);
  end;
GetBits:=((1 shl b)-1) and streamBuf;
streamBuf:=streamBuf shr b;
dec(streamSiz,b);
End;

Procedure PutBits(n,b:LongInt);
Begin;
streamBuf:=(n shl streamSiz) or streamBuf;
inc(streamSiz,b);
while (streamSiz>=8) do begin;
  putByte(streamBuf);
  streamBuf:=streamBuf shr 8;
  dec(streamSiz,8);
  end;
End;



Procedure initialize_bit;
Begin;
streamBuf:=0;
streamSiz:=0;
End;

Procedure initialize_tab;
Var i:LongInt;
Begin;
windowMask:=(1 shl windowBits)-1;
matchMask:=(1 shl matchBits)-1;
storeSize:=1 shl storeBits;
minMatchLen:=(windowBits+matchBits+7) shr 3;
windowPos:=0;
for i:=0 to windowMask do windowBuf[i]:=-2;
End;



Procedure method(i:LongInt);
Const
  modesMax=8;
  modesDat:array[1..modesMax] of record w:Byte;m:Byte;s:Byte; end=(
    (w:8;m:5;s:2),(w:9;m:5;s:2),(w:10;m:6;s:2),(w:11;m:6;s:2),
    (w:12;m:6;s:2),(w:13;m:7;s:2),(w:14;m:7;s:2),(w:15;m:7;s:2)
  );
Begin;
if (i<1) then i:=1;
if (i>modesMax) then i:=modesMax;
windowBits:=modesDat[i].w;
matchBits:=modesDat[i].m;
storeBits:=modesDat[i].s;
End;



Procedure compress;
Label f1,f2;
Var
  inputBuf:array[0..1023] of integer;
  inputPos:LongInt;
  clearBuf:array[1..1023] of byte;
  clearSiz:LongInt;
  bestLen,bestPos:LongInt;
  curLen:LongInt;
  i,o,p:LongInt;

procedure store;
var i:LongInt;
begin;
if (clearSiz<1) then exit;
PutBits(0,1);
PutBits(clearSiz-1,storeBits);
for i:=1 to clearSiz do PutBits(clearBuf[i],8);
clearSiz:=0;
end;

Begin;
initialize_bit;
initialize_tab;
PutBits(windowBits,4);
PutBits(matchBits,4);
PutBits(storeBits,4);
for i:=0 to matchMask do inputBuf[i]:=getByte;
inputPos:=0;
clearSiz:=0;
f1:
if (inputBuf[inputPos]<0) then begin;
  store;
  PutBits(1,1);
  PutBits(0,windowBits);
  PutBits(0,matchBits);
  while (streamSiz<>0) do PutBits(0,1);
  exit;
  end;
bestLen:=0;
bestPos:=0;
for p:=0 to windowMask do begin;
  o:=windowPos+p;
  for curLen:=0 to matchMask do begin;
    o:=(o+1) and windowMask;
    if (o=windowPos) then goto f2;
    if (windowBuf[o]<>inputBuf[(inputPos+curLen) and matchMask]) then goto f2;
    end;
  curLen:=matchMask;
  f2:
  if (curLen<bestLen) then continue;
  bestLen:=curLen;
  bestPos:=p;
  end;
if (bestLen<minMatchLen) then begin;
  i:=inputBuf[inputPos];
  inc(clearSiz);
  clearBuf[clearSiz]:=i;
  windowBuf[windowPos]:=i;
  inputBuf[inputPos]:=getByte;
  inputPos:=(inputPos+1) and matchMask;
  windowPos:=(windowPos+1) and windowMask;
  if (clearSiz>=storeSize) then store;
  goto f1;
  end;
store;
bestPos:=(windowMask-bestPos) and windowMask;
PutBits(1,1);
PutBits(bestPos,windowBits);
PutBits(bestLen,matchBits);
for i:=1 to bestLen do begin;
  windowBuf[windowPos]:=inputBuf[inputPos];
  inputBuf[inputPos]:=getByte;
  inputPos:=(inputPos+1) and matchMask;
  windowPos:=(windowPos+1) and windowMask;
  end;
goto f1;
End;



Function decompress:String;
Label f1;
Var
  i,o,p,q:LongInt;
Begin;
decompress:='error';
initialize_bit;
windowBits:=GetBits(4);
matchBits:=GetBits(4);
storeBits:=GetBits(4);
initialize_tab;
f1:
if (GetBits(1)=0) then begin;
  p:=GetBits(storeBits)+1;
  for o:=1 to p do begin;
    i:=GetBits(8);
    putByte(i);
    windowBuf[windowPos]:=i;
    windowPos:=(windowPos+1) and windowMask;
    end;
  goto f1;
  end;
p:=GetBits(windowBits);
p:=windowPos-p;
q:=GetBits(matchBits);
if (q=0) then begin; decompress:='';exit; end;
for o:=0 to q-1 do begin;
  i:=windowBuf[(p+o) and windowMask];
  putByte(i);
  windowBuf[windowPos]:=i;
  windowPos:=(windowPos+1) and windowMask;
  end;
goto f1;
End;
