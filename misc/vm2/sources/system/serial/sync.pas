{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc memory.inc}
{$include fcs16.inc}

Var
  uppPipe,devPipe:LongInt;
  addrSiz,packSiz:LongInt;
  myName:String;


Procedure immErr(a:String);
Begin;
Writeln(a);
Halt(1);
End;

Procedure WaitForUpperLayer;
Var
  i,o,p:LongInt;
  a:String;
Begin;
Write('waiting for upper level...');
BugOS_SignDaemoning;
pipeLineBegListen;
while (pipeLineGetIncoming(uppPipe)<>0) do relequish;
pipeLineEndListen;
BugOS_MyProcessInfo(i,o,p);
i:=i xor p xor o;
p:=((i shr 16) xor i) and $ffff;
a:='12341234'#0#0#0#0#0#0#0#0+chr(p)+#255+myName+#0;
i:=1;move(i,a[1],sizeof(i));
i:=1500;move(i,a[5],sizeof(i));
pipeLineSend(uppPipe,a[1],length(a));
WriteLn(' done!');
End;



Var
  decodeBufD:array[1..6*1024] of byte;
  decodeBufS:LongInt;
  decodeBitD:LongInt;
  decodeBitS:LongInt;
  decodeOutD:LongInt;
  decodeOutS:LongInt;
  encodeBufD:array[1..6*1024] of byte;
  encodeBufS:LongInt;
  encodeOutD:LongInt;
  encodeOutS:LongInt;
  encodeOutO:LongInt;


Procedure doDecodeBits;
Label f1;
Begin;
if (decodeBitD and $ff=$7e) then begin; {valid flag}
  decodeBitD:=decodeBitD shr 8;
  dec(decodeBitS,8);
  decodeOutD:=0;
  decodeOutS:=0;
  if (decodeBufS<1) then exit;
  if (decodeBufS<4) then begin;
    WriteLn('got a too short packet!');
    decodeBufS:=0;
    exit;
    end;
  if (CalcFCS16(decodeBufD,decodeBufS-2)<>ReadWordLSB(decodeBufD[decodeBufS-1])) then begin;
    WriteLn('got a packet with wrong fcs!');
    decodeBufS:=0;
    exit;
    end;
  pipeLineSend(uppPipe,decodeBufD,decodeBufS-2);
  decodeBufS:=0;
  exit;
  end;
if (decodeBitD and $3f=$3f) then begin; {invalid flag}
  decodeBitD:=decodeBitD shr 6;
  dec(decodeBitS,6);
  decodeBufS:=0;
  decodeOutD:=0;
  decodeOutS:=0;
  exit;
  end;
if (decodeBitD and $3f=$1f) then begin; {stuffed bit}
  decodeBitD:=decodeBitD shr 6;
  dec(decodeBitS,6);
  inc(decodeOutD,$1f shl decodeOutS);
  inc(decodeOutS,5);
  goto f1;
  end;
inc(decodeOutD,(decodeBitD and 1) shl decodeOutS);
inc(decodeOutS,1);
decodeBitD:=decodeBitD shr 1;
dec(decodeBitS,1);
f1:
while (decodeOutS>7) do begin; {bytes}
  if (decodeBufS>=sizeof(decodeBufD)) then decodeBufS:=0;
  inc(decodeBufS);
  decodeBufD[decodeBufS]:=decodeOutD and $ff;
  decodeOutD:=decodeOutD shr 8;
  dec(decodeOutS,8);
  end;
End;

Procedure decodeBytes(var buffer;siz:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  i,o,p:LongInt;
Begin;
for i:=1 to siz do begin;
  inc(decodeBitD,buf[i] shl decodeBitS);
  inc(decodeBitS,8);
  while (decodeBitS>8) do doDecodeBits;
  end;
End;



Procedure encodeBytes(var buffer;siz:LongInt);
Const bitVals:array[0..8] of longint=($01,$02,$04,$08,$10,$20,$40,$80,$100);
Var buf:array[1..1] of byte absolute buffer;

Procedure addLotBits(b,n:LongInt);
Begin;
b:=(bitVals[n]-1) and b;
inc(encodeOutD,b shl encodeOutS);
inc(encodeOutS,n);
while (encodeOutS>7) do begin;
  inc(encodeBufS);
  encodeBufD[encodeBufS]:=encodeOutD and $ff;
  encodeOutD:=encodeOutD shr 8;
  dec(encodeOutS,8);
  end;
End;

Procedure addOneBit(n:LongInt);
Begin;
n:=n and 1;
addLotBits(n,1);
if (n=0) then encodeOutO:=0 else inc(encodeOutO);
if (encodeOutO<5) then exit;
addLotBits(0,1);
encodeOutO:=0;
End;

Var i,o,p:LongInt;
Begin;
WriteWordLSB(buf[siz+1],CalcFCS16(buf,siz));
inc(siz,2);
addLotBits($7e,8);
if (encodeBufS<>0) then addLotBits($7e,8);
fillchar(encodeBufD,addrSiz,$ff);
encodeOutO:=0;
for p:=1 to siz do begin;
  i:=buf[p];
  for o:=1 to 8 do begin;
    addOneBit(i);
    i:=i shr 1;
    end;
  end;
addLotBits($7e,8);
if (encodeBufS<>0) then addLotBits($7e,8);
End;




Procedure doUpper;
Var
  i,o,p:LongInt;
  buf:array[1..4096] of byte;
Begin;
p:=sizeof(buf);
if (pipeLineRecv(uppPipe,buf,p)<>0) then p:=0;
if (p<1) then begin;
  pipeLineStats(uppPipe,o,i,i);
  if (o=0) then immErr('upper level closed connection!');
  exit;
  end;
encodeBufS:=addrSiz;
encodeBytes(buf,p);
pipeLineSend(devPipe,encodeBufD,encodeBufS);
End;



Procedure doLower;
Var
  i,o,p:LongInt;
  buf:array[1..4096] of byte;
Begin;
o:=sizeof(buf);
if (pipelineRecv(devPipe,buf,o)<>0) then o:=0;
if (o<1) then begin;
  pipeLineStats(devPipe,o,i,i);
  if (o=0) then immErr('lower level closed connection!');
  exit;
  end;
decodeBytes(buf[addrSiz+1],o-addrSiz);
End;




Label f1;
Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('sync v1.0, done by Mc at '#%date' '#%time'.');
CreateFCS16Table;
a:=paramStr(1);
devPipe:=BVal(a);
if (devPipe=0) then devPipe:=BugOS_findProcNam(a);
if (devPipe<1) then immErr('using: sync.code <process>');

WriteLn('opening link...');
if (pipeLineCreate(devPipe,devPipe,65536,true)<>0) then immErr('error creating connection!');
for i:=1 to 16 do relequish;
i:=255;
if (pipeLineRecv(devPipe,a[1],i)<>0) then immErr('failed to receive initial data!');
a[0]:=chr(i);
move(a[1],addrSiz,sizeof(addrSiz));
move(a[5],packSiz,sizeof(packSiz));
a:=copy(a,17+addrSiz+addrSiz,255);
myName:='sync on '+copy(a,1,pos(#0,a)-1);
WriteLn('my name: '+myName);
WriteLn('address size: '+BStr(addrSiz));
WriteLn('packet size: '+BStr(packSiz));
WaitForUpperLayer;

decodeBitD:=0;
decodeBitS:=0;
decodeBufS:=0;
decodeOutD:=0;
decodeOutS:=0;
encodeOutD:=0;
encodeOutS:=0;
encodeOutO:=0;
encodeBufS:=0;

f1:
relequish;
doLower;
doUpper;
goto f1;
END.