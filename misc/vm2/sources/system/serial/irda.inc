Type
  OneConnectionRecord=record
    pipe:LongInt;               {peer pipeline}
    stat:LongInt;               {1=cmd, 2=listen, 3=wait, 100=conn}
    adrL:LongInt;               {local lsap}
    adrR:LongInt;               {remote lsap}
    tty:Boolean;                {tinyTP mode}
    wndL:LongInt;               {window local}
    wndR:LongInt;               {remote window}
    end;
Const
  maximumBufferSize=1024+64;
  connectionSpeedValues:array[0..5] of LongInt=(2400,9600,19200,38400,57600,115200);
  upperQueueMax=64;
  ConnectionMax=256;
Var
  packetInputBuf:array[1..maximumBufferSize] of byte;
  packetInputSiz:LongInt;
  packetInputEsc:Boolean;
  packetOutputBuf:array[1..1024] of byte;
  packetOutputSiz:LongInt;
  queueBufferDat:array[1..512] of byte;
  queueBufferSiz:LongInt;
  upperQueueBuf:array[1..upperQueueMax] of String;
  upperQueueSiz:LongInt;
  ConnectionDat:array[1..ConnectionMax] of OneConnectionRecord;
  ConnectionNum:LongInt;
  lastSent:LongInt;
  fastRounds:LongInt;
  localName:String;
  remoteName:String;
  remoteCapa:String;
  localID:LongInt;
  remoteID:LongInt;
  remoteAddr:LongInt;
  localSpeed:LongInt;
  remoteSpeed:LongInt;
  dataSize:LongInt;
  windowSize:LongInt;
  preambSize:LongInt;
  needStop:LongInt;
  remoteHas:LongInt;
  retryCount:LongInt;
  packetTx:LongInt;
  packetRx:LongInt;




Function findOneConnection(loc,rem:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do begin;
  if (ConnectionDat[i].adrL<>loc) then continue;
  if (ConnectionDat[i].adrR=rem) then goto f1;
  end;
i:=0;
f1:
findOneConnection:=i;
End;
