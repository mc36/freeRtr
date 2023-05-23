Const transmitBufSize=512;
Type
  OneConnectionRecord=record
    time:LongInt;               {time of last action}
    retry:LongInt;              {number of retries left}
    adr:OneTCPaddressRecord;    {remote address}
    prt:LongInt;                {remote port}
    stat:LongInt;               {status: 1=tx, 2=rx}
    fileH:xFile;                {file handle}
    fileS:LongInt;              {size of file}
    fileP:LongInt;              {position in file}
    txBuf:array[1..transmitBufSize] of byte;    {transmit buffer}
    txSiz:LongInt;              {bytes in transmit buffer}
    sequ:LongInt;               {sequence number}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  pipeCmd:LongInt;
  pipeDat:LongInt;
  portCmd:LongInt;
  portDat:LongInt;
  addrCmd:OneTCPaddressRecord;
  addrDat:OneTCPaddressRecord;
  lastSent:LongInt;


Function ResizeMem(n:LongInt):Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
ResizeMem:=True;
i:=n*sizeof(OneConnectionRecord);
if (ExtendedMemoryResize(p,i)<i) then exit;
ConnectionNum:=n;
ConnectionDat:=p^;
ResizeMem:=False;
End;

Function FindOneAddress(adr:OneTCPaddressRecord;prt:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do begin;
  if (ConnectionDat^[i].prt<>prt) then continue;
  if TCPcompareAddress(adr,ConnectionDat^[i].adr) then goto f1;
  end;
i:=0;
f1:
FindOneAddress:=i;
End;

Function getWord(var a:String):String;
Var i:LongInt;
Begin;
i:=pos(' ',a);
if (i<1) then i:=666;
getWord:=copy(a,1,i-1);
a:=copy(a,i+1,255);
End;
