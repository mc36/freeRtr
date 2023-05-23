Const
  maxBufferSize=8*1024;
  maxChannelNum=32;
Type
  OneConnectionRecord=record
    rxB:array[1..maxBufferSize] of byte;        {rx buffer}
    txB:array[1..maxBufferSize] of byte;        {tx buffer}
    rxS:LongInt;                {size of rx buffer}
    txS:LongInt;                {size of tx buffer}
    pos:LongInt;                {position in buffer}
    min:LongInt;                {channel first number}
    max:LongInt;                {channel last number}
    lst:LongInt;                {last character saw}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  bytes2buffer:array[1..maxChannelNum] of LongInt;
  GroupNumber:LongInt;
  upperPipe:LongInt;
  serverPipe:LongInt;
  serverPort:LongInt;
  serverAddr:OneTCPaddressRecord;
  remotePort:LongInt;
  remoteAddr:OneTCPaddressRecord;
  packetSize:LongInt;
  bytePerTick:LongInt;
  lastGotTime:LongInt;
  lastGotPack:LongInt;
  lastSentTime:LongInt;
  lastPackSeq:LongInt;
  lastPackTim:LongInt;
  lastPackSiz:LongInt;

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

Function getWord(var a:String):String;
Var i:LongInt;
Begin;
i:=pos(' ',a);
if (i<1) then i:=666;
getWord:=copy(a,1,i-1);
a:=copy(a,i+1,255);
End;
