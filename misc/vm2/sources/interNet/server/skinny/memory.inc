Const
  bufferSampleSize=11;
  bytesPerPack=200;
Type
  OneSkinnyHeaderRecord=record
    siz:LongInt;                {size of message}
    res:LongInt;                {reserved=0}
    typ:LongInt;                {type of message}
    end;
Var
  serverPipe:LongInt;
  serverAddr:OneTCPaddressRecord;
  serverPort:LongInt;
  localAddr:OneTCPaddressRecord;
  localPort:LongInt;
  voicePipe:LongInt;
  voiceAddr:OneTCPaddressRecord;
  voicePort:LongInt;
  voiceMode:LongInt; {1-rx, 2-tx}
  macAddress:array[1..6] of byte;
  processNam:String;
  processPar:String;
  upperPipe:LongInt;
  upperStat:LongInt;    {0=cmd, 1=disconn, 2-dial, 3-wait, 4-rec, 5-play, 6-rec&play, 7-ring}
  lastHeader:OneSkinnyHeaderRecord;
  lastMsgTyp:LongInt;
  lastSent:LongInt;
  myPhoneNumber:String;
  callIdentify:LongInt;
  callPeerNum:String;
  callProcNum:LongInt;
  ticksPerPack:LongInt;
  sentPck:LongInt;
  sentOct:LongInt;
  rcvdPck:LongInt;
  rcvdOct:LongInt;
  sendBuf:array[1..1024] of byte;
  recvBuf:array[1..2048] of byte;
  sendSiz:LongInt;
  sendTim:LongInt;
  recvSiz:LongInt;


Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Procedure deleteFromBuffer(var buffer;var siz:LongInt;len:LongInt);
Var buf:array[1..1] of byte absolute buffer;
Begin;
if (siz<0) then siz:=0;
if (len>siz) then len:=siz;
if (len<1) then exit;
dec(siz,len);
move(buf[len+1],buf,siz);
End;

Function getStationName:String;
Var
  i:LongInt;
  a:String;
Begin;
a:='SEP';
for i:=1 to sizeof(macAddress) do a:=a+byte2hextype(macAddress[i]);
getStationName:=a;
End;

Function padUpZeros(i:LongInt):String;
Var a:String;
Begin;
a:=BStr(i);
while (length(a)<2) do a:='0'+a;
padUpZeros:=a;
End;

Function getStationAddr:String;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
ab0:=sizeof(localAddr);
move(localAddr,ab[1],ab0);
getStationAddr:=copy(a,ab0-3,666);
End;

Function getAsciiz(a:String):String;
Var i:LongInt;
Begin;
i:=pos(#0,a);
if (i<1) then i:=666;
getAsciiz:=copy(a,1,i-1);
End;
