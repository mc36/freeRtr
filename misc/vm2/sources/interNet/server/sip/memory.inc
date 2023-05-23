Const
  oneBufferMaxSize=1024;
  bufferSampleSize=11;
  bytesPerPack=200;
Type
  OneConnectionRecord=record
    stat:LongInt;               {status: 0-serving, 1-wait4try, 2-wait4exec}
                                {3-isHere, 4-wait4ack, 5-disconn, 6-wait4ok}
                                {7-wait4ring, 8-wait4ok, 9-wait4rec}
                                {10-wait4play, 11-wait4duplex}
    time:LongInt;               {time of last action}
    rtry:LongInt;               {number of retries}
    pipe:LongInt;               {upper pipeline number}
    strm:LongInt;               {stream pipeline number}
    codc:LongInt;               {codec to use: 1-a, 2-u}
    addr:OneTCPaddressRecord;   {sip address}
    port:LongInt;               {sip port}
    clld:LongInt;               {called; 1-yes, 2-no}
    rtpA:OneTCPaddressRecord;   {rtp address}
    rtpP:LongInt;               {rtp port}
    rtpT:LongInt;               {rtp type}
    Scid:String;                {sip call id}
    Ssrc:String;                {sip from}
    Strg:String;                {sip to}
    Svia:String;                {sip via}
    Sseq:String;                {sip sseq}
    rcvD:array[1..oneBufferMaxSize] of byte;    {receive buffer}
    sndD:array[1..oneBufferMaxSize] of byte;    {transmit buffer}
    rcvS:LongInt;               {size of rx buf}
    sndS:LongInt;               {size of tx buf}
    sndT:LongInt;               {time of last tx}
    sndP:LongInt;               {sent packets}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  serverAddr:OneTCPaddressRecord;
  serverPort:LongInt;
  localAddr:OneTCPaddressRecord;
  localPort:LongInt;
  localPipe:LongInt;
  myNumber:String;
  inCallPrg:String;
  inCallPar:String;
  lastRegged:LongInt;
  startingProc:LongInt;
  ticksPerPack:LongInt;


Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

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

Function xLevesz(a:String):String;
Begin;
a:=' '+a+' ';
kicserel('  ',' ',a);
xLevesz:=copy(a,2,length(a)-2);
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


Function padUpWithZeros(i:LongInt):String;
Var a:String;
Begin;
a:=BStr(i);
while (length(a)<2) do a:='0'+a;
padUpWithZeros:=a;
End;

Function getCurrentDateStr:String;
Var a,b,c:Word;
Begin;
xGetDate(a,b,c);
getCurrentDateStr:=padUpWithZeros(a)+'-'+padUpWithZeros(b)+'-'+padUpWithZeros(c);
End;

Function getCurrentTimeStr:String;
Var a,b,c:Word;
Begin;
xGetTime(a,b,c);
getCurrentTimeStr:=padUpWithZeros(a)+':'+padUpWithZeros(b)+':'+padUpWithZeros(c);
End;

Function getDateForHeader:String;
Const MonthNames:array[1..12] of String[3]=('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
Var
  a1,a2,a3:Word;
  a:String;
Begin;
xGetDate(a1,a2,a3);
a:=padUpWithZeros(a3)+' '+MonthNames[a2]+' '+padUpWithZeros(a1);
xGetTime(a1,a2,a3);
getDateForHeader:=a+' '+padUpWithZeros(a1)+':'+padUpWithZeros(a2)+':'+padUpWithZeros(a3)+' Z';
End;


Procedure dumpOneMessage(a:String;var buffer;siz:LongInt);
Var
  buf:array[1..1] of char absolute buffer;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
while (ab0<70) do begin;
  inc(ab0);
  ab[ab0]:=$2d;
  end;
WriteLn(a);
for i:=1 to siz do write(buf[i]);
fillchar(a,sizeof(a),$2d);
ab0:=70;
WriteLn(a);
End;


Function FindOneConnectionByCID(cid:String):LongInt;
Label f1;
Var i:LongInt;
Begin;
if (cid='') then begin; i:=0;goto f1; end;
for i:=1 to ConnectionNum do if (ConnectionDat^[i].Scid=cid) then goto f1;
i:=0;
f1:
FindOneConnectionByCID:=i;
End;

Function FindOneConnectionByStat(stat:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do if (ConnectionDat^[i].stat=stat) then goto f1;
i:=0;
f1:
FindOneConnectionByStat:=i;
End;
