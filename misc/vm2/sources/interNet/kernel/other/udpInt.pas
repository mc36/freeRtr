{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc hex.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}

Var
  lowerPipe:LongInt;
  upperPipe:LongInt;
  addrSize:LongInt;
  addrData:array[1..128] of byte;
  addrPtch:Boolean;
  peerAddr:OneTCPaddressRecord;
  peerPort:LongInt;


Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;


Procedure parseAddress(a:String);
Var i:LongInt;
Begin;
if (copy(a,1,1)='x') then begin;
  addrPtch:=false;
  a:=copy(a,2,666);
  end else addrPtch:=true;
addrSize:=0;
kicserel('-','',a);
kicserel(':','',a);
kicserel('.','',a);
while (a<>'') do begin;
  i:=BVal('$'+copy(a,1,2));
  a:=copy(a,3,666);
  inc(addrSize);
  addrData[addrSize]:=i;
  end;
End;


Procedure WaitForUpperLayer;
Var
  i,o,p:LongInt;
  a,b,c:String;
  bb:array[0..1] of byte absolute b;
  bb0:byte absolute b;
Begin;
Write('waiting for upper level...');
BugOS_SignDaemoning;
pipeLineBegListen;
while (pipeLineGetIncoming(upperPipe)<>0) do relequish;
pipeLineEndListen;
a:='udpint with '+ipAddr2string(peerAddr)+' '+BStr(peerPort);
BugOS_MyProcessInfo(i,o,p);
move(addrData,bb[1],addrSize);
bb0:=addrSize;
c:=b;
fillchar(b,sizeof(b),255);
bb0:=addrSize;
a:='12341234'#0#0#0#0#0#0#0#0+c+b+a+#0;
i:=addrSize;move(i,a[1],sizeof(i));
i:=1400;move(i,a[5],sizeof(i));
pipeLineSend(upperPipe,a[1],length(a));
WriteLn(' done!');
End;


Procedure releq2upper;
Var
  buf:array[1..2048] of byte;
  i,o:LongInt;
Begin;
o:=sizeof(buf);
if (pipeLineRecv(upperPipe,buf,o)<>0) then o:=0;
if (o<1) then begin;
  pipeLineStats(upperPipe,o,i,i);
  if (o<>0) then exit;
  immErr('upper closed connection!');
  end;
if addrPtch then begin;
  move(buf[addrSize+1],buf[addrSize+addrSize+1],o);
  move(addrData,buf[addrSize+1],addrSize);
  inc(o,addrSize);
  end;
UDPsendPacket(lowerPipe,peerAddr,peerPort,buf,o);
End;


Procedure releq2lower;
Var
  buf:array[1..2048] of byte;
  i,o:LongInt;
  a:String;
Begin;
o:=sizeof(buf);
if UDPreceivePacket(lowerPipe,a,i,buf,o) then begin;
  if (lowerPipe<>0) then exit;
  immErr('lower closed connection!');
  end;
if addrPtch then begin;
  move(buf[addrSize+1],buf,o);
  dec(o,addrSize);
  end;
pipeLineSend(upperPipe,buf,o);
End;




Label f1;
Var
  i,o:LongInt;
  a:String;
BEGIN;
WriteLn('udp interface v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');

if (paramCount<>4) then immErr('using: int.code <locPort> <addr> <remPort> <[x]addr>');

if string2ipAddr(paramStr(2),peerAddr) then immErr('bad address!');
peerPort:=BVal(paramStr(3));
parseAddress(paramStr(4));

i:=BVal(paramStr(1));
if UDPlistenOnPort(lowerPipe,65536,a,i) then immErr('failed to listen on port!');
if (peerPort=0) then peerPort:=i;
WriteLn('listening on '+ipAddr2string(a)+' '+BStr(i)+'...');
WriteLn('will send to '+ipAddr2string(peerAddr)+' '+BStr(peerPort)+'...');
a:='';
for i:=1 to addrSize do a:=a+'-'+byte2hextype(addrData[i]);
WriteLn('my address: '+copy(a,2,666));
if addrPtch then a:='yes' else a:='no';
WriteLn('remote address truncation: '+a);

WaitForUpperLayer;

f1:
relequish;
releq2upper;
releq2lower;
goto f1;
END.