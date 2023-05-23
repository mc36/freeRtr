{$define debug}
{$undef debug}
{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Const proggyName='mpls v1.0';

{$include memory.inc}
{$include config.inc}
{$include mpls1.inc}
{$include mpls2.inc}
{$include mpls3.inc}
{$include mpls4.inc}


Procedure openDevice;
Var
  a:String;
  i,o:LongInt;
  buf:array[1..1024] of byte;
Begin;
a:=lowerName;
WriteLn('process: '+a);
o:=BugOS_findProcNam(a);
if (o=0) then immErr('process not found!');
WriteLn('process#: '+BStr(o));
i:=PipeLineCreate(lowerPipe,o,65536,true);
if (i<>0) then immErr('unabled to create pipeline!');
WriteLn('pipeline#: '+BStr(lowerPipe));
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(lowerPipe,buf,i)<>0) then i:=0;
if (i<1) then immErr('initial packet not received!');
move(buf[1],i,sizeof(i));
if (i<>sizeof(lowerAddr)) then immErr('ethernet required!');
move(buf[5],i,sizeof(i));
WriteLn('packet size: '+BStr(i));
move(buf[9],i,sizeof(i));
move(buf[13],i,sizeof(i));
o:=17;
move(buf[o],lowerAddr,sizeof(lowerAddr));
inc(o,sizeof(lowerAddr));
move(buf[o],lowerBrod,sizeof(lowerBrod));
inc(o,sizeof(lowerBrod));
WriteLn('station address: '+convert2macaddr(lowerAddr));
WriteLn('broadcast address: '+convert2macaddr(lowerBrod));
a:='';
while (buf[o]<>0) do begin;
  a:=a+chr(buf[o]);
  inc(o);
  end;
writeln('device name: "'+a+'"');
lowerDesc:=a;
End;


Label f1;
Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn(proggyName+', done by Mc at '#%date' '#%time'.');
if pipeLineBegListen then immErr('failed to start listening!');

a:=ParamStr(1);
if (a='') then immErr('using: mpls.code <config>');
ReadUpConfig(a);
openDevice;
BugOS_SignDaemoning;

Write('waiting for upper process...');
while (pipeLineGetIncoming(upperPipe)<>0) do relequish;
a:='';
for i:=1 to sizeof(lowerAddr) do a:=a+chr(lowerAddr[i]);
for i:=1 to sizeof(lowerAddr) do a:=a+#255;
a:='12341234'#0#0#0#0#0#0#0#0+a+'mpls on '+lowerDesc+#0;
i:=sizeof(lowerAddr);move(i,a[1],sizeof(i));
i:=1500;move(i,a[5],sizeof(i));
pipeLineSend(upperPipe,a[1],length(a));
WriteLn('');
Write('waiting for tcp process...');
while TCPfindProcess do begin;
  relequish;
  timer2start;
  relequish2lower;
  relequish2upper;
  end;
for i:=1 to 128 do begin;
  relequish;
  timer2start;
  relequish2lower;
  relequish2upper;
  end;
for i:=1 to ConnectionNum do clearOneConn(ConnectionDat[i]);
WriteLn('');

if (localPort=0) then localPort:=646;
if TCPfindProcess then immErr('failed to find tcp process!');
i:=localPort;
if TCPlistenOnPort(localPipe,65536,a,i) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(a)+' '+BStr(i)+' tcp port...');
i:=localPort;
if UDPlistenOnPort(localPipe,65536,a,i) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(a)+' '+BStr(i)+' udp port...');
localAddr:=convert2ip4addr(a);
BugOS_SignDaemoning;
currTcpPipe:=0;
lastBcastMsg:=0;

f1:
relequish;
timer2start;
if (currTcpPipe=0) then begin;
  while (pipeLineGetIncoming(i)=0) do begin;
    gotNewIncomingConnection(i);
    if (currTcpPipe<>0) then goto f1;
    end;
  end else if not TCPlookConnected(currTcpPipe,a,i,o) then begin;
  WriteLn('connection accepted from '+ipAddr2string(a));
  i:=convert2ip4addr(a);
  o:=findOnePeerAddr(i);
  if (o<1) then begin;
    pipeLineClose(currTcpPipe);
    currTcpPipe:=0;
    goto f1;
    end;
  clearOneConn(ConnectionDat[o]);
  ConnectionDat[o].pipe:=currTcpPipe;
  ConnectionDat[o].stat:=2;
  currTcpPipe:=0;
  end;
for i:=1 to ConnectionNum do relequish2conn(ConnectionDat[i]);
for i:=1 to SessionNum do relequish2sess(SessionDat[i]);
relequish2lower;
relequish2upper;
relequish2udp;
relequish2bcast;
goto f1;
END.