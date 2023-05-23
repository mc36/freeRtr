{$undef debug}
{{$define debug}
{$heap 255k}
{$stack 63k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}
{$sysinc datetime.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$sysinc inet_dns.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

{$include memory.inc}
{$include struct.inc}
{$include zones.inc}
{$include dns.inc}


Label f1,f2;
Var
  con:OneConnectionRecord;
  pck:OnePacketRecord;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('dns server v1.0, done by Mc at '#%date' '#%time'.');
Randomize;
if TCPfindProcess then immErr('failed to find tcp process!');
if DNSstartResolver then immErr('failed to find dns process!');

ConnectionNum:=0;
lastSent:=0;
LastZoneTry:=-99999;
if pipeLineBegListen then immErr('failed to start listening!');

a:=ParamStr(1);
if (a='') then immErr('using: dns.code <config>');
ReadUpConfig(a);

i:=serverPort;
if TCPlistenOnPort(listenPipeTcp,65536,serverAddr,i) then immErr('failed to listen on tcp port!');
WriteLn('listening on '+ipAddr2string(serverAddr)+' '+BStr(i)+' tcp port...');

i:=serverPort;
if UDPlistenOnPort(listenPipeUdp,65536,a,i) then immErr('failed to listen on udp port!');
WriteLn('listening on '+ipAddr2string(a)+' '+BStr(i)+' udp port...');

BugOS_SignDaemoning;
f1:
relequish;
timer2start;
if (GetTimePast(LastZoneTry)>10) then BeginNextZoneFresh;
while (pipeLineGetIncoming(p)=0) do begin;
  if ResizeMem(ConnectionNum+1) then begin;
    a:='server is too busy!'#13#10;
    pipeLineSend(p,a[1],length(a));
    WriteLn('failed to allocate memory!');
    pipeLineClose(p);
    goto f1;
    end;
  fillchar(con,sizeof(con),0);
  con.stat:=1;
  con.pipe:=p;
  con.time:=CurrentTime;
  ConnectionDat^[ConnectionNum]:=con;
  end;
for i:=ConnectionNum downto 1 do if doConn(ConnectionDat^[i],i) then begin;
  con:=ConnectionDat^[i];
  pipeLineClose(con.pipe);
  xClose(con.fileH);
  xClose(con.fileHH);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  end;
f2:
i:=sizeof(pck.d);
if not UDPreceivePacket(listenPipeUdp,con.adr,con.prt,pck.d,i) then begin;
  pck.s:=i;
  i:=AnswerOnePacket(con,pck,false);
  case i of
    1:begin; UDPsendPacket(listenPipeUdp,con.adr,con.prt,pck.d,pck.s);goto f2; end;
    3,4,5:;
    0:goto f2;
    2:goto f2;
    else goto f2;
    end;
  if ResizeMem(ConnectionNum+1) then goto f2;
  DNSresolvePut(i-2,con.origQ);
  con.stat:=i+97;
  con.pipe:=0;
  con.time:=CurrentTime;
  ConnectionDat^[ConnectionNum]:=con;
  end;
i:=DNSresolveGet(a,b);
if (i=0) then goto f1;
doReply(a,b,i);
goto f1;
END.