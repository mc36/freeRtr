{$undef debug}
{{$define debug}
{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

{$include memory.inc}
{$include \sources\internet\server\dns\struct.inc}
{$include dns.inc}


Label f1;
Var
  con:OneConnectionRecord;
  pck:OnePacketRecord;
  ip:OneTCPaddressRecord;
  a:String;
  i,o,p:LongInt;
BEGIN;
WriteLn('dns v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');

ReadUpNameServerList;
if (NameServerList='') then immErr('using: dns.code <cache> <dns1> [dns2] ... [dnsN]');
if ResizeMem(ConnectionAct) then immErr('error allocating memory for cache!');
inc(ConnectionAct);
fillchar(con,sizeof(con),0);
for i:=1 to ConnectionNum do ConnectionDat^[i]:=con;
ConnectionNxt:=0;

serverPort:=0;
if UDPlistenOnPort(listenPipeUdp,65536,serverAddr,serverPort) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(serverAddr)+' '+BStr(serverPort)+' port...');
useIPv6addr:=not isAddressIPv4mask(serverAddr);

if (pipeLineBegListen<>0) then begin;
  WriteLn('failed to start listening!');
  Halt(2);
  end;

BugOS_SignDaemoning;
lastCleanup:=-999999;
f1:
relequish;
timer2start;
while (pipeLineGetIncoming(p)=0) do begin;
  if ResizeMem(ConnectionNum+1) then begin;
    WriteLn('failed to allocate memory!');
    pipeLineClose(p);
    goto f1;
    end;
  fillchar(con,sizeof(con),0);
  con.stat:=1;
  con.pipe:=p;
  ConnectionDat^[ConnectionNum]:=con;
  end;
for i:=ConnectionNum downto ConnectionAct do if doConn(ConnectionDat^[i],i) then begin;
  con:=ConnectionDat^[i];
  pipeLineClose(con.pipe);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  end;
i:=sizeof(pck.d);
if not UDPreceivePacket(listenPipeUdp,ip,o,pck.d,i) then begin;
  pck.s:=i;
  doReply(pck,ip,o);
  end;

if (getTimePast(lastCleanup)>60*60) then begin;
  lastCleanup:=currentTime;
  cleanUpCache;
  end;
goto f1;
END.