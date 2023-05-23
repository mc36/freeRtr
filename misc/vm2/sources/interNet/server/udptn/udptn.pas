{$heap 15k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
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
{$include config.inc}
{$include udptn.inc}


Label f1;
Var
  buf:array[1..1024*4] of byte;
  con:OneConnectionRecord;
  adr:OneTCPaddressRecord;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('udptn server v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');

ConnectionNum:=0;
lastSent:=0;

a:=ParamStr(1);
if (a='') then immErr('using: udptn.code <config>');
ReadUpConfig(a);

i:=serverPort;
if (i=0) then i:=57;
if UDPlistenOnPort(serverPipe,65536,serverAddr,i) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(serverAddr)+' '+BStr(i)+' port...');
serverPort:=i;

BugOS_SignDaemoning;
f1:
relequish;
timer2start;
o:=sizeof(buf);
while not UDPreceivePacket(serverPipe,adr,p,buf,o) do begin;
  i:=findOneConnection(adr,p);
  if (i>0) then doPack(ConnectionDat^[i],buf,o) else doNew(buf,o,p,adr);
  o:=sizeof(buf);
  end;

for i:=ConnectionNum downto 1 do if doConn(ConnectionDat^[i],i) then begin;
  con:=ConnectionDat^[i];
  pipeLineClose(con.term);
  pipeLineClose(con.emu1);
  pipeLineClose(con.emu2);
  BugOS_KillProcess(con.proc);
  xExecBgnd('c:\system\process\killRecursive.code',BStr(con.proc),o);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  end;

goto f1;
END.