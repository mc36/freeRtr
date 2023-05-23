{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

{$include memory.inc}
{$include tftp.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;



Label f1;
Var
  con:OneConnectionRecord;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('tftp server v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');

ConnectionNum:=0;
lastSent:=0;
if pipeLineBegListen then immErr('failed to start listening!');

a:=ParamStr(1);
p:=BVal(a);
if (a='') then immErr('using: tftp.code <userid> [cmdPort] [dataPort]');

i:=BVal(ParamStr(2));
if (i=0) then i:=69;
if UDPlistenOnPort(pipeCmd,65536,addrCmd,i) then immErr('failed to listen on port!');
WriteLn('command listening on '+ipAddr2string(addrCmd)+' '+BStr(i)+' port...');
portCmd:=i;

i:=BVal(ParamStr(3));
if UDPlistenOnPort(pipeDat,65536,addrDat,i) then immErr('failed to listen on port!');
WriteLn('data listening on '+ipAddr2string(addrDat)+' '+BStr(i)+' port...');
portDat:=i;

WriteLn('setting uid to '+BStr(p)+'...');
BugOS_SetOwnerInfo(p);
xChDir('c:\');

BugOS_SignDaemoning;
f1:
relequish;
timer2start;
while ReceivePacket do;
for i:=ConnectionNum downto 1 do if doConn(ConnectionDat^[i]) then begin;
  con:=ConnectionDat^[i];
  xClose(con.fileH);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  end;

goto f1;
END.