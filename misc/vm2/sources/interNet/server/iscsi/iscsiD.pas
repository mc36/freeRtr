{$undef debug}
{$heap 383k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}

Var
  DriveSize:LongInt;
  DriveBegin:LongInt;
  DriveReadOnly:Boolean;
Type OneSectorRecord=array[1..512] of byte;

{$include \sources\filesystem\disk2.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}

{$include \sources\internet\kernel\utils\timer2.inc}
{$include iscsi.inc}
{$include iscsiD.inc}


Label f1,f2;
Var
  a:String;
  i,o,p:LongInt;
BEGIN;
WriteLn('iscsi server v1.0, done by Mc at '#%date' '#%time'.');
if (ParamCount<2) then immErr('using: iscsid.code <process> <drive> [port]');
a:=ParamStr(2);
i:=BVal(a);
a:=ParamStr(1);
WriteLn('opening '+a+' '+BStr(i)+'...');
if (DriveOpen(a,i)<>0) then immErr('error opening drive!');
WriteLn('this disk has '+BStr(DriveSize shl 9)+' bytes total capacity.');
DriveReadOnly:=False;

if pipeLineBegListen then immErr('failed to start listening!');
if TCPfindProcess then immErr('failed to find tcp process!');
getServerName;

serverPort:=BVal(paramStr(2));
if (serverPort=0) then serverPort:=3260;
if TCPlistenOnPort(i,65536,serverAddr,serverPort) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(serverAddr)+' '+BStr(serverPort)+' port...');
BugOS_SignDaemoning;

f1:
relequish;
timer2start;
if (pipeLineGetIncoming(p)<>0) then goto f1;
Write('incoming connection...');

f2:
if TCPlookConnected(p,a,i,o) then begin;
  relequish;
  if (p<>0) then goto f2;
  pipeLineClose(p);
  WriteLn(' failed!');
  goto f1;
  end;
WriteLn(#8#8#8' from '+ipAddr2string(a)+' '+BStr(i)+'...');
while (serveInitiator(p)=0) do ;
pipeLineClose(p);
WriteLn('pipeline closed, serving others...');
goto f1;
END.