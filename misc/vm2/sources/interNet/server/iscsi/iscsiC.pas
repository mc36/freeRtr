{$undef debug}
{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc hex.inc}
{$sysinc pipeline.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}

{$include \sources\internet\kernel\utils\timer2.inc}
{$include iscsi.inc}
{$include iscsiC.inc}


Label f1;
Var
  a:String;
  i,o,p:LongInt;
BEGIN;
WriteLn('iscsi client v1.0, done by Mc at '#%date' '#%time'.');
if (ParamCount<1) then immErr('using: iscsic.code <server> [port]');
if TCPfindProcess then immErr('failed to find tcp process!');
if pipeLineBegListen then immErr('failed to start listening!');
serverName:=paramStr(1);
serverPort:=BVal(paramStr(2));
if (serverPort=0) then serverPort:=3260;
string2ipAddr(serverName,serverAddr);
commandSeq:=2;
doOneLoginProcedure;
BugOS_SignDaemoning;

f1:
relequish;
timer2start;
if (pipeLineGetIncoming(p)<>0) then goto f1;
serveOneClient(p);
pipeLineClose(p);
WriteLn('pipeline closed, serving others...');
goto f1;
END.