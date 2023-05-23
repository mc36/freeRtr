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

{$include pipeForward.inc}

Label f1;
Var
  i,o,p,q:LongInt;
  a:String;
BEGIN;
WriteLn('pipe forward client v1.0, done by Mc at '#%date' '#%time'.');
TCPfindProcess;
o:=BVal(ParamStr(2));
string2ipAddr(ParamStr(1),a);

if (o=0) then begin;
  writeln('using: pipeForwardC.code <host> <port>');
  exit;
  end;

BugOS_MyProcessInfo(p,i,i);
Write('waiting for incoming pipe, my pid='+BStr(p)+'...');
BugOS_SignDaemoning;
pipeLineBegListen;
while (pipeLineGetIncoming(q)<>0) do relequish;
pipeLineEndListen;
pipeLineStats(q,p,i,i);
WriteLn(' ok, pid='+BStr(p)+'!');

Write('connecting to '+ipAddr2string(a)+' '+BStr(o)+'...');
TCPbeginConnect(p,65536,a,o);
f1:
if TCPlookConnected(p,a,i,o) then begin;
  relequish;
  if (p<>0) then goto f1;
  WriteLn(' failed!');
  exit;
  end;
WriteLn(' ok!');
WriteLn('local side is '+ipAddr2string(a)+' '+BStr(i)+'...');

doPipeForward(q,p);
END.