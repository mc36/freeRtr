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

{$include streamForward.inc}

Label f1;
Var
  i,o,p,q:LongInt;
  a:String;
BEGIN;
WriteLn('stream forward server v1.0, done by Mc at '#%date' '#%time'.');
TCPfindProcess;

a:=ParamStr(2);
q:=BVal(a);
if (q=0) then q:=BugOS_findProcNam(a);
if (q=0) then begin;
  writeln('using: streamForwardS.code <port> <process>');
  exit;
  end;

i:=BVal(ParamStr(1));
pipeLineBegListen;
if TCPlistenOnPort(o,65536,a,i) then exit;
WriteLn('listening on '+ipAddr2string(a)+' '+BStr(i)+'...');
BugOS_SignDaemoning;
while (pipeLineGetIncoming(p)<>0) do relequish;
pipeLineEndListen;
pipeLineClose(o);
Write('incoming connection...');

while TCPlookConnected(p,a,i,o) do begin;
  relequish;
  if (p<>0) then continue;
  WriteLn(' failed!');
  exit;
  end;
WriteLn(#8#8#8' from '+ipAddr2string(a)+' '+BStr(i)+'...');

i:=BVal(ParamStr(3));
if (pipeLineCreate(q,q,65536,false)<>0) then begin;
  WriteLn('failed to open pipeline!');
  exit;
  end;

doStreamForward(q,p);
END.