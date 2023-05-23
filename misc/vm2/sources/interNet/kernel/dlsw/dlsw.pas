{$define debug}
{$undef debug}
{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc random.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc memory.inc}

{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Const proggyName='dlsw server v1.0';

{$include memory.inc}
{$include config.inc}
{$include dlsw1.inc}
{$include dlsw2.inc}



Label f1;
Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn(proggyName+', done by Mc at '#%date' '#%time'.');
Randomize;
if TCPfindProcess then immErr('failed to find tcp process!');

a:=paramStr(1);
if (a='') then immErr('using: dlsw.code <config>');
ReadUpConfig(a);

if (servPort=0) then servPort:=2065;
if TCPlistenOnPort(i,65536,servAddr,servPort) then immErr('failed to open listening!');
WriteLn('listening on '+ipAddr2string(servAddr)+' '+BStr(servPort)+'...');

pipeLineBegListen;
BugOS_SignDaemoning;

f1:
relequish;
timer2start;
for i:=1 to ConnectionNum do begin;
  releq2upp(ConnectionDat^[i]);
  relequishConn(ConnectionDat^[i]);
  end;
if (CurrentPipeLine<>0) then begin;
  if TCPlookConnected(CurrentPipeLine,a,i,o) then goto f1;
  WriteLn('connection accepted from '+ipAddr2string(a)+' '+BStr(i));
  o:=FindOneConnection(a);
  if (o=0) then begin;
    pipeLineClose(CurrentPipeLine);
    CurrentPipeLine:=0;
    goto f1;
    end;
  pipeLineClose(ConnectionDat^[o].pipe1);
  ConnectionDat^[o].pipe1:=CurrentPipeLine;
  if (ConnectionDat^[o].stat=0) then ConnectionDat^[o].stat:=1;
  ConnectionDat^[o].time:=currentTime;
  CurrentPipeLine:=0;
  end;
if (pipeLineGetIncoming(i)=0) then gotIncoming(i);
goto f1;
END.