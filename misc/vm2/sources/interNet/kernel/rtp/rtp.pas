{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

{$include memory.inc}
{$include rtp.inc}

Const proggyName='rtp server v1.0';

Procedure gotIncoming(p:LongInt);
Var
  con:OneConnectionRecord;
Begin;
fillchar(con,sizeof(con),0);
con.timR:=currentTime;
con.timT:=currentTime;
con.pipe:=p;
if ResizeMem(ConnectionNum+1) then begin;
  WriteLn('failed to allocate mrmory!');
  pipeLineClose(p);
  exit;
  end;
ConnectionDat^[ConnectionNum]:=con;
End;


Label f1;
Var
  a:String;
  i,o:LongInt;
  con:OneConnectionRecord;
BEGIN;
WriteLn(proggyName+', done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');

portNum:=0;
ConnectionNum:=0;
for i:=1 to paramCount do openOnePort(BVal(paramStr(i)));
if (portNum<1) then immErr('using: rtp.code <port> [port] [port]');

pipeLineBegListen;
BugOS_SignDaemoning;

f1:
relequish;
timer2start;
while (pipeLineGetIncoming(i)=0) do gotIncoming(i);
for i:=1 to portNum do releq2upper(i);
for i:=ConnectionNum downto 1 do if releq2user(ConnectionDat^[i]) then begin;
  con:=ConnectionDat^[i];
  pipeLineClose(con.pipe);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  end;
goto f1;
END.