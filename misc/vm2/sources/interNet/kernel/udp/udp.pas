{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc bugos.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

{$include memory.inc}
{$include udp.inc}


Label f1;
Var
  con:OneConnectionRecord;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('udp connector v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');

listenNum:=paramCount;
for i:=1 to listenNum do listenPort[i]:=BVal(paramStr(i));
for i:=1 to listenNum do begin;
  UDPlistenOnPort(listenPipe[i],65536,serverAddr,listenPort[i]);
  WriteLn('listening on '+ipAddr2string(serverAddr)+' '+BStr(listenPort[i])+'...');
  end;
if (listenNum<1) then immErr('using: udp.code <port> [port] [port]');
ConnectionNum:=0;
lastSent:=0;
if pipeLineBegListen then immErr('failed to start listening!');

BugOS_SignDaemoning;
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
  pipeLineStats(p,o,i,i);
  con.proc:=o;
  con.stat:=1;
  con.pipeL:=p;
  ConnectionDat^[ConnectionNum]:=con;
  end;
for i:=ConnectionNum downto 1 do if doConn(ConnectionDat^[i]) then begin;
  con:=ConnectionDat^[i];
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  if (FindOneTcpPipe(con.pipeT)=0) then pipeLineClose(con.pipeT);
  pipeLineClose(con.pipeL);
  end;

goto f1;
END.