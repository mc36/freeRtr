{{$define debug}
{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc hex.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

{$include listenForward.inc}

Label f1;
Var
  con:OneConnectionRecord;
  i,o,p,q:LongInt;
  a:String;
BEGIN;
WriteLn('listen forward server v1.0, done by Mc at '#%date' '#%time'.');
TCPfindProcess;
Randomize;

p:=BVal(ParamStr(1));
if (p=0) then immErr('using: listenForwardS.code <meetPort> <dataPort>');

i:=p;
pipeLineBegListen;
if TCPlistenOnPort(o,65536,a,i) then exit;
WriteLn('listening on '+ipAddr2string(a)+' '+BStr(i)+'...');
BugOS_SignDaemoning;
while (pipeLineGetIncoming(uplinkPipe)<>0) do relequish;
pipeLineClose(o);
Write('incoming connection...');

while TCPlookConnected(uplinkPipe,a,i,o) do begin;
  relequish;
  if (uplinkPipe=0) then immErr(' failed!');
  end;
WriteLn(#8#8#8' from '+ipAddr2string(a)+' '+BStr(i)+'...');

i:=BVal(ParamStr(2));
if (i=0) then i:=p;
if TCPlistenOnPort(o,65536,a,i) then exit;
WriteLn('listening on '+ipAddr2string(a)+' '+BStr(i)+'...');

ConnectionCur:=0;
uplinkHdrSiz:=0;
uplinkLast:=0;
ResizeMem(0);

f1:
relequish;
timer2start;
p:=uplinkDoUsualWork(i,o,a);
if (p>=0) then case i of
  4:begin; {accept connection}
    p:=findOneConnect(o);
    if (p<1) then uplinkSendFrame(5,o,0,i) else ConnectionDat^[p].stat:=3;
    end;
  end;
if uplinkBufferFull then goto f1;
releq2conn;
if (ConnectionNum>$f000) then goto f1;
if (pipeLineGetIncoming(p)<>0) then goto f1;
if ResizeMem(ConnectionNum+1) then begin;
  WriteLn('failed to allocate memory!');
  pipeLineClose(p);
  goto f1;
  end;
fillchar(con,sizeof(con),0);
con.pipe:=p;
repeat
  i:=random($fff8)+1;
  until (findOneConnect(i)<1);
con.conn:=i;
con.stat:=1;
ConnectionDat^[ConnectionNum]:=con;
goto f1;
END.