{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc param.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$sysinc inet_dns.inc}
{$include \sources\internet\kernel\utils\timer2.inc}


Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function connectOne(host:String;port:LongInt):LongInt;
Var
  a:String;
  pipe:LongInt;
  i,o,p:LongInt;
Begin;
if (length(host)<>1) then begin;
  if string2ipAddr(host,a) then immErr('bad address!');
  Write('connecting to '+ipAddr2string(a)+' '+BStr(port)+'...');
  TCPbeginConnect(pipe,65536,a,port);
  while TCPlookConnected(pipe,a,i,o) do begin;
    if (pipe=0) then immErr(' failed!');
    relequish;
    end;
  WriteLn(' ok!');
  WriteLn('local side is '+ipAddr2string(a)+' '+BStr(i)+'...');
  connectOne:=pipe;
  exit;
  end;
if pipeLineBegListen then immErr('failed to start listening!');
i:=port;
if TCPlistenOnPort(p,65536,a,i) then immErr('failed to listen on tcp port!');
WriteLn('listening on '+ipAddr2string(a)+' '+BStr(i)+' tcp port...');
while (pipeLineGetIncoming(pipe)<>0) do relequish;
if pipeLineEndListen then immErr('failed to start listening!');
pipeLineClose(p);
Write('accepting connection...');
while TCPlookConnected(pipe,a,i,o) do begin;
  if (pipe=0) then immErr(' failed!');
  relequish;
  end;
WriteLn(' ok!');
WriteLn('accepted connection from '+ipAddr2string(a)+' '+BStr(i)+'...');
connectOne:=pipe;
End;

Var pipe1,pipe2:LongInt;

Procedure doConnect;
Var
  p1,p2,r1,r2,t1,t2:LongInt;
  buf:array[1..16*1024] of byte;
Begin;
pipeLineStats(pipe1,p1,r1,t1);
pipeLineStats(pipe2,p2,r2,t2);
if (p1=0) then if (r1<1) then immErr('peer #1 closed connection!');
if (p2=0) then if (r2<1) then immErr('peer #2 closed connection!');
if (r1>t2) then r1:=t2;
if (r1>sizeof(buf)) then r1:=sizeof(buf);
if (r1>0) then begin;
  pipeLineRecv(pipe1,buf,r1);
  pipeLineSend(pipe2,buf,r1);
  end;
if (r2>t1) then r2:=t1;
if (r2>sizeof(buf)) then r2:=sizeof(buf);
if (r2>0) then begin;
  pipeLineRecv(pipe2,buf,r2);
  pipeLineSend(pipe1,buf,r2);
  end;
End;


Label f1;
BEGIN;
WriteLn('port connecter v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');
if (paramCount<>4) then immErr('using: conn.code <addr1> <port1> <addr2> <port2>');
pipe1:=connectOne(paramStr(1),BVal(paramStr(2)));
pipe2:=connectOne(paramStr(3),BVal(paramStr(4)));
f1:
doConnect;
relequish;
goto f1;
END.