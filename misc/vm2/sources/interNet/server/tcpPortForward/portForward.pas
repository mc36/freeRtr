{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc crt.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

{$include memory.inc}
{$include portForward.inc}


Label f1;
Var
  con:OneConnectionRecord;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('port forwarder (tcp) v1.0, done by Mc at '#%date' '#%time'.');

if (paramCount<3) then immErr('using: portforward.code <localport> <ipaddr> <port> [<proto> <proto>]');
sourcePort:=BVal(ParamStr(1));
if string2ipAddr(ParamStr(2),targetAddr) then immErr('error in address!');
targetPort:=BVal(ParamStr(3));
if (targetPort=0) then targetPort:=sourcePort;
srcProto:=decodeOneProto(paramStr(4));
trgProto:=decodeOneProto(paramStr(5));

case srcProto of
  0:if TCPfindProcess then immErr('failed to find tcp process!');
  1:if TLSfindProcess then immErr('failed to find tls process!');
  2:if SSHfindProcess then immErr('failed to find ssh process!');
  else immErr('bug!');
  end;

if TCPlistenOnPort(p,65536,sourceAddr,sourcePort) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(sourceAddr)+' '+BStr(sourcePort)+' '+encodeOneProto(srcProto)+' port...');
WriteLn('will connect to '+ipAddr2string(targetAddr)+' '+BStr(targetPort)+' '+encodeOneProto(trgProto)+' port...');

case trgProto of
  0:begin;
    if TCPfindProcess then immErr('failed to find tcp process!');
    magicWord:='';
    end;
  1:begin;
    if TLSfindProcess then immErr('failed to find tls process!');
    magicWord:='secure'#13;
    end;
  2:begin;
    if SSHfindProcess then immErr('failed to find ssh process!');
    magicWord:='connect'#13;
    end;
  else immErr('bug!');
  end;

authBufferSiz:=0;
if (trgProto=2) then begin;
  askUserAuth('username: ',255,0);
  askUserAuth('password: ',0,42);
  end;

ConnectionNum:=0;
if pipeLineBegListen then immErr('failed to start listening!');
WriteLn('serving others...');
BugOS_SignDaemoning;
f1:
relequish;
timer2start;
while (pipeLineGetIncoming(p)=0) do begin;
  if ResizeMem(ConnectionNum+1) then begin;
    pipeLineClose(p);
    WriteLn('failed to allocate memory!');
    goto f1;
    end;
  fillchar(con,sizeof(con),0);
  con.pipe1:=p;
  con.time:=CurrentTime;
  con.stat:=1;
  ConnectionDat^[ConnectionNum]:=con;
  end;
for i:=ConnectionNum downto 1 do if doConn(ConnectionDat^[i],i) then begin;
  con:=ConnectionDat^[i];
  pipeLineClose(con.pipe1);
  pipeLineClose(con.pipe2);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  end;

goto f1;
END.