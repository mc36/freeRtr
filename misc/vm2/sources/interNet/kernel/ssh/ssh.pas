{$heap 255k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}
{$sysinc crypto.inc}
{$sysinc bignum.inc}
{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\system\login\authenticator.inc}
{$include \sources\system\login\login.inc}
{$include \sources\internet\kernel\utils\timer2.inc}
{$include \sources\internet\kernel\utils\keys.inc}

Const ProggyName='ssh2 server v1.0';

{$include memory.inc}
{$include config.inc}
{$include ssh1.inc}
{$include digsig.inc}
{$include ssh2.inc}
{$include ssh3.inc}


Label f1;
Var
  con:OneConnectionRecord;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn(ProggyName+', done by Mc at '#%date' '#%time'.');
Randomize;
if TCPfindProcess then immErr('failed to find tcp process!');
if CryptoStartActions then immErr('failed to find crypto process!');
if BigNumStartActions then immErr('failed to find bignum process!');

ConnectionNum:=0;
lastSent:=0;
if pipeLineBegListen then immErr('failed to start listening!');

a:=ParamStr(1);
if (a='') then immErr('using: ssh.code <config>');
ReadUpConfig(a);

i:=serverPort;
if (i=0) then i:=22;
if TCPlistenOnPort(p,65536,serverAddr,i) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(serverAddr)+' '+BStr(i)+' port...');
serverPort:=i;

BugOS_SignDaemoning;
f1:
relequish;
timer2start;
while (pipeLineGetIncoming(p)=0) do begin;
  if ResizeMem(ConnectionNum+1) then begin;
    a:='server is too busy!'#13#10;
    pipeLineSend(p,a[1],length(a));
    WriteLn('failed to allocate memory!');
    pipeLineClose(p);
    goto f1;
    end;
  fillchar(con,sizeof(con),0);
  con.pipe:=p;
  con.stat:=1;
  pipeLineStats(p,o,i,i);
  con.proc:=o;
  if (o<>TCPprocessId) then con.stat:=99;
  con.time:=CurrentTime;
  con.secure:=false;
  con.authed:=false;
  con.userid:=GuestUserIDmasking;
  con.packRL:=0;
  con.packLR:=0;
  ConnectionDat^[ConnectionNum]:=con;
  end;
for i:=ConnectionNum downto 1 do if doConn(ConnectionDat^[i],i) then begin;
  con:=ConnectionDat^[i];
  pipeLineClose(con.pipe);
  for o:=1 to con.ChanNum do pipeLineClose(con.ChanDat[o].pipe);
  a:=con.err;
  if (a<>'') then writeln(a+' ('+ipAddr2string(con.adr)+' '+BStr(con.prt)+')');
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  end;

for i:=1 to ListenerNum do begin;
  if (ListenerDat[i].pipe=0) then continue;
  pipeLineStats(ListenerDat[i].pipe,p,o,o);
  if (p<>0) then continue;
  pipeLineClose(ListenerDat[i].pipe);
  ListenerDat[i].pipe:=0;
  ListenerDat[i].proc:=0;
  end;

goto f1;
END.