{$heap 303k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}
{$sysinc crypto.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Const ProggyName='pop3 server v1.0';

{$include memory.inc}
{$include config.inc}
{$include pop3.inc}


Label f1;
Var
  con:OneConnectionRecord;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn(ProggyName+', done by Mc at '#%date' '#%time'.');
Randomize;
if CryptoStartActions then immErr('failed to find crypto process!');

CryptoGetHasherList(usersListDat,p);
md5algorithm:=CryptoFindOneAlgo(usersListDat,p,'md5');
if (md5algorithm<1) then immErr('failed to find md5 algorithm!');

ConnectionNum:=0;
lastSent:=0;
if pipeLineBegListen then immErr('failed to start listening!');

a:=ParamStr(1);
if (a='') then immErr('using: pop3.code <config>');
ReadUpConfig(a);

i:=serverSPort;
if (i<>-1) then begin;
  if TLSfindProcess then immErr('failed to find tls process!');
  if (i=0) then i:=995;
  if TCPlistenOnPort(p,65536,serverAddr,i) then immErr('failed to listen on port!');
  WriteLn('listening on '+ipAddr2string(serverAddr)+' '+BStr(i)+' tls port...');
  serverSPort:=i;
  end;

i:=serverPort;
if (i<>-1) then begin;
  if TCPfindProcess then immErr('failed to find tcp process!');
  if (i=0) then i:=110;
  if TCPlistenOnPort(p,65536,serverAddr,i) then immErr('failed to listen on port!');
  WriteLn('listening on '+ipAddr2string(serverAddr)+' '+BStr(i)+' tcp port...');
  serverPort:=i;
  end;

BugOS_SignDaemoning;
f1:
relequish;
timer2start;
while (pipeLineGetIncoming(p)=0) do begin;
  if ResizeMem(ConnectionNum+1) then begin;
    a:='-ERR server is too busy!'#13#10;
    pipeLineSend(p,a[1],length(a));
    WriteLn('failed to allocate memory!');
    pipeLineClose(p);
    goto f1;
    end;
  fillchar(con,sizeof(con),0);
  con.stat:=1;
  con.pipe:=p;
  con.time:=CurrentTime;
  con.usrNum:=0;
  ConnectionDat^[ConnectionNum]:=con;
  end;
for i:=ConnectionNum downto 1 do if doConn(ConnectionDat^[i],i) then begin;
  con:=ConnectionDat^[i];
  pipeLineClose(con.pipe);
  xDirClose(con.fileD);
  xClose(con.fileD);
  xtClose(con.fileH);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  end;

goto f1;
END.