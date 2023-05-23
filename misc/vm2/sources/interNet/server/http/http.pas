{$heap 303k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc datetime.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}
{$include \sources\interNet\client\base64.inc}

Const
  ProggyName='http server v1.0';

{$include memory.inc}
{$include config.inc}
{$include http.inc}




Label f1;
Var
  con:OneConnectionRecord;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn(ProggyName+', done by Mc at '#%date' '#%time'.');
Randomize;

ConnectionNum:=0;
lastSent:=0;
if pipeLineBegListen then immErr('failed to start listening!');

a:=ParamStr(1);
if (a='') then immErr('using: http.code <config>');
ReadUpConfig(a);

i:=serverPortS;
if (i<>-1) then begin;
  if TLSfindProcess then immErr('failed to find tls process!');
  if (i=0) then i:=443;
  if TCPlistenOnPort(p,65536,serverAddr,i) then immErr('failed to listen on port!');
  WriteLn('listening on '+ipAddr2string(serverAddr)+' '+BStr(i)+' tls port...');
  serverPortS:=i;
  end;

i:=serverPortP;
if (i<>-1) then begin;
  if TCPfindProcess then immErr('failed to find tcp process!');
  if (i=0) then i:=80;
  if TCPlistenOnPort(p,65536,serverAddr,i) then immErr('failed to listen on port!');
  WriteLn('listening on '+ipAddr2string(serverAddr)+' '+BStr(i)+' tcp port...');
  serverPortP:=i;
  end;

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
  con.stat:=1;
  con.pipe:=p;
  con.time:=CurrentTime;
  con.ibufS:=1;
  ConnectionDat^[ConnectionNum]:=con;
  end;
for i:=ConnectionNum downto 1 do if doConn(ConnectionDat^[i],i) then begin;
  BugOS_SetOwnerInfo(0);
  con:=ConnectionDat^[i];
  pipeLineClose(con.pipe);
  xClose(con.fileH);
  BugOS_KillProcess(con.process);
  xErase(TempPath+con.request+RequestExt);
  xErase(TempPath+con.request+ResponseExt);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  end;

goto f1;
END.