{$heap 303k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc datetime.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$sysinc inet_dns.inc}
{$include \sources\internet\kernel\utils\timer2.inc}
{$include \sources\internet\client\base64.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

{$include memory.inc}
{$include config.inc}
{$include smtp1.inc}
{$include smtp2.inc}


Label f1;
Var
  con:OneConnectionRecord;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('smtp server v1.0, done by Mc at '#%date' '#%time'.');
Randomize;
if DNSstartResolver then immErr('failed to find dns process!');

ConnectionNum:=0;
fillchar(CurrOutgoing,sizeof(CurrOutgoing),0);
lastSent:=0;
if pipeLineBegListen then immErr('failed to start listening!');

a:=ParamStr(1);
if (a='') then immErr('using: smtp.code <config>');
ReadUpConfig(a);

i:=serverSPort;
if (i<>-1) then begin;
  if TLSfindProcess then immErr('failed to find tls process!');
  if (i=0) then i:=465;
  if TCPlistenOnPort(p,65536,serverAddr,i) then immErr('failed to listen on port!');
  WriteLn('listening on '+ipAddr2string(serverAddr)+' '+BStr(i)+' tls port...');
  serverSPort:=i;
  end;

if TCPfindProcess then immErr('failed to find tcp process!');
i:=serverPort;
if (i<>-1) then begin;
  if (i=0) then i:=25;
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
    a:='421 server is too busy!'#13#10;
    pipeLineSend(p,a[1],length(a));
    WriteLn('failed to allocate memory!');
    pipeLineClose(p);
    goto f1;
    end;
  fillchar(con,sizeof(con),0);
  con.stat:=1;
  con.pipe:=p;
  con.time:=CurrentTime;
  ConnectionDat^[ConnectionNum]:=con;
  end;
for i:=ConnectionNum downto 1 do if doConn(ConnectionDat^[i],i) then begin;
  con:=ConnectionDat^[i];
  pipeLineClose(con.pipe);
  xClose(con.fileH);
  xErase(TempPath+con.fileN+MsgdatExt);
  xErase(TempPath+con.fileN+LocalExt);
  xErase(TempPath+con.fileN+RemoteExt);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  end;
doTrans(CurrOutgoing);

goto f1;
END.