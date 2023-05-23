{$heap 63k}
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

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

{$include memory.inc}
{$include linePrinter.inc}

Label f1;
Var
  con:OneConnectionRecord;
  i,o,p:LongInt;
  a:String;
BEGIN;
WriteLn('line printer server v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');
Randomize;

a:=ParamStr(1);
if (a='') then immErr('using: lpd.code <config>');
ReadUpConfig(a);

if pipeLineBegListen then immErr('failed to start listening!');
ConnectionNum:=0;

i:=serverPort;
if (i=0) then i:=515;
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
  con.stat:=1;
  con.pipe:=p;
  con.time:=CurrentTime;
  ConnectionDat^[ConnectionNum]:=con;
  end;
for i:=ConnectionNum downto 1 do if doConn(ConnectionDat^[i],i) then begin;
  con:=ConnectionDat^[i];
  pipeLineClose(con.pipe);
  xDirClose(con.file);
  xClose(con.file);
  xErase(con.name+headFileExt);
  xErase(con.name+dataFileExt);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  end;

goto f1;
END.