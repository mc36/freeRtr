{$heap 31k}
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
{$include \sources\internet\kernel\utils\timer2.inc}
{$include \sources\system\login\login.inc}
{$include \sources\system\login\authenticator.inc}

{$include memory.inc}
{$include ftp.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;



Label f1;
Var
  con:OneConnectionRecord;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('ftp server v1.0, done by Mc at '#%date' '#%time'.');
Randomize;
if TCPfindProcess then immErr('failed to find tcp process!');

GetCurrentYear;
ConnectionNum:=0;
lastSent:=0;
if pipeLineBegListen then immErr('failed to start listening!');

i:=BVal(ParamStr(1));
if (i=0) then i:=21;
if TCPlistenOnPort(p,4096,addrCmd,i) then immErr('failed to listen on port!');
WriteLn('control listening on '+ipAddr2string(addrCmd)+' '+BStr(i)+' port...');
portCmd:=i;

i:=BVal(ParamStr(2));
if (i=0) then i:=portCmd-1;
if TCPlistenOnPort(p,65536,addrDat,i) then immErr('failed to listen on port!');
WriteLn('data listening on '+ipAddr2string(addrDat)+' '+BStr(i)+' port...');
portDat:=i;

defaultPath:=copy(paramStr(3),1,1);
if (defaultPath='') then defaultPath:='c';

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
  con.uid:=GuestUserIDmasking;
  con.pipe:=p;
  con.time:=CurrentTime;
  ConnectionDat^[ConnectionNum]:=con;
  end;
for i:=ConnectionNum downto 1 do if doConn(ConnectionDat^[i],i) then begin;
  con:=ConnectionDat^[i];
  pipeLineClose(con.pipe);
  pipeLineClose(con.data);
  xClose(con.fileH);
  xDirClose(con.fileH);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  end;

goto f1;
END.