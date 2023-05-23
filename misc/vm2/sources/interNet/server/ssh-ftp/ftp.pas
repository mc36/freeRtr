{$undef debug}
{{$define debug}
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
{$sysinc datetime.inc}
{$sysinc pipeline.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}
{$include \sources\internet\kernel\utils\unixtime.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

{$include memory.inc}
{$include ftp.inc}


Label f1;
Var
  con:OneConnectionRecord;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('ssh-ftp server v1.0, done by Mc at '#%date' '#%time'.');
if SSHfindProcess then immErr('failed to find ssh process!');
unixTime_generateTable;

ConnectionNum:=0;
lastSent:=0;
if pipeLineBegListen then immErr('failed to start listening!');

i:=BVal(ParamStr(1));
if (i=0) then i:=21;
if TCPlistenOnPort(p,65536,a,i) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(a)+' '+BStr(i)+' port...');

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
  BugOS_SetOwnerInfo(con.user);
  for o:=1 to maxHandles do begin;
    xDirClose(con.handles[o]);
    xClose(con.handles[o]);
    end;
  ResizeMem(ConnectionNum-1);
  end;

goto f1;
END.