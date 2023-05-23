{$heap 15k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

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
{$include config.inc}
{$include telnet.inc}


Label f1;
Var
  con:OneConnectionRecord;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('telnet server v1.0, done by Mc at '#%date' '#%time'.');

lastSent:=0;
ProtocolNum:=0;
ConnectionNum:=0;
initString:='';
a:=ParamStr(1);
if (a='') then immErr('using: telnet.code <config>');
ReadUpConfig(a);
if (ProtocolNum<1) then immErr('no protocol selected!');
if pipeLineBegListen then immErr('failed to start listening!');

WriteLn('opening protocols...');
for o:=1 to ProtocolNum do begin;
  a:=ProtocolDat[o].name;
  Write(a+': ');
  p:=BugOS_findProcNam(a);
  ProtocolDat[o].proc:=p;
  if (p=0) then immErr('process not found!');
  TCPprocessId:=p;
  i:=ProtocolDat[o].port;
  if (i=0) then i:=23;
  if TCPlistenOnPort(p,65536,a,i) then immErr('failed to listen on port!');
  move(a,ProtocolDat[o].addr,sizeof(OneTCPaddressRecord));
  ProtocolDat[o].port:=i;
  WriteLn('listening on '+ipAddr2string(a)+' '+BStr(i));
  end;

WriteLn('serving others...');
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
  con.tcp:=p;
  con.time:=CurrentTime;
  ConnectionDat^[ConnectionNum]:=con;
  end;
for i:=ConnectionNum downto 1 do if doConn(ConnectionDat^[i],i) then begin;
  con:=ConnectionDat^[i];
  pipeLineClose(con.tcp);
  pipeLineClose(con.term);
  pipeLineClose(con.emu1);
  pipeLineClose(con.emu2);
  BugOS_KillProcess(con.proc);
  xExecBgnd('c:\system\process\killRecursive.code',BStr(con.proc),o);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  end;

goto f1;
END.