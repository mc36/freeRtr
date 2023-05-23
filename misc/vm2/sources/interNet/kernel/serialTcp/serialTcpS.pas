{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc random.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

{$include memory.inc}
{$include config.inc}
{$include serialTcpS.inc}

{$include \sources\system\serial\serial.inc}


Label f1;
Var
  a:String;
  i,o,p:LongInt;
  con:OneConnectionRecord;
BEGIN;
WriteLn('serial server v1.0, done by Mc at '#%date' '#%time'.');
Randomize;
if TCPfindProcess then immErr('failed to find tcp process!');

a:=paramStr(1);
if (a='') then immErr('using: ser.code <config>');
ReadUpConfig(a);

for p:=1 to ConnectionNum do begin;
  con:=ConnectionDat^[p];
  Write('opening port #'+BStr(p)+'...');
  serialProc:=con.serC;
  serialPort:=con.serD;
  serialCtrl:=0;
  serialData:=0;
  SerialClose;
  for i:=1 to 32 do relequish;
  SerialOpen;
  for i:=1 to 32 do relequish;
  con.serC:=serialCtrl;
  con.serD:=serialData;
  WriteLn(' Ok.');
  ConnectionDat^[p]:=con;
  end;


if (servPort=0) then servPort:=3001;
i:=servPort;
if TCPlistenOnPort(p,65536,servAddr,i) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(servAddr)+' '+BStr(i)+' port...');
servPort:=i;

pipeLineBegListen;
BugOS_SignDaemoning;
timer2start;

f1:
relequish;
timer2start;
while (pipeLineGetIncoming(p)=0) do begin;
  o:=FindConnectByStat(0);
  if (o<1) then begin;
    a:='out of connection slots!'#13#10;
    pipeLineSend(p,a[1],length(a));
    pipeLineClose(p);
    goto f1;
    end;
  con:=ConnectionDat^[o];
  con.stat:=1;
  con.pipe:=p;
  con.time:=CurrentTime;
  ConnectionDat^[o]:=con;
  end;
for i:=1 to ConnectionNum do if doConn(ConnectionDat^[i]) then begin;
  con:=ConnectionDat^[i];
  pipeLineClose(con.pipe);
  con.stat:=0;
  con.pipe:=0;
  ConnectionDat^[i]:=con;
  end;

goto f1;
END.