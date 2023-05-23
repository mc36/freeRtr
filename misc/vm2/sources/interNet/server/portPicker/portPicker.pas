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
{$include portpicker.inc}


Label f1;
Var
  con:OneConnectionRecord;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('port picker v1.0, done by Mc at '#%date' '#%time'.');

if (paramCount<5) then immErr('using: portpicker.code <localport> <timeout> <text> <ipaddr> <port> [<text> <ipaddr> <port>]');
sourcePort:=BVal(ParamStr(1));
readedTim:=BVal(ParamStr(2));
targetNum:=0;
p:=3;
while (p<paramCount) do begin;
  b:=paramStr(p);
  inc(p);
  convertText(b);
  if string2ipAddr(ParamStr(p),a) then immErr('error in address!');
  inc(p);
  i:=BVal(ParamStr(p));
  inc(p);
  if (i<1) then immErr('error in port!');
  inc(targetNum);
  targetPort[targetNum]:=i;
  move(a,targetAddr[targetNum],sizeof(sourceAddr));
  targetText[targetNum]:=b;
  end;

readedMin:=666;
readedMax:=-666;
for o:=1 to targetNum do begin;
  i:=length(targetText[o]);
  if (i<readedMin) then readedMin:=i;
  if (i>readedMax) then readedMax:=i;
  end;
if (readedMin<1) then readedMin:=1;

if TCPfindProcess then immErr('failed to find tcp process!');

if TCPlistenOnPort(p,65536,sourceAddr,sourcePort) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(sourceAddr)+' '+BStr(sourcePort)+' port...');
WriteLn('will use '+BStr(targetNum)+' alternate servers, timeout='+BStr(readedTim));
for p:=1 to targetNum do WriteLn('#'+BStr(p)+': '+ipAddr2string(targetAddr[p])+' '+BStr(targetPort[p])+' (len='+BStr(length(targetText[p]))+')');

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
for i:=ConnectionNum downto 1 do if doConn(ConnectionDat^[i]) then begin;
  con:=ConnectionDat^[i];
  pipeLineClose(con.pipe1);
  pipeLineClose(con.pipe2);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  end;

goto f1;
END.