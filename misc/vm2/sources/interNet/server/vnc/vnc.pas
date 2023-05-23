{$undef debug}
{{$define debug}
{$heap 63k}
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
{$include auth.inc}
{$include vnc.inc}


Label f1;
Var
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('rfb server v1.0, done by Mc at '#%date' '#%time'.');
Randomize;
if TCPfindProcess then immErr('failed to find tcp process!');

a:=ParamStr(1);
if (a='') then immErr('using: vnc.code <config>');
ReadUpConfig(a);

if (screenSizX<16) or (screenSizX>8192) then screenSizX:=320;
if (screenSizY<16) or (screenSizY>8192) then screenSizX:=200;
screenSizP:=screenSizX*screenSizY;
ConnectionSiz:=((screenSizP*3)+sizeof(OneConnectionRecord)+3) and $7ffffffc;
ConnectionNum:=0;
lastSent:=0;
if pipeLineBegListen then immErr('failed to start listening!');
{$ifdef debug}
WriteLn('screen size will be '+BStr(screenSizX)+'x'+BStr(screenSizY)+'='+BStr(screenSizP)+' pixels...');
WriteLn('process will be '+processNam+' '+processPar+'...');
{$endif}

i:=serverPort;
if (i=0) then i:=5900;
if TCPlistenOnPort(p,65536,serverAddr,i) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(serverAddr)+' '+BStr(i)+' port...');
serverPort:=i;

BugOS_SignDaemoning;
f1:
relequish;
timer2start;
while (pipeLineGetIncoming(p)=0) do begin;
  pipeLineStats(p,o,i,i);
  if (o<>TCPprocessId) then begin;
    appendConn(p,o);
    continue;
    end;
  if ResizeMem(ConnectionNum+1) then begin;
    a:='server is too busy!';
    a:=#0#0#0#0+chr(length(a))+#0#0#0+a;
    pipeLineSend(p,a[1],length(a));
    WriteLn('failed to allocate memory!');
    pipeLineClose(p);
    goto f1;
    end;
  openConn(ConnectionDat^[(ConnectionNum-1)*ConnectionSiz],p);
  end;
o:=ConnectionNum*ConnectionSiz;
for i:=ConnectionNum downto 1 do begin;
  dec(o,ConnectionSiz);
  if doConn(ConnectionDat^[o]) then closeConn(ConnectionDat^[o]);
  end;

goto f1;
END.