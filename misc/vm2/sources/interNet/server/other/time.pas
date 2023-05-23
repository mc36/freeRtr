{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc datetime.inc}
{$sysinc param.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\unixtime.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;



Const
  defaultPort=37;
  protocolConst=$83aa7e80;
Var
  secondsToAdd:LongInt;

Procedure ReadLocalhostFile;
Var
  t:xtText;
  a:String;
Begin;
if (xtOpen(t,'c:\system\localHost.text',true)<>0) then exit;
a:=xtReadLn(t,255);
a:=xtReadLn(t,255);
a:=xtReadLn(t,255);
a:=xtReadLn(t,255);
secondsToAdd:=(BVal(copy(a,1,3))*3600)+(BVal(copy(a,1,1)+copy(a,4,2))*60);
xtClose(t);
End;


Function getCurrentDate:String;
Var
  b,c,d,e,f,g:Word;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
xGetDate(b,c,d);
xGetTime(e,f,g);
ab0:=4;
i:=unixTime_convertTo(b,c,d,e,f,g)-secondsToAdd+protocolConst;
WriteLongMSB(ab[1],i);
getCurrentDate:=a;
End;

Procedure setSystemDate(i:LongInt);
Var
  b,c,d,e,f,g:Word;
  o:LongInt;
function x(i:longInt):string;var a:string;begin;a:=bstr(i);while (length(a)<2) do a:='0'+a;x:=a;end;
function y:string;begin;y:=x(b)+'-'+x(c)+'-'+x(d)+' '+x(e)+':'+x(f)+':'+x(g);end;
Begin;
xGetDate(b,c,d);
xGetTime(e,f,g);
o:=unixTime_convertTo(b,c,d,e,f,g);
WriteLn('original date is '+y+'...');
unixTime_convertFrom(i,b,c,d,e,f,g);
WriteLn('going to set to '+y+' (difference is '+BStr(o-i)+' seconds)...');
if (xExec('\system\drivers\system\setdatetime.code',y,b)<>0) then b:=1;
if (b<>0) then immErr('error setting date!');
xExec('\system\systemtime.code','',b);
End;



Label f1;
Var
  pipeUdp,pipeTcp:LongInt;
  buf:array[1..1024] of byte;
  i,o,p,q:LongInt;
  a,b:String;
BEGIN;
WriteLn('time server v1.0, done by Mc at '#%date' '#%time'.');
ReadLocalhostFile;
unixTime_generateTable;
if TCPfindProcess then immErr('failed to find tcp process!');

a:=GetAllParameters;
if (a<>'') then begin;
  if string2ipAddr(a,buf) then immErr('invalid ip address!');
  i:=defaultPort;
  Write('connecting to '+ipAddr2string(buf)+' '+BStr(i)+'...');
  TCPbeginConnect(p,4096,buf,i);
  while TCPlookConnected(p,a,i,o) do begin;
    relequish;
    if (p=0) then immErr(' failed!');
    end;
  WriteLn(' ok!');
  WriteLn('local side is '+ipAddr2string(a)+' '+BStr(i)+'...');
  while (1=1) do begin;
    relequish;
    pipeLineStats(p,q,o,i);
    if (o>=4) then break;
    if (q=0) then immErr('remote closed pipeline!');
    end;
  i:=sizeof(buf);
  if (pipeLineRecv(p,buf,i)<>0) then i:=0;
  if (i<4) then immErr('error receiving data!');
  setSystemDate(ReadLongMSB(buf)+secondsToAdd-protocolConst);
  exit;
  end;

if pipeLineBegListen then immErr('failed to start listening!');

i:=defaultPort;
if UDPlistenOnPort(pipeUdp,4096,a,i) then immErr('failed to listen on udp port!');
WriteLn('listening on '+ipAddr2string(a)+' '+BStr(i)+' udp port...');

i:=defaultPort;
if TCPlistenOnPort(pipeTcp,4096,a,i) then immErr('failed to listen on tcp port!');
WriteLn('listening on '+ipAddr2string(a)+' '+BStr(i)+' tcp port...');

WriteLn('will update time by '+BStr(secondsToAdd)+' seconds...');

BugOS_SignDaemoning;
f1:
relequish;
i:=sizeof(buf);
if not UDPreceivePacket(pipeUdp,b,o,buf,i) then begin;
  WriteLn('got UDP request from '+ipAddr2string(b)+' '+BStr(o));
  a:=getCurrentDate;
  UDPsendPacket(pipeUdp,b,o,a[1],length(a));
  end;
if (pipeLineGetIncoming(p)<>0) then goto f1;
while TCPlookConnected(p,b,o,i) do begin;
  relequish;
  if (p=0) then begin;
    WriteLn('got buggy TCP connection!');
    goto f1;
    end;
  end;
WriteLn('got TCP request from '+ipAddr2string(b)+' '+BStr(o));
a:=getCurrentDate;
pipeLineSend(p,a[1],length(a));
pipeLineClose(p);
goto f1;
END.