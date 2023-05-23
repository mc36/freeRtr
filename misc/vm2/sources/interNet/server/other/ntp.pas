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
{$include \sources\internet\kernel\utils\timer2.inc}
{$include \sources\internet\kernel\utils\unixtime.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;



Const
  defaultPort=123;
  protocolConst=$83aa7e80;
Type
  OneTimestampRecord=record
    i:LongInt;          {seconds since 1900jan1 00:00:00}
    m:LongInt;          {nanoseconds}
    end;
  OneNetTimeProtoPacket=record
    ver:Byte;                   {li, vn, mode}
    str:Byte;                   {stratum}
    pol:Byte;                   {poll}
    prc:Byte;                   {precision}
    dly:LongInt;                {root delay}
    ref:LongInt;                {root reference}
    rid:LongInt;                {reference id}
    ft:OneTimestampRecord;      {reference time}
    ot:OneTimestampRecord;      {originate time}
    rt:OneTimestampRecord;      {receive time}
    tt:OneTimestampRecord;      {transmit time}
    end;
Var secondsToAdd:LongInt;


Function findLastSunday(year,month:LongInt):LongInt;
Var i:LongInt;
Begin;
for i:=GetDaysInMonth(year,month) downto 1 do if (GetDayOfWeek(year,month,i)=0) then break;
findLastSunday:=unixTime_convertTo(year,month,i,1,0,0);
End;

Procedure adjustTime(var tim:LongInt;rcvd:Boolean);
Var a,b,c,d,e,f:word;
Begin;
if rcvd then inc(tim,secondsToAdd-protocolConst);
unixTime_convertFrom(tim,a,b,c,d,e,f);
if (findLastSunday(a,3)<tim) and (tim<findLastSunday(a,10)) then a:=60*60 else a:=0;
if (secondsToAdd=0) then a:=0;
if rcvd then inc(tim,a) else dec(tim,a);
if not rcvd then inc(tim,protocolConst-secondsToAdd);
End;


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


Procedure putCurrentDate(var buffer);
Var
  buf:array[1..1] of byte absolute buffer;
  b,c,d,e,f,g:Word;
  i:LongInt;
Begin;
xGetDate(b,c,d);
xGetTime(e,f,g);
i:=unixTime_convertTo(b,c,d,e,f,g)-secondsToAdd+protocolConst;
WriteLongMSB(buf[1],i);
WriteLongMSB(buf[5],0);
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



Function ntpParsePacket(var buffer;siz:LongInt;var tim:LongInt):LongInt;
Label f1;
Var
  buf:array[1..1024] of byte absolute buffer;
  pdu:OneNetTimeProtoPacket absolute buffer;
  org:OneTimestampRecord;
  ver:LongInt;
  i,o,p:LongInt;
Begin;
ntpParsePacket:=0;
fillchar(org,sizeof(org),0);
tim:=ReadLongMSB(pdu.tt)+secondsToAdd-protocolConst;
if (siz<1) then begin;
  putCurrentDate(org);
  ver:=3;
  p:=3;
  goto f1;
  end;
if (siz<>sizeof(pdu)) then begin;
  WriteLn('got invalid packet size!');
  exit;
  end;
i:=pdu.ver;
ver:=(i shr 3) and 7;
if (ver<1) or (ver>4) then begin;
  WriteLn('got invalid version '+BStr(ver)+' packet!');
  exit;
  end;
org:=pdu.tt;
p:=4;
f1:
fillchar(pdu,sizeof(pdu),0);
pdu.ver:=(ver shl 3) or p;
pdu.str:=1;
pdu.pol:=7;
pdu.prc:=0;
pdu.dly:=0;
pdu.ref:=0;
WriteLongMSB(pdu.rid,$47505300);
putCurrentDate(pdu.ft);
pdu.ot:=org;
pdu.rt:=pdu.ft;
pdu.tt:=pdu.ft;
ntpParsePacket:=sizeof(pdu);
End;



Label f1;
Var
  pipeUdp:LongInt;
  buf:array[1..1024] of byte;
  i,o,p,q:LongInt;
  a,b:String;
BEGIN;
WriteLn('network time server v1.0, done by Mc at '#%date' '#%time'.');
ReadLocalhostFile;
unixTime_generateTable;
if TCPfindProcess then immErr('failed to find tcp process!');

b:=GetAllParameters;
if (b<>'') then begin;
  i:=0;
  if UDPlistenOnPort(pipeUdp,4096,a,i) then immErr('failed to listen on port!');
  WriteLn('local side is '+ipAddr2string(a)+' '+BStr(i)+' port...');
  if string2ipAddr(b,a) then immErr('invalid ip address!');
  WriteLn('sending to '+ipAddr2string(a)+' '+BStr(defaultPort)+'...');
  i:=ntpParsePacket(buf,0,o);
  if UDPsendPacket(pipeUdp,a,defaultPort,buf,i) then immErr('error sending query!');
  timer2start;
  p:=currentTime;
  repeat
    relequish;
    timer2start;
    if (getTimePast(p)>16) then immErr('no response received!');
    i:=sizeof(buf);
    until not UDPreceivePacket(pipeUdp,a,o,buf,i);
  WriteLn('got reply from '+ipAddr2string(a)+' '+BStr(o)+'...');
  i:=ntpParsePacket(buf,i,o);
  if (i<1) then immErr('error in reply packet!');
  setSystemDate(o);
  exit;
  end;

i:=defaultPort;
if UDPlistenOnPort(pipeUdp,4096,a,i) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(a)+' '+BStr(i)+' port...');

WriteLn('will update time by '+BStr(secondsToAdd)+' seconds...');

BugOS_SignDaemoning;
f1:
relequish;
i:=sizeof(buf);
if UDPreceivePacket(pipeUdp,b,p,buf,i) then goto f1;
WriteLn('got request from '+ipAddr2string(b)+' '+BStr(p));
i:=ntpParsePacket(buf,i,o);
if (i<1) then goto f1;
UDPsendPacket(pipeUdp,b,p,buf,i);
goto f1;
END.