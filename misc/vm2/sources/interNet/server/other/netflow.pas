{$heap 31k}
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
{$sysinc datetime.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\unixtime.inc}

Type
  netFlowV1headerRecord=record
    ver:Word;                   {version number}
    cnt:Word;                   {number of records}
    upTime:LongInt;             {millisecs since boot}
    unixT:LongInt;              {unix time generated}
    unixN:LongInt;              {nanosecs unix time}
    end;
  netFlowV1dataRecord=record
    srcA:LongInt;               {source ip address}
    dstA:LongInt;               {target ip address}
    hop:LongInt;                {next hop ip address}
    ifIn:Word;                  {input interface}
    ifOut:Word;                 {output interface}
    packs:LongInt;              {packets in flow}
    bytes:LongInt;              {bytes in flow}
    start:LongInt;              {uptime flow started}
    stop:LongInt;               {uptime flow stopped}
    srcP:Word;                  {source port}
    dstP:Word;                  {target port}
    res1:Word;                  {reserved}
    prot:Byte;                  {protocol id}
    tos:Byte;                   {type of service}
    flags:Byte;                 {all tcp flags or'd}
    res2:array[1..7] of byte;   {reserved}
    end;
  netFlowV5headerRecord=record
    ver:Word;                   {version number}
    cnt:Word;                   {number of records}
    upTime:LongInt;             {millisecs since boot}
    unixT:LongInt;              {unix time generated}
    unixN:LongInt;              {nanosecs unix time}
    flowSeq:LongInt;            {flow sequence number}
    engType:Byte;               {type of exporter}
    engSlot:Byte;               {slot id of exporter}
    res1:Word;                  {reserved}
    end;
  netFlowV5dataRecord=record
    srcA:LongInt;               {source ip address}
    dstA:LongInt;               {target ip address}
    hop:LongInt;                {next hop ip address}
    ifIn:Word;                  {input interface}
    ifOut:Word;                 {output interface}
    packs:LongInt;              {packets in flow}
    bytes:LongInt;              {bytes in flow}
    start:LongInt;              {uptime flow started}
    stop:LongInt;               {uptime flow stopped}
    srcP:Word;                  {source port}
    dstP:Word;                  {target port}
    res1:Byte;                  {reserved}
    flags:Byte;                 {all tcp flags or'd}
    prot:Byte;                  {protocol id}
    tos:Byte;                   {type of service}
    srcS:Word;                  {source as}
    dstS:Word;                  {target as}
    srcM:Byte;                  {source mask}
    dstM:Byte;                  {target mask}
    res2:Word;                  {reserved}
    end;
  netFlowV7headerRecord=record
    ver:Word;                   {version number}
    cnt:Word;                   {number of records}
    upTime:LongInt;             {millisecs since boot}
    unixT:LongInt;              {unix time generated}
    unixN:LongInt;              {nanosecs unix time}
    flowSeq:LongInt;            {flow sequence number}
    res1:LongInt;               {reserved}
    end;
  netFlowV7dataRecord=record
    srcA:LongInt;               {source ip address}
    dstA:LongInt;               {target ip address}
    hop:LongInt;                {next hop ip address}
    ifIn:Word;                  {input interface}
    ifOut:Word;                 {output interface}
    packs:LongInt;              {packets in flow}
    bytes:LongInt;              {bytes in flow}
    start:LongInt;              {uptime flow started}
    stop:LongInt;               {uptime flow stopped}
    srcP:Word;                  {source port}
    dstP:Word;                  {target port}
    flg1:Byte;                  {fields are valid}
    flags:Byte;                 {all tcp flags or'd}
    prot:Byte;                  {protocol id}
    tos:Byte;                   {type of service}
    srcS:Word;                  {source as}
    dstS:Word;                  {target as}
    srcM:Byte;                  {source mask}
    dstM:Byte;                  {target mask}
    flg2:Word;                  {fields are valid}
    router:LongInt;             {bypassing router}
    end;
Var
  serverAddr:OneTCPaddressRecord;
  serverPort:LongInt;
  serverPipe:LongInt;
  logFileName:String;
  logFileDate:LongInt;
  logFileHndr:xtText;
  logFileLock:Boolean;



Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function padUpWithZeros(i:LongInt):String;
Var a:String;
Begin;
a:=BStr(i);
while (length(a)<2) do a:='0'+a;
padUpWithZeros:=a;
End;

Function getCurrentDateBin:LongInt;
Var a,b,c:Word;
Begin;
xGetDate(a,b,c);
getCurrentDateBin:=(c shl 16) or (b shl 8) or c;
End;

Function getCurrentDateUnx:LongInt;
Var a,b,c,d,e,f:Word;
Begin;
xGetDate(a,b,c);
xGetTime(d,e,f);
getCurrentDateUnx:=unixTime_convertTo(a,b,c,d,e,f);
End;

Function getCurrentDateStr:String;
Var a,b,c:Word;
Begin;
xGetDate(a,b,c);
getCurrentDateStr:=padUpWithZeros(a)+'-'+padUpWithZeros(b)+'-'+padUpWithZeros(c);
End;

Function getCurrentTimeStr:String;
Var a,b,c:Word;
Begin;
xGetTime(a,b,c);
getCurrentTimeStr:=padUpWithZeros(a)+':'+padUpWithZeros(b)+':'+padUpWithZeros(c);
End;

Function adr2str(var buffer):String;
Var
  a:string;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
ab0:=4;
move(buffer,ab[1],ab0);
a:=IPv4addressPrefix+a;
adr2str:=ipAddr2string(ab[1]);
End;

Function unx2str(i:LongInt):String;
Var a,b,c,d,e,f:Word;
Begin;
unixTime_convertFrom(i,a,b,c,d,e,f);
unx2str:=padUpWithZeros(a)+'-'+padUpWithZeros(b)+'-'+padUpWithZeros(c)+' '
        +padUpWithZeros(d)+':'+padUpWithZeros(e)+':'+padUpWithZeros(f);
End;

Function upt2str(i:LongInt):String;
Var a:String;
Begin;
i:=i div 1000;
a:=BStr(i div 86400)+' days ';
i:=i mod 86400;
a:=a+padUpWithZeros(i div 3600)+':';
i:=i mod 3600;
a:=a+padUpWithZeros(i div 60)+':'+padUpWithZeros(i mod 60);
upt2str:=a;
End;

Procedure append2log(act:String);
Var
  i:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
i:=getCurrentDateBin;
if (i<>logFileDate) then begin;
  logFileDate:=i;
  xtClose(logFileHndr);
  a:=logFileName;
  i:=pos('%',a);
  if (i>0) then a:=copy(a,1,i-1)+getCurrentDateStr+copy(a,i+1,666);
  xCreate(a);
  if (xtOpen(logFileHndr,a,false)<>0) then immErr('error opening '+a+'!');
  end;
if (copy(act,1,1)='%') then act:=getCurrentTimeStr+' '+copy(act,2,666);
xtWriteLn(logFileHndr,act);
if logFileLock then exit;
xtClose(logFileHndr);
dec(logFileDate);
End;


Procedure ReadUpConfig(a:String);
Var t:xtText;

function gnl:string;
var
  i:longint;
  a:String;
begin;
a:=xtReadLn(t,255);
i:=pos(';',a);
if (i>0) then a:=copy(a,1,i-1);
a:=' '+a+' ';
kicserel('  ',' ',a);
a:=copy(a,2,length(a)-2);
gnl:=a;
end;

Begin;
WriteLn('reading '+a+'...');
if (xtOpen(t,a,true)<>0) then immErr('error opening!');
serverPort:=BVal(gnl);
logFileName:=gnl;
logFileLock:=(BVal(gnl)<>0);
xtClose(t);
End;




Procedure GotOnePacket(var adr,buffer;siz,prt:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  hdr7:netFlowV7headerRecord absolute buffer;
  hdr5:netFlowV5headerRecord absolute buffer;
  hdr1:netFlowV1headerRecord absolute buffer;
  dat7:netFlowV7dataRecord;
  dat5:netFlowV5dataRecord absolute dat7;
  dat1:netFlowV1dataRecord absolute dat7;
  ps,ds:LongInt;
  i,o:LongInt;
Begin;
if (siz<sizeof(hdr1)) then begin;
  WriteLn('got too short packet from '+ipAddr2string(adr)+' '+BStr(prt));
  exit;
  end;
case ReadWordMSB(hdr1.ver) of
  1:begin; ps:=sizeof(hdr1);ds:=sizeof(dat1); end;
  5:begin; ps:=sizeof(hdr5);ds:=sizeof(dat5); end;
  7:begin; ps:=sizeof(hdr7);ds:=sizeof(dat7); end;
  else begin;
    WriteLn('got invalid version from '+ipAddr2string(adr)+' '+BStr(prt));
    exit;
    end;
  end;
if (ReadWordMSB(hdr1.cnt)*ds+ps>siz) then begin;
  WriteLn('got truncated packet from '+ipAddr2string(adr)+' '+BStr(prt));
  exit;
  end;
inc(ps);
o:=ReadLongMSB(hdr1.upTime);
append2log('%from='+ipAddr2string(adr)+' '+BStr(prt)+' time='+unx2str(ReadLongMSB(hdr1.unixT))+' up='+upt2str(o));
o:=getCurrentDateUnx-(o div 1000);
for i:=1 to ReadWordMSB(hdr1.cnt) do begin;
  move(buf[ps],dat7,sizeof(dat7));
  inc(ps,ds);
  append2log(
   'tos='+BStr(dat1.tos)+
   ' prot='+BStr(dat1.prot)+
   ' src='+adr2str(dat1.srcA)+' '+BStr(ReadWordMSB(dat1.srcP))+
   ' trg='+adr2str(dat1.dstA)+' '+BStr(ReadWordMSB(dat1.dstP))+
   ' hop='+adr2str(dat1.hop)+
   ' pck='+BStr(ReadLongMSB(dat1.packs))+' oct='+BStr(ReadLongMSB(dat1.bytes))+
   ' in='+BStr(ReadWordMSB(dat1.ifIn))+' out='+BStr(ReadWordMSB(dat1.ifOut))+
   ' start='+unx2str(o+(ReadLongMSB(dat1.start) div 1000))+
   ' stop='+unx2str(o+(ReadLongMSB(dat1.stop) div 1000))
   );
  end;
End;



Label f1;
Var
  d:array[1..2*1024] of byte;
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('netflow server v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');

a:=ParamStr(1);
if (a='') then immErr('using: netflow.code <config>');
ReadUpConfig(a);
logFileDate:=0;

if (serverPort=0) then serverPort:=9991;
if UDPlistenOnPort(serverPipe,65536,serverAddr,serverPort) then immErr('failed to listen on udp port!');
WriteLn('listening on '+ipAddr2string(serverAddr)+' '+BStr(serverPort)+' udp port...');
unixTime_generateTable;

BugOS_SignDaemoning;
f1:
relequish;
o:=sizeof(d);
while not UDPreceivePacket(serverPipe,a,i,d,o) do begin;
  GotOnePacket(a,d,o,i);
  o:=sizeof(d);
  end;
goto f1;
END.