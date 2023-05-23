{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc hex.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Type
  oneNLRIrecord=record
    len:Byte;
    adr:OneTCPaddressRecord;
    end;
Var
  dataPipe:LongInt;
  localAS:LongInt;
  remoteAS:LongInt;
  localAddr:OneTCPaddressRecord;
  remoteAdr:OneTCPaddressRecord;
  bufDat:array[1..4096] of byte;
  bufSiz:LongInt;
  lastSent:LongInt;
  dataBuf:array[1..512] of oneNLRIrecord;
  dataNum:LongInt;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function convertNLRI(var dat:oneNLRIrecord):String;
Begin;
convertNLRI:=ipAddr2string(dat.adr)+'/'+BStr(dat.len);
End;

Function findNLRI(var dat:oneNLRIrecord):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to dataNum do begin;
  if (dataBuf[i].len<>dat.len) then continue;
  if not TCPcompareAddress(dataBuf[i].adr,dat.adr) then continue;
  goto f1;
  end;
i:=0;
f1:
findNLRI:=i;
End;


Procedure sendBGPmsg(a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
a:=#255#255#255#255#255#255#255#255#255#255#255#255#255#255#255#255#0#0+a;
WriteWordMSB(ab[17],ab0);
pipeLineSend(dataPipe,ab[1],ab0);
End;

Function recvBGPmsg:LongInt;
Var i,o,p:LongInt;
Begin;
recvBGPmsg:=0;
pipeLineStats(dataPipe,p,i,o);
if (p=0) then begin;
  pipeLineClose(dataPipe);
  dataPipe:=0;
  exit;
  end;
if (bufSiz<19) then o:=19 else o:=ReadWordMSB(bufDat[17]);
dec(o,bufSiz);
if (o<1) then begin;
  dec(bufSiz,18);
  move(bufDat[19],bufDat,bufSiz);
  recvBGPmsg:=bufSiz;
  bufSiz:=0;
  exit;
  end;
if (i<1) then exit;
pipeLineRecv(dataPipe,bufDat[bufSiz+1],o);
inc(bufSiz,o);
End;

Function getBGPmsg:String;
Var
  i:LongInt;
  a:String;
Begin;
getBGPmsg:='';
i:=recvBGPmsg;
if (i<1) then exit;
if (i>255) then i:=255;
move(bufDat,a[1],i);
a[0]:=chr(i);
getBGPmsg:=a;
End;



Procedure getRoute(var dat:oneNLRIrecord;var buffer;var p:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  i:LongInt;
Begin;
fillchar(dat,sizeof(dat),0);
i:=buf[p];
if (i<0) then i:=0;
if (i>128) then i:=128;
dat.len:=i;
inc(p);
i:=(i+7) shr 3;
move(buf[p],dat.adr,i);
inc(p,i);
End;

Function putRoute(var dat:oneNLRIrecord;var buffer):LongInt;
Var
  buf:array[1..1] of byte absolute buffer;
  i:LongInt;
Begin;
i:=dat.len;
if (i<0) then i:=0;
if (i>128) then i:=128;
buf[1]:=i;
i:=(i+7) shr 3;
move(dat.adr,buf[2],i);
putRoute:=i+1;
End;


Procedure addAttr(var b:String;typ:longint;a:String);
Var
  i:longint;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
i:=ab0;
a:='123'+a;
writeWordMSB(a[1],typ);
ab[3]:=i;
b:=b+a;
End;


Function putAttrs(aspt:String;orgn,med,prf:LongInt):String;
Var
  a,b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
b:='';
a:=chr(orgn);addAttr(b,$4001,a);
a:=aspt;addAttr(b,$4002,a);
ab0:=4;writeLongMSB(a[1],med);if (med<>0) then addAttr(b,$8004,a);
ab0:=4;writeLongMSB(a[1],prf);if (prf<>0) then addAttr(b,$4005,a);
putAttrs:=b;
End;



Procedure delRoute(var dat:oneNLRIrecord);
Var i,o:LongInt;
Begin;
WriteLn(' del '+convertNLRI(dat));
i:=findNLRI(dat);
if (i=0) then exit;
dataBuf[i]:=dataBuf[dataNum];
dec(dataNum);
End;

Procedure addRoute(var dat:oneNLRIrecord);
Var i,o:LongInt;
Begin;
WriteLn(' add '+convertNLRI(dat));
i:=findNLRI(dat);
if (i<>0) then exit;
inc(dataNum);
dataBuf[dataNum]:=dat;
End;


Function getAttrs(var buffer):LongInt;
Var
  dat:oneNLRIrecord;
  buf:array[1..1] of byte absolute buffer;
  i,o,p,q:LongInt;
Begin;
if (buf[1] and $10=0) then begin;
  q:=buf[3]+3;
  p:=4;
  end else begin;
  q:=readWordMSB(buf[3])+4;
  p:=5;
  end;
getAttrs:=q;
case buf[2] of
  14:begin; {update}
    inc(p,2);
    inc(p,buf[p+1]);
    inc(p,3);
    while (p<q) do begin;
      getRoute(dat,buf,p);
      addRoute(dat);
      end;
    end;
  15:begin; {withdraw}
    inc(p,3);
    while (p<q) do begin;
      getRoute(dat,buf,p);
      delRoute(dat);
      end;
    end;
  end;
End;


Procedure sendWithdraw(var dat:oneNLRIrecord);
Var
  a,b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
ab0:=putRoute(dat,ab[1]);
a:=#0#2#1+a;
b:='';
addAttr(b,$800f,a);
a:=b;
a:=#2#0#0#0+chr(ab0)+a;
sendBGPmsg(a);
End;


Procedure sendAnnounce(var dat:oneNLRIrecord);
Var
  a,b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
ab0:=putRoute(dat,ab[1]);
b:=a;
move(localAddr,ab[1],sizeof(localAddr));
ab0:=sizeof(localAddr);
a:=#0#2#1+chr(ab0)+a+#0+b;
b:='';
addAttr(b,$800e,a);
a:=putAttrs('',2,1,1)+b;
a:=#2#0#0#0+chr(ab0)+a;
sendBGPmsg(a);
End;



Function generateReply(var buffer):LongInt;
Var
  buf:array[1..1] of byte absolute buffer;
  i,o:LongInt;

Procedure add(a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
a:=a+#13#10;
move(ab[1],buf[o+1],ab0);
inc(o,ab0);
End;

Begin;
o:=0;
add('prefixes:');
for i:=1 to dataNum do add(convertNLRI(dataBuf[i]));
generateReply:=o;
End;




Label f1;
Var
  dat:oneNLRIrecord;
  buf:array[1..4096] of byte;
  i,o,p,q:LongInt;
  a:String;
BEGIN;
WriteLn('bgp listener v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');
if pipeLineBegListen then immErr('failed to start listening!');

localAS:=BVal(paramStr(1));
if string2ipAddr(paramStr(2),remoteAdr) then immErr('using: bgp.code <myAS> <peer> [port]');
i:=BVal(paramStr(3));
if (i=0) then i:=179;

Write('connecting to '+ipAddr2string(remoteAdr)+' '+BStr(i)+'...');
TCPbeginConnect(dataPipe,65536,remoteAdr,i);
while TCPlookConnected(dataPipe,localAddr,i,o) do begin;
  if (dataPipe=0) then immErr(' failed!');
  relequish;
  end;
WriteLn(' ok!');
WriteLn('local side is '+ipAddr2string(localAddr)+' '+BStr(i)+'...');
lastSent:=-999999;
bufSiz:=0;

string2ipaddr('1234::FFFF:A01:37',localAddr);
Write('exchanging open...');
a:=#1#4'12'#0#120'abcd'#8#2#6#1#4#0#2#0#1;
WriteWordMSB(a[3],localAS);
move(localAddr[13],a[7],4);
sendBGPmsg(a);
repeat
  a:=getBGPmsg;
  relequish;
  until (a<>'');
WriteLn(' ok!');
if (a[1]<>#1) then immErr('unknown type!');
if (a[2]<>#4) then immErr('unknown version!');
remoteAS:=readWordMSB(a[3]);
a:=IPv4addressPrefix+copy(a,7,4);
WriteLn('remote id='+ipAddr2string(a[1])+' as='+BStr(remoteAS)+'...');
dataNum:=0;

BugOS_SignDaemoning;
f1:
relequish;
timer2start;
o:=0;
while (pipeLineGetIncoming(p)=0) do begin;
  if (o=0) then o:=generateReply(buf);
  pipeLineSend(p,buf,o);
  pipeLineClose(p);
  end;
if (dataPipe=0) then immErr('remote closed connection!');
if (getTimePast(lastSent)>30) then begin;
  WriteLn('sending keepalive...');
  lastSent:=currentTime;
  sendBGPmsg(#4);
  end;
q:=recvBGPmsg;
if (q<1) then goto f1;
i:=bufDat[1];
if (i=4) then begin;
  WriteLn('got keepalive...');
  goto f1;
  end;
if (i<>2) then begin;
  WriteLn('got unknown packet!');
  halt(1);
  end;
WriteLn('got update...');
p:=2;
o:=ReadWordMSB(bufDat[p]);
inc(p,2);
inc(p,o);
o:=ReadWordMSB(bufDat[p]);
inc(p,2);
inc(o,p);
while (p<o) do inc(p,getAttrs(bufDat[p]));
goto f1;
END.