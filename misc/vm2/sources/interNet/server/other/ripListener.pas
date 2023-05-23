{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc hex.inc}
{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Const defaultPort=520;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Type
  OneMemoryRecord=record
    time:LongInt;
    from:LongInt;
    addr:LongInt;
    mask:LongInt;
    hop:LongInt;
    metr:LongInt;
    tag:LongInt;
    end;
  allRoutesType=array[1..1] of OneMemoryRecord;
Var
  allRoutesDat:^allRoutesType;
  allRoutesNum:LongInt;


Function ResizeMemory(n:LongInt):Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
ResizeMemory:=True;
i:=n*sizeof(OneMemoryRecord);
if (ExtendedMemoryResize(p,i)<i) then exit;
allRoutesNum:=n;
allRoutesDat:=p^;
ResizeMemory:=False;
End;


Function convAdr(i:LongInt):String;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
a:=IPv4addressPrefix;
move(i,ab[ab0+1],sizeof(i));
convAdr:=ipAddr2string(ab[1]);
End;

Function convAdrP(i:LongInt):String;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
a:=convAdr(i);
while (ab0<17) do begin;
  inc(ab0);
  ab[ab0]:=32;
  end;
convAdrP:=a;
End;

Function getClassfulMask(a:LongInt):LongInt;
Var
  ab:array[1..4] of byte absolute a;
  ab1:byte absolute a;
Begin;
case ab1 of
  0..127:if (a=0) then a:=0 else writeLongMSB(a,$ff000000);
  128..191:writeLongMSB(a,$ffff0000);
  192..223:writeLongMSB(a,$ffffff00);
  else writeLongMSB(a,$ffffffff);
  end;
getClassfulMask:=a;
End;



Procedure updateOneRoute(from,addr,mask,hop,metr,tag:LongInt);
Label f1;
Var
  i,o:LongInt;
Begin;
if (mask=0) then mask:=getClassfulMask(addr);
if (hop=0) then hop:=from;
if (metr<0) or (metr>16) then metr:=16;
for i:=1 to allRoutesNum do begin;
  if (from<>allRoutesDat^[i].from) then continue;
  if (addr<>allRoutesDat^[i].addr) then continue;
  if (mask<>allRoutesDat^[i].mask) then continue;
  if (tag<>allRoutesDat^[i].tag) then continue;
  goto f1;
  end;
if resizeMemory(allRoutesNum+1) then exit;
i:=allRoutesNum;
allRoutesDat^[i].from:=from;
allRoutesDat^[i].addr:=addr;
allRoutesDat^[i].mask:=mask;
allRoutesDat^[i].tag:=tag;
f1:
allRoutesDat^[i].hop:=hop;
allRoutesDat^[i].metr:=metr;
allRoutesDat^[i].time:=currentTime;
End;

Function doTimeOutOneEntry(var d:OneMemoryRecord):Boolean;
Begin;
doTimeOutOneEntry:=false;
if (getTimePast(d.time)<180) then exit;
if (d.metr>=16) then begin; doTimeOutOneEntry:=True;exit; end;
d.metr:=16;
d.time:=currentTime;
End;

Procedure timeOutAllEntries;
Var i,o:LongInt;
Begin;
o:=0;
for i:=1 to allRoutesNum do begin;
  if doTimeOutOneEntry(allRoutesDat^[i]) then continue;
  inc(o);
  allRoutesDat^[o]:=allRoutesDat^[i];
  end;
resizeMemory(o);
End;


Type
  OneRIPheader=record
    cmd:byte; {command}
    ver:byte; {version}
    res:word; {reserved}
    end;
  OneRIPentry=record
    af:word;     {address family}
    tag:word;    {route tag}
    adr:longint; {address}
    msk:longint; {subnet mask}
    hop:longint; {next hop}
    mtr:longint; {metric}
    end;



Procedure displayPacket(var data;size:LongInt);
Var
  buf:array[1..1] of byte absolute data;
  ps:LongInt;
  hdr:OneRIPheader;
  ntry:OneRIPentry;
  i,o:LongInt;
  a:String;
Begin;
move(data,hdr,sizeof(hdr));
writeln('version: v'+BStr(hdr.ver));
case hdr.cmd of
  1:a:='request';
  2:a:='response';
  3:a:='trace on';
  4:a:='trace off';
  5:a:='sun';
  6:a:='triggered request';
  7:a:='triggered response';
  8:a:='triggered acknowledgement';
  9:a:='update request';
  10:a:='update response';
  11:a:='update acknowledgement';
  end;
writeln('command: '+a);
ps:=sizeof(hdr);
WriteLn('address          subnet mask      next hop         tag    metric');
while (ps+sizeof(ntry)<=size) do begin;
  move(buf[ps+1],ntry,sizeof(ntry));
  inc(ps,sizeof(ntry));
  if (readWordMSB(ntry.af)<>2) then continue;
  write(convAdrP(ntry.adr));
  i:=ntry.msk;
  if (i=0) then i:=getClassfulMask(ntry.adr);
  write(convAdrP(i));
  write(convAdrP(ntry.hop));
  write(copy(BStr(ReadWordMSB(ntry.tag))+'       ',1,7));
  writeln(BStr(ReadLongMSB(ntry.mtr)));
  end;
End;


Procedure gotOnePacket(from:LongInt;var data;size:LongInt);
Var
  buf:array[1..1] of byte absolute data;
  ps:LongInt;
  hdr:OneRIPheader;
  ntry:OneRIPentry;
  i,o:LongInt;
  a:String;
Begin;
move(data,hdr,sizeof(hdr));
if (hdr.cmd<>2) then exit;
ps:=sizeof(hdr);
while (ps+sizeof(ntry)<=size) do begin;
  move(buf[ps+1],ntry,sizeof(ntry));
  inc(ps,sizeof(ntry));
  if (readWordMSB(ntry.af)<>2) then continue;
  updateOneRoute(from,ntry.adr,ntry.msk,ntry.hop,ReadLongMSB(ntry.mtr),ReadWordMSB(ntry.tag));
  end;
End;

Function generateReply(var buffer):LongInt;
Var
  buf:array[1..1] of byte absolute buffer;
  d:OneMemoryRecord;
  i,o,p:LongInt;

Procedure addStr(a:String);
Var ab0:byte absolute a;
Begin;
move(a[1],buf[p+1],ab0);
inc(p,ab0);
End;

Begin;
p:=0;
addStr('from              address           subnet            hop               metric'#13#10);
for i:=1 to allRoutesNum do begin;
  d:=allRoutesDat^[i];
  addStr(convAdrP(d.from)+convAdrP(d.addr)+convAdrP(d.mask)++convAdrP(d.hop)+BStr(d.metr)+#13#10);
  end;
generateReply:=p;
End;




Label f1,f2;
Var
  pipe,time:LongInt;
  a:String;
  i,o,p:LongInt;
  buf:array[1..32*1024] of byte;
BEGIN;
WriteLn('rip listener v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');

if pipeLineBegListen then immErr('failed to start listening!');

i:=defaultPort;
if UDPlistenOnPort(pipe,4096,a,i) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(a)+' '+BStr(i)+' port...');

allRoutesNum:=0;
ResizeMemory(0);
time:=0;

BugOS_SignDaemoning;
f1:
relequish;
timer2start;
if (GetTimePast(time)>15) then begin;
  timeOutAllEntries;
  time:=currentTime;
  end;
o:=0;
while (pipeLineGetIncoming(p)=0) do begin;
  if (o=0) then o:=generateReply(buf);
  pipeLineSend(p,buf,o);
  pipeLineClose(p);
  end;
f2:
i:=sizeof(buf);
if UDPreceivePacket(pipe,a,o,buf,i) then goto f1;
move(a[length(IPv4addressPrefix)],p,sizeof(p));
WriteLn('got packet from '+convAdr(p)+' '+BStr(o));
gotOnePacket(p,buf,i);
goto f2;
END.