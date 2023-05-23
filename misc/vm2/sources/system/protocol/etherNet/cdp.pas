{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$include \sources\internet\kernel\utils\checksum.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Const proggyName='cdp v1.0, done by Mc at '#%date' '#%time'.';

Type
  oneAddressRecord=array[1..6] of byte;
  onePacketRecord=record
    adr:oneAddressRecord;
    typ:Word;
    buf:array[1..4*1024] of byte;
    siz:LongInt;
    end;
  oneAdvertiseRecord=record
    addr:oneAddressRecord;      {ethernet address}
    time:LongInt;               {time last packet received}
    hold:LongInt;               {hold down time}
    pver:LongInt;               {protocol version}
    name:String;                {device id}
    port:String;                {port id}
    capa:String;                {capabilities}
    vers:String;                {version}
    plat:String;                {platform}
    adrs:String;                {addresses}
    end;
Var
  etherPipe:LongInt;
  localAddr:oneAddressRecord;
  broadAddr:oneAddressRecord;
  deviceName:String;
  neighborDat:^array[1..1] of oneAdvertiseRecord;
  neighborNum:LongInt;



Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function ResizeMemory(n:LongInt):Boolean;
Var
  p:Pointer;
  o:LongInt;
Begin;
ResizeMemory:=True;
o:=n*sizeof(oneAdvertiseRecord)+1024;
if (ExtendedMemoryResize(p,o)<o) then exit;
neighborDat:=p^;
neighborNum:=n;
ResizeMemory:=False;
End;

Function conv2hex(i:LongInt):String;
Begin;
conv2hex:=byte2hextype(i shr 24)+byte2hextype(i shr 16)+byte2hextype(i shr 8)+byte2hextype(i);
End;

Function convEtherAddr(var d:oneAddressRecord):String;
Var
  i:LongInt;
  a:String;
Begin;
a:='';
for i:=1 to sizeof(d) do a:=a+'-'+byte2hextype(d[i]);
convEtherAddr:=copy(a,2,255);
End;

Function compareEtherAddr(var a1,a2:oneAddressRecord):Boolean;
Var i:LongInt;
Begin;
compareEtherAddr:=False;
for i:=1 to sizeof(a1) do if (a1[i]<>a2[i]) then exit;
compareEtherAddr:=True;
End;

Function convText(a:String):String;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
for i:=1 to ab0 do if (ab[i]<32) then ab[i]:=32;
while ((ab0>0) and (ab[1]=32)) do a:=copy(a,2,666);
convText:=a;
End;


Function parseCDPpacket(var pck:onePacketRecord;var adv:oneAdvertiseRecord):Boolean;
Label f1,f2;
Const maxSiz=255;
Var
  i,o,p:LongInt;
  a,b,c:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;

Function getTag(siz:LongInt):String;
Var
  i,o:LongInt;
  b:String;
  bb:array[0..1] of byte absolute b;
  bb0:byte absolute b;
Begin;
if (siz=2) then begin;
  o:=readWordMSB(ab[1]);
  siz:=2;
  end else begin;
  o:=ab[1];
  siz:=1;
  end;
i:=o;
if (i>maxSiz) then i:=maxSiz;
bb0:=i;
move(ab[siz+1],bb[1],i);
a:=copy(a,o+siz+1,666);
getTag:=b;
End;

Function hexer(a:String):String;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  b:String;
  i:LongInt;
Begin;
b:='';
for i:=1 to ab0 do b:=b+'-'+byte2hextype(ab[i]);
hexer:=copy(b,2,666);
End;

Begin;
parseCDPpacket:=True;
fillchar(adv,sizeof(adv),0);
adv.time:=currentTime;
adv.addr:=pck.adr;
i:=readWordMSB(pck.typ);
if (i<>$2000) then begin;
  WriteLn('got invalid type ($'+byte2hextype(i shr 8)+byte2hextype(i)+') from '+convEtherAddr(pck.adr));
  exit;
  end;
i:=pck.buf[1];
adv.pver:=i;
if not (i in [1,2]) then begin;
  WriteLn('got invalid version ('+BStr(i)+') from '+convEtherAddr(pck.adr));
  exit;
  end;
adv.hold:=pck.buf[2];
if (CalculateSum(pck.buf,pck.siz)<>$ffff) then begin;
  WriteLn('got invalid checksum from '+convEtherAddr(pck.adr));
  exit;
  end;
p:=4;
f1:
if (p>=pck.siz) then goto f2;
o:=readWordMSB(pck.buf[p+3])-4;
if (o<0) then o:=0;
i:=o;
if (i>maxSiz) then i:=maxSiz;
ab0:=i;
move(pck.buf[p+5],ab[1],i);
i:=readWordMSB(pck.buf[p+1]);
inc(p,o+4);
case i of
  1:adv.name:=convText(a);
  2:begin;
    i:=readLongMSB(ab[1]);
    a:=copy(a,5,666);
    while (ab0>0) do begin;
      i:=ab[1];
      a:=copy(a,2,666);
      b:=getTag(1);
      c:=getTag(2);
      if (b=#$cc) then begin;
        c:=IPv4addressPrefix+c;
        adv.adrs:=adv.adrs+' ip4='+ipAddr2string(c[1]);
        end;
      if (b=#170#170#3#0#0#0#8#0) then adv.adrs:=adv.adrs+' ip6='+ipAddr2string(c[1]);
      if (b=#170#170#3#0#0#0#129#55) then adv.adrs:=adv.adrs+' ipx='+hexer(c);
      end;
    end;
  3:adv.port:=convText(a);
  4:begin;
    a:=adv.capa;
    i:=readLongMSB(ab[1]);
    if (i and $01<>0) then a:=a+' router';
    if (i and $02<>0) then a:=a+' bridge';
    if (i and $04<>0) then a:=a+' srcrut-bridge';
    if (i and $08<>0) then a:=a+' switch';
    if (i and $10<>0) then a:=a+' host';
    if (i and $20<>0) then a:=a+' noigrp';
    if (i and $40<>0) then a:=a+' repeater';
    adv.capa:=a;
    end;
  5:adv.vers:=convText(a);
  6:adv.plat:=convText(a);
  7:begin;
    while (ab0>0) do begin;
      b:=IPv4addressPrefix+a;
      adv.adrs:=adv.adrs+' msk='+ipAddr2string(b[1])+'/'+BStr(ab[5]);
      a:=copy(a,6,666);
      end;
    end;
  else writeln('got invalid tag ('+BStr(i)+'='+convText(a)+') from '+convEtherAddr(pck.adr));
  end;
goto f1;
f2:
adv.adrs:=convText(adv.adrs);
adv.capa:=convText(adv.capa);
parseCDPpacket:=False;
End;



Procedure createCDPpacket(var pck:onePacketRecord;var adv:oneAdvertiseRecord);

Procedure add(t:LongInt;a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
writeWordMSB(pck.buf[pck.siz+1],t);
writeWordMSB(pck.buf[pck.siz+3],ab0+4);
move(ab[1],pck.buf[pck.siz+5],ab0);
inc(pck.siz,ab0+4);
End;

Var
  w:Word;
  a:String;
  i:LongInt;
Begin
a:=#1#0#12#204#204#204;
move(a[1],pck.adr,sizeof(pck.adr));
writeWordMSB(pck.typ,$2000);
pck.buf[1]:=1;
pck.buf[2]:=adv.hold;
pck.siz:=4;
a:=adv.name;if (a<>'') then add(1,a);
a:=adv.port;if (a<>'') then add(3,a);
a:=kicsi(adv.capa);
i:=0;
if (pos('host',a)<>0) then i:=i or $10;
if (pos('switch',a)<>0) then i:=i or $08;
if (pos('router',a)<>0) then i:=i or $01;
if (i<>0) then add(4,#0#0#0+chr(i));
a:=adv.vers;if (a<>'') then add(5,a);
a:=adv.plat;if (a<>'') then add(6,a);
w:=0;
move(w,pck.buf[3],sizeof(w));
w:=not CalculateSum(pck.buf,pck.siz);
move(w,pck.buf[3],sizeof(w));
End;

Function getHostName:String;
Var t:xtText;
Begin;
getHostName:='localHost?!';
if (xtOpen(t,'c:\system\localHost.text',true)<>0) then exit;
getHostName:=xtReadLn(t,255);
xtClose(t);
End;

Procedure doPurging(var d:oneAdvertiseRecord;var max:LongInt);
Begin;
if (getTimePast(d.time)>d.hold) then begin;
  writeln('neighbor '+convEtherAddr(d.addr)+' died');
  exit;
  end;
inc(max);
neighborDat^[max]:=d;
End;

Procedure generateReport(var d:oneAdvertiseRecord;var b;var s:LongInt);
Var buf:array[1..1] of byte absolute b;

procedure add(a:string);
var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
begin;
move(a[1],buf[s+1],ab0);
inc(s,ab0);
end;

Begin;
s:=0;
add('---'#13#10);
add('etheraddr='+convEtherAddr(d.addr));
add(' holdtime='+BStr(getTimePast(d.time))+'/'+BStr(d.hold));
add(' protver='+BStr(d.pver)+#13#10);
add('name="');add(d.name);add('"'#13#10);
add('port="');add(d.port);add('"'#13#10);
add('addresses="');add(d.adrs);add('"'#13#10);
add('capabilities="');add(d.capa);add('"'#13#10);
add('version="');add(d.vers);add('"'#13#10);
add('platform="');add(d.plat);add('"'#13#10);
End;



Label f1;
Var
  lastSent:LongInt;
  buf:array[1..5*1024] of byte;
  pck:onePacketRecord absolute buf;
  own:oneAdvertiseRecord;
  adv:oneAdvertiseRecord;
  i,o,p:LongInt;
  a:String;
BEGIN;
WriteLn(proggyName);

a:=ParamStr(1);
if (a='') then immErr('using: cdp.code <process> [router/host]');
WriteLn('process: '+a);
o:=BugOS_findProcNam(a);
if (o=0) then immErr('process not found!');
WriteLn('process#: '+BStr(o));
i:=pipeLineCreate(etherPipe,o,65536,true);
if (i<>0) then immErr('unabled to create pipeline!');
WriteLn('pipeline#: '+BStr(etherPipe));
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(etherPipe,buf,i)<>0) then i:=0;
if (i<1) then immErr('initial packet not received!');
move(buf[1],i,sizeof(i));WriteLn('address size: '+BStr(i));
move(buf[5],i,sizeof(i));WriteLn('packet size: '+BStr(i));
move(buf[9],i,sizeof(i));WriteLn('io base: '+conv2hex(i));
move(buf[13],i,sizeof(i));WriteLn('mem base: '+conv2hex(i));
o:=17;
move(buf[o],localAddr,sizeof(localAddr));inc(o,sizeof(localAddr));
move(buf[o],broadAddr,sizeof(broadAddr));inc(o,sizeof(broadAddr));
deviceName:='';
while (buf[o]<>0) do begin;
  deviceName:=deviceName+chr(buf[o]);
  inc(o);
  end;
WriteLn('station address: '+convEtherAddr(localAddr));
WriteLn('broadcast address: '+convEtherAddr(broadAddr));
WriteLn('device name: "'+deviceName+'"');

own.name:=getHostName;
own.port:=deviceName;
own.vers:=proggyName;
own.plat:='BugOS';
own.capa:=paramStr(2);
own.hold:=190;
createCDPpacket(pck,own);
parseCDPpacket(pck,own);
lastSent:=0;
timer2start;
neighborNum:=0;
ResizeMemory(0);

pipeLineBegListen;
BugOS_SignDaemoning;

f1:
o:=sizeof(buf);
if (pipeLineRecv(etherPipe,buf,o)=0) then begin;
  pck.siz:=o-8;
  if parseCDPpacket(pck,adv) then goto f1;
  writeln('got good packet from '+convEtherAddr(pck.adr));
  for i:=1 to neighborNum do if compareEtherAddr(pck.adr,neighborDat^[i].addr) then begin;
    neighborDat^[i]:=adv;
    goto f1;
    end;
  o:=neighborNum+1;
  if ResizeMemory(o) then begin;
    WriteLn('failed to allocate memory!');
    goto f1;
    end;
  neighborDat^[o]:=adv;
  goto f1;
  end;
while (pipeLineGetIncoming(p)=0) do begin;
  for i:=1 to neighborNum do begin;
    generateReport(neighborDat^[i],buf,o);
    pipeLineSend(p,buf,o);
    end;
  pipeLineClose(p);
  end;
relequish;
timer2start;
if (getTimePast(lastSent)<60) then goto f1;
lastSent:=currentTime;
o:=0;
for i:=1 to neighborNum do doPurging(neighborDat^[i],o);
neighborNum:=o;
ResizeMemory(neighborNum);
if (own.capa='') then goto f1;
createCDPpacket(pck,own);
pipeLineSend(etherPipe,pck,pck.siz+8);
WriteLn('advertisement sent out right now.');
goto f1;
END.