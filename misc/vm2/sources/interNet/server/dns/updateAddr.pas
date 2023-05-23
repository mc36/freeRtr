{$define debug}
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
{$sysinc random.inc}
{$sysinc datetime.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}

{$include memory.inc}
{$include struct.inc}
{$include zones.inc}

Procedure tryToOpen(var f:xFile;a:String;m:LongInt);
Label f1;
Var i:LongInt;
Begin;
WriteLn('opening '+a+'...');
f1:
i:=xOpen(f,a,m);
if (i=0) then exit;
if (i=5) then begin; relequish;goto f1; end;
immErr('error: '+xGetErrorName(i));
End;

Var
  d:OneZoneEntryHeaderRec;
  a,b,c:String;
  i,o,p,q,w,z:LongInt;
  buf1:array[1..1024] of byte;
  buf2:array[1..1024] of byte;
  bufS:LongInt;
  h1,h2:xFile;
BEGIN;
WriteLn('dns zone updater v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<>3) then immErr('using: updater <zonedir> <name> <address>');
timer2start;
TempPath:=paramStr(1);
if (copy(TempPath,length(TempPath),255)<>'\') then TempPath:=TempPath+'\';
c:=paramStr(2);
if string2ipAddr(paramStr(3),buf1) then immErr('invalid address!');
WriteLn('zone dir: '+tempPath);
WriteLn('new address: '+ipAddr2string(buf1));
a:=GetParentalDomainName(c);
WriteLn('zone: '+a);
WriteLn('name: '+c);
z:=FindOneZoneName(a);
if (z<1) then immErr('zone not found!');
tryToOpen(h1,TempPath+ZonesDescriptorFile+ZoneSortedIndexPlus,xGenFilMod_r);
xSeek(h1,(z-1)*sizeof(z));
xBlockRead(h1,z,sizeof(z));
xClose(h1);
z:=(z div sizeof(OneZoneDescriptorRec))+1;
a:=TempPath+OneZoneFilePostFix+BStr(z);
tryToOpen(h1,a,xGenFilMod_rw or xFileMode_allowRead);
a:=a+ZoneSortedIndexPlus;
tryToOpen(h2,a,xGenFilMod_rw or xFileMode_allowRead);
if isAddressIPv4mask(buf1) then begin;
  move(buf1[sizeof(OneTCPaddressRecord)-3],buf1,4);
  bufS:=4;
  o:=Type_A;
  end else begin;
  bufS:=sizeof(OneTCPaddressRecord);
  o:=Type_AAAA;
  end;
WriteLn('type: ('+BStr(o)+') '+GetQTypeName(o));
if not FindOneDomainName(h1,h2,c,o,q,w) then immErr('name not found!');
xSeek(h2,(q-1)*sizeof(q));
xBlockRead(h2,q,sizeof(q));
xSeek(h1,q);
xBlockRead(h1,d,sizeof(d));
p:=xFilePos(h1)+d.nameSize+sizeof(OneAnswerHeader);
dec(d.dataSize,sizeof(OneAnswerHeader));
xSeek(h1,p);
xBlockRead(h1,buf2,d.dataSize);
xSeek(h1,p);
if (d.dataSize<>bufS) then immErr('data sizes are not equal!');
o:=0;
for i:=1 to bufS do if (buf1[i]<>buf2[i]) then inc(o);
if (o<1) then begin;
  WriteLn('data records equal, skipping update!');
  Halt(0);
  end;
WriteLn('updating database...');
if (xBlockWrite(h1,buf1,bufS)<>0) then immErr('failed!');
xClose(h2);
xClose(h1);
UpdateLastTimeAndSerial(z,2);
WriteLn('successfully finished!');
END.