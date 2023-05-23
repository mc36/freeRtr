Type
  OneZoneDescriptorRec=record
    typ:byte;                   {type: 1-fixed, 2-zoneXfer}
    name:string[248];           {name of zone}
    host:string[248];           {host/file where from load it}
    downable:boolean;           {available for axfr}
    lastCheck:longint;          {last time this was checked}
    hostPort:longint;           {port to check}
    prntNum:longint;            {parent zone of this zone}
    prntPos:longint;            {parent position of this zone}
    end;
  OneZoneEntryHeaderRec=record
    typ:byte;                   {type: 1-rr, 2-zone}
    nameSize:Byte;              {size of the name}
    dataSize:Word;              {size of the data}
    dataType:longint;           {data, typ1=qType, typ2=zoneNum}
    name2sort:String;           {name prepared for sorting}
    end;
Const
  ZonesDescriptorFile='zones';
  CurrentDownZonePlus='.new';
  ZoneSortedIndexPlus='.idx';
  OneZoneFilePostFix='zone-';


Function ConvertNameTypeForDomainSearch(b:String;t:Word):String;
Var
  bb:array[0..255] of byte absolute b;
  bs:byte absolute b;
  i:LongInt;
Begin;
for i:=1 to bs do b[i]:=LowCase(b[i]);
move(t,bb[sizeof(b)-sizeof(t)],sizeof(t));
move(b[1],bb[sizeof(b)-sizeof(t)-bs],bs);
for i:=1 to sizeof(b)-sizeof(t)-bs-1 do bb[i]:=0;
bs:=sizeof(b)-1;
ConvertNameTypeForDomainSearch:=b;
End;

Procedure ReadUpZoneFile(deep:LongInt;var t:xtText;name:string);
Label f1,f2;
Const
  ZoneBeg='...zone';
Var
  MyZoneNum:LongInt;
  myZoneTtl:LongInt;
  h1,h2,h3:xFile;
  tt:xtText;
  a,b,c:String;
  i:LongInt;
  zd:OneZoneDescriptorRec;
  hr:OneZoneEntryHeaderRec;
  ip:OneTCPaddressRecord;

Function GetDomainName(s:String):String;
Begin;
if (s='@') then s:=name;
if (copy(s,length(s)-1,255)='.@') then s:=copy(s,1,length(s)-1)+name;
GetDomainName:=s;
End;

Procedure AddOneRRrecord(t:word;q,a:String);
Var
  buf:array[1..1024] of byte;
  hr:OneZoneEntryHeaderRec absolute buf;
  hdr:OneAnswerHeader;
  b:string;
  i:LongInt;
  siz:word;
Begin;
i:=xFilePos(h1);
xBlockWrite(h2,i,sizeof(i));
b:=ConvertNameTypeForDomainSearch(q,t);
hr.typ:=1;
hr.dataType:=t;
hr.name2sort:=b;
b:=ConvertDomain2protocol(q);
hr.nameSize:=length(b);
hr.dataSize:=length(a)+sizeof(OneAnswerHeader);
WriteWordMSB(hdr.qtype,t);
WriteWordMSB(hdr.class,Class_IN);
WriteLongMSB(hdr.ttl,myZoneTtl);
WriteWordMSB(hdr.len,length(a));
siz:=sizeof(hr);
move(b[1],buf[siz+1],length(b));inc(siz,length(b));
move(hdr,buf[siz+1],sizeof(hdr));inc(siz,sizeof(hdr));
move(a[1],buf[siz+1],length(a));inc(siz,length(a));
xBlockWrite(h1,buf,siz);
End;

Function long2str(i:LongInt):String;
Var
  a:String;
  d:array[0..255] of byte absolute a;
Begin;
d[0]:=4;
writeLongMSB(d[1],i);
long2str:=a;
End;

Function bin2str(var dat;siz:LongInt):String;
Var a:String;
Begin;
a[0]:=chr(siz);
move(dat,a[1],siz);
bin2str:=a;
End;


Begin;
MyZoneNum:=ZonesNum;
a:=TempPath+OneZoneFilePostFix+BStr(ZonesNum);
xErase(a);xCreate(a);
xOpen(h1,a,xGenFilMod_rw);
a:=a+ZoneSortedIndexPlus;
xErase(a);xCreate(a);
xOpen(h2,a,xGenFilMod_rw);
if (deep=0) then begin;
  myZoneTtl:=86400;
  goto f1;
  end;
a:=GetDomainName(xLevesz(xtReadLn(t,255),true));
c:=ConvertDomain2protocol(a);
a:=GetDomainName(xLevesz(xtReadLn(t,255),true));
c:=c+ConvertDomain2protocol(a);
a:=kicsi(xLevesz(xtReadLn(t,255),true));
if (a='rnd') then a:=BStr(Random($7fffffff));
if (a='date') then a:=GetCurrentDate+' 01';
kicserel('-','',a);
kicserel(' ','',a);
c:=c+long2str(BVal(a));
c:=c+long2str(BVal(xLevesz(xtReadLn(t,255),true)));
c:=c+long2str(BVal(xLevesz(xtReadLn(t,255),true)));
c:=c+long2str(BVal(xLevesz(xtReadLn(t,255),true)));
c:=c+long2str(BVal(xLevesz(xtReadLn(t,255),true)));
myZoneTtl:=BVal(xLevesz(xtReadLn(t,255),true));
if (myZoneTtl<1) then myZoneTtl:=86400;
AddOneRRrecord(Type_SOA,name,c);
f1:
if xtEOF(t) then begin;
  xClose(h2);
  xClose(h1);
  Exit;
  end;
b:=xtReadLn(t,255);
b:=xLevesz(b,true);
if (b='') then goto f1;
a:=GetNextString(b);
fillchar(hr,sizeof(hr),0);
if (kicsi(a)=ZoneBeg) then begin;
  fillchar(zd,sizeof(zd),0);
  zd.lastCheck:=-99999;
  zd.name:=GetDomainName(GetNextString(b));
  zd.prntNum:=MyZoneNum;
  zd.prntPos:=xFileSize(h1)+sizeof(hr);
  a:=GetNextString(b);
  i:=0;
  if (a='file') then i:=1;
  if (a='host') then i:=2;
  if (i=0) then goto f1;
  zd.downable:=(BVal(GetNextString(b))=1);
  zd.typ:=i;
  if (i=2) then begin;
    a:=GetNextString(b);
    if string2ipAddr(a,ip) then goto f1;
    i:=BVal(b);
    if (i=0) then i:=serverPort;
    zd.hostPort:=i;
    b:=bin2str(ip,sizeof(ip));
    end;
  zd.host:=b;
  if (zd.typ=1) then if (xtOpen(tt,zd.host,true)<>0) then goto f1;
  inc(ZonesNum);
  a:=TempPath+OneZoneFilePostFix+BStr(ZonesNum);
  xErase(a);xCreate(a);
  a:=a+ZoneSortedIndexPlus;
  xErase(a);xCreate(a);
  a:=TempPath+ZonesDescriptorFile;
  xOpen(h3,a,xGenFilMod_rw);
  i:=xFileSize(h3);
  xSeek(h3,i);
  xBlockWrite(h3,zd,sizeof(zd));
  xClose(h3);
  a:=a+ZoneSortedIndexPlus;
  if zd.downable then begin;
    xOpen(h3,a,xGenFilMod_rw);
    xSeek(h3,xFileSize(h3));
    xBlockWrite(h3,i,sizeof(i));
    xClose(h3);
    end;
  hr.typ:=2;
  hr.nameSize:=0;
  hr.dataSize:=0;
  hr.dataType:=ZonesNum;
  hr.name2sort:='';
  xBlockWrite(h1,hr,sizeof(hr));
  writeln('adding zone '+zd.name+'...');
  if (zd.typ=2) then goto f1;
  ReadUpZoneFile(deep+1,tt,zd.name);
  xtClose(tt);
  Goto f1;
  end;
c:=GetDomainName(a);
a:=kicsi(GetNextString(b));
if not string2ipAddr(c,ip) then c:=ConvertIPaddr2ptrName(ip);
if (a='a') then begin;
  a:=GetNextString(b);
  if string2ipAddr(a,ip) then Goto f1;
  a:=bin2str(ip[sizeof(ip)-4+1],4);
  AddOneRRrecord(Type_A,c,a);
  Goto f1;
  end;
if (a='aaaa') then begin;
  a:=GetNextString(b);
  if string2ipAddr(a,ip) then Goto f1;
  a:=bin2str(ip,sizeof(ip));
  AddOneRRrecord(Type_AAAA,c,a);
  Goto f1;
  end;
if (a='ptr') then begin;
  a:=GetDomainName(GetNextString(b));
  AddOneRRrecord(Type_PTR,c,ConvertDomain2protocol(a));
  Goto f1;
  end;
if (a='cname') then begin;
  a:=GetDomainName(GetNextString(b));
  AddOneRRrecord(Type_CNAME,c,ConvertDomain2protocol(a));
  Goto f1;
  end;
if (a='ns') then begin;
  a:=GetDomainName(GetNextString(b));
  AddOneRRrecord(Type_NS,c,ConvertDomain2protocol(a));
  Goto f1;
  end;
if (a='mx') then begin;
  i:=BVal(GetNextString(b));
  a:=GetDomainName(GetNextString(b));
  AddOneRRrecord(Type_MX,c,copy(long2str(i),3,2)+ConvertDomain2protocol(a));
  Goto f1;
  end;
if (a='txt') then begin;
  a:=ConvertString2protocol(GetNextString(b));
  AddOneRRrecord(Type_TXT,c,a);
  Goto f1;
  end;
if (a='spf') then begin;
  a:=ConvertString2protocol(GetNextString(b));
  AddOneRRrecord(Type_SPF,c,a);
  Goto f1;
  end;
if (a='hinfo') then begin;
  a:=ConvertString2protocol(GetNextString(b));
  a:=a+ConvertString2protocol(GetNextString(b));
  AddOneRRrecord(Type_HINFO,c,a);
  Goto f1;
  end;
if (a='gpos') then begin;
  a:=ConvertString2protocol(GetNextString(b));
  a:=a+ConvertString2protocol(GetNextString(b));
  a:=a+ConvertString2protocol(GetNextString(b));
  AddOneRRrecord(Type_HINFO,c,a);
  Goto f1;
  end;
if (a='rp') then begin;
  a:=ConvertDomain2protocol(GetDomainName(GetNextString(b)));
  a:=a+ConvertDomain2protocol(GetDomainName(GetNextString(b)));
  AddOneRRrecord(Type_RP,c,a);
  Goto f1;
  end;
if (a='x25') then begin;
  a:=ConvertString2protocol(GetNextString(b));
  AddOneRRrecord(Type_X25,c,a);
  Goto f1;
  end;
if (a='isdn') then begin;
  a:=ConvertString2protocol(GetNextString(b));
  a:=a+ConvertString2protocol(GetNextString(b));
  AddOneRRrecord(Type_ISDN,c,a);
  Goto f1;
  end;
if (a='rt') then begin;
  i:=BVal(GetNextString(b));
  a:=GetDomainName(GetNextString(b));
  AddOneRRrecord(Type_RT,c,copy(long2str(i),3,2)+ConvertDomain2protocol(a));
  Goto f1;
  end;
Goto f1;
End;




Procedure SortRRsInOneZone(i:LongInt);
Var
  d:OneZoneEntryHeaderRec;
  h1,h2:xFile;
  a:String;

Function QuickReadOne(n:LongInt):String;
Begin;
xSeek(h2,(n-1)*sizeof(n));
xBlockRead(h2,n,sizeof(n));
xSeek(h1,n);
xBlockRead(h1,d,sizeof(d));
QuickReadOne:=d.name2sort;
End;

Procedure QuickSwapOne(n1,n2:LongInt);
Var d1,d2:LongInt;
Begin;
n1:=(n1-1)*sizeof(n1);
n2:=(n2-1)*sizeof(n2);
xSeek(h2,n1);xBlockRead(h2,d1,sizeof(d1));
xSeek(h2,n2);xBlockRead(h2,d2,sizeof(d2));
xSeek(h2,n2);xBlockWrite(h2,d1,sizeof(d1));
xSeek(h2,n1);xBlockWrite(h2,d2,sizeof(d2));
End;

{$sysinc quicksrt.inc}

Begin;
a:=TempPath+OneZoneFilePostFix+BStr(i);
xOpen(h1,a,xGenFilMod_r);
a:=a+ZoneSortedIndexPlus;
xOpen(h2,a,xGenFilMod_rw);
i:=xFileSize(h2) div sizeof(i);
if (i>1) then QuickSort(1,i);
xClose(h1);
xClose(h2);
End;










Procedure ReadUpConfig(a:String);
Var
  d:OneZoneDescriptorRec;
  h1,h2:xFile;
  i:LongInt;
  t:xtText;

Function QuickReadOne(n:LongInt):String;
Begin;
xSeek(h2,(n-1)*sizeof(n));
xBlockRead(h2,n,sizeof(n));
xSeek(h1,n);
xBlockRead(h1,d,sizeof(d));
QuickReadOne:=kicsi(d.name);
End;

Procedure QuickSwapOne(n1,n2:LongInt);
Var d1,d2:LongInt;
Begin;
n1:=(n1-1)*sizeof(n1);
n2:=(n2-1)*sizeof(n2);
xSeek(h2,n1);xBlockRead(h2,d1,sizeof(d1));
xSeek(h2,n2);xBlockRead(h2,d2,sizeof(d2));
xSeek(h2,n2);xBlockWrite(h2,d1,sizeof(d1));
xSeek(h2,n1);xBlockWrite(h2,d2,sizeof(d2));
End;

{$sysinc quicksrt.inc}

Begin;
if (xtOpen(t,a,true)<>0) then ImmErr('error reading config file!');
serverPort:=BVal(xLevesz(xtReadLn(t,255),true));
if (serverPort=0) then serverPort:=53;
TempPath:=xLevesz(xtReadLn(t,255),true);
if (copy(TempPath,length(TempPath),255)<>'\') then TempPath:=TempPath+'\';
fershIntrvl:=BVal(xLevesz(xtReadLn(t,255),true));
FrwrdReq:=(BVal(xLevesz(xtReadLn(t,255),true)) and 1=1);

WriteLn('reading zones...');
ZonesNum:=0;
a:=TempPath+ZonesDescriptorFile;
xErase(a);xCreate(a);
a:=a+ZoneSortedIndexPlus;
xErase(a);xCreate(a);
ReadUpZoneFile(0,t,'');
xtClose(t);

WriteLn('sorting zones...');
a:=TempPath+ZonesDescriptorFile;
xOpen(h1,a,xGenFilMod_r);
a:=a+ZoneSortedIndexPlus;
xOpen(h2,a,xGenFilMod_rw);
i:=xFileSize(h2) div sizeof(i);
if (i>1) then QuickSort(1,i);
xClose(h2);
SortRRsInOneZone(0);
xSeek(h1,0);
for i:=1 to ZonesNum do begin;
  xBlockRead(h1,d,sizeof(d));
  writeln('sorting zone '+d.name+'...');
  SortRRsInOneZone(i);
  end;
xClose(h1);
End;










Function FindOneZoneName(b:String):LongInt;
Var
  d:OneZoneDescriptorRec;
  h1,h2:xFile;
  i:LongInt;
  a:String;

Function QuickReadOne(n:LongInt):String;
Begin;
xSeek(h2,(n-1)*sizeof(n));
xBlockRead(h2,n,sizeof(n));
xSeek(h1,n);
xBlockRead(h1,d,sizeof(d));
QuickReadOne:=kicsi(d.name);
End;

Procedure QuickSwapOne(n1,n2:LongInt);Begin;End;

{$sysinc quicksrt.inc}

Begin;
a:=TempPath+ZonesDescriptorFile;
xOpen(h1,a,xGenFilMod_r);
a:=a+ZoneSortedIndexPlus;
xOpen(h2,a,xGenFilMod_r);
i:=xFileSize(h2) div sizeof(i);
FindOneZoneName:=QuickFind(1,i,kicsi(b));
xClose(h2);
xClose(h1);
End;



Function FindOneDomainName(var h1,h2:xFile;b:String;t:Word;var first,last:LongInt):Boolean;
Label vege;
Var
  d:OneZoneEntryHeaderRec;
  bb:byte absolute b;

Function QuickReadOne(n:LongInt):String;
Begin;
xSeek(h2,(n-1)*sizeof(n));
xBlockRead(h2,n,sizeof(n));
xSeek(h1,n);
xBlockRead(h1,d,sizeof(d));
if (t=Type_ANY) then begin;
  b:=d.name2sort;
  dec(bb,sizeof(t));
  QuickReadOne:=b;
  end else QuickReadOne:=d.name2sort;
End;

Procedure QuickSwapOne(n1,n2:LongInt);Begin;End;

{$sysinc quicksrt.inc}

Var
  i,o:LongInt;
  a:String;
Begin;
FindOneDomainName:=False;
first:=-1;
last:=-2;
a:=ConvertNameTypeForDomainSearch(b,t);
o:=xFileSize(h2) div sizeof(i);
if (t=Type_ANY) then begin; b:=a;dec(bb,sizeof(t));a:=b; end;
i:=QuickFind(1,o,a);
if (i<1) then exit;
first:=i;
last:=i;
while (first>1) do begin;
  dec(first);
  if (QuickReadOne(first)=a) then continue;
  inc(first);
  break;
  end;
while (last<o) do begin;
  inc(last);
  if (QuickReadOne(last)=a) then continue;
  dec(last);
  break;
  end;
FindOneDomainName:=True;
End;



Procedure UpdateLastTimeAndSerial(zone,parents:LongInt);
Label f1,f2;
Var
  pck:OnePacketRecord;
  zhdr:OneZoneDescriptorRec;
  rhdr:OneZoneEntryHeaderRec;
  ahdr:OneAnswerHeader;
  i,o,p:LongInt;
  f:xFile;
Begin;
xOpen(f,TempPath+ZonesDescriptorFile,xGenFilMod_rw);
xSeek(f,(zone-1)*sizeof(zhdr));
xBlockRead(f,zhdr,sizeof(zhdr));
zhdr.lastCheck:=CurrentTime;
xSeek(f,(zone-1)*sizeof(zhdr));
xBlockWrite(f,zhdr,sizeof(zhdr));
xClose(f);
if (parents=1) then exit;
f1:
if (parents=0) then zone:=zhdr.prntNum;
if (zone<1) then exit;
xOpen(f,TempPath+ZonesDescriptorFile,xGenFilMod_r);
xSeek(f,(zone-1)*sizeof(zhdr));
xBlockRead(f,zhdr,sizeof(zhdr));
xClose(f);
xOpen(f,TempPath+OneZoneFilePostFix+BStr(zone),xGenFilMod_rw);
xBlockRead(f,rhdr,sizeof(rhdr));
pck.s:=rhdr.nameSize+rhdr.dataSize;
xBlockRead(f,pck.d,pck.s);
p:=0;
GetPckProtocol2domain(pck,p);
move(pck.d[p+1],ahdr,sizeof(ahdr));
inc(p,sizeof(ahdr));
if (ReadWordMSB(ahdr.qtype)<>Type_SOA) then goto f2;
GetPckProtocol2domain(pck,p);
GetPckProtocol2domain(pck,p);
inc(p);
if (p>pck.s) then goto f2;
i:=ReadLongMSB(pck.d[p]);
WriteLongMSB(pck.d[p],i+1);
xSeek(f,sizeof(rhdr));
xBlockWrite(f,pck.d,pck.s);
{$ifdef debug}WriteLn('serial of '+zhdr.name+' zone has updated');{$endif}
f2:
xClose(f);
if (parents<>0) then zone:=zhdr.prntNum;
goto f1;
End;






Function DoOneLocalQuery(n:String;t:Word;var reply:OnePacketRecord):Boolean;
Var
  d:OneZoneEntryHeaderRec;
  h1,h2:xFile;

Procedure seek2rr(n:LongInt);
Begin;
xSeek(h2,(n-1)*sizeof(n));
xBlockRead(h2,n,sizeof(n));
xSeek(h1,n);
xBlockRead(h1,d,sizeof(d));
End;

Procedure rr2pack(var p:OnePacketRecord;a:String);
Var i:LongInt;
Begin;
inc(p.c);
if (a='') then begin;
  i:=d.nameSize+d.dataSize;
  if (i>=sizeof(p.d)) then exit;
  xBlockRead(h1,p.d[p.s+1],i);
  inc(p.s,i);
  exit;
  end;
a:=ConvertDomain2protocol(a);
if (length(a)+d.dataSize>=sizeof(p.d)) then exit;
move(a[1],p.d[p.s+1],length(a));
inc(p.s,length(a));
xSeek(h1,xFilePos(h1)+d.nameSize);
xBlockRead(h1,p.d[p.s+1],d.dataSize);
inc(p.s,d.dataSize);
End;

Procedure AddQuestion(var p:OnePacketRecord);
Var
  hdr:OneQuestionHeader;
  a:String;
Begin;
a:=ConvertDomain2protocol(n);
move(a[1],p.d,length(a));
p.s:=length(a);
WriteWordMSB(hdr.qtype,t);
WriteWordMSB(hdr.class,Class_IN);
move(hdr,p.d[p.s+1],sizeof(hdr));
inc(p.s,sizeof(hdr));
End;

Label f1,vege;
Var
  hdr:OneQuestionPacketHeader;
  ans,aut:OnePacketRecord;
  q,w:Longint;
  i,o:LongInt;
  a,b,c:String;
Begin;
DoOneLocalQuery:=False;
fillchar(ans,sizeof(ans),0);
fillchar(aut,sizeof(aut),0);
AddQuestion(ans);
f1:
b:=n;c:='';o:=0;
while (b<>'') do begin;
  o:=FindOneZoneName(b);
  if (o>0) then break;
  c:=b;
  b:=GetParentalDomainName(b);
  end;
if (o<1) then begin;
  if (ans.c+aut.c>0) then goto vege;
  exit;
  end;
if (xOpen(h1,TempPath+ZonesDescriptorFile+ZoneSortedIndexPlus,xGenFilMod_r)<>0) then exit;
xSeek(h1,(o-1)*sizeof(o));
xBlockRead(h1,o,sizeof(o));
xClose(h1);
o:=(o div sizeof(OneZoneDescriptorRec))+1;
{$ifdef debug}WriteLn('authorized zone: (#'+BStr(o)+') '+b);{$endif}
a:=TempPath+OneZoneFilePostFix+BStr(o);
if (xOpen(h1,a,xGenFilMod_r)<>0) then exit;
a:=a+ZoneSortedIndexPlus;
if (xOpen(h2,a,xGenFilMod_r)<>0) then begin; xClose(h1);exit; end;
if FindOneDomainName(h1,h2,n,t,q,w) then begin;
  for i:=q to w do begin; seek2rr(i);rr2pack(ans,''); end;
  goto vege;
  end;
if FindOneDomainName(h1,h2,n,Type_CNAME,q,w) then begin;
  for i:=q to w do begin; seek2rr(i);rr2pack(ans,''); end;
  seek2rr(q);
  xSeek(h1,xFilePos(h1)+d.nameSize+sizeof(OneAnswerHeader));
  i:=d.dataSize-sizeof(OneAnswerHeader);
  if (i>255) then i:=255;
  xBlockRead(h1,b[1],i);
  b[0]:=chr(i);
  n:=ConvertProtocol2domain(b);
  xClose(h1);
  xClose(h2);
  goto f1;
  end;
if FindOneDomainName(h1,h2,c,Type_NS,q,w) then begin;
  for i:=q to w do begin; seek2rr(i);rr2pack(aut,''); end;
  goto vege;
  end;
a:='*.'+GetParentalDomainName(n);
if FindOneDomainName(h1,h2,a,t,q,w) then begin;
  for i:=q to w do begin; seek2rr(i);rr2pack(ans,n); end;
  goto vege;
  end;
vege:
o:=Respon_OK;
if (ans.c<1) then begin;
  if not FindOneDomainName(h1,h2,n,Type_ANY,q,w) then begin;
    {$ifdef debug}WriteLn('no entry for '+n);{$endif}
    o:=Respon_NAME;
    end;
  if FindOneDomainName(h1,h2,b,Type_SOA,q,w) then begin;
    for i:=q to w do begin; seek2rr(i);rr2pack(aut,''); end;
    end;
  end;
xClose(h1);
xClose(h2);
i:=sizeof(ans.d)-ans.s;
if (i>aut.s) then i:=aut.s;
move(aut.d,ans.d[ans.s+1],i);
inc(ans.s,i);
o:=o or Flags_AA;
if FrwrdReq then o:=o or Flags_RA;
if (ans.s>512) then begin;
  ans.s:=512;
  o:=o or Flags_TC;
  end;
o:=o or Flags_QR;
WriteWordMSB(hdr.flag,o);
WriteWordMSB(hdr.qdc,1);
WriteWordMSB(hdr.anc,ans.c);
WriteWordMSB(hdr.nsc,aut.c);
WriteWordMSB(hdr.arc,0);
move(hdr,reply.d,sizeof(hdr));
move(ans.d,reply.d[sizeof(hdr)+1],ans.s);
reply.s:=sizeof(hdr)+ans.s;
DoOneLocalQuery:=True;
End;







Function AnswerOnePacket(var con:OneConnectionRecord;var pck:OnePacketRecord;tcp:Boolean):LongInt;
{0-nothing, 1-send, 2-axfr, 3-resolveA, 4-resolvePTR, 5-resolveMX,}
Label frm,prh,error;
Var
  phdr:OneQuestionPacketHeader;
  qhdr:OneQuestionHeader;
  i,o:LongInt;
  a,b:String;
Begin;
AnswerOnePacket:=0;
move(pck.d,phdr,sizeof(phdr));
if (pck.s<sizeof(phdr)+sizeof(qhdr)) then begin;
  WriteLn('got a too short packet from '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  goto frm;
  end;
i:=ReadWordMSB(phdr.flag);
if (i and Flags_QR<>0) then begin;
  WriteLn('got reply from '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  goto frm;
  end;
if (i and Flags_OPCODE<>Opcode_QUERY) then begin;
  WriteLn('got invalid opcode from '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  goto frm;
  end;
i:=ReadWordMSB(phdr.qdc);
if (i<>1) then begin;
  WriteLn('got '+BStr(i)+' questions from '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  if (i<1) then goto frm;
  end;
i:=pck.s-sizeof(phdr);
if (i>255) then i:=255;
move(pck.d[sizeof(phdr)+1],a[1],i);
a[0]:=chr(i);
b:=ConvertProtocol2domain(a);
move(a[1],qhdr,sizeof(qhdr));
if (ReadWordMSB(qhdr.class)<>Class_IN) then begin;
  WriteLn('got invalid class from '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  goto prh;
  end;
o:=ReadWordMSB(qhdr.qtype);
{$ifdef debug}WriteLn('got qname='+b+' qtype='+GetQTypeName(o)+' from '+ipAddr2string(con.adr)+' '+BStr(con.prt));{$endif}
if (o in [Type_MAILB,Type_MAILA]) then goto frm;
if (o in [Type_AXFR,Type_IXFR]) then begin;
  if not tcp then goto frm;
  i:=FindOneZoneName(b);
  if (i<1) then goto prh;
  con.origQ:=b;
  con.origI:=phdr.id;
  con.origF:=ReadWordMSB(phdr.flag) and Flags_COPIED;
  con.quest:=kicsi(b);
  con.zoneCur:=i;
  AnswerOnePacket:=2;
  exit;
  end;
if DoOneLocalQuery(b,o,pck) then begin;
  o:=phdr.id;
  i:=ReadWordMSB(phdr.flag) and Flags_COPIED;
  move(pck.d,phdr,sizeof(phdr));
  phdr.id:=o;
  WriteWordMSB(phdr.flag,ReadWordMSB(phdr.flag) or i);
  move(phdr,pck.d,sizeof(phdr));
  {$ifdef debug}WriteLn('got answer from local zones...');{$endif}
  AnswerOnePacket:=1;
  exit;
  end;
if not FrwrdReq then goto prh;
if (ReadWordMSB(phdr.flag) and Flags_RD=0) then goto prh;
case o of
  Type_A,Type_AAAA:o:=3;
  Type_PTR:begin;
    if ConvertPtrName2ipAddr(b,b) then goto prh;
    o:=4;
    end;
  Type_MX:o:=5;
  else goto prh;
  end;
move(b,con.origQ,sizeof(con.origQ));
con.origI:=phdr.id;
con.origF:=ReadWordMSB(phdr.flag) and Flags_COPIED;
con.quest:=kicsi(b);
{$ifdef debug}WriteLn('using external resolver to answer...');{$endif}
AnswerOnePacket:=o;
exit;
error:
i:=ReadWordMSB(phdr.flag);
if FrwrdReq then o:=o or Flags_RA;
o:=(i and Flags_COPIED) or o;
o:=o or Flags_QR;
WriteWordMSB(phdr.flag,o);
move(phdr,pck.d,sizeof(phdr));
if (pck.s<sizeof(phdr)) then pck.s:=sizeof(phdr);
AnswerOnePacket:=1;
exit;
frm:
{$ifdef debug}WriteLn('sending format error...');{$endif}
o:=Respon_FORM;
goto error;
prh:
{$ifdef debug}WriteLn('sending forbidden error...');{$endif}
o:=Respon_NOWAY;
goto error;
End;
