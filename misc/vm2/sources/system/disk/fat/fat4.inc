Type
  OneSearchedRecord=record
    siz:LongInt;
    rgt:LongInt;
    own:LongInt;
    crt:xDirEntryDateTimeRec;
    mdf:xDirEntryDateTimeRec;
    nam:String;
    cls:LongInt;
    prn:LongInt;
    end;

Function openOneDirCluster(var d:OneStreamRecord;c:LongInt):Boolean;
Label f1;
Begin;
openOneDirCluster:=True;
if (c=0) then begin;
  c:=RootBeg;
  if (c<>0) then goto f1;
  fillchar(d,sizeof(d),0);
  d.posMax:=RootMax*32;
  d.clstMax:=1;
  openOneDirCluster:=False;
  exit;
  end;
f1:
if chainOpen(d,c) then exit;
d.posMax:=d.clstMax*ClusterSiz*512;
openOneDirCluster:=False;
End;

Function updateDirEntry(nt:OneStreamRecord;date:Boolean):Boolean;
Label f1;
Var
  d:OneStreamRecord;
  std:OneSTRdirectoryEntryRecord;
  lfn:OneLFNdirectoryEntryRecord absolute std;
  i,o:LongInt;
  w1,w2,w3:Word;
Begin;
updateDirEntry:=True;
if openOneDirCluster(d,nt.parent) then Exit;
i:=nt.clst1st and $ffff;
o:=nt.clst1st shr 16;
f1:
if streamRead(d,std,sizeof(std)) then exit;
if (lfn.counter=$00) then exit;
if (lfn.counter=$e5) then goto f1;
if (std.attr and $08<>0) then goto f1;
if (std.clst1<>i) then goto f1;
if (std.clst2<>o) then goto f1;
if date then begin;
  WriteWordLSB(std.modfDate,compressDateForDirent);
  WriteWordLSB(std.modfTime,compressTimeForDirent);
  end;
WriteLongLSB(std.size,nt.posMax);
dec(d.posCur,sizeof(std));
if streamWrite(d,std,sizeof(std)) then exit;
updateDirEntry:=False;
End;



Function ReadNextDirEntry(var d:OneStreamRecord;var dat:OneSearchedRecord;valid:Boolean):Boolean;

function utf8(var d;s:word):string;
var a:string;i:word;
begin;
a[0]:=chr(s);
move(d,a[1],s);
while (copy(a,length(a),255)=' ') do a:=copy(a,1,length(a)-1);
utf8:=a;
end;

function utf16(var dd;s:word):string;
var a:string;i,o:word;d:array[1..1] of word absolute dd;
begin;
a:='';
for o:=1 to s shr 1 do begin;
  i:=readWordLSB(d[o]);
  a:=a+chr(i);
  end;
utf16:=a;
end;

Label f1,f2,f3;
Var
  std:OneSTRdirectoryEntryRecord;
  lfn:OneLFNdirectoryEntryRecord absolute std;
  i,o:LongInt;
  a:String;
Begin;
ReadNextDirEntry:=True;
f1:
fillchar(dat,sizeof(dat),0);
if streamRead(d,std,sizeof(std)) then exit;
if (lfn.counter=$00) then begin; dec(d.posCur,sizeof(std));exit; end;
if (lfn.counter=$e5) then goto f1;
if UseLFN then if (std.attr and $0f=$0f) then goto f2;
if (std.attr and $08<>0) then goto f1;
dat.nam:=utf8(std.nam,sizeof(std.nam));
a:=utf8(std.ext,sizeof(std.ext));
if (a<>'') then dat.nam:=dat.nam+'.'+a;
dat.nam:=kicsi(dat.nam);
f3:
if valid then begin;
  if (dat.nam='') then goto f1;
  if (dat.nam='.') then goto f1;
  if (dat.nam='..') then goto f1;
  end;
dat.rgt:=xRights_OwnRead;
if (std.attr and $10<>0) then inc(dat.rgt,xRights_Directory);
if (std.attr and $04<>0) then inc(dat.rgt,xRights_HasRootPriv);
if (std.attr and $01=0) then inc(dat.rgt,xRights_OwnWrite);
if (std.attr and $20<>0) then inc(dat.rgt,xRights_OwnExec);
dat.own:=0;
dat.siz:=readLongLSB(std.size);
dat.cls:=(readWordLSB(std.clst2) shl 16)+readWordLSB(std.clst1);
i:=ReadWordLSB(std.creaDate);
dat.crt.year:=(i shr 9)+1980;
dat.crt.month:=(i shr 5) and $0f;
dat.crt.day:=i and $1f;
i:=ReadWordLSB(std.creaTime);
dat.crt.hour:=i shr 11;
dat.crt.minute:=(i shr 5) and $3f;
dat.crt.second:=(i and $1f) shl 1;
i:=ReadWordLSB(std.modfDate);
dat.mdf.year:=(i shr 9)+1980;
dat.mdf.month:=(i shr 5) and $0f;
dat.mdf.day:=i and $1f;
i:=ReadWordLSB(std.modfTime);
dat.mdf.hour:=i shr 11;
dat.mdf.minute:=(i shr 5) and $3f;
dat.mdf.second:=(i and $1f) shl 1;
ReadNextDirEntry:=False;
exit;
f2:
if (lfn.counter and $40=0) then goto f1;
a:=utf16(lfn.nam1,sizeof(lfn.nam1))+utf16(lfn.nam2,sizeof(lfn.nam2))+utf16(lfn.nam3,sizeof(lfn.nam3));
for i:=(lfn.counter and $3f)-1 downto 1 do begin;
  if streamRead(d,lfn,sizeof(lfn)) then exit;
  if (lfn.counter<>i) then goto f1;
  a:=utf16(lfn.nam1,sizeof(lfn.nam1))+utf16(lfn.nam2,sizeof(lfn.nam2))+utf16(lfn.nam3,sizeof(lfn.nam3))+a;
  end;
i:=pos(#0,a);
if (i>0) then a:=copy(a,1,i-1);
dat.nam:=a;
if streamRead(d,std,sizeof(std)) then exit;
goto f3;
End;



Function eraseOneDirEntry(c:LongInt;a:String):Boolean;
Label f1;
Var
  d:OneStreamRecord;
  sr:OneSearchedRecord;
  std:OneSTRdirectoryEntryRecord;
  lfn:OneLFNdirectoryEntryRecord absolute std;
  i:LongInt;
Begin;
eraseOneDirEntry:=True;
if openOneDirCluster(d,c) then Exit;
a:=kicsi(a);
if (a='') then exit;
if (a='.') then exit;
if (a='..') then exit;
f1:
i:=d.posCur;
if ReadNextDirEntry(d,sr,false) then exit;
if (kicsi(sr.nam)<>a) then goto f1;
c:=d.posCur;
d.posCur:=i;
while (d.posCur<c) do begin;
  if streamRead(d,lfn,sizeof(lfn)) then exit;
  lfn.counter:=$e5;
  dec(d.posCur,sizeof(std));
  if streamWrite(d,lfn,sizeof(lfn)) then exit;
  end;
eraseOneDirEntry:=False;
End;

Function searchOneDirEntry(c:LongInt;a:String;var sr:OneSearchedRecord):Boolean;
Label f1;
Var d:OneStreamRecord;
Begin;
searchOneDirEntry:=True;
if openOneDirCluster(d,c) then Exit;
a:=kicsi(a);
f1:
if ReadNextDirEntry(d,sr,true) then exit;
if (kicsi(sr.nam)<>a) then goto f1;
searchOneDirEntry:=False;
End;

Function appendOneDirEntry(c:LongInt;a:String;atr:Byte;cls,siz:LongInt):Boolean;
Var
  d:OneStreamRecord;
  std:OneSTRdirectoryEntryRecord;
  lfn:OneLFNdirectoryEntryRecord absolute std;

procedure utf16(var dat;siz:Word;a:String);
var i:byte;d:array[1..1] of word absolute dat;
begin;
siz:=siz shr 1;
while (length(a)<siz) do a:=a+#255;
for i:=1 to siz do WriteWordLSB(d[i],ord(a[i]));
end;

function trunc(a:string;siz:Word):string;
begin;
kicserel('.','',a);
kicserel(' ','',a);
while (length(a)<siz) do a:=a+' ';
trunc:=copy(a,1,siz);
end;

Var
  i,o:LongInt;
  b:String;
Begin;
appendOneDirEntry:=True;
if (a='') then exit;
if (a='.') then exit;
if (a='..') then exit;
if (pos(#0,a)<>0) then exit;
if (pos(#255,a)<>0) then exit;
if (pos(':',a)<>0) then exit;
if (pos('\',a)<>0) then exit;
if (length(a)>250) then exit;
if (a[1]=#229) then exit;
if openOneDirCluster(d,c) then Exit;
while not streamRead(d,lfn,sizeof(lfn)) do if (lfn.counter=0) then begin; dec(d.posCur,sizeof(lfn));break; end;
o:=666;
for i:=1 to length(a) do if (a[i]='.') then o:=i;
b:=nagy(trunc(copy(a,1,o),sizeof(std.nam))+trunc(copy(a,o,255),sizeof(std.ext)));
if UseLFN then begin;
  a:=a+#0;
  fillchar(lfn,sizeof(lfn),0);
  lfn.counter:=((length(a)+12) div 13) or $40;
  lfn.attr:=$0f;
  lfn.res1:=$00;
  lfn.chksum:=calculateCheckSum(b);
  lfn.clst1:=$00;
  for i:=lfn.counter and $3f downto 1 do begin;
    o:=(i-1)*13;
    utf16(lfn.nam1,sizeof(lfn.nam1),copy(a,o+1,255));
    utf16(lfn.nam2,sizeof(lfn.nam2),copy(a,o+6,255));
    utf16(lfn.nam3,sizeof(lfn.nam3),copy(a,o+12,255));
    if streamWrite(d,lfn,sizeof(lfn)) then exit;
    lfn.counter:=i-1;
    end;
  a:=copy(a,1,length(a)-1);
  end;
fillchar(std,sizeof(std),0);
move(b[1],std.nam,sizeof(std.nam));
move(b[9],std.ext,sizeof(std.ext));
std.attr:=atr;
std.casee:=0;
std.creaTime2:=0;
WriteWordLSB(std.creaTime,compressTimeForDirent);
WriteWordLSB(std.creaDate,compressDateForDirent);
std.accsDate:=std.creaDate;
WriteWordLSB(std.clst2,cls shr 16);
std.modfTime:=std.creaTime;
std.modfDate:=std.creaDate;
WriteWordLSB(std.clst1,cls);
WriteLongLSB(std.size,siz);
if streamWrite(d,std,sizeof(std)) then exit;
fillchar(std,sizeof(std),0);
if streamWrite(d,std,sizeof(std)) then exit;
appendOneDirEntry:=False;
End;

Function WriteInitialDirSector(c,p:LongInt;initial:Boolean):Boolean;
Var
  d:OneStreamRecord;
  std:OneSTRdirectoryEntryRecord;
  lfn:OneLFNdirectoryEntryRecord absolute std;
  buf:array[1..3] of OneSTRdirectoryEntryRecord;
  i:LongInt;
Begin;
WriteInitialDirSector:=True;
fillchar(std,sizeof(std),0);
buf[3]:=std;
fillchar(std.nam,sizeof(std.nam),$20);
fillchar(std.ext,sizeof(std.ext),$20);
std.attr:=$10;
WriteWordLSB(std.creaTime,compressTimeForDirent);
WriteWordLSB(std.creaDate,compressDateForDirent);
std.accsDate:=std.creaDate;
std.modfTime:=std.creaTime;
std.modfDate:=std.creaDate;
std.size:=0;
std.nam[1]:='.';
WriteWordLSB(std.clst1,c);
WriteWordLSB(std.clst2,c shr 16);
buf[1]:=std;
std.nam[2]:='.';
WriteWordLSB(std.clst1,p);
WriteWordLSB(std.clst2,p shr 16);
buf[2]:=std;
if openOneDirCluster(d,c) then Exit;
if initial then i:=sizeof(buf) else i:=sizeof(std)*2;
if streamWrite(d,buf,i) then exit;
WriteInitialDirSector:=False;
End;

Function setDateOfDirEntry(c:LongInt;a:String;t1,t2:xDirEntryDateTimeRec):Boolean;
Label f1;
Var
  d:OneStreamRecord;
  sr:OneSearchedRecord;
  std:OneSTRdirectoryEntryRecord;
  lfn:OneLFNdirectoryEntryRecord absolute std;

procedure x1(var w1,w2:Word;var t:xDirEntryDateTimeRec);
begin;
WriteWordLSB(w1,((t.year-1980) shl 9) or (t.month shl 5) or t.day);
WriteWordLSB(w2,(t.hour shl 11) or (t.minute shl 5) or (t.second shr 1));
end;

Begin;
setDateOfDirEntry:=True;
if openOneDirCluster(d,c) then Exit;
a:=kicsi(a);
f1:
if ReadNextDirEntry(d,sr,true) then exit;
if (kicsi(sr.nam)<>a) then goto f1;
dec(d.posCur,sizeof(std));
if streamRead(d,std,sizeof(std)) then exit;
x1(std.creaDate,std.creaTime,t1);
x1(std.modfDate,std.modfTime,t2);
dec(d.posCur,sizeof(std));
if streamWrite(d,std,sizeof(std)) then exit;
setDateOfDirEntry:=False;
End;
