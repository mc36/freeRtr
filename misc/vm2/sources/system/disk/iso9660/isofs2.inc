Procedure FindISO9660idInTrack;
Label f1;
Var
  root:iso_primary_descriptor;
  ntry:iso_directory_record;
  CurSec:defWord;
  i,o,p:defWord;
Begin;
Write('finding primary descriptor...');
for CurSec:=0 to 255 do begin;
  if CDreadBytes((curSec+volumeBeg)*sizeof(OneSectorData),root,sizeof(root)) then continue;
  if (ISOnum711(root.typ)<>1) then continue;
  if (ISOnum711(root.version)<>1) then continue;
  if (ISOstring(root.id,sizeof(root.id))<>'CD001') then continue;
  goto f1;
  end;
immErr(' failed!');
f1:
WriteLn(' done!');
move(root.root_directory_record,ntry,sizeof(ntry));
i:=ISOnum723(root.logical_block_size);
if (i<>sizeof(OneSectorData)) then immErr('invalid block size: '+BStr(i));
volumeSeq:=ISOnum723(root.volume_sequence_number);
volumeRot.beg:=ISOnum733(ntry.extent)*sizeof(OneSectorData);
volumeRot.siz:=ISOnum733(ntry.size);
volumeRot.pat:=DriveLetter+':\';
WriteLn('system id: '+ISOstring(root.system_id,sizeof(root.system_id)));
WriteLn('volume id: '+ISOstring(root.volume_id,sizeof(root.volume_id)));
WriteLn('current volume size: '+alakit(ISOnum733(root.volume_space_size)));
WriteLn('volume set: '+BStr(ISOnum723(root.volume_sequence_number))+'/'+BStr(ISOnum723(root.volume_set_size)));
WriteLn('block size: '+BStr(ISOnum723(root.logical_block_size)));
WriteLn('volume set: '+ISOstring(root.volume_set_id,sizeof(root.volume_set_id)));
WriteLn('publisher: '+ISOstring(root.publisher_id,sizeof(root.publisher_id)));
WriteLn('preparer: '+ISOstring(root.preparer_id,sizeof(root.preparer_id)));
WriteLn('application: '+ISOstring(root.application_id,sizeof(root.application_id)));
WriteLn('copyright file: '+ISOstring(root.copyright_file_id,sizeof(root.copyright_file_id)));
WriteLn('abstract file: '+ISOstring(root.abstract_file_id,sizeof(root.abstract_file_id)));
WriteLn('bibliography file: '+ISOstring(root.bibliographic_file_id,sizeof(root.bibliographic_file_id)));
WriteLn('created: '+ISOstring(root.creation_date,sizeof(root.creation_date)));
WriteLn('modified: '+ISOstring(root.modification_date,sizeof(root.modification_date)));
WriteLn('expire: '+ISOstring(root.expiration_date,sizeof(root.expiration_date)));
WriteLn('effective: '+ISOstring(root.effective_date,sizeof(root.effective_date)));
WriteLn('root directory: '+alakit(volumeRot.beg)+'..'+alakit(volumeRot.beg+volumeRot.siz));
End;



Function FindNextDirEntry(var sr:OneSearchRecord):Byte;

Function UnPlugName(s:String):String;
Var i:defWord;
Begin;
Kicserel(#0,'',s);
Kicserel(#255,'',s);
Kicserel(' ','',s);
if (copy(s,length(s)-1,255)=';1') then s:=copy(s,1,length(s)-2);
UnPlugName:=s;
End;

Function GetRockRidgeName(s:String):String;
Label f1;
Var
  len:defWord;
  id:string[2];
  dat:string;
Begin;
GetRockRidgeName:='';
f1:
if (copy(s,4,1)<>#1) then Exit;
len:=ord(s[3]);
if (length(s)<len) then Exit;
id:=copy(s,1,2);
dat:=copy(s,5,len-4);
delete(s,1,len);
if (id<>'NM') then goto f1;
GetRockRidgeName:=UnPlugName(copy(dat,2,255));
End;

Label f1,f2;
Var
  a,b:String;
  d:iso_directory_record;
  dlen:defWord;
  nlen:defWord;
  i,o:defWord;
Begin;
f1:
FindNextDirEntry:=14;
sr.nam:='';
sr.siz:=0;
sr.flg:=0;
sr.beg:=0;
if (sr.p>=sr.e) then begin;
  FindNextDirEntry:=0;
  Exit;
  end;
if CDreadBytes(sr.p,d,sizeof(d)) then exit;
dlen:=ISOnum711(d.length);
if (dlen<=SizeOf(d)) then begin;
  inc(sr.p);
  goto f1;
  end;
nlen:=ISOnum711(d.name_len);
if (nlen+SizeOf(d)>dlen) then goto f2;
if (ISOnum711(d.interleave)<>0) then goto f2;
if (ISOnum723(d.volume_sequence_number)<>volumeSeq) then goto f2;
i:=nlen;if (i>$ff) then i:=$ff;
a[0]:=Chr(i);
if CDreadBytes(sr.p+sizeof(d),a[1],length(a)) then exit;
if (a=#0) or (a=#1) then goto f2;
a:=UnPlugName(a);
o:=sizeof(d)+nlen;inc(o,o and 1);
i:=dlen-o;if (i>$ff) then i:=$ff;
b[0]:=Chr(i);
if CDreadBytes(sr.p+o,b[1],length(b)) then exit;
b:=GetRockRidgeName(b);
if (b<>'') then a:=b;
if (a='') then goto f2;
inc(sr.p,dlen);
if (a='..') or (a='.') then goto f2;
sr.nam:=a;
sr.siz:=ISOnum733(d.size);
if (ISOnum711(d.flags) and 2<>0) then sr.flg:=2 else sr.flg:=1;
sr.beg:=ISOnum733(d.extent)*sizeof(OneSectorData);
sr.dat.year:=ISOnum711(d.date_year)+1900;
sr.dat.month:=ISOnum711(d.date_month);
sr.dat.day:=ISOnum711(d.date_day);
sr.dat.hour:=ISOnum711(d.date_hour);
sr.dat.minute:=ISOnum711(d.date_minute);
sr.dat.second:=ISOnum711(d.date_second);
FindNextDirEntry:=0;
Exit;
f2:
inc(sr.p,dlen);
goto f1;
End;

Procedure BeginFindNext(cd:OneDirectoryRecord;var sr:OneSearchRecord);
Begin;
fillchar(sr,sizeof(sr),0);
sr.p:=cd.beg;
sr.e:=cd.beg+cd.siz;
End;

Function ChangeDir(var cd:OneDirectoryRecord;s:String):Byte;
Label f1,f2,f3,f4,ok;
Var
  a:String;
  i,o:defWord;
  sr:OneSearchRecord;
Begin;
ChangeDir:=6;
if (copy(s,2,1)=':') then delete(s,1,2);
if (copy(cd.pat,2,1)=':') then delete(cd.pat,1,2);
if (s='') then s:=cd.pat;
if (copy(s,length(s),1)<>'\') then s:=s+'\';
if (pos('\\',s)<>0) then exit;
f1:
i:=pos('\',s);
if (i=0) then Exit;
a:=copy(s,1,i-1);
delete(s,1,i);
if (a='') then begin; cd.pat:='\';goto f2; end;
if (a='.') then goto f2;
if (a='..') then begin;
  a:=cd.pat;o:=1;
  for i:=1 to length(a)-1 do if (a[i]='\') then o:=i;
  cd.pat:=copy(a,1,o);
  goto f2;
  end;
cd.pat:=cd.pat+a+'\';
if (length(cd.pat)>250) then exit;
f2:
if (s<>'') then goto f1;
s:=copy(cd.pat,2,255);
cd:=volumeRot;
f3:
if (s='') then goto ok;
i:=pos('\',s);
a:=Kicsi(copy(s,1,i-1));
delete(s,1,i);
BeginFindNext(cd,sr);
f4:
i:=FindNextDirEntry(sr);
if (i<>0) then begin; ChangeDir:=i;exit; end;
if (sr.nam='') then exit;
if (sr.flg<>2) then goto f4;
if (kicsi(sr.nam)<>a) then goto f4;
cd.pat:=cd.pat+sr.nam+'\';
cd.beg:=sr.beg;
cd.siz:=sr.siz;
goto f3;
ok:
ChangeDir:=0;
End;


Function OpenOneFile(d:OneDirectoryRecord;var f:OneFileRecord;var s:String):Byte;
Label f1;
Var
  i,o:defWord;
  sr:OneSearchRecord;
Begin;
OpenOneFile:=7;
fillchar(f,sizeof(f),0);
o:=0;for i:=1 to length(s) do if (s[i]='\') then o:=i;
i:=ChangeDir(d,copy(s,1,o));
if (i<>0) then begin; OpenOneFile:=i;exit; end;
delete(s,1,o);
s:=Kicsi(s);
BeginFindNext(d,sr);
f1:
i:=FindNextDirEntry(sr);
if (i<>0) then begin; OpenOneFile:=i;exit; end;
if (sr.nam='') then exit;
if (sr.flg<>1) then goto f1;
if (kicsi(sr.nam)<>s) then goto f1;
s:=d.pat+sr.nam;
f.beg:=sr.beg;
f.pos:=0;
f.siz:=sr.siz;
OpenOneFile:=0;
End;

Function ReadFromFile(var f:OneFileRecord;var s:LongInt;var d):Byte;
Var
  i:LongInt;
Begin;
ReadFromFile:=14;
i:=f.siz-f.pos;
if (i<0) then i:=0;
if (s>i) then s:=i;
if CDreadBytes(f.beg+f.pos,d,s) then exit;
inc(f.pos,s);
ReadFromFile:=0;
End;
