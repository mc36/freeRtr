Function FindNextDirEntry(var d:OneStreamRecord;var e:OneDirectoryEntryRecord):Boolean;
Label f1;
Var
  i,o:LongInt;
Begin;
FindNextDirEntry:=True;
f1:
fillchar(e,sizeof(e),0);
o:=d.pos;
i:=StreamRead(d,sizeof(e),e);
d.pos:=o;
if (i<8) then exit;
o:=8+e.namlen;
if (i<o) then exit;
if (e.recsiz<o) then exit;
inc(d.pos,e.recsiz);
if (e.namlen=0) then goto f1;
FindNextDirEntry:=False;
End;

Function GetFileNameFromEntry(var e:OneDirectoryEntryRecord):String;
Var
  i:LongInt;
  a:String;
Begin;
a:='';
for i:=1 to e.namlen do a:=a+e.namdat[i];
kicserel(#0,' ',a);
kicserel(#255,' ',a);
GetFileNameFromEntry:=a;
End;

Function GetFileTypeFromMode(w:LongInt):LongInt;
Begin;
case w and $c000 of
  $8000:w:=1;
  $4000:w:=2;
  else w:=0;
  end;
GetFileTypeFromMode:=w;
End;

Procedure PutFileNameToEntry(var e:OneDirectoryEntryRecord;a:String;ino:LongInt);
Begin;
fillchar(e,sizeof(e),0);
e.ino:=ino;
e.namlen:=length(a);
e.typ:=0;
move(a[1],e.namdat,length(a));
e.recsiz:=8+length(a);
while (e.recsiz and 15<>0) do inc(e.recsiz);
End;

Function CreateEmptyInode(dir:boolean):LongInt;
Var
  num:LongInt;
  dat:OneInodeDataRecord;
  i:LongInt;
Begin;
CreateEmptyInode:=0;
num:=FindEmptyInode;
if (num=0) then exit;
if WriteInodeBitmap(num,true) then exit;
fillchar(dat,sizeof(dat),0);
if dir then i:=$41ff else i:=$81ff;
dat.mode:=i;
dat.uid:=0;
dat.size:=0;
dat.gid:=0;
dat.atime:=$dead;
dat.ctime:=$dead;
dat.mtime:=$dead;
dat.dtime:=0;
if dir then i:=2 else i:=1;
dat.links_count:=i;
dat.blocks:=0;
dat.flags:=0;
dat.version:=$babe;
dat.file_acl:=0;
dat.dir_acl:=0;
dat.faddr:=0;
dat.frag:=0;
dat.fsize:=0;
if WriteInodeData(num,dat) then exit;
CreateEmptyInode:=num;
End;


Function FindOneDirEntry(var d:OneStreamRecord;n:String;var e:OneDirectoryEntryRecord):Boolean;
Label f1;
Var
  a:String;
Begin;
FindOneDirEntry:=True;
n:=kicsi(n);
if (n='') then exit;
d.pos:=0;
repeat
  FindNextDirEntry(d,e);
  a:=GetFileNameFromEntry(e);
  if (kicsi(a)=n) then goto f1;
  until (a='');
exit;
f1:
FindOneDirEntry:=False;
End;



Function SelectOneDirectory(s:String;var dir:OneStreamRecord):Boolean;
Label f1,f2;
Var
  dat:OneDirectoryEntryRecord;
  a:String;
  i:Byte;
Begin;
SelectOneDirectory:=True;
if StreamOpen(dir,2) then exit;
f1:
if (GetFileTypeFromMode(dir.inoDat.mode)<>2) then exit;
if (s='') then goto f2;
i:=pos('\',s);
if (i=0) then Exit;
a:=Copy(s,1,i-1);delete(s,1,i);
if FindOneDirEntry(dir,a,dat) then Exit;
if StreamOpen(dir,dat.ino) then exit;
goto f1;
f2:
SelectOneDirectory:=False;
End;

Function DeleteOneDirOnlyEntry(var dir:OneStreamRecord;ntry:OneDirectoryEntryRecord):Boolean;
Begin;
DeleteOneDirOnlyEntry:=True;
dec(dir.pos,ntry.recsiz);
ntry.namlen:=0;
ntry.ino:=0;
if (StreamWrite(dir,8,ntry)=0) then exit;
DeleteOneDirOnlyEntry:=False;
End;
