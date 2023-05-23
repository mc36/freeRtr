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
if (i<>sizeof(e)) then exit;
inc(d.pos,sizeof(e));
if (e.nam[1]=#0) then goto f1;
FindNextDirEntry:=False;
End;

Function GetFileNameFromEntry(var e:OneDirectoryEntryRecord):String;
Var
  i:Word;
  a:String;
Begin;
a:='';
for i:=1 to sizeof(e.nam) do a:=a+e.nam[i];
a:=a+#0;
i:=pos(#0,a);
a:=copy(a,1,i-1);
GetFileNameFromEntry:=a;
End;

Function GetFileTypeFromMode(w:Word):Word;
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
while (length(a)<sizeof(e.nam)) do a:=a+#0;
move(a[1],e.nam,sizeof(e.nam));
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
if dir then i:=2 else i:=1;
dat.nlinks:=i;
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
dec(d.pos,sizeof(e));
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
if StreamOpen(dir,1) then exit;
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


Function DeleteOneDirOnlyEntry(var dir:OneStreamRecord;nam:string):Boolean;
Var
  ntry:OneDirectoryEntryRecord;
  p1,p2:LongInt;
Begin;
DeleteOneDirOnlyEntry:=True;
if FindOneDirEntry(dir,nam,ntry) then exit;
fillchar(ntry,sizeof(ntry),0);
StreamWrite(dir,sizeof(ntry),ntry);
DeleteOneDirOnlyEntry:=False;
End;
