Type
  OneFileEntryRecord=record
    size:LongInt;
    rights:LongInt;
    owner:LongInt;
    created:xDirEntryDateTimeRec;
    modified:xDirEntryDateTimeRec;
    name:String;
    beginning:LongInt;
    id:LongInt;
    end;
  OneFileHandlerRecord=record
    id:LongInt;
    num:LongInt;
    pos:LongInt;
    beg:LongInt;
    siz:LongInt;
    rgt:LongInt;
    end;
  MemoryBufferRecord=array[1..1] of byte;
Var
  MemoryData:^MemoryBufferRecord;
  MemorySys:LongInt;
  MemorySiz:LongInt;
  filesNum:LongInt;
  Sequence:LongInt;
  MaxMemSize:LongInt;

Function resize(n:LongInt):Boolean;
Var p:Pointer;
Begin;
resize:=True;
if (n>MaxMemSize) then exit;
if (ExtendedMemoryResize(p,n+16)<n) then exit;
MemoryData:=p^;
MemorySiz:=n;
resize:=False;
End;

Procedure initializeToZero;
Begin;
resize(0);
filesNum:=0;
MemorySys:=1;
MemorySiz:=1;
End;

Procedure getCurrentTime(var d:xDirEntryDateTimeRec);
Var w1,w2,w3:Word;
Begin;
xGetDate(w1,w2,w3);
d.year:=w1;
d.month:=w2;
d.day:=w3;
xGetTime(w1,w2,w3);
d.hour:=w1;
d.minute:=w2;
d.second:=w3;
End;

Procedure MemoryMoveBytes(beg,mov:LongInt);
Var
  i,o:LongInt;
  d:OneFileEntryRecord;
Begin;
o:=1-sizeof(d);
for i:=1 to filesNum do begin;
  inc(o,sizeof(d));
  move(memoryData^[o],d,sizeof(d));
  if (d.beginning<beg) then continue;
  inc(d.beginning,mov);
  move(d,memoryData^[o],sizeof(d));
  end;
End;

Function MemoryAddBytes(beg,siz:LongInt):Boolean;
Label f1;
Begin;
MemoryAddBytes:=True;
if (siz<1) then goto f1;
if resize(MemorySiz+siz) then exit;
move(memoryData^[beg],memoryData^[beg+siz],MemorySiz-beg-siz+1);
MemoryMoveBytes(beg,siz);
f1:
MemoryAddBytes:=False;
End;

Function MemoryDelBytes(beg,siz:LongInt):Boolean;
Label f1;
Begin;
MemoryDelBytes:=True;
if (siz<1) then goto f1;
move(memoryData^[beg+siz],memoryData^[beg],MemorySiz-beg-siz+1);
resize(MemorySiz-siz);
MemoryMoveBytes(beg+1,-siz);
f1:
MemoryDelBytes:=False;
End;




Function readDirEntry(n:LongInt;var d:OneFileEntryRecord):Boolean;
Begin;
readDirEntry:=True;
if (n<1) or (n>filesNum) then exit;
move(MemoryData^[(n-1)*sizeof(d)+1],d,sizeof(d));
readDirEntry:=False;
End;

Function WriteDirEntry(n:LongInt;var d:OneFileEntryRecord):Boolean;
Begin;
WriteDirEntry:=True;
if (n<1) or (n>filesNum) then exit;
move(d,MemoryData^[(n-1)*sizeof(d)+1],sizeof(d));
WriteDirEntry:=False;
End;

Function findOneDirEntry(a:String):LongInt;
Var
  d:OneFileEntryRecord;
  i:LongInt;
Begin;
findOneDirEntry:=-1;
a:=kicsi(a);
for i:=1 to filesNum do begin;
  if readDirEntry(i,d) then exit;
  if (kicsi(d.name)<>a) then continue;
  findOneDirEntry:=i;
  exit;
  end;
End;

Function addDirEntry(var d:OneFileEntryRecord):LongInt;
Label f1;
Var
  dd:OneFileEntryRecord;
  i:LongInt;
Begin;
addDirEntry:=-1;
f1:
inc(Sequence);
if (Sequence=0) then goto f1;
for i:=1 to filesNum do begin;
  if readDirEntry(i,dd) then continue;
  if (dd.id=Sequence) then goto f1;
  end;
d.id:=Sequence;
if MemoryAddBytes(MemorySys,sizeof(d)) then exit;
move(d,MemoryData^[MemorySys],sizeof(d));
inc(MemorySys,sizeof(d));
inc(filesNum);
addDirEntry:=filesNum;
End;

Function delDirEntry(n:LongInt):Boolean;
Begin;
delDirEntry:=True;
if (n<1) or (n>filesNum) then exit;
dec(filesNum);
if MemoryDelBytes((n-1)*sizeof(OneFileEntryRecord)+1,sizeof(OneFileEntryRecord)) then exit;
dec(MemorySys,sizeof(OneFileEntryRecord));
delDirEntry:=False;
End;





Function CreateNewFile(a:String):LongInt;
Var
  d:OneFileEntryRecord;
  i:LongInt;
Begin;
CreateNewFile:=8;
fillchar(d,sizeof(d),0);
d.name:=a;
if (findOneDirEntry(d.name)>0) then exit;
CreateNewFile:=3;
i:=addDirEntry(d);
if (i<1) then exit;
d.size:=0;
d.rights:=$03;
d.owner:=0;
getCurrentTime(d.created);
d.modified:=d.created;
d.beginning:=MemorySiz;
CreateNewFile:=14;
if WriteDirEntry(i,d) then exit;
CreateNewFile:=0;
End;

Function DeleteOneFile(a:String):LongInt;
Var
  d:OneFileEntryRecord;
  i:LongInt;
Begin;
DeleteOneFile:=7;
i:=findOneDirEntry(a);
if (i<1) then exit;
DeleteOneFile:=14;
if readDirEntry(i,d) then exit;
if MemoryDelBytes(d.beginning,d.size) then exit;
if delDirEntry(i) then exit;
DeleteOneFile:=0;
End;

Function RenameOneFile(a,b:String):LongInt;
Var
  d:OneFileEntryRecord;
  i:LongInt;
Begin;
RenameOneFile:=7;
i:=findOneDirEntry(a);
if (i<1) then exit;
RenameOneFile:=14;
if readDirEntry(i,d) then exit;
d.name:=b;
RenameOneFile:=8;
if (findOneDirEntry(d.name)>0) then exit;
RenameOneFile:=14;
if writeDirEntry(i,d) then exit;
RenameOneFile:=0;
End;

Function ReopenOneFile(var d:OneFileHandlerRecord):LongInt;
Label f1,f2;
Var
  dd:OneFileEntryRecord;
  i:LongInt;
Begin;
ReopenOneFile:=9;
if readDirEntry(d.num,dd) then goto f1;
if (d.id=dd.id) then goto f2;
f1:
for i:=1 to filesNum do begin;
  if readDirEntry(i,dd) then continue;
  if (d.id<>dd.id) then continue;
  d.num:=i;
  goto f2;
  end;
exit;
f2:
d.siz:=dd.size;
d.beg:=dd.beginning;
ReopenOneFile:=0;
End;

Function OpenOneFile(var d:OneFileHandlerRecord;a:String):LongInt;
Var
  dd:OneFileEntryRecord;
  i:LongInt;
Begin;
OpenOneFile:=7;
i:=findOneDirEntry(a);
if (i<1) then exit;
OpenOneFile:=14;
if readDirEntry(i,dd) then exit;
fillchar(d,sizeof(d),0);
d.id:=dd.id;
d.rgt:=dd.rights;
d.num:=i;
d.pos:=0;
OpenOneFile:=ReopenOneFile(d);
End;

Function ReadFromFile(var d:OneFileHandlerRecord;var buf;siz:LongInt):LongInt;
Begin;
ReadFromFile:=16;
if (d.pos+siz>d.siz) then exit;
move(MemoryData^[d.beg+d.pos],buf,siz);
inc(d.pos,siz);
ReadFromFile:=0;
End;

Function WriteToFile(var d:OneFileHandlerRecord;var buf;siz:LongInt):LongInt;
Var
  dd:OneFileEntryRecord;
  i:LongInt;
Begin;
WriteToFile:=3;
i:=d.pos+siz-d.siz;
if (i>0) then begin;
  if readDirEntry(d.num,dd) then exit;
  if MemoryAddBytes(d.beg+d.siz,i) then exit;
  inc(dd.size,i);
  inc(d.siz,i);
  if writeDirEntry(d.num,dd) then exit;
  end;
move(buf,MemoryData^[d.beg+d.pos],siz);
inc(d.pos,siz);
WriteToFile:=0;
End;

Function TruncateFile(var d:OneFileHandlerRecord):LongInt;
Var
  dd:OneFileEntryRecord;
  i:LongInt;
Begin;
TruncateFile:=16;
if readDirEntry(d.num,dd) then exit;
i:=WriteToFile(d,i,1);
if (i<>0) then begin; TruncateFile:=i;exit; end;
dec(d.pos);
if MemoryDelBytes(d.beg+d.pos,d.siz-d.pos) then exit;
if readDirEntry(d.num,dd) then exit;
dd.size:=d.pos;
d.siz:=d.pos;
if writeDirEntry(d.num,dd) then exit;
TruncateFile:=0;
End;


Procedure displayDirList;
function x(i:longint):string;var a:string;begin; a:=bstr(i);while (length(a)<10) do a:=a+' ';x:=a; end;
Var
  i:LongInt;
  d:OneFileEntryRecord;
Begin;
WriteLn('size='+BStr(MemorySiz)+' sys='+BStr(MemorySys));
for i:=1 to filesNum do begin;
  readDirEntry(i,d);
  WriteLn(x(d.id)+x(d.beginning)+x(d.size)+d.name);
  end;
End;
