Procedure DetectLongFileNameUsage;
Var
  d:OneStreamRecord;
  std:OneSTRdirectoryEntryRecord;
  lfn:OneLFNdirectoryEntryRecord absolute std;
Begin;
UseLFN:=False;
if openOneDirCluster(d,RootBeg) then Exit;
while not streamRead(d,std,sizeof(std)) do begin;
  if (std.attr and $0f<>$0f) then continue;
  UseLFN:=True;
  exit;
  end;
End;


Function SelectDirectory(var a:String;var cls:LongInt):Boolean;
Label f1,f2;
Var
  sr:OneSearchedRecord;
  b,c:String;
  i,o:LongInt;
Begin;
SelectDirectory:=True;
if (copy(currentPath,2,1)=':') then currentPath:=copy(currentPath,4,255);
b:='';
if (copy(a,2,1)=':') then a:=copy(a,3,255);
if (copy(a,1,1)='\') then begin; a:=copy(a,2,255);goto f1; end;
b:=currentPath;
f1:
c:=b+a;
b:='';
while (c<>'') do begin;
  i:=pos('\',c);
  if (i<1) then i:=666;
  a:=copy(c,1,i-1);
  c:=copy(c,i+1,255);
  if (a='') then exit;
  if (a='.') then continue;
  if (a<>'..') then begin; b:=b+a+'\';continue; end;
  o:=0;
  for i:=1 to length(b)-1 do if (b[i]='\') then o:=i;
  b:=copy(b,1,o);
  end;
c:=DriveLetter+':\';
o:=RootBeg;
f2:
if (b='') then begin;
  SelectDirectory:=False;
  a:=c;
  cls:=o;
  exit;
  end;
i:=pos('\',b);
if (i<1) then i:=666;
if searchOneDirEntry(o,copy(b,1,i-1),sr) then exit;
if (sr.rgt and xRights_Directory=0) then exit;
b:=copy(b,i+1,255);
o:=sr.cls;
c:=c+sr.nam+'\';
goto f2;
End;

Procedure ExtractFilePathName(var pat,nam:String);
Var i,o:LongInt;
Begin;
o:=0;
for i:=1 to length(nam) do if (nam[i]='\') then o:=i;
pat:=copy(nam,1,o);
nam:=copy(nam,o+1,255);
End;

Function SelectFileName(var a:String;var sr:OneSearchedRecord):Boolean;
Var
  i:LongInt;
  b:String;
Begin;
SelectFileName:=True;
ExtractFilePathName(b,a);
if SelectDirectory(b,i) then exit;
if searchOneDirEntry(i,a,sr) then exit;
a:=b+sr.nam;
sr.prn:=i;
SelectFileName:=False;
End;
