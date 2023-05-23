Type
  OneFileRecord=record
    ino:LongInt;
    pos:LongInt;
    siz:LongInt;
    end;

Procedure GetDiskStatistics(var free,used,bad:LongInt);
Label f1;
Var
  grp:OneGroupDataRecord;
  i,o:LongInt;
Begin;
free:=0;
used:=0;
for i:=1 to GroupNM do begin;
  ReadOneGroup(i-1,grp);
  inc(free,grp.free_blocks_count);
  inc(used,BlockPG-grp.free_blocks_count);
  end;
bad:=0;
End;


Function ChangeDir(nd:String;var dir:OneStreamRecord):String;
Label f1,f2;
Var
  s,a:String;
  i,o:Byte;
Begin;
ChangeDir:='';
s:=CurPath;
delete(s,1,3);
if (copy(nd,2,1)=':') then delete(nd,1,2);
if (copy(nd,1,1)='\') then begin; s:='';delete(nd,1,1); end;
f1:
if (Length(s)>250) then Exit;
if (nd='') then goto f2;
i:=pos('\',nd);
if (i=0) then i:=length(nd)+1;
a:=copy(nd,1,i-1);
delete(nd,1,i);
if (a='') then Exit;
if (a='..') then begin;
  s:=copy(s,1,length(s)-1);
  o:=0;for i:=1 to length(s) do if (s[i]='\') then o:=i;
  s:=Copy(s,1,o);
  goto f1;
  end;
if (a='.') then goto f1;
if (a[1]='.') then Exit;
s:=s+Kicsi(a)+'\';
goto f1;
f2:
if SelectOneDirectory(s,dir) then Exit;
ChangeDir:=DriveLetter+':\'+s;
End;

Function GetNameAndChgDir(b:String;var dir:OneStreamRecord):String;
Var
  a:String;
  i,o:Byte;
  ntry:OneDirectoryEntryRecord;
Begin;
GetNameAndChgDir:='';
o:=0;
for i:=1 to Length(b) do if (b[i]='\') then o:=i;
a:=copy(b,1,o);
delete(b,1,o);
a:=ChangeDir(a,dir);
if (a='') then Exit;
CurPath:=a;
GetNameAndChgDir:=b;
End;
