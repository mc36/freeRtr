{$heap 255k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc filesys.inc}
{$sysinc param.inc}
{$sysinc memory.inc}

Const
  checkFile='fileChecker.data';
Type
  OneEntryRecord=record
    name:String;
    size:LongInt;
    crc:LongInt;
    end;
Var
  basePath:String;
  errorCnt:LongInt;
  filesSaw:LongInt;
  drctrSaw:LongInt;
  buffer:array[1..1024] of byte;
  entry:OneEntryRecord;
  dirRec:xDirEntryRec;
  handler:xFile;

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Var crc32table:Array [0..255] of LongInt;

Procedure crc32build;
Var
  crc:LongInt;
  i,n:word;
Begin;
for i:=0 to 255 do begin;
  crc:=i;
  for n:=1 to 8 do
   if (crc and 1<>0) then crc:=(crc shr 1) xor $EDB88320 else crc:=crc shr 1;
  crc32table[i]:=crc;
  end;
End;

Procedure crc32update(var crc:LongInt;c:LongInt);
Begin
crc:=crc32table[(crc and $ff) xor c] xor (crc shr 8);
End;



Function generateCheck(var d:OneEntryRecord):Boolean;
Label vege;
Var
  i,o,p,s,c:LongInt;
  f:xFile;
Begin;
generateCheck:=True;
if (d.name=checkFile) then begin; s:=0;c:=0;goto vege; end;
inc(filesSaw);
Write(BStr(filesSaw)+#13);
if (xOpen(f,basePath+d.name,xGenFilMod_r)<>0) then exit;
s:=xFileSize(f);
p:=0;
xSeek(f,p);
c:=$ffffffff;
while (p<s) do begin;
  o:=s-p;
  if (o>sizeof(buffer)) then o:=sizeof(buffer);
  if (xBlockRead(f,buffer,o)<>0) then exit;
  for i:=1 to o do crc32update(c,buffer[i]);
  inc(p,o);
  end;
xClose(f);
vege:
d.crc:=c xor $ffffffff;
d.size:=s;
generateCheck:=False;
End;




Procedure genOneDir(curPath:String);
Label f1,f2,f3;
Var f:xFile;
Begin;
inc(drctrSaw);
fillChar(entry,sizeof(entry),0);
entry.name:=curPath;
entry.size:=-2;
xBlockWrite(handler,entry,sizeof(entry));
if (xDirOpen(f,basePath+curPath)<>0) then immErr('error opening directory!');
f1:
if (xDirRead(f,dirRec)<>0) then immErr('error reading directory!');
if (dirRec.name='') then goto f2;
if (copy(dirRec.name,1,1)='.') then goto f1;
fillChar(entry,sizeof(entry),0);
entry.name:=curPath+dirRec.name;
if (dirRec.rights and xRights_Directory<>0) then begin;
  entry.size:=-1;
  end else begin;
  if generateCheck(entry) then immErr('error opening file!');
  end;
xBlockWrite(handler,entry,sizeof(entry));
goto f1;
f2:
xDirClose(f);
fillChar(entry,sizeof(entry),0);
entry.size:=-3;
xBlockWrite(handler,entry,sizeof(entry));
if (xDirOpen(f,basePath+curPath)<>0) then immErr('error opening directory!');
f3:
if (xDirRead(f,dirRec)<>0) then immErr('error reading directory!');
if (dirRec.name='') then begin;
  xDirClose(f);
  exit;
  end;
if (dirRec.rights and xRights_Directory=0) then goto f3;
if (copy(dirRec.name,1,1)='.') then goto f3;
genOneDir(curPath+dirRec.name+'\');
goto f3;
End;


Procedure chkOneDir;
Label f1,f2,f3;
Var
  ntry:OneEntryRecord;
  curPath:String;
  files:LongInt;
  f:xFile;
Begin;
xBlockRead(handler,entry,sizeof(entry));
if (entry.size<>-2) then immErr('error in list file!');
curPath:=entry.name;
if (xDirOpen(f,basePath+curPath)<>0) then begin;
  inc(errorCnt);
  WriteLn(curPath+' removed!');
  repeat xBlockRead(handler,entry,sizeof(entry)); until (entry.size=-3);
  exit;
  end;
files:=0;
f1:
if (xDirRead(f,dirRec)<>0) then immErr('error reading directory!');
if (dirRec.name='') then goto f2;
if (copy(dirRec.name,1,1)='.') then goto f1;
inc(files);
goto f1;
f2:
xDirClose(f);
inc(drctrSaw);
f3:
xBlockRead(handler,entry,sizeof(entry));
if (entry.size=-3) then begin;
  if (files=0) then exit;
  inc(errorCnt);
  WriteLn(curPath+' has new files!');
  exit;
  end;
ntry:=entry;
if (entry.size<0) then begin;
  dec(files);
  goto f3;
  end;
if generateCheck(ntry) then begin;
  inc(errorCnt);
  WriteLn(entry.name+' removed!');
  goto f3;
  end;
dec(files);
if (ntry.size=entry.size) and (ntry.crc=entry.crc) then goto f3;
inc(errorCnt);
WriteLn(entry.name+' changed!');
goto f3;
End;




Label f1;
Var
  i:LongInt;
  a:String;
  f:xFile;
BEGIN;
WriteLn('file checker v1.0, done by Mc at '#%date' '#%time'.');
crc32build;
a:=kicsi(paramStr(1));
basePath:=paramStr(2);
i:=0;
if (a='check') then i:=1;
if (a='generate') then i:=2;
if (basePath='') or (i<1) then begin;
  WriteLn('using: filchk.code <command> <path>');
  WriteLn('commands: generate, check');
  exit;
  end;

if (copy(basePath,length(basePath),255)<>'\') then basePath:=basePath+'\';
errorCnt:=0;
filesSaw:=0;
drctrSaw:=0;
a:=basePath+checkFile;
if (i=2) then begin;
  WriteLn('generating '+basePath+'...');
  xErase(a);
  xCreate(a);
  if (xOpen(handler,a,xGenFilMod_rw)<>0) then immErr('error creating filelist!');
  genOneDir('');
  end else begin;
  WriteLn('checking '+basePath+'...');
  if (xOpen(handler,a,xGenFilMod_r)<>0) then immErr('error opening filelist!');
  i:=xFileSize(handler);
  while (xFilePos(handler)<i) do chkOneDir;
  end;
xClose(handler);
WriteLn(BStr(drctrSaw)+' directories, '+BStr(filesSaw)+' files.');

if (errorCnt<>0) then immErr(BStr(errorCnt)+' errors found!');
WriteLn('Successfully finished!');
END.