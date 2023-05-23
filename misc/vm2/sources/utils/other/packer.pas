{$heap 127k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}

Const
  BufferSize=32*1024;

{$include packer.inc}

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






Procedure ImmErr(a:String);
Begin;
WriteLn('');
WriteLn(a);
Halt(2);
End;

function repairPath(a:string):string;
begin;
if (a='') then a:='.\';
if (copy(a,length(a),255)<>'\') then a:=a+'\';
repairPath:=a;
end;



Procedure archiveAppend(var fo:xFile;var dir:xDirEntryRec;pat:String);
Label f1;
Var
  fi:xFile;
  buf:array[1..BufferSize] of byte;
  hdr:OneFileHeaderRecord;
  op,sz,ps,sm:LongInt;
  i,o:LongInt;
Begin;
fillchar(hdr,sizeof(hdr),0);
hdr.rgt:=dir.rights;
hdr.own:=dir.owner;
move(dir.created,hdr.crt,sizeof(hdr.crt));
move(dir.modified,hdr.mdf,sizeof(hdr.mdf));
hdr.nam:=dir.name;
for i:=length(hdr.nam)+1 to 255 do hdr.nam[i]:=#0;
Write('0  % '+hdr.nam+#13);
if (hdr.rgt and xRights_Directory=0) then begin;
  if (xOpen(fi,pat+hdr.nam,xGenFilMod_r)<>0) then immErr('error opening source');
  sz:=xFileSize(fi);
  end else sz:=0;
ps:=0;
sm:=-1;
op:=xFileSize(fo);
hdr.id:=not IdentifyValue;
hdr.crc:=not sm;
hdr.siz:=sz;
if (hdr.rgt and xRights_Directory<>0) then goto f1;
xSeek(fo,op);
if (xBlockWrite(fo,hdr,sizeof(hdr))<>0) then immErr('error writing header');
while (ps<sz) do begin;
  o:=sz-ps;
  if (o>sizeof(buf)) then o:=sizeof(buf);
  if (ps=0) then begin;
    i:=xFileSize(fo);
    i:=sizeof(buf)-(i mod sizeof(buf));
    if (o>i) then o:=i;
    end;
  if (xBlockRead(fi,buf,o)<>0) then immErr('error reading source');
  if (xBlockWrite(fo,buf,o)<>0) then immErr('error writing data');
  inc(ps,o);
  for i:=1 to o do crc32update(sm,buf[i]);
  write(BStr(ps*100 div sz)+#13);
  end;
xClose(fi);
f1:
hdr.id:=IdentifyValue;
hdr.crc:=not sm;
xSeek(fo,op);
if (xBlockWrite(fo,hdr,sizeof(hdr))<>0) then immErr('error writing header');
WriteLn('ok!  ');
End;

Function archiveExtract(var fi:xFile;hdr:OneFileHeaderRecord;pat:String;onlyName:Boolean):Boolean;
Label f1;
Var
  fo:xFile;
  buf:array[1..BufferSize] of byte;
  ps,sz,sm:LongInt;
  i,o:LongInt;
Begin;
archiveExtract:=True;
if (hdr.id<>IdentifyValue) then immErr('invalid file header');
Write('0  % '+hdr.nam+#13);
if onlyName then hdr.nam:=xFileName(hdr.nam,2)+xFileName(hdr.nam,3);
pat:=pat+hdr.nam;
if (hdr.rgt and xRights_Directory<>0) then begin;
  i:=xMkDir(pat);
  goto f1;
  end;
if (xCreate(pat)<>0) then immErr('error creating target');
if (xOpen(fo,pat,xGenFilMod_rw)<>0) then immErr('error opening target');
sz:=hdr.siz;
ps:=0;
sm:=-1;
while (ps<sz) do begin;
  o:=sz-ps;
  if (o>sizeof(buf)) then o:=sizeof(buf);
  if (xBlockRead(fi,buf,o)<>0) then immErr('error reading source');
  if (xBlockWrite(fo,buf,o)<>0) then immErr('error writing data');
  inc(ps,o);
  for i:=1 to o do crc32update(sm,buf[i]);
  write(BStr(ps*100 div sz)+#13);
  end;
if (not sm=hdr.crc) then i:=0 else i:=1;
xTruncate(fo);
xClose(fo);
f1:
if (i=0) then WriteLn('ok!  ') else WriteLn('crc! ');
archiveExtract:=(i<>0);
xSetRight(pat,hdr.own,hdr.rgt);
xSetDate(pat,hdr.crt,hdr.mdf);
End;




Procedure CreateArchive(var f:xFile;fn:String);
Var i:LongInt;
Begin;
WriteLn('opening archive '+fn+'...');
xCreate(fn);
if (xOpen(f,fn,xGenFilMod_rw)<>0) then immErr('error opening archive');
if (xFileSize(f)=0) then exit;
xBlockRead(f,i,sizeof(i));
if (i<>IdentifyValue) then immErr('not an archive');
End;

Procedure OpenArchive(var f:xFile;fn:String);
Var i:LongInt;
Begin;
WriteLn('opening archive '+fn+'...');
if (xOpen(f,fn,xGenFilMod_r)<>0) then immErr('error opening archive');
End;



Function AddOneFile(arc,fil:String):Boolean;
Label f1,f2;
Var
  pat:String;
  dir:xDirEntryRec;
  sr:xFile;
  fo:xFile;
Begin;
if (fil='') then begin; AddOneFile:=true;exit end;
AddOneFile:=false;
CreateArchive(fo,arc);
pat:=xFileName(fil,1);
if (xDirOpen(sr,pat)<>0) then immErr('error opening directory');
fil:=kicsi(xFileName(fil,2)+xFileName(fil,3));
f1:
if (xDirRead(sr,dir)<>0) then goto f2;
if (dir.name='') then goto f2;
if (dir.rights and xRights_Directory<>0) then goto f1;
if not FileMaskingTestOne(dir.name,fil) then goto f1;
archiveAppend(fo,dir,pat);
goto f1;
f2:
xClose(fo);
WriteLn('successful!');
End;

Procedure AddRecursively(arc,root:String);
Label f1,f2;
Var
  dir:xDirEntryRec;
  fo:xFile;

Procedure addDir(pat:String);
Label f1;
Var sr:xFile;
Begin;
if (xDirOpen(sr,root+pat)<>0) then immErr('error opening directory');
f1:
if (xDirRead(sr,dir)<>0) then exit;
if (dir.name='') then exit;
dir.name:=pat+dir.name;
archiveAppend(fo,dir,root);
if (dir.rights and xRights_Directory<>0) then addDir(dir.name+'\');
goto f1;
End;

Begin;
root:=repairPath(root);
CreateArchive(fo,arc);
addDir('');
f2:
xClose(fo);
WriteLn('successful!');
End;



Procedure TestOneArchive(a:String);
Var
  f:xFile;
  er,ps,sz,sm:LongInt;
  as,ap:LongInt;
  hdr:OneFileHeaderRecord;
  buf:array[1..BufferSize] of byte;
  i,o:LongInt;
Begin;
OpenArchive(f,a);
er:=0;
as:=xFileSize(f);
ap:=0;
while (ap<as) do begin;
  if (xBlockRead(f,hdr,sizeof(hdr))<>0) then immErr('error reading header');
  inc(ap,sizeof(hdr));
  if (hdr.id<>IdentifyValue) then immErr('error in header');
  Write('0  % '+hdr.nam+#13);
  ps:=0;
  sz:=hdr.siz;
  sm:=-1;
  while (ps<sz) do begin;
    o:=sz-ps;
    if (o>sizeof(buf)) then o:=sizeof(buf);
    if (xBlockRead(f,buf,o)<>0) then immErr('error reading data');
    inc(ps,o);
    for i:=1 to o do crc32update(sm,buf[i]);
    write(BStr(ps*100 div sz)+#13);
    end;
  if (not sm=hdr.crc) then a:='ok!  ' else begin; a:='CRC! ';inc(er); end;
  WriteLn(a);
  inc(ap,sz);
  end;
xClose(f);
if (er=0) then a:='successful!' else a:=BStr(er)+' buggy files found!';
WriteLn(a);
End;



Function ExtractOneFile(arc,fil,pat:String):Boolean;
Var
  f:xFile;
  er,ps,sz:LongInt;
  hdr:OneFileHeaderRecord;
Begin;
pat:=repairPath(pat);
if (fil='') then begin; ExtractOneFile:=true;exit; end;
ExtractOneFile:=false;
OpenArchive(f,arc);
sz:=xFileSize(f);
ps:=0;
fil:=xFileName(fil,2)+xFileName(fil,3);
er:=0;
while (ps<sz) do begin;
  xSeek(f,ps);
  if (xBlockRead(f,hdr,sizeof(hdr))<>0) then immErr('error reading header');
  inc(ps,sizeof(hdr));
  inc(ps,hdr.siz);
  if FileMaskingTestOne(hdr.nam,fil) then if archiveExtract(f,hdr,pat,true) then inc(er);
  end;
xClose(f);
if (er=0) then fil:='successful!' else fil:=BStr(er)+' buggy files found!';
WriteLn(fil);
End;

Procedure ExtractAllFiles(arc,pat:String);
Var
  f:xFile;
  er,ps,sz:LongInt;
  hdr:OneFileHeaderRecord;
Begin;
pat:=repairPath(pat);
OpenArchive(f,arc);
sz:=xFileSize(f);
ps:=0;
er:=0;
while (ps<sz) do begin;
  xSeek(f,ps);
  if (xBlockRead(f,hdr,sizeof(hdr))<>0) then immErr('error reading header');
  inc(ps,sizeof(hdr));
  inc(ps,hdr.siz);
  if archiveExtract(f,hdr,pat,false) then inc(er);
  end;
xClose(f);
if (er=0) then pat:='successful!' else pat:=BStr(er)+' buggy files found!';
WriteLn(pat);
End;




Label f1;
Var a:String;
BEGIN;
WriteLn('packer v1.0, done by Mc at '#%date' '#%time'.');
crc32build;
if (ParamCount<2) then goto f1;
a:=ParamStr(1);
a:=LowCase(a[1]);
case a[1] of
  't':TestOneArchive(ParamStr(2));
  'a':if AddOneFile(paramStr(2),paramStr(3)) then goto f1;
  'r':AddRecursively(paramStr(2),paramStr(3));
  'e':if ExtractOneFile(paramStr(2),paramStr(3),paramStr(4)) then goto f1;
  'x':ExtractAllFiles(paramStr(2),paramStr(3));
  else goto f1;
  end;
Halt(0);

f1:
WriteLn('using: packer.code <command> <archive> [file] [path]');
WriteLn('commands are:');
WriteLn('  t - test archive');
WriteLn('  a - add one file to archive');
WriteLn('  r - add files recursive to archive');
WriteLn('  e - extract one file from archive to path');
WriteLn('  x - extract all files from archive to path');
halt(1);
END.