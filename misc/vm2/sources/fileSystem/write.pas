{$heap 143k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc datetime.inc}
{$sysinc bugos.inc}
{$sysinc filesys2.inc}

{$include fs_type.inc}          {types, structures, basic io}
{$include disk1.inc}            {disk in file schema...}
{$include cache1.inc}           {caching disabled...}
{$include fs_umap.inc}          {disk usage bitmap routines}
{$include fs_inod.inc}          {inode basic io routines}
{$include fs_strm.inc}          {stream handling routines}
{$include fs_dirs.inc}          {directory io routines}
{$include fs_syst.inc}          {upper level system codes}

Var
  f:xFile;
  inod:OneInodeRecord;
  a:String;
  b:array[1..1024*8] of char;
  p,s,i:LongInt;
BEGIN;
if (paramCount<2) then begin;
  writeln('using: write.exe <disk> <os> [bugOS]');
  halt(2);
  end;
a:=paramStr(1);
i:=pos(',',a);
if (i<1) then i:=666;
p:=BVal(copy(a,i+1,666));
a:=copy(a,1,i-1);
if (DriveOpen(a,p)<>0) then begin;
  WriteLn('error opening drive!');
  Halt(1);
  end;
CacheInit;
if (FilesysOpen(false)<>0) then begin;
  WriteLn('error opening filesystem!');
  Halt(1);
  end;

a:=paramStr(2);
if (xOpen(f,a,xGenFilMod_r)<>0) then begin;
  WriteLn('error opening source file!');
  halt(1);
  end;
s:=xFileSize(f);
p:=0;
a:=xFileName(a,2)+xFileName(a,3);
if (paramCount>2) then a:=ParamStr(3);
i:=CreateFile(a);
if (i<>0) then begin;
  WriteLn('error creating file!');
  Halt(1);
  end;
i:=OpenOneFile(a,inod,7);
if (i<>0) then begin;
  WriteLn('error opening target file!');
  Halt(1);
  end;
StreamSetPos(inod,p);
writeln('writing '+BStr(s)+' bytes...');
while (p<s) do begin;
  i:=s-p;
  if (i>sizeof(b)) then i:=sizeof(b);
  xBlockRead(f,b,i);
  if (StreamWrite(inod,i,b)<>0) then begin;
    WriteLn('error writing file!');
    Halt(1);
    end;
  inc(p,i);
  end;
i:=StreamTruncate(inod);
if (i<>0) then begin;
  WriteLn('error truncating file: '+xGetErrorName(i));
  Halt(1);
  end;
i:=InodeWriteBack(inod,true);
if (i<>0) then begin;
  WriteLn('error closing file: '+xGetErrorName(i));
  Halt(1);
  end;
FilesysClose(true);
CacheFlush;
DriveClose;
END.