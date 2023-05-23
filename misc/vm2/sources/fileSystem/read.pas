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
  writeln('using: read.exe <disk> <bugOS> [os]');
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
if (FilesysOpen(true)<>0) then begin;
  WriteLn('error opening filesystem!');
  Halt(1);
  end;

a:=paramStr(2);
i:=OpenOneFile(a,inod,7);
if (i<>0) then begin;
  WriteLn('error opening source file!');
  Halt(1);
  end;
a:=xFileName(a,2)+xFileName(a,3);
if (paramCount>2) then a:=ParamStr(3);
xErase(a);
xCreate(a);
if (xOpen(f,a,xGenFilMod_rw)<>0) then begin;
  WriteLn('error opening target file!');
  halt(1);
  end;
s:=inod.size;
p:=0;
StreamSetPos(inod,p);
writeln('reading '+BStr(s)+' bytes...');
while (p<s) do begin;
  i:=s-p;
  if (i>sizeof(b)) then i:=sizeof(b);
  if (StreamRead(inod,i,b)<>0) then begin;
    WriteLn('error reading file!');
    Halt(1);
    end;
  xblockwrite(f,b,i);
  inc(p,i);
  end;
xclose(f);

FilesysClose(true);
CacheFlush;
DriveClose;
END.