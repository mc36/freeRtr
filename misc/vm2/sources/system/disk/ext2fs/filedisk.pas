{$heap 767k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc crt.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$sysinc datetime.inc}
Var
  DriveSize:LongInt;
  DriveBegin:LongInt;
  DriveReadOnly:Boolean;
{$include \sources\filesystem\disk2.inc}
{$sysinc random.inc}

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

{$include ext2fs1.inc}
{$include ext2fs2.inc}
{$include ext2fs3.inc}
{$include ext2fs4.inc}
{$include ext2fs5.inc}
{$include ext2fs6.inc}


{$include \sources\filesystem\filedisk1.inc}


Function ListUpSectorsOfGivenFile(b:String):Boolean;
Var
  strm1,strm2:OneStreamRecord;
  ntry:OneDirectoryEntryRecord;
  i,o,p:LongInt;
  a:String;
Begin;
ListUpSectorsOfGivenFile:=True;
Randomize;
if ReadUpBootSector then immErr('this is not a '+proggyName+' drive!');
curPath:='\';
a:=GetNameAndChgDir(b,strm1);
if (a='') then immErr('error opening directory!');
if FindOneDirEntry(strm1,a,ntry) then immErr('error finding file!');
if StreamOpen(strm2,ntry.ino) then immErr('error opening file!');
if (strm2.inoDat.mode and $8000=0) then immErr('not a regular file!');
for o:=1 to (strm2.siz div 512) div BlockSiz do begin;
  i:=GetBlkNumOfStream(strm2.inoDat,o);
  if (i<1) then immErr('error processing chain!');
  appendSomeSectorToList(i*BlockSiz,BlockSiz);
  end;
TruncateTheSectorList(strm2.siz div 512);
ListUpSectorsOfGivenFile:=False;
End;

{$include \sources\filesystem\filedisk2.inc}
