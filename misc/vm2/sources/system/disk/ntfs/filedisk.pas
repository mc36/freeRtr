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

{$include ntfs1.inc}
{$include ntfs2.inc}
{$include ntfs3.inc}
{$include ntfs4.inc}
{$include ntfs5.inc}
{$include ntfs6.inc}


{$include \sources\filesystem\filedisk1.inc}


Function ListUpSectorsOfGivenFile(b:String):Boolean;
Var
  ntry:OneDirectoryEntryRecord;
  dir,dir2:OneDirectoryDescRecord;
  fil:OneFileRecord;
  i,o,p:LongInt;
  a:String;
Begin;
ListUpSectorsOfGivenFile:=True;
Randomize;
if ReadUpBootSector then immErr('this is not a '+ProggyName+' drive!');
if ReadUpMftListFromDisk then immErr('error reading mft!');
curPath:='\';
a:=GetNameAndChgDir(b,dir);
if (a='') then immErr('directory not found!');
if FindEntryInDirectory(a,dir,dir2,ntry,2) then immErr('file not found!');
if ntry.dir then immErr('this is not a file!');
if OpenOneMFTfile(ntry.mft,dir2.fil,1) then immErr('error opening file!');
if FindOneAttribute(dir2.fil,dir.fil,$80,2) then immErr('error opening attribute!');
if dir.fil.residnt then immErr('this is an embedded file!');
for o:=1 to (dir.fil.datSiz div 512) div ClusterSiz do begin;
  i:=GetClusterFromRunList(dir.fil,o-1);
  if (i=0) then immErr('error processing chain!');
  i:=GetSectorFromCluster(i);
  appendSomeSectorToList(i,ClusterSiz);
  end;
TruncateTheSectorList(dir.fil.datSiz div 512);
ListUpSectorsOfGivenFile:=False;
End;

{$include \sources\filesystem\filedisk2.inc}
