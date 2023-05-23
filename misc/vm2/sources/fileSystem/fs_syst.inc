Function FilesysOpen(readonly:Boolean):Word;
Label f1,error;
Var
  sec:OneSectorRecord;
  boot:OneBootSectorRecord;
  inod:OneInodeRecord;
  i,o:numberType;
  w:Word;
Begin;
DriveReadOnly:=readonly;
CurrentPath:='\';
CurrentUser:=UIDofAdmin;
w:=CacheRead(0,sec);
if (w<>0) then begin; FilesysOpen:=w;exit; end;
move(sec[BootsectorBegin+1],boot,sizeof(boot));
if (boot.Identify<>IdentificationFilesys) then begin;
  error:
  FilesysOpen:=9;
  exit;
  end;
if (boot.DiskSize>DriveSize) then goto error;
DriveSize:=boot.DiskSize;
DriveBitmap:=boot.BitmapBegin;
DriveRoot:=boot.RootDirInode;
DriveLastMap:=boot.LastMap;
DriveFlags:=boot.Flags;
if (DriveLastMap>=(DriveSize+BitsPerSector-1) div BitsPerSector) then DriveLastMap:=0;
w:=InodeReadUp(inod,DriveRoot);
if (w<>0) then begin; FilesysOpen:=w;exit; end;
if DriveReadOnly then goto f1;
boot.Flags:=DriveFlags or Flags_Dirty;
move(boot,sec[BootsectorBegin+1],sizeof(boot));
w:=CacheWrite(0,sec);
if (w<>0) then begin; FilesysOpen:=w;exit; end;
f1:
FilesysOpen:=0;
End;



Function FilesysClose(finalClose:Boolean):Word;
Label f1;
Var
  sec:OneSectorRecord;
  boot:OneBootSectorRecord;
  inod:OneInodeRecord;
  i,o:numberType;
  w:Word;
Begin;
w:=CacheRead(0,sec);
if (w<>0) then begin; FilesysClose:=w;exit; end;
move(sec[BootsectorBegin+1],boot,sizeof(boot));
if DriveReadOnly then goto f1;
if finalClose then boot.Flags:=DriveFlags;
boot.LastMap:=DriveLastMap;
w:=CacheWrite(0,sec);
if (w<>0) then begin; FilesysClose:=w;exit; end;
f1:
FilesysClose:=0;
End;
