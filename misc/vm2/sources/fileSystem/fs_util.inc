Function FileSysFormat(botFilSiz:numberType):Word;
Var
  BitmapSiz:numberType;
  sec:OneSectorRecord;
  boot:OneBootSectorRecord;
  inod:OneInodeRecord;
  ntry:OneDirEntryRecord;
  i,o:numberType;
  w:Word;
Begin;
DriveLastMap:=0;
CurrentUser:=UIDofAdmin;
boot.Identify:=not IdentificationFilesys;
boot.DiskBegin:=DriveBegin;
boot.DiskSize:=DriveSize;
boot.BootFileSize:=botFilSiz;
boot.LastMap:=0;
boot.DiskAdd:=0;
i:=0;
if (botFilSiz<>0) then i:=i or Flags_HasBoot;
boot.Flags:=i;
DriveBitmap:=1+botFilSiz;
DriveReadOnly:=False;
BitmapSiz:=(DriveSize+BitsPerSector-1) div BitsPerSector;
o:=DriveBitmap+BitmapSiz-1;
boot.BitmapBegin:=DriveBitmap;
if (o>=DriveSize) then begin;
  FileSysFormat:=3;
  exit;
  end;
fillChar(sec,BytesPerSector,0);
for i:=0 to o do begin;
  w:=CacheWrite(i,sec);
  if (w<>0) then begin; FileSysFormat:=w;exit; end;
  end;
for i:=0 to o do begin;
  w:=BitmapWriteValue(i,true);
  if (w<>0) then begin; FileSysFormat:=w;exit; end;
  end;
for i:=DriveSize to BitmapSiz*BitsPerSector-1 do begin;
  w:=BitmapWriteValue(i,true);
  if (w<>0) then begin; FileSysFormat:=w;exit; end;
  end;
w:=InodeCreateNew(inod,false);
if (w<>0) then begin; FileSysFormat:=w;exit; end;
for i:=1 to botFilSiz do begin;
  o:=i;
  w:=InodeAppendOne(inod,o);
  if (w<>0) then begin; FileSysFormat:=w;exit; end;
  end;
inod.size:=botFilSiz*BytesPerSector;
w:=InodeWriteBack(inod,true);
if (w<>0) then begin; FileSysFormat:=w;exit; end;
w:=FillUpDirEntry(ntry,'\','BootImage.code',inod.InodeNumber);
if (w<>0) then begin; FileSysFormat:=w;exit; end;
w:=InodeCreateNew(inod,true);
if (w<>0) then begin; FileSysFormat:=w;exit; end;
i:=0;
w:=InodeAppendOne(inod,i);
if (w<>0) then begin; FileSysFormat:=w;exit; end;
fillChar(sec,BytesPerSector,0);
move(ntry,sec,sizeof(ntry));
w:=CacheWrite(i,sec);
if (w<>0) then begin; FileSysFormat:=w;exit; end;
inod.size:=sizeof(ntry);
inod.rights:=inod.rights or Rights_AnyRead;
w:=InodeWriteBack(inod,true);
if (w<>0) then begin; FileSysFormat:=w;exit; end;
boot.RootDirInode:=inod.InodeNumber;
boot.Identify:=IdentificationFilesys;
fillChar(sec,BytesPerSector,0);
move(boot,sec[BootsectorBegin+1],sizeof(boot));
w:=CacheWrite(0,sec);
FileSysFormat:=w;
End;








Function FileSysClear:Word;
Var
  sec:OneSectorRecord;
  i,o:numberType;
  b:Boolean;
Begin;
FileSysClear:=1;
fillchar(sec,sizeof(sec),0);
for o:=1 to DriveSize-1 do begin;
  displayStatusToUser(o,DriveSize);
  i:=BitmapReadValue(o,b);
  if (i<>0) then begin; FileSysClear:=i;exit; end;
  if b then continue;
  DriveWrite(o,sec);
  end;
DriveFlags:=DriveFlags or Flags_Dirty;
FileSysClear:=0;
End;









Function FileSysCheck:Word;
Type memoryRecordType=array[1..512] of byte;
Var
  corrections,error:numberType;
  memoryRecordData:^memoryRecordType;
  memoryRecordSize:numberType;
  inod1,inod2:OneInodeRecord;
  ntry:OneDirEntryRecord;
  sect:OneInodesList;
  i:numberType;
  b:Boolean;

Function ResizeMemory(o:numberType):Boolean;
Var p:Pointer;
Begin;
ResizeMemory:=True;
memoryRecordSize:=o;
if (ExtendedMemoryResize(p,o)<o) then exit;
memoryRecordData:=p^;
ResizeMemory:=False;
End;

Function getBitValue(n:numberType):Boolean;
Var i,o:numberType;
Begin;
o:=(n div BitsPerOneByte)+1;
i:=1 shl (n mod BitsPerOneByte);
getBitValue:=(memoryRecordData^[o] and i<>0);
End;

Procedure putBitValue(n:numberType;v:Boolean);
Var i,o:numberType;
Begin;
o:=(n div BitsPerOneByte)+1;
i:=1 shl (n mod BitsPerOneByte);
if v then begin;
  memoryRecordData^[o]:=memoryRecordData^[o] or i;
  end else begin;
  memoryRecordData^[o]:=memoryRecordData^[o] and (not i);
  end;
End;

Function ReadUpBitmap:Boolean;
Var i,o,p,q:numberType;
Begin;
ReadUpBitmap:=True;
o:=(DriveSize+BitsPerSector-1) div BitsPerSector;
error:=2;
if ResizeMemory(o*BytesPerSector) then exit;
p:=DriveBitmap;
q:=1;
for i:=1 to o do begin;
  error:=CacheRead(p,memoryRecordData^[q]);
  if (error<>0) then exit;
  inc(q,BytesPerSector);
  inc(p);
  end;
ReadUpBitmap:=False;
End;

Function CheckCurrent:Word;
Var res:numberType;

Function chckLevel(n,l:numberType):Boolean;
Label vege;
Var i:numberType;
Begin;
chckLevel:=True;
if (n=0) then goto vege;
if (n>=DriveSize) then exit;
if not getBitValue(n) then exit;
putBitValue(n,false);
if (l<1) then goto vege;
error:=CacheRead(n,sect);
if (error<>0) then begin; res:=2;exit; end;
for i:=1 to NumbersPerSector do begin;
  if (sect[i]=0) then continue;
  if chckLevel(sect[i],l-1) then exit;
  error:=CacheRead(n,sect);
  if (error<>0) then begin; res:=2;exit; end;
  end;
vege:
chckLevel:=False;
End;

Label vege;
Var i:numberType;
Begin;
res:=1;
if not getBitValue(inod2.inodeNumber) then goto vege;
putBitValue(inod2.inodeNumber,false);
for i:=1 to InodeLimit0 do if chckLevel(inod2.level0[i],0) then goto vege;
for i:=1 to InodeLimit3 do if chckLevel(inod2.level3[i],3) then goto vege;
res:=0;
vege:
CheckCurrent:=res;
End;

Function doOneDir(inodNum:numberType;path:String):Boolean;
Label f1,f2;
Var o:numberType;
Begin;
doOneDir:=True;
error:=InodeReadUp(inod1,inodNum);
if (error<>0) then exit;
if (inod1.size mod sizeof(ntry)<>0) then begin;
  Write(path);
  WriteLn(' has misaligned size!');
  if askUserToAccept('do you want to truncate to good size?') then begin;
    inod1.size:=(inod1.size div sizeof(ntry))*sizeof(ntry);
    error:=InodeWriteBack(inod1,false);
    if (error<>0) then exit;
    inc(corrections);
    end;
  end;
inod1.currPos:=0;
f1:
if (inod1.size-inod1.currPos<sizeof(ntry)) then begin;
  doOneDir:=False;
  exit;
  end;
error:=StreamRead(inod1,sizeof(ntry),ntry);
if (error<>0) then exit;
if (ntry.Identify<>IdentificationEntry) then begin;
  Write(path);
  Write(ntry.name);
  WriteLn(' has wrong identify!');
  if askUserToAccept('do you want to repair identify?') then begin;
    inod1.currPos:=inod1.currPos-sizeof(ntry);
    ntry.Identify:=IdentificationEntry;
    error:=StreamWrite(inod1,sizeof(ntry),ntry);
    if (error<>0) then exit;
    end;
  end;
if (ntry.inode<1) or (ntry.inode>=DriveSize) then begin;
  Write(path);
  Write(ntry.name);
  WriteLn(' has invalid inode number!');
  f2:
  if not askUserToAccept('do you want to erase this entry?') then goto f1;
  o:=inod1.currPos-sizeof(ntry);
  inod1.currPos:=o;
  error:=RemoveDirEntry(inod1);
  if (error<>0) then exit;
  error:=InodeWriteBack(inod1,false);
  if (error<>0) then exit;
  inod1.currPos:=o;
  inc(corrections);
  goto f1;
  end;
error:=BitmapReadValue(ntry.inode,b);
if (error<>0) then exit;
if (not getBitValue(ntry.inode)) and b then goto f1;
i:=InodeReadUp(inod2,ntry.inode);
if (i<>0) then begin;
  Write(path);
  Write(ntry.name);
  WriteLn(' has bad inode header!');
  goto f2;
  end;
i:=CheckCurrent;
if (i=1) then begin;
  Write(path);
  Write(ntry.name);
  WriteLn(' has bad inode chain!');
  goto f2;
  end;
if (i<>0) then exit;
error:=InodeGetSize(inod2,o);
if (error<>0) then exit;
i:=(inod2.size+BytesPerSector-1) div BytesPerSector;
if (i<o) then begin;
  Write(path);
  Write(ntry.name);
  Write(' has extra bytes!');
  end;
if (i>o) then begin;
  Write(path);
  Write(ntry.name);
  WriteLn(' has not enough bytes!');
  if not askUserToAccept('do you want to truncate it?') then goto f1;
  inod2.size:=o*BytesPerSector;
  error:=InodeWriteBack(inod2,false);
  if (error<>0) then exit;
  end;
if (inod2.rights and Rights_Directory=0) then goto f1;
o:=inod1.currPos;
if doOneDir(ntry.inode,path+ntry.name+'\') then exit;
error:=InodeReadUp(inod1,inodNum);
if (error<>0) then exit;
inod1.currPos:=o;
goto f1;
End;

Function CheckBitmap:Boolean;
Var i,o,p,q:numberType;
Begin;
CheckBitmap:=True;
putBitValue(0,false);
o:=DriveBitmap;
for p:=1 to memoryRecordSize div BytesPerSector do begin;
  b:=getBitValue(o);
  putBitValue(o,false);
  inc(o);
  if b then continue;
  WriteLn('block '+BStr(o)+' is free but part of usage bitmap.');
  if not askUserToAccept('do you want to mark it?') then continue;
  error:=BitmapWriteValue(o,true);
  if (error<>0) then exit;
  end;
for o:=DriveSize to (memoryRecordSize*BitsPerOneByte)-1 do begin;
  b:=getBitValue(o);
  putBitValue(o,false);
  if b then continue;
  WriteLn('block '+BStr(o)+' is free but over the disk surface.');
  if not askUserToAccept('do you want to mark it?') then continue;
  error:=BitmapWriteValue(o,true);
  if (error<>0) then exit;
  end;
for q:=1 to memoryRecordSize do begin;
  if (memoryRecordData^[q]=0) then continue;
  for p:=0 to 7 do begin;
    if ((1 shl p) and memoryRecordData^[q]=0) then continue;
    o:=((q-1)*BitsPerOneByte)+p;
    WriteLn('block '+BStr(o)+' is allocated, but not used!');
    if not askUserToAccept('do you want to free it up?') then continue;
    error:=BitmapWriteValue(o,false);
    if (error<>0) then exit;
    end;
  end;
CheckBitmap:=False;
End;

Label f1,f2,f3;
Begin;
FileSysCheck:=1;
error:=1;
error:=BitmapReadValue(0,b);
if (error<>0) then goto f3;
if not b then begin;
  WriteLn('boot sector not allocated!');
  if askUserToAccept('do you want to mark it?') then begin;
    error:=BitmapWriteValue(0,true);
    if (error<>0) then goto f3;
    end;
  end;
f1:
corrections:=0;
if ReadUpBitmap then goto f3;
error:=InodeReadUp(inod2,DriveRoot);
if (error<>0) then begin;
  f2:
  WriteLn('root has bad inode!');
  f3:
  if (error=0) then error:=1;
  FileSysCheck:=error;
  exit;
  end;
if (CheckCurrent<>0) then goto f2;
if doOneDir(DriveRoot,'\') then goto f3;
if (corrections<>0) then goto f1;
if CheckBitmap then goto f3;
DriveFlags:=DriveFlags and (not Flags_Dirty);
FileSysCheck:=0;
End;
