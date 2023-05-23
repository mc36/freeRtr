Function CheckForRightsRWX(var inod:OneInodeRecord;rqrgt:numberType):Word;
Begin;
rqrgt:=(Rights_OwnRead+Rights_OwnWrite+Rights_OwnExec) and rqrgt;
if (CurrentUser=inod.owner) then if (inod.rights and rqrgt=rqrgt) then begin; CheckForRightsRWX:=0;exit; end;
rqrgt:=rqrgt*(Rights_AnyRead div Rights_OwnRead);
if (inod.rights and rqrgt=rqrgt) then begin; CheckForRightsRWX:=0;exit; end;
if (CurrentUser=UIDofAdmin) then begin; CheckForRightsRWX:=0;exit; end;
CheckForRightsRWX:=4;
End;





Function CheckForRightsOWN(var inod:OneInodeRecord):Word;
Begin;
if (CurrentUser=inod.owner) then begin; CheckForRightsOWN:=0;exit; end;
if (CurrentUser=UIDofAdmin) then begin; CheckForRightsOWN:=0;exit; end;
CheckForRightsOWN:=4;
End;





Function FindDirEntry(var inod:OneInodeRecord;var ntry:OneDirEntryRecord;nam:String):Word;
Var w:Word;
Begin;
{$ifdef nocase}nam:=kicsi(nam);{$endif}
inod.CurrPos:=0;
while (inod.CurrPos<inod.size) do begin;
  w:=StreamRead(inod,sizeof(ntry),ntry);
  if (w<>0) then begin; FindDirEntry:=w;exit; end;
  {$ifdef nocase}if (nam=kicsi(ntry.name)) then
  {$else}if (nam=ntry.name) then
  {$endif}begin;
    dec(inod.CurrPos,sizeof(ntry));
    FindDirEntry:=0;
    exit;
    end;
  end;
fillchar(ntry,sizeof(ntry),0);
FindDirEntry:=0;
End;





Function ChangeDir(var inod:OneInodeRecord;var path:String):Word;
Label f1;
Var
  old:String;
  ntry:OneDirEntryRecord;
  a:String;
  w:Word;
Begin;
old:=CurrentPath;
if (copy(path,2,1)=':') then path:=copy(path,3,255);
if (copy(path,1,1)<>'\') then path:=old+path;
if (copy(path,length(path),1)<>'\') then path:=path+'\';
if (copy(path,1,1)='\') then path:=copy(path,2,255);
old:='';
while (path<>'') do begin;
  w:=pos('\',path);
  if (w=0) then begin; ChangeDir:=6;exit; end;
  a:=copy(path,1,w);
  path:=copy(path,w+1,255);
  if (a='\') then begin; ChangeDir:=6;exit; end;
  if (a='.\') then goto f1;
  if (a='..\') then begin;
    old:=copy(old,1,length(old)-1);
    for w:=length(old) downto 1 do if (old[w]='\') then begin;
      old:=copy(old,1,w);
      goto f1;
      end;
    old:='';
    goto f1;
    end;
  old:=old+a;
  f1:
  end;
path:=DriveLetter+':\';
w:=InodeReadUp(inod,DriveRoot);
if (w<>0) then begin; ChangeDir:=w;exit; end;
if (inod.rights and Rights_Directory=0) then begin; ChangeDir:=6;exit; end;
w:=CheckForRightsRWX(inod,Rights_OwnRead);
if (w<>0) then begin; ChangeDir:=w;exit; end;
while (old<>'') do begin;
  w:=pos('\',old);
  a:=copy(old,1,w-1);
  old:=copy(old,w+1,255);
  w:=FindDirEntry(inod,ntry,a);
  if (w<>0) then begin; ChangeDir:=w;exit; end;
  if (inod.CurrPos>=inod.size) then begin; ChangeDir:=6;exit; end;
  w:=InodeReadUp(inod,ntry.inode);
  if (w<>0) then begin; ChangeDir:=w;exit; end;
  if (inod.rights and Rights_Directory=0) then begin; ChangeDir:=6;exit; end;
  w:=CheckForRightsRWX(inod,Rights_OwnRead);
  if (w<>0) then begin; ChangeDir:=w;exit; end;
  path:=path+ntry.name+'\';
  end;
CurrentPath:=copy(path,3,255);
ChangeDir:=0;
End;





Function ChgDirGetName(var inod:OneInodeRecord;var path,name:String):Word;
Var q,w:Word;
Begin;
q:=0;
for w:=1 to length(name) do if (name[w]='\') then q:=w;
path:=copy(name,1,q);
name:=copy(name,q+1,255);
w:=ChangeDir(inod,path);
ChgDirGetName:=w;
End;





Function CreateDirEntry(name:string;dir:Boolean):Word;
Var
  par,new:OneInodeRecord;
  ntry:OneDirEntryRecord;
  a:String;
  w:Word;
Begin;
w:=ChgDirGetName(par,a,name);
if (w<>0) then begin; CreateDirEntry:=w;exit; end;
w:=CheckForRightsRWX(par,Rights_OwnWrite);
if (w<>0) then begin; CreateDirEntry:=w;exit; end;
w:=FindDirEntry(par,ntry,name);
if (w<>0) then begin; CreateDirEntry:=w;exit; end;
if (par.CurrPos<par.size) then begin; CreateDirEntry:=8;exit; end;
w:=FillUpDirEntry(ntry,a,name,0);
if (w<>0) then begin; CreateDirEntry:=w;exit; end;
w:=InodeCreateNew(new,dir);
if (w<>0) then begin; CreateDirEntry:=w;exit; end;
ntry.inode:=new.InodeNumber;
par.CurrPos:=par.size;
w:=StreamWrite(par,sizeof(ntry),ntry);
if (w<>0) then begin; CreateDirEntry:=w;exit; end;
w:=InodeWriteBack(par,true);
CreateDirEntry:=w;
End;





Function RemoveDirEntry(var inod:OneInodeRecord):Word;
Var
  ntry:OneDirEntryRecord;
  p1,p2:numberType;
  w:Word;
Begin;
p1:=inod.CurrPos;
p2:=((inod.size-sizeof(ntry)) div sizeof(ntry))*sizeof(ntry);
inod.CurrPos:=p2;
w:=StreamRead(inod,sizeof(ntry),ntry);
if (w<>0) then begin; RemoveDirEntry:=w;exit; end;
inod.CurrPos:=p1;
w:=StreamWrite(inod,sizeof(ntry),ntry);
if (w<>0) then begin; RemoveDirEntry:=w;exit; end;
inod.CurrPos:=p2;
w:=StreamTruncate(inod);
RemoveDirEntry:=w;
End;





Function EraseDirEntry(name:String;dir:Boolean):Word;
Var
  par,old:OneInodeRecord;
  ntry:OneDirEntryRecord;
  a:String;
  w:Word;
Begin;
w:=ChgDirGetName(par,a,name);
if (w<>0) then begin; EraseDirEntry:=w;exit; end;
w:=CheckForRightsRWX(par,Rights_OwnWrite);
if (w<>0) then begin; EraseDirEntry:=w;exit; end;
w:=FindDirEntry(par,ntry,name);
if (w<>0) then begin; EraseDirEntry:=w;exit; end;
if (par.CurrPos>=par.size) then begin; EraseDirEntry:=7;exit; end;
w:=InodeReadUp(old,ntry.inode);
if (w<>0) then begin; EraseDirEntry:=w;exit; end;
w:=CheckForRightsOWN(old);
if (w<>0) then begin; EraseDirEntry:=w;exit; end;
if dir then begin;
  if (old.size<>0) and (old.linkCount<2) then begin; EraseDirEntry:=10;exit; end;
  if (old.rights and Rights_Directory=0) then begin; EraseDirEntry:=12;exit; end;
  end else begin;
  if (old.rights and Rights_Directory<>0) then begin; EraseDirEntry:=12;exit; end;
  end;
dec(old.linkCount);
if (old.linkCount>0) then begin;
  w:=InodeWriteBack(old,true);
  end else begin;
  w:=InodeEraseOne(old);
  end;
if (w<>0) then begin; EraseDirEntry:=w;exit; end;
w:=RemoveDirEntry(par);
if (w<>0) then begin; EraseDirEntry:=w;exit; end;
w:=InodeWriteBack(par,true);
EraseDirEntry:=w;
End;





Function CreateDir(name:String):Word;
Var w:Word;
Begin;
w:=CreateDirEntry(name,true);
CreateDir:=w;
End;





Function CreateFile(name:String):Word;
Var w:Word;
Begin;
w:=CreateDirEntry(name,false);
CreateFile:=w;
End;





Function EraseFile(name:String):Word;
Var w:Word;
Begin;
w:=EraseDirEntry(name,false);
EraseFile:=w;
End;





Function EraseDir(name:String):Word;
Var w:Word;
Begin;
w:=EraseDirEntry(name,true);
EraseDir:=w;
End;





Function CreateLink(srcN,trgN:String):Word;
Var
  inod,srcI,trgI:OneInodeRecord;
  ntry1,ntry2:OneDirEntryRecord;
  a:String;
  w:Word;
Begin;
w:=ChgDirGetName(srcI,a,srcN);
if (w<>0) then begin; CreateLink:=w;exit; end;
w:=CheckForRightsRWX(srcI,Rights_OwnRead);
if (w<>0) then begin; CreateLink:=w;exit; end;
w:=FindDirEntry(srcI,ntry1,srcN);
if (w<>0) then begin; CreateLink:=w;exit; end;
if (srcI.CurrPos>=srcI.size) then begin; CreateLink:=7;exit; end;
w:=InodeReadUp(inod,ntry1.inode);
if (w<>0) then begin; CreateLink:=w;exit; end;
w:=CheckForRightsOWN(inod);
if (w<>0) then begin; CreateLink:=w;exit; end;
w:=ChgDirGetName(trgI,a,trgN);
if (w<>0) then begin; CreateLink:=w;exit; end;
w:=CheckForRightsRWX(trgI,Rights_OwnWrite);
if (w<>0) then begin; CreateLink:=w;exit; end;
w:=FindDirEntry(trgI,ntry2,trgN);
if (w<>0) then begin; CreateLink:=w;exit; end;
if (trgI.CurrPos<trgI.size) then begin; CreateLink:=8;exit; end;
w:=FillUpDirEntry(ntry2,a,trgN,ntry1.inode);
if (w<>0) then begin; CreateLink:=w;exit; end;
trgI.CurrPos:=trgI.size;
w:=StreamWrite(trgI,sizeof(ntry2),ntry2);
if (w<>0) then begin; CreateLink:=w;exit; end;
w:=InodeWriteBack(trgI,true);
if (w<>0) then begin; CreateLink:=w;exit; end;
inc(inod.linkCount);
w:=InodeWriteBack(inod,false);
CreateLink:=w;
End;





Function RenameEntry(srcN,trgN:String):Word;
Var
  inod,srcI,trgI:OneInodeRecord;
  ntry1,ntry2:OneDirEntryRecord;
  srcP,trgP:String;
  w:Word;
Begin;
w:=ChgDirGetName(srcI,srcP,srcN);
if (w<>0) then begin; RenameEntry:=w;exit; end;
w:=CheckForRightsRWX(srcI,Rights_OwnRead);
if (w<>0) then begin; RenameEntry:=w;exit; end;
w:=FindDirEntry(srcI,ntry1,srcN);
if (w<>0) then begin; RenameEntry:=w;exit; end;
if (srcI.CurrPos>=srcI.size) then begin; RenameEntry:=7;exit; end;
w:=InodeReadUp(inod,ntry1.inode);
if (w<>0) then begin; RenameEntry:=w;exit; end;
w:=CheckForRightsOWN(inod);
if (w<>0) then begin; RenameEntry:=w;exit; end;
srcP:=srcP+ntry1.name;
w:=ChgDirGetName(trgI,trgP,trgN);
if (w<>0) then begin; RenameEntry:=w;exit; end;
w:=CheckForRightsRWX(trgI,Rights_OwnWrite);
if (w<>0) then begin; RenameEntry:=w;exit; end;
w:=FindDirEntry(trgI,ntry2,trgN);
if (w<>0) then begin; RenameEntry:=w;exit; end;
if (trgI.CurrPos<trgI.size) then begin; RenameEntry:=8;exit; end;
if (inod.rights and Rights_Directory<>0) then
 if (copy(trgP,1,length(srcP))=srcP) then begin; RenameEntry:=11;exit; end;
w:=FillUpDirEntry(ntry2,trgP,trgN,ntry1.inode);
if (w<>0) then begin; RenameEntry:=w;exit; end;
w:=RemoveDirEntry(srcI);
if (w<>0) then begin; RenameEntry:=w;exit; end;
if (srcI.InodeNumber=trgI.InodeNumber) then begin;
  trgI:=srcI;
  end else begin;
  w:=InodeWriteBack(srcI,true);
  if (w<>0) then begin; RenameEntry:=w;exit; end;
  end;
trgI.CurrPos:=trgI.size;
w:=StreamWrite(trgI,sizeof(ntry2),ntry2);
if (w<>0) then begin; RenameEntry:=w;exit; end;
w:=InodeWriteBack(trgI,true);
RenameEntry:=w;
End;





Function OpenOneFile(var name:String;var inod:OneInodeRecord;mode:numberType):Word;
Var
  ntry:OneDirEntryRecord;
  a:String;
  w:Word;
Begin;
w:=ChgDirGetName(inod,a,name);
if (w<>0) then begin; OpenOneFile:=w;exit; end;
w:=CheckForRightsRWX(inod,Rights_OwnRead);
if (w<>0) then begin; OpenOneFile:=w;exit; end;
w:=FindDirEntry(inod,ntry,name);
if (w<>0) then begin; OpenOneFile:=w;exit; end;
if (inod.CurrPos>=inod.size) then begin; OpenOneFile:=7;exit; end;
name:=a+ntry.name;
w:=InodeReadUp(inod,ntry.inode);
if (w<>0) then begin; OpenOneFile:=w;exit; end;
if (inod.rights and Rights_Directory<>0) then begin; OpenOneFile:=12;exit; end;
w:=CheckForRightsRWX(inod,mode);
OpenOneFile:=w;
End;





Function DirectoryOpen(var name:String;var inod:OneInodeRecord):Word;
Var
  a:String;
  w:Word;
Begin;
if (copy(name,length(name),1)<>'\') then name:=name+'\';
w:=ChgDirGetName(inod,a,name);
if (w<>0) then begin; DirectoryOpen:=w;exit; end;
name:=a;
if (inod.rights and Rights_Directory=0) then begin; DirectoryOpen:=12;exit; end;
w:=CheckForRightsRWX(inod,Rights_OwnRead);
DirectoryOpen:=w;
End;





Function DirectoryRead(var inod:OneInodeRecord;var fi:OneInodeRecord;var fn:OneDirEntryRecord):Word;
Var w:Word;
Begin;
if (inod.CurrPos+sizeof(fn)>inod.size) then begin;
  fillchar(fi,sizeof(fi),0);
  fillchar(fn,sizeof(fn),0);
  DirectoryRead:=0;
  exit;
  end;
w:=StreamRead(inod,sizeof(fn),fn);
if (w<>0) then begin; DirectoryRead:=w;exit; end;
w:=InodeReadUp(fi,fn.inode);
DirectoryRead:=w;
End;





Function ChangeFileRight(name:String;mode,owner:numberType):Word;
Var
  inod:OneInodeRecord;
  ntry:OneDirEntryRecord;
  a:String;
  w:Word;
  i,o:numberType;
Begin;
w:=ChgDirGetName(inod,a,name);
if (w<>0) then begin; ChangeFileRight:=w;exit; end;
w:=CheckForRightsRWX(inod,Rights_OwnRead);
if (w<>0) then begin; ChangeFileRight:=w;exit; end;
w:=FindDirEntry(inod,ntry,name);
if (w<>0) then begin; ChangeFileRight:=w;exit; end;
if (inod.CurrPos>=inod.size) then begin; ChangeFileRight:=7;exit; end;
name:=a+ntry.name;
w:=InodeReadUp(inod,ntry.inode);
if (w<>0) then begin; ChangeFileRight:=w;exit; end;
w:=CheckForRightsOWN(inod);
if (w<>0) then begin; ChangeFileRight:=w;exit; end;
i:=Rights_OwnRead or Rights_OwnWrite or Rights_OwnExec
 or Rights_AnyRead or Rights_AnyWrite or Rights_AnyExec;
o:=Rights_Directory;
if (CurrentUser=UIDofAdmin) then i:=i or Rights_HasRootPriv;
inod.rights:=(mode and i) or (inod.rights and o);
if (CurrentUser=UIDofAdmin) then inod.owner:=owner;
w:=InodeWriteBack(inod,true);
ChangeFileRight:=w;
End;





Function ChangeFileDate(name:String;create,modify:OneDateTimeRecord):Word;
Var
  inod:OneInodeRecord;
  ntry:OneDirEntryRecord;
  a:String;
  w:Word;
Begin;
w:=ChgDirGetName(inod,a,name);
if (w<>0) then begin; ChangeFileDate:=w;exit; end;
w:=CheckForRightsRWX(inod,Rights_OwnRead);
if (w<>0) then begin; ChangeFileDate:=w;exit; end;
w:=FindDirEntry(inod,ntry,name);
if (w<>0) then begin; ChangeFileDate:=w;exit; end;
if (inod.CurrPos>=inod.size) then begin; ChangeFileDate:=7;exit; end;
name:=a+ntry.name;
w:=InodeReadUp(inod,ntry.inode);
if (w<>0) then begin; ChangeFileDate:=w;exit; end;
w:=CheckForRightsOWN(inod);
if (w<>0) then begin; ChangeFileDate:=w;exit; end;
inod.create:=create;
inod.modify:=modify;
w:=InodeWriteBack(inod,false);
ChangeFileDate:=w;
End;
