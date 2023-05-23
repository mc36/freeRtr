{$heap 767k}
{$stack 3k}
{$sysinc system.inc}
{{$sysinc filesys.inc}
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

Type
  OneDriveCommunicationRecord=record
    cmd:longint;
    uid:longint;
    rgt:longint;
    siz:longint;
    dir:string;
    fn1:string;
    fn2:string;
    hdr:array[1..512] of byte;
    dat:array[1..65536] of byte;
    end;

Label f1,f2,f3;
Var
  DriveComm:OneDriveCommunicationRecord;
  ntry,ntry2,ntry3:OneDirectoryEntryRecord;
  dir,dir2,dir3:OneDirectoryDescRecord;
  fil:OneFileRecord;
  a,b,c:String;
  i,o,p,q:LongInt;
BEGIN;
WriteLn(ProggyName+', done by Mc at '#%date' '#%time'.');
Randomize;
if (paramCount<3) then begin;
  WriteLn('using: ntfs.code <process> <drive> <letter>');
  Halt(1);
  end;
a:=ParamStr(2);
i:=BVal(a);
a:=ParamStr(1);
WriteLn('opening '+a+' '+BStr(i)+'...');
if (DriveOpen(a,i)<>0) then begin;
  WriteLn('error opening drive!');
  Halt(1);
  end;
DriveReadOnly:=False;
WriteLn('this disk has '+BStr(DriveSize*512)+' bytes total capacity.');
if ReadUpBootSector then immErr('this is not a ntfs drive!');
if ReadUpMftListFromDisk then immErr('error reading mft!');

a:=kicsi(ParamStr(3));
driveLetter:=a[1];
curPath:='';
WriteLn('going to log in as '+DriveLetter+':\...');
FillChar(DriveComm,sizeof(DriveComm),0);
if (BugOS_driveLogin(DriveLetter,DriveComm)<>0) then begin;
  WriteLn('error logging in!');
  Halt(1);
  end;
BugOS_SignDaemoning;

f1:
if (DriveComm.cmd shr 16<>$8000) then begin;
  if keypressed then goto f3;
  Relequish;
  goto f1;
  end;
i:=DriveComm.cmd and $ffffff;
case i of
  $0e:begin;{read file}
    move(DriveComm.hdr,fil,sizeof(fil));
    DriveComm.cmd:=1;
    if OpenOneMFTfile(fil.fileMft,dir2.fil,1) then goto f2;
    if FindOneAttribute(dir2.fil,dir.fil,$80,2) then goto f2;
    dir.fil.datPos:=fil.pos;
    i:=ReadFromAttribute(dir.fil,DriveComm.siz,DriveComm.dat);
    fil.pos:=dir.fil.datPos;
    if (i<>DriveComm.siz) then goto f2;
    DriveComm.cmd:=0;
    move(fil,DriveComm.hdr,sizeof(fil));
    end;
  $0f:begin;{write file}
    move(DriveComm.hdr,fil,sizeof(fil));
    DriveComm.cmd:=1;
    if OpenOneMFTfile(fil.fileMft,dir2.fil,1) then goto f2;
    if FindOneAttribute(dir2.fil,dir.fil,$80,2) then goto f2;
    i:=fil.pos+DriveComm.siz;
    if (i>fil.size) then begin;
      if AppendRunsListBySome(dir.fil,dir2.fil,dir3.fil,i,1,3) then goto f2;
      if CloseOneMFTfile(dir.fil) then goto f2;
      end;
    dir.fil.datPos:=fil.pos;
    i:=WriteToAttribute(dir.fil,DriveComm.siz,DriveComm.dat);
    if (i<>DriveComm.siz) then goto f2;
    fil.pos:=dir.fil.datPos;
    fil.size:=dir.fil.datSiz;
    if dir.fil.residnt then if CloseOneMFTfile(dir.fil) then goto f2;
    UpdateFileSizeInEntry(fil.dirMft,fil.fileMft,fil.size);
    DriveComm.cmd:=0;
    move(fil,DriveComm.hdr,sizeof(fil));
    end;
  $10:begin;{seek file}
    DriveComm.cmd:=0;
    move(DriveComm.dat,i,sizeof(i));
    move(DriveComm.hdr,fil,sizeof(fil));
    fil.pos:=i;
    move(fil,DriveComm.hdr,sizeof(fil));
    end;
  $11:begin;{get file size}
    DriveComm.cmd:=0;
    move(DriveComm.hdr,fil,sizeof(fil));
    i:=fil.size;
    move(i,DriveComm.dat,sizeof(i));
    end;
  $12:begin;{get file pos}
    DriveComm.cmd:=0;
    move(DriveComm.hdr,fil,sizeof(fil));
    i:=fil.pos;
    move(i,DriveComm.dat,sizeof(i));
    end;
  $0d:begin;{open file}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    DriveComm.cmd:=7;
    curPath:=DriveComm.dir;
    a:=GetNameAndChgDir(DriveComm.fn1,dir);
    if (a='') then goto f2;
    DriveComm.fn1:=curPath+a;
    if FindEntryInDirectory(a,dir,dir2,ntry,2) then goto f2;
    if ntry.dir then goto f2;
    if OpenOneMFTfile(ntry.mft,dir2.fil,1) then goto f2;
    if FindOneAttribute(dir2.fil,dir.fil,$80,2) then goto f2;
    fillchar(fil,sizeof(fil),0);
    fil.dirMft:=dir.mft;
    fil.fileMft:=ntry.mft;
    fil.pos:=dir.fil.datPos;
    fil.size:=dir.fil.datSiz;
    DriveComm.cmd:=0;
    move(fil,DriveComm.hdr,sizeof(fil));
    i:=fil.fileMft;
    o:=$07;
    move(i,DriveComm.dat[1],sizeof(i));
    move(o,DriveComm.dat[5],sizeof(o));
    move(DriveComm.fn1,DriveComm.dat[9],sizeof(DriveComm.fn1));
    end;
  $01:begin;{change directory}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    DriveComm.cmd:=7;
    curPath:=DriveComm.dir;
    a:=ChangeDir(DriveComm.fn1,dir);
    if (a='') then goto f2;
    DriveComm.fn1:=a;
    DriveComm.cmd:=0;
    end;
  $0c:begin;{read directory}
    move(DriveComm.hdr,dir,sizeof(dir));
    DriveComm.cmd:=1;
    if OpenOneMFTfile(dir.mft,dir.fil,1) then
     fillchar(ntry,sizeof(ntry),0) else
     if FindNextDirEntry(dir,dir2,ntry,2) then
      fillchar(ntry,sizeof(ntry),0);
    DriveComm.cmd:=0;
    move(dir,DriveComm.hdr,sizeof(dir));
    move(ntry,DriveComm.dat,sizeof(ntry));
    end;
  $0b:begin;{open directory}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    DriveComm.cmd:=1;
    curPath:=DriveComm.dir;
    a:=ChangeDir(DriveComm.fn1,dir);
    if (a='') then goto f2;
    DriveComm.fn1:=a;
    dir.pos:=0;
    dir.rot:=true;
    DriveComm.cmd:=0;
    move(dir,DriveComm.hdr,sizeof(dir));
    p:=dir.mft;
    o:=$07;
    move(p,DriveComm.dat[1],sizeof(p));
    move(o,DriveComm.dat[5],sizeof(o));
    move(DriveComm.fn1,DriveComm.dat[9],sizeof(DriveComm.fn1));
    end;
  $13:begin;{truncate file}
    move(DriveComm.hdr,fil,sizeof(fil));
    DriveComm.cmd:=1;
    fil.size:=fil.pos;
    if OpenOneMFTfile(fil.fileMft,dir2.fil,1) then goto f2;
    if FindOneAttribute(dir2.fil,dir.fil,$80,2) then goto f2;
    if AppendRunsListBySome(dir.fil,dir2.fil,dir3.fil,fil.pos,1,3) then goto f2;
    if TruncateRunsListBySome(dir.fil,dir2.fil,dir3.fil,fil.pos,1,3) then goto f2;
    if CloseOneMFTfile(dir.fil) then goto f2;
    UpdateFileSizeInEntry(fil.dirMft,fil.fileMft,fil.size);
    DriveComm.cmd:=0;
    move(fil,DriveComm.hdr,sizeof(fil));
    end;
  $05:begin;{create file}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    curPath:=DriveComm.dir;
    DriveComm.cmd:=1;
    a:=GetNameAndChgDir(DriveComm.fn1,dir);
    if (a='') then goto f2;
    DriveComm.cmd:=DoInsertOneEntry(dir,false,a);
    end;
  $06:begin;{erase file}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    curPath:=DriveComm.dir;
    DriveComm.cmd:=1;
    a:=GetNameAndChgDir(DriveComm.fn1,dir);
    if (a='') then goto f2;
    DriveComm.cmd:=DoDeleteOneFile(dir,a);
    end;
  $03:begin;{create directory}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    curPath:=DriveComm.dir;
    DriveComm.cmd:=1;
    a:=GetNameAndChgDir(DriveComm.fn1,dir);
    if (a='') then goto f2;
    DriveComm.cmd:=DoInsertOneEntry(dir,true,a);
    end;
  $04:begin;{erase directory}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    curPath:=DriveComm.dir;
    DriveComm.cmd:=1;
    a:=GetNameAndChgDir(DriveComm.fn1,dir);
    if (a='') then goto f2;
    DriveComm.cmd:=DoDeleteOneDir(dir,a);
    end;
  $07:begin;{rename}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    curPath:=DriveComm.dir;
    DriveComm.cmd:=1;
    a:=GetNameAndChgDir(DriveComm.fn1,dir);
    if (a='') then goto f2;
    b:=GetNameAndChgDir(DriveComm.fn2,dir2);
    if (b='') then goto f2;
    DriveComm.cmd:=DoRenameOneFile(dir.mft,dir2.mft,a,b);
    end;
  $08:begin;{make link}
    DriveComm.cmd:=4;
    end;
  $09:begin;{set rights}
    DriveComm.cmd:=4;
    end;
  $0a:begin;{set date}
    DriveComm.cmd:=4;
    end;
  $02:begin;{drive statistics}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    GetDiskStatistics(i,o,p);
    q:=ClusterSiz*512;
    move(i,DriveComm.dat[1],sizeof(i));
    move(o,DriveComm.dat[5],sizeof(o));
    move(p,DriveComm.dat[9],sizeof(p));
    move(q,DriveComm.dat[13],sizeof(q));
    DriveComm.cmd:=0;
    end;
  else DriveComm.cmd:=1;
  end;
f2:
BugOS_driveDone;
goto f1;

f3:
WriteLn('exiting...');
END.