{$heap 143k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc datetime.inc}
{$sysinc bugos.inc}
{$include fs_all.inc}
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
Label f1,f2;
Var
  lastUpdateDay:LongInt;
  inod1,inod2:OneinodeRecord;
  time1,time2:OneDateTimeRecord;
  ntry1:OneDirEntryRecord;
  DriveComm:OneDriveCommunicationRecord;
  a:String;
  i,o,p,q:LongInt;
BEGIN;
WriteLn(FilesysName+', done by Mc at '#%date' '#%time'.');
if (ParamCount<>3) then begin;
  WriteLn('using: filesys.code <process> <drive> <driveLetter>');
  Halt(1);
  end;
a:=ParamStr(3);
DriveLetter:=LowCase(a[1]);
a:=ParamStr(2);
i:=BVal(a);
a:=ParamStr(1);
WriteLn('opening '+a+' '+BStr(i)+'...');
if (DriveOpen(a,i)<>0) then begin;
  WriteLn('error opening drive!');
  Halt(1);
  end;
CacheInit;
WriteLn('this disk has '+BStr(DriveSize*BytesPerSector)+' bytes total capacity.');
if (FilesysOpen(false)<>0) then begin;
  WriteLn('error opening filesystem!');
  Halt(1);
  end;
WriteLn('going to log in as '+DriveLetter+':\...');
FillChar(DriveComm,sizeof(DriveComm),0);
if (BugOS_driveLogin(DriveLetter,DriveComm)<>0) then begin;
  WriteLn('error logging in!');
  Halt(1);
  end;
lastUpdateDay:=0;
BugOS_SignDaemoning;

f1:
if (DriveComm.cmd shr 16<>$8000) then begin;
  Relequish;
  goto f1;
  end;
i:=DriveComm.cmd and $ffffff;
CurrentUser:=DriveComm.uid;
CurrentPath:=copy(DriveComm.dir,3,255);
case i of
  $0e:begin;{read file}
    move(DriveComm.hdr,inod1,sizeof(inod1));
    i:=InodeReRead(inod1);
    if (i<>0) then begin;
      DriveComm.cmd:=i;
      goto f2;
      end;
    DriveComm.cmd:=StreamRead(inod1,DriveComm.siz,DriveComm.dat);
    move(inod1,DriveComm.hdr,sizeof(inod1));
    end;
  $0f:begin;{write file}
    move(DriveComm.hdr,inod1,sizeof(inod1));
    i:=InodeReRead(inod1);
    if (i<>0) then begin;
      DriveComm.cmd:=i;
      goto f2;
      end;
    i:=StreamWrite(inod1,DriveComm.siz,DriveComm.dat);
    if (i<>0) then begin;
      DriveComm.cmd:=i;
      goto f2;
      end;
    DriveComm.cmd:=InodeWriteBack(inod1,true);
    move(inod1,DriveComm.hdr,sizeof(inod1));
    end;
  $10:begin;{seek file}
    move(DriveComm.hdr,inod1,sizeof(inod1));
    i:=InodeReRead(inod1);
    if (i<>0) then begin;
      DriveComm.cmd:=i;
      goto f2;
      end;
    move(DriveComm.dat,i,sizeof(i));
    DriveComm.cmd:=StreamSetPos(inod1,i);
    move(inod1,DriveComm.hdr,sizeof(inod1));
    end;
  $11:begin;{get file size}
    move(DriveComm.hdr,inod1,sizeof(inod1));
    DriveComm.cmd:=InodeReRead(inod1);
    i:=inod1.size;
    move(i,DriveComm.dat,sizeof(i));
    end;
  $12:begin;{get file pos}
    move(DriveComm.hdr,inod1,sizeof(inod1));
    DriveComm.cmd:=InodeReRead(inod1);
    i:=inod1.CurrPos;
    move(i,DriveComm.dat,sizeof(i));
    end;
  $13:begin;{truncate file}
    move(DriveComm.hdr,inod1,sizeof(inod1));
    i:=InodeReRead(inod1);
    if (i<>0) then begin;
      DriveComm.cmd:=i;
      goto f2;
      end;
    i:=StreamTruncate(inod1);
    if (i<>0) then begin;
      DriveComm.cmd:=i;
      goto f2;
      end;
    DriveComm.cmd:=InodeWriteBack(inod1,true);
    move(inod1,DriveComm.hdr,sizeof(inod1));
    end;
  $0d:begin;{open file}
    DriveComm.cmd:=OpenOneFile(DriveComm.fn1,inod1,DriveComm.rgt);
    move(inod1,DriveComm.hdr,sizeof(inod1));
    i:=inod1.InodeNumber;
    o:=inod1.rights;
    move(i,DriveComm.dat[1],sizeof(i));
    move(o,DriveComm.dat[5],sizeof(o));
    move(DriveComm.fn1,DriveComm.dat[9],sizeof(DriveComm.fn1));
    end;
  $01:begin;{change directory}
    DriveComm.cmd:=ChangeDir(inod1,DriveComm.fn1);
    end;
  $0c:begin;{read directory}
    move(DriveComm.hdr,inod1,sizeof(inod1));
    i:=InodeReRead(inod1);
    if (i<>0) then begin;
      DriveComm.cmd:=i;
      goto f2;
      end;
    DriveComm.cmd:=DirectoryRead(inod1,inod2,ntry1);
    move(inod1,DriveComm.hdr,sizeof(inod1));
    move(inod2,DriveComm.dat[1],sizeof(inod2));
    move(ntry1.name,DriveComm.dat[27],sizeof(ntry1));
    end;
  $0b:begin;{open directory}
    DriveComm.cmd:=DirectoryOpen(DriveComm.fn1,inod1);
    move(inod1,DriveComm.hdr,sizeof(inod1));
    i:=inod1.InodeNumber;
    o:=inod1.rights;
    move(i,DriveComm.dat[1],sizeof(i));
    move(o,DriveComm.dat[5],sizeof(o));
    move(DriveComm.fn1,DriveComm.dat[9],sizeof(DriveComm.fn1));
    end;
  $03:begin;{create directory}
    DriveComm.cmd:=CreateDir(DriveComm.fn1);
    end;
  $04:begin;{erase directory}
    DriveComm.cmd:=EraseDir(DriveComm.fn1);
    end;
  $05:begin;{create file}
    DriveComm.cmd:=CreateFile(DriveComm.fn1);
    end;
  $06:begin;{erase file}
    DriveComm.cmd:=EraseFile(DriveComm.fn1);
    end;
  $07:begin;{rename}
    DriveComm.cmd:=RenameEntry(DriveComm.fn1,DriveComm.fn2);
    end;
  $08:begin;{make link}
    DriveComm.cmd:=CreateLink(DriveComm.fn1,DriveComm.fn2);
    end;
  $09:begin;{set rights}
    move(DriveComm.dat[1],i,sizeof(i));
    move(DriveComm.dat[5],o,sizeof(o));
    DriveComm.cmd:=ChangeFileRight(DriveComm.fn1,i,o);
    end;
  $0a:begin;{set date}
    move(DriveComm.dat[1],time1,sizeof(time1));
    move(DriveComm.dat[sizeof(time1)+1],time2,sizeof(time2));
    DriveComm.cmd:=ChangeFileDate(DriveComm.fn1,time1,time2);
    end;
  $02:begin;{drive statistics}
    BitmapCountDiskUsage(i,o,p,q);
    move(i,DriveComm.dat[1],sizeof(i));
    move(o,DriveComm.dat[5],sizeof(o));
    move(p,DriveComm.dat[9],sizeof(p));
    move(q,DriveComm.dat[13],sizeof(q));
    DriveComm.cmd:=0;
    end;
  else DriveComm.cmd:=1;
  end;
f2:
i:=CacheFlush;
if (i<>0) then if (DriveComm.cmd=0) then DriveComm.cmd:=i;
BugOS_driveDone;
BugOS_KernelUptime(i,o,p);
if (i=lastUpdateDay) then goto f1;
lastUpdateDay:=i;
FilesysClose(false);
CacheFlush;
goto f1;

{FilesysClose(true);
CacheFlush;
DriveClose;}
END.