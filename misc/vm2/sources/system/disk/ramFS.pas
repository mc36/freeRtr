{$heap 83k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc datetime.inc}
{$sysinc bugos.inc}
{$include ramfs.inc}

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
Var
  DriveComm:OneDriveCommunicationRecord;
  DriveLetter:Char;


Procedure justName(var a:String;full:Boolean);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
o:=0;
for i:=1 to ab0 do if (ab[i]=$5c) then o:=i;
a:=copy(a,o+1,255);
if full then a:=DriveLetter+':\'+a;
End;

Label f1,f2;
Var
  hdr:OneFileHandlerRecord;
  ntry:OneFileEntryRecord;
  time1,time2:xDirEntryDateTimeRec;
  i,o,p:LongInt;
  a:String;
BEGIN;
WriteLn('ramFS v1.0, done by Mc at '#%date' '#%time'.');

a:=GetAllParameters;
if (a='') then begin;
  WriteLn('using: ramfs.code <driveLetter> [maximumSize]');
  Halt(1);
  end;
DriveLetter:=LowCase(a[1]);
i:=pos(' ',a);
if (i<1) then i:=666;
a:=copy(a,i+1,255);
MaxMemSize:=BVal(a)*1024;
if (MaxMemSize<1) then MaxMemSize:=$7fffffff;

WriteLn('this disk has maximum '+BStr(MaxMemSize)+' bytes capacity!');
initializeToZero;
WriteLn('going to log in as '+DriveLetter+':\...');
FillChar(DriveComm,sizeof(DriveComm),0);
if (BugOS_driveLogin(DriveLetter,DriveComm)<>0) then begin;
  WriteLn('error logging in!');
  Halt(1);
  end;
BugOS_SignDaemoning;

f1:
if (DriveComm.cmd shr 16<>$8000) then begin;
  Relequish;
  goto f1;
  end;
i:=DriveComm.cmd and $ffffff;
case i of
  $0e:begin;{read file}
    move(DriveComm.hdr,hdr,sizeof(hdr));
    i:=ReopenOneFile(hdr);
    if (i<>0) then begin;
      DriveComm.cmd:=i;
      goto f2;
      end;
    DriveComm.cmd:=ReadFromFile(hdr,DriveComm.dat,DriveComm.siz);
    move(hdr,DriveComm.hdr,sizeof(hdr));
    end;
  $0f:begin;{write file}
    move(DriveComm.hdr,hdr,sizeof(hdr));
    i:=ReopenOneFile(hdr);
    if (i<>0) then begin;
      DriveComm.cmd:=i;
      goto f2;
      end;
    DriveComm.cmd:=WriteToFile(hdr,DriveComm.dat,DriveComm.siz);
    move(hdr,DriveComm.hdr,sizeof(hdr));
    end;
  $10:begin;{seek file}
    move(DriveComm.hdr,hdr,sizeof(hdr));
    i:=ReopenOneFile(hdr);
    if (i<>0) then begin;
      DriveComm.cmd:=i;
      goto f2;
      end;
    move(DriveComm.dat,i,sizeof(i));
    hdr.pos:=i;
    DriveComm.cmd:=0;
    move(hdr,DriveComm.hdr,sizeof(hdr));
    end;
  $11:begin;{get file size}
    move(DriveComm.hdr,hdr,sizeof(hdr));
    DriveComm.cmd:=ReopenOneFile(hdr);
    i:=hdr.siz;
    move(i,DriveComm.dat,sizeof(i));
    end;
  $12:begin;{get file pos}
    move(DriveComm.hdr,hdr,sizeof(hdr));
    DriveComm.cmd:=ReopenOneFile(hdr);
    i:=hdr.pos;
    move(i,DriveComm.dat,sizeof(i));
    end;
  $13:begin;{truncate file}
    move(DriveComm.hdr,hdr,sizeof(hdr));
    i:=ReopenOneFile(hdr);
    if (i<>0) then begin;
      DriveComm.cmd:=i;
      goto f2;
      end;
    DriveComm.cmd:=TruncateFile(hdr);
    move(hdr,DriveComm.hdr,sizeof(hdr));
    end;
  $0d:begin;{open file}
    justName(DriveComm.fn1,false);
    i:=OpenOneFile(hdr,DriveComm.fn1);
    if (i<>0) then begin;
      DriveComm.cmd:=i;
      goto f2;
      end;
    if readDirEntry(hdr.num,ntry) then begin; DriveComm.cmd:=14;goto f2; end;
    DriveComm.cmd:=4;
    if (DriveComm.uid<>0) then begin;
      o:=ntry.rights;
      if (DriveComm.uid<>ntry.owner) then o:=o shr 3;
      i:=DriveComm.rgt and 7;
      if (o and i<>i) then goto f2;
      end;
    justName(DriveComm.fn1,true);
    move(hdr,DriveComm.hdr,sizeof(hdr));
    i:=hdr.id;
    o:=hdr.rgt;
    move(i,DriveComm.dat[1],sizeof(i));
    move(o,DriveComm.dat[5],sizeof(o));
    move(DriveComm.fn1,DriveComm.dat[9],sizeof(DriveComm.fn1));
    DriveComm.cmd:=0;
    end;
  $01:begin;{change directory}
    DriveComm.cmd:=0;
    DriveComm.fn1:=DriveLetter+':\';
    end;
  $0c:begin;{read directory}
    move(DriveComm.hdr,o,sizeof(o));
    if (o>=filesNum) then fillChar(ntry,sizeof(ntry),0) else begin;
      inc(o);
      readDirEntry(o,ntry);
      end;
    DriveComm.cmd:=0;
    move(o,DriveComm.hdr,sizeof(o));
    move(ntry,DriveComm.dat[1],sizeof(ntry));
    end;
  $0b:begin;{open directory}
    DriveComm.fn1:=DriveLetter+':\';
    DriveComm.cmd:=0;
    i:=0;o:=$03;
    move(i,DriveComm.hdr,sizeof(i));
    move(i,DriveComm.dat[1],sizeof(i));
    move(o,DriveComm.dat[5],sizeof(o));
    move(DriveComm.fn1,DriveComm.dat[9],sizeof(DriveComm.fn1));
    end;
  $03:begin;{create directory}
    DriveComm.cmd:=4;
    end;
  $04:begin;{erase directory}
    DriveComm.cmd:=4;
    end;
  $05:begin;{create file}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    justName(DriveComm.fn1,false);
    DriveComm.cmd:=CreateNewFile(DriveComm.fn1);
    end;
  $06:begin;{erase file}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    justName(DriveComm.fn1,false);
    DriveComm.cmd:=DeleteOneFile(DriveComm.fn1);
    end;
  $07:begin;{rename}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    justName(DriveComm.fn1,false);
    justName(DriveComm.fn2,false);
    DriveComm.cmd:=RenameOneFile(DriveComm.fn1,DriveComm.fn2);
    end;
  $08:begin;{make link}
    DriveComm.cmd:=4;
    end;
  $09:begin;{set rights}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    justName(DriveComm.fn1,false);
    p:=findOneDirEntry(DriveComm.fn1);
    if (p<1) then begin; DriveComm.cmd:=7;goto f2; end;
    if readDirEntry(p,ntry) then begin; DriveComm.cmd:=14;goto f2; end;
    move(DriveComm.dat[1],i,sizeof(i));
    move(DriveComm.dat[5],o,sizeof(o));
    ntry.rights:=i;
    ntry.owner:=o;
    if writeDirEntry(p,ntry) then begin; DriveComm.cmd:=14;goto f2; end;
    DriveComm.cmd:=0;
    end;
  $0a:begin;{set date}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    justName(DriveComm.fn1,false);
    p:=findOneDirEntry(DriveComm.fn1);
    if (p<1) then begin; DriveComm.cmd:=7;goto f2; end;
    if readDirEntry(p,ntry) then begin; DriveComm.cmd:=14;goto f2; end;
    move(DriveComm.dat[1],time1,sizeof(time1));
    move(DriveComm.dat[sizeof(time1)+1],time2,sizeof(time2));
    ntry.created:=time1;
    ntry.modified:=time2;
    if writeDirEntry(p,ntry) then begin; DriveComm.cmd:=14;goto f2; end;
    DriveComm.cmd:=0;
    end;
  $02:begin;{drive statistics}
    o:=MemorySiz-MemorySys;
    p:=MemorySys-1;
    i:=MaxMemSize-MemorySiz+1;
    move(i,DriveComm.dat[1],sizeof(i));
    move(o,DriveComm.dat[5],sizeof(o));
    move(p,DriveComm.dat[9],sizeof(p));
    i:=1;
    move(i,DriveComm.dat[13],sizeof(i));
    DriveComm.cmd:=0;
    end;
  else DriveComm.cmd:=1;
  end;
f2:
BugOS_driveDone;
goto f1;
END.