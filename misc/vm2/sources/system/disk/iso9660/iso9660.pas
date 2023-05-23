{$heap 127k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc crt.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

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
  OneDriveCommSearchRecord=record
    size:LongInt;
    rights:LongInt;
    owner:LongInt;
    create:xDirEntryDateTimeRec;
    modify:xDirEntryDateTimeRec;
    name:String;
    end;

{$include \sources\internet\kernel\utils\timer.inc}
{$include \sources\system\disk\cddriver.inc}
{$include cache.inc}
{$include isofs1.inc}
{$include isofs2.inc}


Label f1,f2,f3,f4;
Var
  DriveComm:OneDriveCommunicationRecord;
  DriveNtry:OneDriveCommSearchRecord;
  a,b,c:String;
  i,o,p,q:LongInt;
  sr:OneSearchRecord;
  dr:OneDirectoryRecord;
  fl:OneFileRecord;
BEGIN;
WriteLn('iso9660 filesystem v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<3) then ImmErr('using: iso9660.code <process> <drive> <letter> [track]');
a:=ParamStr(3);
DriveLetter:=LowCase(a[1]);
a:=ParamStr(1);
i:=BVal(ParamStr(2));
WriteLn('opening '+a+' '+BStr(i)+'...');
if CDdriverOpen(a,i) then ImmErr('error opening cd driver!');
if CDdriverIdentify(a,b,c) then ImmErr('error reading model number!');
CacheNum:=512;
WriteLn('model: "'+a+'"');
WriteLn('serial: "'+b+'"');
WriteLn('firm: "'+c+'"');
if InitSectorCache then ImmErr('error allocating cache!');

Write('waiting drive to get ready...');
BugOS_KernelUptime(i,p,o);
f1:
relequish;
if (GetTimePast(p)>10) then ImmErr(' failed!');
if discErr then goto f1;
WriteLn(' yes!');
a:=ParamStr(4);
if (a='') then i:=-1 else i:=BVal(a);
FindSpecifiedDataTrack(i);
FindISO9660idInTrack;

Write('locking the door...');
if CDdriverLockDoor(true) then writeln(' failed!') else writeln(' done!');

WriteLn('going to log in as '+DriveLetter+':\...');
FillChar(DriveComm,sizeof(DriveComm),0);
if (BugOS_driveLogin(DriveLetter,DriveComm)<>0) then begin;
  WriteLn('error logging in!');
  Halt(1);
  end;
BugOS_SignDaemoning;

f2:
if (DriveComm.cmd shr 16<>$8000) then begin;
  if keypressed then goto f4;
  Relequish;
  goto f2;
  end;
i:=DriveComm.cmd and $ffffff;
case i of
  $0e:begin;{read file}
    move(DriveComm.hdr,fl,sizeof(fl));
    DriveComm.cmd:=ReadFromFile(fl,DriveComm.siz,DriveComm.dat);
    move(fl,DriveComm.hdr,sizeof(fl));
    end;
  $10:begin;{seek file}
    DriveComm.cmd:=0;
    move(DriveComm.dat,i,sizeof(i));
    move(DriveComm.hdr,fl,sizeof(fl));
    fl.pos:=i;
    move(fl,DriveComm.hdr,sizeof(fl));
    end;
  $11:begin;{get file size}
    DriveComm.cmd:=0;
    move(DriveComm.hdr,fl,sizeof(fl));
    i:=fl.siz;
    move(i,DriveComm.dat,sizeof(i));
    end;
  $12:begin;{get file pos}
    DriveComm.cmd:=0;
    move(DriveComm.hdr,fl,sizeof(fl));
    i:=fl.pos;
    move(i,DriveComm.dat,sizeof(i));
    end;
  $0d:begin;{open file}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f3; end;
    dr.pat:=DriveComm.dir;
    DriveComm.cmd:=OpenOneFile(dr,fl,DriveComm.fn1);
    move(fl,DriveComm.hdr,sizeof(fl));
    i:=fl.beg;
    o:=$07;
    move(i,DriveComm.dat[1],sizeof(i));
    move(o,DriveComm.dat[5],sizeof(o));
    move(DriveComm.fn1,DriveComm.dat[9],sizeof(DriveComm.fn1));
    end;
  $01:begin;{change directory}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f3; end;
    dr.pat:=DriveComm.dir;
    DriveComm.cmd:=ChangeDir(dr,DriveComm.fn1);
    DriveComm.fn1:=dr.pat;
    end;
  $0c:begin;{read directory}
    move(DriveComm.hdr,sr,sizeof(sr));
    DriveComm.cmd:=FindNextDirEntry(sr);
    move(sr,DriveComm.hdr,sizeof(sr));
    DriveNtry.size:=sr.siz;
    case sr.flg of
      1:DriveNtry.rights:=$07;
      2:DriveNtry.rights:=$83;
      else DriveNtry.rights:=0;
      end;
    DriveNtry.owner:=0;
    DriveNtry.create:=sr.dat;
    DriveNtry.modify:=sr.dat;
    DriveNtry.name:=sr.nam;
    move(DriveNtry,DriveComm.dat,sizeof(DriveNtry));
    end;
  $0b:begin;{open directory}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f3; end;
    dr.pat:=DriveComm.dir;
    DriveComm.cmd:=ChangeDir(dr,DriveComm.fn1);
    BeginFindNext(dr,sr);
    move(sr,DriveComm.hdr,sizeof(sr));
    i:=dr.beg;
    o:=$07;
    move(i,DriveComm.dat[1],sizeof(i));
    move(o,DriveComm.dat[5],sizeof(o));
    move(dr.pat,DriveComm.dat[9],sizeof(dr.pat));
    end;
  $0f:begin;{write file}
    DriveComm.cmd:=4;
    end;
  $13:begin;{truncate file}
    DriveComm.cmd:=4;
    end;
  $03:begin;{create directory}
    DriveComm.cmd:=4;
    end;
  $04:begin;{erase directory}
    DriveComm.cmd:=4;
    end;
  $05:begin;{create file}
    DriveComm.cmd:=4;
    end;
  $06:begin;{erase file}
    DriveComm.cmd:=4;
    end;
  $07:begin;{rename}
    DriveComm.cmd:=4;
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
    i:=0;o:=volumeSiz;p:=0;q:=sizeof(OneSectorData);
    move(i,DriveComm.dat[1],sizeof(i));
    move(o,DriveComm.dat[5],sizeof(o));
    move(p,DriveComm.dat[9],sizeof(p));
    move(q,DriveComm.dat[13],sizeof(q));
    DriveComm.cmd:=0;
    end;
  else DriveComm.cmd:=1;
  end;
f3:
BugOS_driveDone;
goto f2;

f4:
Write('unlocking the door...');
if CDdriverLockDoor(false) then writeln(' failed!') else writeln(' done!');
write('opening the door...');
if CDdriverOpenDoor(true) then writeln(' failed!') else writeln(' done!');
END.