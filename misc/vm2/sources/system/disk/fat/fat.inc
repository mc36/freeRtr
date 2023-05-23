{$heap 127k}
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

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

{$include fat1.inc}
{$include fat2.inc}
{$include fat3.inc}
{$include fat4.inc}
{$include fat5.inc}

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
  a,b,c:String;
  i,o,p,q:LongInt;
  sr,sr2:OneSearchedRecord;
  tim1,tim2:xDirEntryDateTimeRec;
  fl:OneStreamRecord;
BEGIN;
WriteLn(ProggyName+' filesystem v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<3) then begin;
  WriteLn('using: '+proggyName+'.code <process> <drive> <letter> [lfn/nolfn]');
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
if ReadUpBootSector then immErr('this is not a '+proggyName+' drive!');
a:=kicsi(ParamStr(3));
driveLetter:=a[1];
a:=kicsi(ParamStr(4));
i:=8;
if (a='lfn') then i:=1;
if (a='nolfn') then i:=0;
UseLFN:=(i=1);
currentPath:='';
if (i>1) then DetectLongFileNameUsage;
if UseLFN then a:='enabled' else a:='disabled';
WriteLn('lfn usage '+a+'.');

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
    move(DriveComm.hdr,fl,sizeof(fl));
    DriveComm.cmd:=streamRead(fl,DriveComm.dat,DriveComm.siz);
    move(fl,DriveComm.hdr,sizeof(fl));
    end;
  $0f:begin;{write file}
    move(DriveComm.hdr,fl,sizeof(fl));
    if chainReopen(fl) then begin; DriveComm.cmd:=9;goto f2; end;
    if streamWrite(fl,DriveComm.dat,DriveComm.siz) then begin; DriveComm.cmd:=14;goto f2; end;
    DriveComm.cmd:=updateDirEntry(fl,true);
    move(fl,DriveComm.hdr,sizeof(fl));
    end;
  $10:begin;{seek file}
    DriveComm.cmd:=0;
    move(DriveComm.dat,i,sizeof(i));
    move(DriveComm.hdr,fl,sizeof(fl));
    fl.posCur:=i;
    move(fl,DriveComm.hdr,sizeof(fl));
    end;
  $11:begin;{get file size}
    DriveComm.cmd:=0;
    move(DriveComm.hdr,fl,sizeof(fl));
    i:=fl.posMax;
    move(i,DriveComm.dat,sizeof(i));
    end;
  $12:begin;{get file pos}
    DriveComm.cmd:=0;
    move(DriveComm.hdr,fl,sizeof(fl));
    i:=fl.posCur;
    move(i,DriveComm.dat,sizeof(i));
    end;
  $0d:begin;{open file}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    currentPath:=DriveComm.dir;
    if SelectFileName(DriveComm.fn1,sr) then begin; DriveComm.cmd:=7;goto f2; end;
    DriveComm.cmd:=chainOpen(fl,sr.cls);
    fl.posMax:=sr.siz;
    fl.parent:=sr.prn;
    move(fl,DriveComm.hdr,sizeof(fl));
    i:=fl.clst1st;
    o:=$07;
    move(i,DriveComm.dat[1],sizeof(i));
    move(o,DriveComm.dat[5],sizeof(o));
    move(DriveComm.fn1,DriveComm.dat[9],sizeof(DriveComm.fn1));
    end;
  $01:begin;{change directory}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    currentPath:=DriveComm.dir;
    DriveComm.cmd:=SelectDirectory(DriveComm.fn1,p);
    end;
  $0c:begin;{read directory}
    move(DriveComm.hdr,fl,sizeof(fl));
    if ReadNextDirEntry(fl,sr,true) then fillChar(sr,sizeof(sr),0);
    DriveComm.cmd:=0;
    move(fl,DriveComm.hdr,sizeof(fl));
    move(sr,DriveComm.dat,sizeof(sr));
    end;
  $0b:begin;{open directory}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    currentPath:=DriveComm.dir;
    if SelectDirectory(DriveComm.fn1,p) then begin; DriveComm.cmd:=6;goto f2; end;
    DriveComm.cmd:=openOneDirCluster(fl,p);
    move(fl,DriveComm.hdr,sizeof(fl));
    o:=$07;
    move(p,DriveComm.dat[1],sizeof(p));
    move(o,DriveComm.dat[5],sizeof(o));
    move(DriveComm.fn1,DriveComm.dat[9],sizeof(DriveComm.fn1));
    end;
  $13:begin;{truncate file}
    move(DriveComm.hdr,fl,sizeof(fl));
    if chainReopen(fl) then begin; DriveComm.cmd:=9;goto f2; end;
    if chainTrunc(fl,(fl.posCur div 512) div ClusterSiz) then begin; DriveComm.cmd:=14;goto f2; end;
    fl.posMax:=fl.posCur;
    DriveComm.cmd:=updateDirEntry(fl,true);
    move(fl,DriveComm.hdr,sizeof(fl));
    end;
  $05:begin;{create file}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    currentPath:=DriveComm.dir;
    b:=DriveComm.fn1;
    ExtractFilePathName(a,b);
    if SelectDirectory(a,p) then begin; DriveComm.cmd:=6;goto f2; end;
    if not searchOneDirEntry(p,b,sr) then begin; DriveComm.cmd:=8;goto f2; end;
    q:=findEmptyCluster;
    if (q<1) then begin; DriveComm.cmd:=3;goto f2; end;
    WriteOneFatEntry(q,ClustEndE);
    DriveComm.cmd:=appendOneDirEntry(p,b,$20,q,0);
    end;
  $06:begin;{erase file}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    currentPath:=DriveComm.dir;
    if SelectFileName(DriveComm.fn1,sr) then begin; DriveComm.cmd:=7;goto f2; end;
    if (sr.rgt and xRights_Directory<>0) then begin; DriveComm.cmd:=12;goto f2; end;
    if chainErase(sr.cls) then begin; DriveComm.cmd:=14;goto f2; end;
    DriveComm.cmd:=eraseOneDirEntry(sr.prn,sr.nam);
    end;
  $03:begin;{create directory}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    currentPath:=DriveComm.dir;
    b:=DriveComm.fn1;
    ExtractFilePathName(a,b);
    if SelectDirectory(a,p) then begin; DriveComm.cmd:=6;goto f2; end;
    if not searchOneDirEntry(p,b,sr) then begin; DriveComm.cmd:=8;goto f2; end;
    q:=findEmptyCluster;
    if (q<1) then begin; DriveComm.cmd:=3;goto f2; end;
    WriteOneFatEntry(q,ClustEndE);
    if appendOneDirEntry(p,b,$10,q,0) then begin; DriveComm.cmd:=17;goto f2; end;
    DriveComm.cmd:=WriteInitialDirSector(q,p,true);
    end;
  $04:begin;{erase directory}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    currentPath:=DriveComm.dir;
    if SelectFileName(DriveComm.fn1,sr) then begin; DriveComm.cmd:=7;goto f2; end;
    if (sr.rgt and xRights_Directory=0) then begin; DriveComm.cmd:=12;goto f2; end;
    if openOneDirCluster(fl,sr.cls) then begin; DriveComm.cmd:=14;goto f2; end;
    if not ReadNextDirEntry(fl,sr2,true) then begin; DriveComm.cmd:=10;goto f2; end;
    if chainErase(sr.cls) then begin; DriveComm.cmd:=14;goto f2; end;
    DriveComm.cmd:=eraseOneDirEntry(sr.prn,sr.nam);
    end;
  $07:begin;{rename}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    currentPath:=DriveComm.dir;
    if SelectFileName(DriveComm.fn1,sr) then begin; DriveComm.cmd:=7;goto f2; end;
    b:=DriveComm.fn2;
    ExtractFilePathName(a,b);
    if (b='') then b:=sr.nam;
    if SelectDirectory(a,p) then begin; DriveComm.cmd:=6;goto f2; end;
    if not searchOneDirEntry(p,b,sr2) then begin; DriveComm.cmd:=8;goto f2; end;
    if (sr.rgt and xRights_Directory=0) then i:=$20 else i:=$10;
    if appendOneDirEntry(p,b,i,sr.cls,sr.siz) then begin; DriveComm.cmd:=17;goto f2; end;
    if eraseOneDirEntry(sr.prn,sr.nam) then begin; DriveComm.cmd:=14;goto f2; end;
    if (sr.rgt and xRights_Directory=0) then begin; DriveComm.cmd:=0;goto f2; end;
    DriveComm.cmd:=WriteInitialDirSector(sr.cls,p,false);
    end;
  $08:begin;{make link}
    DriveComm.cmd:=4;
    end;
  $09:begin;{set rights}
    DriveComm.cmd:=4;
    end;
  $0a:begin;{set date}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    currentPath:=DriveComm.dir;
    move(DriveComm.dat[1],tim1,sizeof(tim1));
    move(DriveComm.dat[sizeof(tim1)+1],tim2,sizeof(tim2));
    if SelectFileName(DriveComm.fn1,sr) then begin; DriveComm.cmd:=7;goto f2; end;
    DriveComm.cmd:=setDateOfDirEntry(sr.prn,sr.nam,tim1,tim2);
    end;
  $02:begin;{drive statistics}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    CountDiskUsage(i,o,p,q);
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