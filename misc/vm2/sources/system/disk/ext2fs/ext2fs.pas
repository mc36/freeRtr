{$heap 255k}
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

{$include ext2fs1.inc}
{$include ext2fs2.inc}
{$include ext2fs3.inc}
{$include ext2fs4.inc}
{$include ext2fs5.inc}
{$include ext2fs6.inc}

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
  OneSearchedRecord=record
    size:LongInt;
    rights:LongInt;
    owner:LongInt;
    created:array[1..7] of byte;
    modified:array[1..7] of byte;
    name:String;
    end;

Label f1,f2,f3,f4;
Var
  DriveComm:OneDriveCommunicationRecord;
  strm1,strm2:OneStreamRecord;
  ntry,ntry2,ntry3:OneDirectoryEntryRecord;
  ser:OneSearchedRecord;
  fil:OneFileRecord;
  a,b,c:String;
  i,o,p,q:LongInt;
BEGIN;
WriteLn(proggyName+', done by Mc at '#%date' '#%time'.');
Randomize;
if (paramCount<3) then begin;
  WriteLn('using: ext2fs.code <process> <drive> <letter>');
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
if ReadUpBootSector then immErr('this is not a ext2fs drive!');

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
    if StreamOpen(strm1,fil.ino) then goto f2;
    strm1.siz:=fil.siz;
    strm1.pos:=fil.pos;
    i:=StreamRead(strm1,DriveComm.siz,DriveComm.dat);
    if (i<>DriveComm.siz) then goto f2;
    fil.pos:=strm1.pos;
    DriveComm.cmd:=0;
    move(fil,DriveComm.hdr,sizeof(fil));
    end;
  $0f:begin;{write file}
    move(DriveComm.hdr,fil,sizeof(fil));
    DriveComm.cmd:=1;
    if StreamOpen(strm1,fil.ino) then goto f2;
    strm1.siz:=fil.siz;
    strm1.pos:=fil.pos;
    i:=StreamWrite(strm1,DriveComm.siz,DriveComm.dat);
    if (i<>DriveComm.siz) then goto f2;
    fil.pos:=strm1.pos;
    fil.siz:=strm1.siz;
    if StreamClose(strm1) then goto f2;
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
    i:=fil.siz;
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
    a:=GetNameAndChgDir(DriveComm.fn1,strm1);
    if (a='') then goto f2;
    if FindOneDirEntry(strm1,a,ntry) then goto f2;
    if StreamOpen(strm2,ntry.ino) then goto f2;
    if (strm2.inoDat.mode and $8000=0) then goto f2;
    fil.ino:=strm2.InoNum;
    fil.siz:=strm2.siz;
    fil.pos:=strm2.pos;
    DriveComm.cmd:=0;
    move(fil,DriveComm.hdr,sizeof(fil));
    i:=fil.ino;
    o:=$07;
    move(i,DriveComm.dat[1],sizeof(i));
    move(o,DriveComm.dat[5],sizeof(o));
    move(DriveComm.fn1,DriveComm.dat[9],sizeof(DriveComm.fn1));
    end;
  $01:begin;{change directory}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    DriveComm.cmd:=7;
    curPath:=DriveComm.dir;
    a:=ChangeDir(DriveComm.fn1,strm1);
    if (a='') then goto f2;
    DriveComm.fn1:=a;
    DriveComm.cmd:=0;
    end;
  $0c:begin;{read directory}
    move(DriveComm.hdr,fil,sizeof(fil));
    if StreamOpen(strm1,fil.ino) then fillchar(strm1,sizeof(strm1),0);
    strm1.pos:=fil.pos;
    fillchar(ser,sizeof(ser),0);
    DriveComm.cmd:=1;
    while (1=1) do begin;
      if FindNextDirEntry(strm1,ntry) then break;
      a:=GetFileNameFromEntry(ntry);
      if (a='..') then continue;
      if (a='.') then continue;
      if (a='') then break;
      if StreamOpen(strm2,ntry.ino) then continue;
      ser.name:=a;
      ser.size:=strm2.inoDat.size;
      if (GetFileTypeFromMode(strm2.inoDat.mode)=2) then ser.rights:=$83 else ser.rights:=$07;
      break;
      end;
    fil.pos:=strm1.pos;
    DriveComm.cmd:=0;
    move(fil,DriveComm.hdr,sizeof(fil));
    move(ser,DriveComm.dat,sizeof(ser));
    end;
  $0b:begin;{open directory}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    DriveComm.cmd:=1;
    curPath:=DriveComm.dir;
    a:=ChangeDir(DriveComm.fn1,strm1);
    if (a='') then goto f2;
    DriveComm.fn1:=a;
    fil.pos:=0;
    fil.ino:=strm1.inoNum;
    DriveComm.cmd:=0;
    move(fil,DriveComm.hdr,sizeof(fil));
    p:=fil.ino;
    o:=$07;
    move(p,DriveComm.dat[1],sizeof(p));
    move(o,DriveComm.dat[5],sizeof(o));
    move(DriveComm.fn1,DriveComm.dat[9],sizeof(DriveComm.fn1));
    end;
  $13:begin;{truncate file}
    move(DriveComm.hdr,fil,sizeof(fil));
    DriveComm.cmd:=1;
    fil.siz:=fil.pos;
    if StreamOpen(strm1,fil.ino) then goto f2;
    strm1.siz:=fil.siz;
    strm1.pos:=fil.pos;
    if StreamTruncate(strm1) then goto f2;
    fil.pos:=strm1.pos;
    fil.siz:=strm1.siz;
    if StreamClose(strm1) then goto f2;
    DriveComm.cmd:=0;
    move(fil,DriveComm.hdr,sizeof(fil));
    end;
  $05:begin;{create file}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    curPath:=DriveComm.dir;
    DriveComm.cmd:=1;
    a:=GetNameAndChgDir(DriveComm.fn1,strm1);
    if (a='') then goto f2;
    if not FindOneDirEntry(strm1,a,ntry) then goto f2;
    strm1.pos:=strm1.siz;
    i:=CreateEmptyInode(false);
    if (i=0) then goto f2;
    PutFileNameToEntry(ntry,a,i);
    if (StreamWrite(strm1,ntry.recsiz,ntry)=0) then goto f2;
    if StreamClose(strm1) then goto f2;
    DriveComm.cmd:=0;
    end;
  $06:begin;{erase file}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    curPath:=DriveComm.dir;
    DriveComm.cmd:=1;
    a:=GetNameAndChgDir(DriveComm.fn1,strm1);
    if (a='') then goto f2;
    if FindOneDirEntry(strm1,a,ntry) then goto f2;
    if StreamOpen(strm2,ntry.ino) then goto f2;
    if (GetFileTypeFromMode(strm2.inoDat.mode)<>1) then goto f2;
    f4:
    strm2.pos:=0;
    if StreamTruncate(strm2) then goto f2;
    fillchar(strm2.inoDat,sizeof(strm2.inoDat),0);
    if WriteInodeBitmap(strm2.inoNum,false) then goto f2;
    if WriteInodeData(strm2.inoNum,strm2.inoDat) then goto f2;
    if DeleteOneDirOnlyEntry(strm1,ntry) then goto f2;
    DriveComm.cmd:=0;
    end;
  $03:begin;{create directory}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    curPath:=DriveComm.dir;
    DriveComm.cmd:=1;
    a:=GetNameAndChgDir(DriveComm.fn1,strm1);
    if (a='') then goto f2;
    if not FindOneDirEntry(strm1,a,ntry) then goto f2;
    strm1.pos:=strm1.siz;
    o:=CreateEmptyInode(true);
    if (o=0) then goto f2;
    PutFileNameToEntry(ntry,a,o);
    if (StreamWrite(strm1,ntry.recsiz,ntry)=0) then goto f2;
    p:=strm1.InoNum;
    if StreamClose(strm1) then goto f2;
    if StreamOpen(strm1,o) then goto f2;
    PutFileNameToEntry(ntry,'.',o);
    if (StreamWrite(strm1,ntry.recsiz,ntry)=0) then goto f2;
    PutFileNameToEntry(ntry,'..',p);
    if (StreamWrite(strm1,ntry.recsiz,ntry)=0) then goto f2;
    if StreamClose(strm1) then goto f2;
    DriveComm.cmd:=0;
    end;
  $04:begin;{erase directory}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    curPath:=DriveComm.dir;
    DriveComm.cmd:=1;
    b:=a;
    if (ChangeDir(a,strm1)='') then goto f2;
    strm1.pos:=0;
    while (1=1) do begin;
      if FindNextDirEntry(strm1,ntry) then break;
      a:=GetFileNameFromEntry(ntry);
      if (a='') then break;
      if (a='.') then continue;
      if (a='..') then continue;
      goto f2;
      end;
    strm2:=strm1;
    a:=GetNameAndChgDir(b,strm1);
    if (a='') then goto f2;
    if FindOneDirEntry(strm1,a,ntry) then goto f2;
    goto f4;
    end;
  $07:begin;{rename}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    curPath:=DriveComm.dir;
    DriveComm.cmd:=1;
    a:=GetNameAndChgDir(DriveComm.fn1,strm1);
    if (a='') then goto f2;
    b:=GetNameAndChgDir(DriveComm.fn2,strm2);
    if (b='') then goto f2;
    if FindOneDirEntry(strm1,a,ntry) then goto f2;
    if not FindOneDirEntry(strm2,b,ntry2) then goto f2;
    PutFileNameToEntry(ntry2,b,ntry.ino);
    strm2.pos:=strm2.siz;
    if (StreamWrite(strm2,ntry2.recsiz,ntry2)=0) then goto f2;
    p:=strm2.inoNum;
    if StreamClose(strm2) then goto f2;
    if DeleteOneDirOnlyEntry(strm1,ntry) then goto f2;
    if StreamOpen(strm2,ntry2.ino) then goto f2;
    if (GetFileTypeFromMode(strm2.inoDat.mode)=2) then begin;
      if FindOneDirEntry(strm2,'..',ntry) then goto f2;
      ntry.ino:=p;
      dec(strm2.pos,ntry.recsiz);
      if (StreamWrite(strm2,8,ntry)=0) then goto f2;
      end;
    DriveComm.cmd:=0;
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
    q:=BlockSiz*512;
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