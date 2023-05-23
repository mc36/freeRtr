{$heap 83k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc bugos.inc}
{$include \sources\developer\utils\codereader.inc}
{$include \sources\utils\other\packer.inc}

Var DriveLetter:Char;
Type
  OneDriveCommunicationRecord=record
    cmd:longint;
    uid:longint;
    rgt:longint;
    siz:longint;
    dir:string;
    fn1:string;
    fn2:string;
    hnd:array[1..512] of byte;
    dat:array[1..65536] of byte;
    end;
  OneFileEntryRecord=record
    size:LongInt;
    rights:LongInt;
    owner:LongInt;
    created:xDirEntryDateTimeRec;
    modified:xDirEntryDateTimeRec;
    name:String;
    beginning:LongInt;
    id:LongInt;
    end;
  OneFileHandlerRecord=record
    pos:LongInt;
    beg:LongInt;
    siz:LongInt;
    end;



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


Label f1,f2,f3;
Var
  DriveComm:OneDriveCommunicationRecord;
  systemStartup:Boolean;
  hnd:OneFileHandlerRecord;
  hdr:OneFileHeaderRecord;
  ntry:OneFileEntryRecord;
  i,o,p:LongInt;
  a:String;
BEGIN;
WriteLn('romFS v1.0, done by Mc at '#%date' '#%time'.');
DriveLetter:='!';
a:=GetAllParameters;
if (a<>'') then DriveLetter:=a[1];
systemStartup:=(a='!!!startup!!!');
if systemStartup then WriteLn('system startup detected!');

WriteLn('going to log in as '+DriveLetter+':\...');
FillChar(DriveComm,sizeof(DriveComm),0);
if (BugOS_driveLogin(DriveLetter,DriveComm)<>0) then begin;
  WriteLn('error logging in!');
  Halt(1);
  end;
BugOS_SignDaemoning;

if systemStartup then xExecBgnd('!:\systemLoader.CODE','!:\systemLoader.CFG',i);

f1:
if (DriveComm.cmd shr 16<>$8000) then begin;
  Relequish;
  goto f1;
  end;
i:=DriveComm.cmd and $ffffff;
case i of
  $0e:begin;{read file}
    move(DriveComm.hnd,hnd,sizeof(hnd));
    DriveComm.cmd:=16;
    if (hnd.pos<0) then goto f2;
    if (hnd.pos+DriveComm.siz>hnd.siz) then goto f2;
    readBytesAfterCode(hnd.beg+hnd.pos,DriveComm.siz,DriveComm.dat);
    inc(hnd.pos,DriveComm.siz);
    DriveComm.cmd:=0;
    move(hnd,DriveComm.hnd,sizeof(hnd));
    end;
  $10:begin;{seek file}
    move(DriveComm.hnd,hnd,sizeof(hnd));
    move(DriveComm.dat,i,sizeof(i));
    hnd.pos:=i;
    DriveComm.cmd:=0;
    move(hnd,DriveComm.hnd,sizeof(hnd));
    end;
  $11:begin;{get file size}
    move(DriveComm.hnd,hnd,sizeof(hnd));
    DriveComm.cmd:=0;
    i:=hnd.siz;
    move(i,DriveComm.dat,sizeof(i));
    end;
  $12:begin;{get file pos}
    move(DriveComm.hnd,hnd,sizeof(hnd));
    DriveComm.cmd:=0;
    i:=hnd.pos;
    move(i,DriveComm.dat,sizeof(i));
    end;
  $0d:begin;{open file}
    a:=kicsi(DriveComm.fn1);
    justName(a,false);
    DriveComm.cmd:=7;
    p:=4;
    repeat
      readBytesAfterCode(p,sizeof(i),i);
      if (i<>IdentifyValue) then goto f2;
      readBytesAfterCode(p,sizeof(hdr),hdr);
      inc(p,sizeof(hdr));
      o:=p;
      inc(p,hdr.siz);
      justName(hdr.nam,false);
      until (kicsi(hdr.nam)=a);
    p:=o;
    DriveComm.cmd:=4;
    if (DriveComm.uid<>0) then begin;
      o:=hdr.rgt;
      if (DriveComm.uid<>hdr.own) then o:=o shr 3;
      i:=DriveComm.rgt and 7;
      if (o and i<>i) then goto f2;
      end;
    DriveComm.fn1:=hdr.nam;
    justName(DriveComm.fn1,true);
    hnd.pos:=0;
    hnd.beg:=p;
    hnd.siz:=hdr.siz;
    move(hnd,DriveComm.hnd,sizeof(hnd));
    o:=hdr.rgt or $40;
    move(p,DriveComm.dat[1],sizeof(p));
    move(o,DriveComm.dat[5],sizeof(o));
    move(DriveComm.fn1,DriveComm.dat[9],sizeof(DriveComm.fn1));
    DriveComm.cmd:=0;
    end;
  $01:begin;{change directory}
    DriveComm.cmd:=0;
    DriveComm.fn1:=DriveLetter+':\';
    end;
  $0c:begin;{read directory}
    move(DriveComm.hnd,p,sizeof(p));
    f3:
    fillChar(ntry,sizeof(ntry),0);
    readBytesAfterCode(p,sizeof(i),i);
    if (i=IdentifyValue) then begin;
      readBytesAfterCode(p,sizeof(hdr),hdr);
      ntry.beginning:=p;
      ntry.id:=p;
      inc(p,sizeof(hdr));
      inc(p,hdr.siz);
      a:=hdr.nam;
      justName(a,false);
      if (a='') then goto f3;
      if (hdr.rgt and xRights_Directory<>0) then goto f3;
      ntry.size:=hdr.siz;
      ntry.rights:=hdr.rgt or $40;
      ntry.owner:=hdr.own;
      ntry.created:=hdr.crt;
      ntry.modified:=hdr.mdf;
      ntry.name:=a;
      end;
    DriveComm.cmd:=0;
    move(p,DriveComm.hnd,sizeof(p));
    move(ntry,DriveComm.dat[1],sizeof(ntry));
    end;
  $0b:begin;{open directory}
    DriveComm.fn1:=DriveLetter+':\';
    DriveComm.cmd:=0;
    i:=4;o:=$03;
    move(i,DriveComm.hnd,sizeof(i));
    move(i,DriveComm.dat[1],sizeof(i));
    move(o,DriveComm.dat[5],sizeof(o));
    move(DriveComm.fn1,DriveComm.dat[9],sizeof(DriveComm.fn1));
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
    i:=0;
    move(i,DriveComm.dat[1],sizeof(i));
    move(i,DriveComm.dat[5],sizeof(o));
    move(i,DriveComm.dat[9],sizeof(p));
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