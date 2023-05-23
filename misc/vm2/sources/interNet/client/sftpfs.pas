{$heap 255k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc datetime.inc}
{$sysinc param.inc}
{$sysinc random.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$sysinc inet_dns.inc}
{$include \sources\internet\kernel\utils\timer2.inc}
{$include \sources\internet\kernel\utils\unixtime.inc}
{$include sftp.inc}

Const maxHandles=16;
Var
  handlesDat:array[1..maxHandles] of String;
  positionDat:array[1..maxHandles] of LongInt;
  handlesNum:LongInt;
  DriveLetter:Char;
  basePath:String;
  curPath:String;
  inodNum:LongInt;


Function findOneHandle(a:String):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to handlesNum do if (handlesDat[i]=a) then goto f1;
i:=0;
f1:
findOneHandle:=i;
End;

Procedure freeOneHandle;
Begin;
if (handlesNum<maxHandles) then exit;
secftp_close(handlesDat[handlesNum]);
dec(handlesNum);
End;

Procedure markUsedHandle(i:LongInt);
Var
  a:String;
  o:LongInt;
Begin;
a:=handlesDat[i];
o:=positionDat[i];
move(handlesDat[1],handlesDat[2],(i-1)*sizeof(a));
move(positionDat[1],positionDat[2],(i-1)*sizeof(o));
handlesDat[1]:=a;
positionDat[1]:=o;
End;

Procedure appendHandle(a:String);
Begin;
if (handlesNum<maxHandles) then inc(handlesNum);
handlesDat[handlesNum]:=a;
positionDat[handlesNum]:=0;
markUsedHandle(handlesNum);
End;


Procedure ImmErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Function readIDfile(a:String):String;
Var t:xtText;
Begin;
readIDfile:='';
if (a='') then exit;
if (xtOpen(t,a,true)<>0) then exit;
a:=xtReadLn(t,666)+#13+xtReadLn(t,666)#13#10;
xtClose(t);
if (a=#13#13) then exit;
readIDfile:=a;
End;

Function ChangeDir(nd:String):String;
Label f1,f2;
Var
  s,a:String;
  i,o:Byte;
Begin;
ChangeDir:='';
s:=CurPath;
if (copy(s,length(s),666)='\') then s:=copy(s,1,length(s)-1);
delete(s,1,2);
if (copy(nd,2,1)=':') then delete(nd,1,2);
if (copy(nd,1,1)='\') then begin; s:='';delete(nd,1,1); end;
f1:
if (Length(s)>250) then Exit;
if (nd='') then goto f2;
i:=pos('\',nd);
if (i=0) then i:=length(nd)+1;
a:=copy(nd,1,i-1);
delete(nd,1,i);
if (a='') then goto f1;
if (a='..') then begin;
  o:=0;
  for i:=1 to length(s) do if (s[i]='\') then o:=i;
  s:=Copy(s,1,o-1);
  goto f1;
  end;
if (a='.') then goto f1;
s:=s+'\'+a;
goto f1;
f2:
if (s='') then s:='\';
ChangeDir:=DriveLetter+':'+s;
End;

Function toUnix(a:String):String;
Begin;
kicserel('\',#13,a);
kicserel('/','\',a);
kicserel(#13,'/',a);
toUnix:=basePath+copy(a,3,666);
End;

Function doReadFile(var buffer;siz:LongInt):LongInt;
Label f1,f2;
Const max=4*1024;
Var
  buf:array[1..1] of byte absolute buffer;
  i,o,p:LongInt;
Begin;
p:=0;
f1:
o:=siz-p;
if (o<1) then goto f2;
if (o>max) then o:=max;
i:=secftp_readFile(handlesDat[1],positionDat[1]+p,buf[p+1],o);
if (i<>0) then begin; doReadFile:=i;exit; end;
if (o<1) then begin; doReadFile:=1;exit; end;
inc(p,o);
goto f1;
f2:
inc(positionDat[1],siz);
doReadFile:=0;
End;

Function doWriteFile(var buffer;siz:LongInt):LongInt;
Label f1,f2;
Const max=4*1024;
Var
  buf:array[1..1] of byte absolute buffer;
  i,o,p:LongInt;
Begin;
p:=0;
f1:
o:=siz-p;
if (o<1) then goto f2;
if (o>max) then o:=max;
i:=secftp_writeFile(handlesDat[1],positionDat[1]+p,buf[p+1],o);
if (i<>0) then begin; doWriteFile:=i;exit; end;
inc(p,o);
goto f1;
f2:
inc(positionDat[1],siz);
doWriteFile:=0;
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
  time1,time2:xDirEntryDateTimeRec;
  lastTime:LongInt;
  ntry:xDirEntryRec;
  ser:OneSearchedRecord;
  a,b,c:String;
  i,o,p,q:LongInt;
BEGIN;
WriteLn('sftp filesystem v1.0, done by Mc at '#%date' '#%time'.');
if SSHfindProcess then immErr('failed to find ssh process!');
if DNSstartResolver then immErr('failed to find dns process!');
unixTime_generateTable;
Randomize;
if (paramCount<2) then immErr('using: sftpfs.code <hostname> <letter> [base] [idfile]');

a:=ParamStr(1);
p:=21;
Write('resolving '+a+'...');
DNSresolvePut(1,a);
while (1=1) do begin;
  i:=DNSresolveGet(a,b);
  if (i=0) then begin; relequish;continue; end;
  if (i and $80=0) then break;
  immErr(' failed!');
  end;
WriteLn(' ok!');
WriteLn('connecting to '+ipAddr2string(b)+' '+BStr(p)+'...');
secftp_connect(b,p,secftp_versMax,readIDfile(paramStr(4)));
if (secftp_pipe=0) then immErr('failed!');
WriteLn('successful, protocol version='+BStr(secftp_vers));
c:='.';
if (secftp_realpath(c)<>0) then WriteLn('error getting home directory!');
WriteLn('home dir: '+c);
basePath:=paramStr(3);

a:=kicsi(ParamStr(2));
driveLetter:=a[1];
handlesNum:=0;
curPath:='';
WriteLn('going to log in as '+DriveLetter+':\...');
FillChar(DriveComm,sizeof(DriveComm),0);
if (BugOS_driveLogin(DriveLetter,DriveComm)<>0) then immErr('error logging in!');
lastTime:=0;
BugOS_SignDaemoning;

f1:
if (DriveComm.cmd shr 16<>$8000) then begin;
  if keypressed then goto f3;
  if (secftp_pipe=0) then immErr('remote closed connection!');
  Relequish;
  timer2start;
  if (GetTimePast(lastTime)<60) then goto f1;
  a:='.';
  secftp_realpath(a);
  lastTime:=currentTime;
  goto f1;
  end;
i:=DriveComm.cmd and $ffffff;
case i of
  $0e:begin;{read file}
    move(DriveComm.hdr,b,sizeof(b));
    p:=findOneHandle(b);
    if (p<1) then begin; DriveComm.cmd:=9;goto f2; end;
    markUsedHandle(p);
    DriveComm.cmd:=doReadFile(DriveComm.dat,DriveComm.siz);
    end;
  $0f:begin;{write file}
    move(DriveComm.hdr,b,sizeof(b));
    p:=findOneHandle(b);
    if (p<1) then begin; DriveComm.cmd:=9;goto f2; end;
    markUsedHandle(p);
    DriveComm.cmd:=doWriteFile(DriveComm.dat,DriveComm.siz);
    end;
  $10:begin;{seek file}
    move(DriveComm.hdr,b,sizeof(b));
    p:=findOneHandle(b);
    if (p<1) then begin; DriveComm.cmd:=9;goto f2; end;
    markUsedHandle(p);
    move(DriveComm.dat,i,sizeof(i));
    DriveComm.cmd:=0;
    positionDat[1]:=i;
    end;
  $11:begin;{get file size}
    move(DriveComm.hdr,b,sizeof(b));
    p:=findOneHandle(b);
    if (p<1) then begin; DriveComm.cmd:=9;goto f2; end;
    markUsedHandle(p);
    DriveComm.cmd:=secftp_getSize(b,i);
    move(i,DriveComm.dat,sizeof(i));
    end;
  $12:begin;{get file pos}
    move(DriveComm.hdr,b,sizeof(b));
    p:=findOneHandle(b);
    if (p<1) then begin; DriveComm.cmd:=9;goto f2; end;
    markUsedHandle(p);
    DriveComm.cmd:=0;
    i:=positionDat[1];
    move(i,DriveComm.dat,sizeof(i));
    end;
  $0d:begin;{open file}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    freeOneHandle;
    curPath:=DriveComm.dir;
    c:=ChangeDir(DriveComm.fn1);
    a:=toUnix(c);
    i:=secftp_fileOpen(b,a,DriveComm.rgt,false);
    if (i<>0) then begin; DriveComm.cmd:=i;goto f2; end;
    appendHandle(b);
    move(b,DriveComm.hdr,sizeof(b));
    inc(inodNum);
    o:=$07;
    move(inodNum,DriveComm.dat[1],sizeof(inodNum));
    move(o,DriveComm.dat[5],sizeof(o));
    move(c,DriveComm.dat[9],sizeof(c));
    DriveComm.cmd:=0;
    end;
  $01:begin;{change directory}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    DriveComm.cmd:=7;
    curPath:=DriveComm.dir;
    a:=ChangeDir(DriveComm.fn1);
    if (a='') then goto f2;
    DriveComm.fn1:=a;
    a:=toUnix(a);
    DriveComm.cmd:=secftp_realpath(a);
    end;
  $0c:begin;{read directory}
    move(DriveComm.hdr,b,sizeof(b));
    p:=findOneHandle(b);
    if (p<1) then begin; DriveComm.cmd:=9;goto f2; end;
    i:=secftp_dirRead(b,ntry);
    if (i<>0) then begin; DriveComm.cmd:=i;goto f2; end;
    markUsedHandle(p);
    ser.size:=ntry.size;
    ser.rights:=ntry.rights;
    ser.owner:=ntry.owner;
    move(ntry.created,ser.created,sizeof(ser.created));
    move(ntry.modified,ser.modified,sizeof(ser.modified));
    ser.name:=ntry.name;
    DriveComm.cmd:=0;
    move(b,DriveComm.hdr,sizeof(b));
    move(ser,DriveComm.dat,sizeof(ser));
    end;
  $0b:begin;{open directory}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    freeOneHandle;
    curPath:=DriveComm.dir;
    c:=ChangeDir(DriveComm.fn1);
    a:=toUnix(c);
    i:=secftp_dirOpen(a,b);
    if (i<>0) then begin; DriveComm.cmd:=i;goto f2; end;
    appendHandle(b);
    move(b,DriveComm.hdr,sizeof(b));
    inc(inodNum);
    o:=$07;
    move(inodNum,DriveComm.dat[1],sizeof(inodNum));
    move(o,DriveComm.dat[5],sizeof(o));
    move(c,DriveComm.dat[9],sizeof(c));
    DriveComm.cmd:=0;
    end;
  $13:begin;{truncate file}
    move(DriveComm.hdr,b,sizeof(b));
    p:=findOneHandle(b);
    if (p<1) then begin; DriveComm.cmd:=9;goto f2; end;
    markUsedHandle(p);
    DriveComm.cmd:=secftp_truncate(b,positionDat[1]);
    end;
  $05:begin;{create file}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    freeOneHandle;
    curPath:=DriveComm.dir;
    a:=toUnix(ChangeDir(DriveComm.fn1));
    i:=secftp_fileOpen(b,a,0,true);
    if (i=0) then secftp_close(b);
    DriveComm.cmd:=i;
    end;
  $06:begin;{erase file}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    curPath:=DriveComm.dir;
    a:=toUnix(ChangeDir(DriveComm.fn1));
    DriveComm.cmd:=secftp_erase(a);
    end;
  $03:begin;{create directory}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    curPath:=DriveComm.dir;
    a:=toUnix(ChangeDir(DriveComm.fn1));
    DriveComm.cmd:=secftp_mkDir(a);
    end;
  $04:begin;{erase directory}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    curPath:=DriveComm.dir;
    a:=toUnix(ChangeDir(DriveComm.fn1));
    DriveComm.cmd:=secftp_rmDir(a);
    end;
  $07:begin;{rename}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    curPath:=DriveComm.dir;
    a:=toUnix(ChangeDir(DriveComm.fn1));
    b:=toUnix(ChangeDir(DriveComm.fn2));
    DriveComm.cmd:=secftp_rename(a,b);
    end;
  $08:begin;{make link}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    curPath:=DriveComm.dir;
    a:=toUnix(ChangeDir(DriveComm.fn1));
    b:=toUnix(ChangeDir(DriveComm.fn2));
    DriveComm.cmd:=secftp_mkLink(a,b);
    end;
  $09:begin;{set rights}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    a:=toUnix(ChangeDir(DriveComm.fn1));
    move(DriveComm.dat[1],i,sizeof(i));
    move(DriveComm.dat[5],o,sizeof(o));
    DriveComm.cmd:=secftp_setOwner(a,i,o);
    end;
  $0a:begin;{set date}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    a:=toUnix(ChangeDir(DriveComm.fn1));
    move(DriveComm.dat[1],time1,sizeof(time1));
    move(DriveComm.dat[sizeof(time1)+1],time2,sizeof(time2));
    DriveComm.cmd:=secftp_setDates(a,time2,time1);
    end;
  $02:begin;{drive statistics}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    DriveComm.cmd:=1;
    end;
  else DriveComm.cmd:=1;
  end;
f2:
BugOS_driveDone;
goto f1;

f3:
WriteLn('exiting...');
END.