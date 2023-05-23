{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc bugos.inc}
{{$sysinc pipeline.inc}
Var
  DriveSize:LongInt;
  DriveBegin:LongInt;
  DriveReadOnly:Boolean;
{$include \sources\filesystem\disk2.inc}
Const
  TargetDrive='c';
  DriveNam='idehdd.code';
Var
  packages:String;
  sysPath:String;
  DriveNum:LongInt;
  PartiNum:LongInt;
  autoUpdate:Boolean;
Const
  preserveNum=8;
  preserveDat:array[1..preserveNum] of String[63]=(
    ':\system\localHost.text',
    ':\system\userList.data',
    ':\system\loginScreen.text',
    ':\system\randomNumber.seed',
    ':\system\hostKeys\currentKey.dss',
    ':\system\hostKeys\currentKey.rsa',
    ':\system\hostKeys\currentCert.dss',
    ':\system\hostKeys\currentCert.rsa'
    );
  preserveBeg=':\installerTemporary';
  packagesNum=4;
  packagesDat:array[1..4] of String[15]=(
    'sources',
    'system',
    'utils',
    'internet'
    );


Procedure Question(a:String);
Begin;
WriteLn('');
TextColor($0f);Write(a);
TextColor($07);WriteLn('');
End;

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function AskUser(a:String):Char;
Label f1;
Var
  w:Word;
  c:Char;
Begin;
while keypressed do readkey;
Write('choose: ');
f1:
w:=readKey;
if (w and $fe00<>0) then goto f1;
c:=LowCase(chr(w));
if (pos(c,a)=0) then goto f1;
Write(#13'                     '#13);
AskUser:=c;
End;

Procedure executeProgramInMe(a,b,c:String;show,user,sign,kill:Boolean);
Label f1,f2;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p,q:LongInt;
  w:Word;
Begin;
i:=xExecInside(a,b,q,p);
if (i<>0) then goto f2;
pipeLineSend(p,c[1],length(c));
f1:
i:=128;
if (pipeLineRecv(p,ab[1],i)<>0) then i:=0;
if (i>0) then begin;
  ab0:=i;
  if (pos(#0,a)<>0) then begin;
    for i:=1 to 16 do relequish;
    i:=16;
    if (pipeLineRecv(p,ab[ab0+1],i)<>0) then i:=0;
    inc(ab0,i);
    end;
  if show then Write(a);
  if sign then if (pos(#0#0,a)<>0) then goto f2;
  goto f1;
  end;
if user then while keypressed do begin;
  w:=ReadKey;
  pipeLineSend(p,w,sizeof(w));
  end;
pipeLineStats(p,o,i,i);
if (o=0) then goto f2;
relequish;
goto f1;
f2:
pipeLineClose(p);
if kill then BugOS_KillProcess(q);
TextColor($07);
WriteLn('');
End;





Procedure SelectHardDrive;
Label f1;
Var
  sizes:array[1..4] of LongInt;
  names:array[1..4] of String;
  i,o:LongInt;
  a,b:String;
Begin;
Question('which hard disk would you install to?');
fillchar(sizes,sizeof(sizes),0);
fillchar(names,sizeof(names),0);
for o:=1 to 4 do begin;
  Write(BStr(o)+' - ');
  i:=DriveOpen(DriveNam,o-1);
  DriveClose;
  DriveReadOnly:=True;
  if (i<>0) then begin;
    WriteLn('nothing');
    continue;
    end;
  sizes[o]:=DriveSize;
  names[o]:=DriveInternalIdentifier.model;
  a:=BStr(sizes[o] shr 11)+' MB';
  if (sizes[o]=0) then a:='CDROM';
  while (length(a)<12) do a:=' '+a;
  WriteLn(a+' - '+names[o]);
  end;
o:=0;
for i:=1 to 4 do if (sizes[i]<>0) then inc(o);
if (o<1) then ImmErr('no hard disk found!');
f1:
i:=BVal(AskUser('1234'));
if (sizes[i]=0) then goto f1;
DriveNum:=i-1;
End;


Procedure SelectPartition;
Label f1;
Var
  parti:record
    code:array[1..446] of byte;
    tab:array[1..4] of record
      flags:Byte;
      begHed:Byte;begSec:Byte;begCyl:Byte;
      sysId:Byte;
      endHed:Byte;endSec:Byte;endCyl:Byte;
      pos:LongWord;siz:LongWord;
      end;
    sig1:Byte;sig2:Byte;
    end;
  sizes:array[1..4] of LongInt;
  i,o:LongInt;
  a:String;
Begin;
Question('which partition would you install to?');
fillchar(sizes,sizeof(sizes),0);
if (DriveOpen(DriveNam,DriveNum)<>0) then immErr('error opening drive!');
DriveReadOnly:=True;
if (DriveRead(0,parti)<>0) then immErr('error reading partition table!');
DriveClose;
if (parti.sig1<>$55) or (parti.sig2<>$aa) then immErr('invalid partition table!');
for o:=1 to 4 do begin;
  Write(BStr(o)+' - ');
  if (parti.tab[o].siz=0) then begin;
    WriteLn('nothing');
    continue;
    end;
  if (parti.tab[o].sysid<>$b0) then begin;
    WriteLn('other');
    continue;
    end;
  sizes[o]:=parti.tab[o].siz;
  a:=BStr(sizes[o] shr 11)+' MB';
  while (length(a)<12) do a:=' '+a;
  WriteLn(a+' - BugOS fileSystem');
  end;
o:=0;
for i:=1 to 4 do if (sizes[i]<>0) then inc(o);
if (o<1) then ImmErr('no partition found!');
f1:
i:=BVal(AskUser('1234'));
if (sizes[i]=0) then goto f1;
PartiNum:=i-1;
End;

Function testDrive(a:String):Boolean;
Var
  b:String;
  i:LongInt;
Begin;
b:=xGetDir;
i:=xChDir(a);
xChDir(b);
testDrive:=(i<>0);
End;

Procedure TestFile(a:String);
Var
  f:xFile;
  i:LongInt;
Begin;
i:=xOpen(f,a,xGenFilMod_r);
xClose(f);
if (i=0) then exit;
immErr(a+' not found!');
End;

Function RenameOWEX(src,trg:String):LongInt;
Var
  f:xFile;
  i:LongInt;
Begin;
i:=xOpen(f,src,xGenFilMod_r);
xClose(f);
RenameOWEX:=i;
if (i<>0) then exit;
xErase(trg);
RenameOWEX:=xRename(src,trg);
End;




Label updater;
Var
  i,o,p:LongInt;
  a,b:String;
  t:xtText;
  w:Word;
BEGIN;
WriteLn('system installer v1.0, done by Mc at '#%date' '#%time'.');
packages:=paramStr(1);
autoUpdate:=(paramStr(2)='autoupdate');
if (packages='') then packages:='.';
if (copy(packages,length(packages),255)<>'\') then packages:=packages+'\';
sysPath:=xFileName(GetMyFullFileName,1);
WriteLn('packages dir="'+packages+'"');
WriteLn('utilities dir="'+sysPath+'"');
xChDir(sysPath);

TestFile('shell.code');
TestFile('packer.code');
for i:=1 to packagesNum do TestFile(packages+packagesDat[i]+'.pck');

WriteLn('');WriteLn('');WriteLn('');
TextColor($0a);Write('welcome to ');
TextColor($0b);Write('BugOS');
TextColor($0a);Write(' installation!');
TextColor($07);WriteLn('');
WriteLn('');WriteLn('');WriteLn('');

if autoUpdate then begin;
  WriteLn('performing automatic update...');
  goto updater;
  end;

Question('what would you like to do?');
WriteLn('s - get a shell...');
WriteLn('i - (re)install system...');
WriteLn('r - create ram drive...');
WriteLn('u - upgrade running system...');

case AskUser('siur') of
  's':begin;
    WriteLn('starting shell...');
    xExec('shell.code','',w);
    WriteLn('exiting...');
    Halt(0);
    end;
  'i':begin;
    end;
  'r':begin;
    if (BugOS_findProcNam('ramdrive.code')<>0) then immErr('ramdrive already exists!');
    if not testDrive(TargetDrive+':\') then immErr('drive '+TargetDrive+':\ already exists!');
    Question('creating ramdrive...');
    executeProgramInMe('ramdrive.code','65536','',true,false,true,false);
    Question('creating filesystem...');
    executeProgramInMe('makefs.code','ramdrive.code 0 0','y'#0,true,false,false,true);
    Question('opening filesystem...');
    executeProgramInMe('filesystem.code','ramdrive.code 0 '+TargetDrive,'',true,false,true,false);
    if testDrive(TargetDrive+':\') then immErr('error accessing drive!');
    WriteLn('exiting...');
    Halt(0);
    end;
  'u':begin;
    Question('do you really want to upgrade your system on '+TargetDrive+':\?');
    if (AskUser('yn')<>'y') then ImmErr('user aborted!');
    goto updater;
    end;
  else ImmErr('bug!');
  end;

SelectHardDrive;
Question('partition your hard drive!');
WriteLn('you have to create a partition for BugOS.');
WriteLn('it''s type must be $b0 (BugOS FileSystem).');
WriteLn('if you still have one, simply exit from fdisk.');
WriteLn('press c to continue!');
AskUser('c');
executeProgramInMe('fdisk.code',DriveNam+' '+BStr(DriveNum),'',true,true,false,true);
SelectPartition;
Question('do you want to install boot manager?');
if (AskUser('yn')='y') then begin;
  executeProgramInMe('installpartitionmanager.code',DriveNam+' '+BStr(DriveNum),'y'#0,true,false,false,true);
  end;
Question('opening partition...');
executeProgramInMe('partition.code',DriveNam+' '+BStr(DriveNum)+' 1024','',true,false,true,false);
Question('do you want to format target partition?');
WriteLn('WARNiNG! this will erase all data on it!');
if (AskUser('yn')='y') then i:=1 else i:=0;
if (i<>0) then begin;
  executeProgramInMe('makefs.code','partition.code '+BStr(PartiNum)+' 384','y'#0,true,false,false,true);
  executeProgramInMe('installbootsector.code','partition.code '+BStr(PartiNum),'y'#0,true,false,false,true);
  end;
Question('opening filesystem...');
executeProgramInMe('filesystem.code','partition.code '+BStr(PartiNum)+' '+TargetDrive,'',true,false,true,false);
if testDrive(TargetDrive+':\') then immErr('error accessing drive!');
if (i=0) then goto updater;
xMkDir(TargetDrive+':\bootUp');
xRename(TargetDrive+':\bootimage.code',TargetDrive+':\bootUp\bootImage.code');
a:=TargetDrive+':\bootUp\bootOrder1.cfg';
xCreate(a);
xtOpen(t,a,false);
xtWriteLn(t,';mount filesystem...');
xtWriteLn(t,'@:\'+DriveNam);
xtWriteLn(t,'@:\partition.code '+DriveNam+' '+BStr(DriveNum)+' 1024');
xtWriteLn(t,'@:\filesystem.code partition.code '+BStr(PartiNum)+' c');
xtWriteLn(t,'');
xtWriteLn(t,';continue booting...');
xtWriteLn(t,'c:\system\drivers\console\sysload.code c:\bootup\bootorder2.cfg');
xtClose(t);
a:=TargetDrive+':\bootUp\bootOrder2.cfg';
xCreate(a);
xtOpen(t,a,false);
xtWriteLn(t,';system stuff...');
xtWriteLn(t,'c:\system\drivers\console\keyboard.code n');
xtWriteLn(t,'c:\system\authentication\authen_local.code');
xtWriteLn(t,'c:\system\authentication\authenticator.code c:\system\authentication\authenticator.cfg');
xtWriteLn(t,'c:\system\process\killprocess.code startupramdrive.code');
xtWriteLn(t,'c:\system\drivers\console\clipboard.code 1024');
xtWriteLn(t,'c:\system\drivers\console\randomnumber.code');
xtWriteLn(t,'');
xtWriteLn(t,'');
xtWriteLn(t,'');
xtWriteLn(t,'');
xtWriteLn(t,';the console...');
xtWriteLn(t,'c:\system\drivers\console\console.code keyboard.code c:\system\authentication\login.code console c:\ c:\utils\shell.code');
xtClose(t);
a:=TargetDrive+':\bootUp\bootOrder1.lst';
xCreate(a);
xtOpen(t,a,false);
xtWriteLn(t,'\system\drivers\console\sysload.code sysload.cod');
xtWriteLn(t,'\bootup\bootorder1.cfg sysload.cfg');
xtWriteLn(t,'\system\drivers\disks\idehdd.code');
xtWriteLn(t,'\system\drivers\disks\partition.code');
xtWriteLn(t,'\system\filesystem\filesystem.code');
xtClose(t);

updater:
Question('saving sensitive files...');
for i:=1 to preserveNum do begin;
  a:=TargetDrive+preserveDat[i];
  b:=TargetDrive+preserveBeg+BStr(i);
  xErase(b);
  if (RenameOWEX(a,b)=0) then continue;
  WriteLn(a+' cannot archived!');
  end;

xMkDir(TargetDrive+':\temp');
xSetRight(TargetDrive+':\temp',0,$8b);

for p:=1 to packagesNum do begin;
  b:=packagesDat[p];
  Question('installing '+b+' package...');
  xMkDir(TargetDrive+':\'+b);
  xSetRight(TargetDrive+':\'+b,0,$8b);
  executeProgramInMe('shell.code','rec-del '+TargetDrive+':\'+b+'\*','',false,false,false,true);
  executeProgramInMe('packer.code','x '+packages+b+'.pck '+TargetDrive+':\'+b+'\','',true,false,false,true);
  end;
executeProgramInMe('shell.code','copy '+TargetDrive+':\utils\bc-launcher-i80386.code '+TargetDrive+':\bc','',false,false,false,true);
executeProgramInMe('shell.code','chmod '+TargetDrive+':\bc $24','',false,false,false,true);

Question('restoring sensitive files...');
for i:=1 to preserveNum do begin;
  a:=TargetDrive+preserveDat[i];
  b:=TargetDrive+preserveBeg+BStr(i);
  if (RenameOWEX(b,a)=0) then continue;
  WriteLn(a+' cannot restored!');
  end;

xChDir(TargetDrive+':\');
if autoUpdate then begin;
  i:=1;
  end else begin;
  Question('do you want to install boot image?');
  if (AskUser('yn')='y') then i:=1 else i:=0;
  end;
if (i=1) then begin;
  executeProgramInMe('\utils\developer\utils\maker.code','\system\kernel\makeromdrive-def.mak','y'#0,true,false,false,true);
  end;

if not autoUpdate then begin;
  Question('you have to create user accounts!');
  WriteLn('it''s recommended to create a user named sysop');
  WriteLn('with uid=0 and a simple user, which will often');
  WriteLn('often used. optionally you should create a');
  WriteLn('guest user with uid=$ffffffff.');
  WriteLn('press c to continue!');
  AskUser('c');
  executeProgramInMe('\system\drivers\console\randomnumber.code','','',true,false,true,false);
  executeProgramInMe('\system\authentication\usermanager.code','','',true,true,false,true);
  end;

Question('installation successfully finished!');
if not autoUpdate then begin;
  WriteLn('press r to restart your computer!');
  AskUser('r');
  end;
WriteLn('rebooting...');
executeProgramInMe(sysPath+'processmonitor.code','',#5#134#29#128,false,false,false,true);
WriteLn('failed...');
END.