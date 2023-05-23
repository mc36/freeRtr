{$heap 31k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc hex.inc}
{$sysinc crt.inc}
{$sysinc bugos.inc}

Const
  tempDrive='z:\';
  imageName='instCD.iso';
  imageDrive='y:\';

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Procedure processKiller(a:String);
Label f1;
Var i:LongInt;
Begin;
f1:
i:=BugOS_findProcNam(a);
if (i=0) then exit;
BugOS_KillProcess(i);
goto f1;
End;

Procedure simpleRun(a,b:String);
Var
  i:LongInt;
  w:Word;
Begin;
i:=xExec(a,b,w);
if (i or w<>0) then immErr('error running '+a+'!');
End;

Function userChoose(a:String):LongInt;
Label f1;
Var i:LongInt;
Begin;
f1:
i:=ReadKey;
if (i and $fe00<>0) then goto f1;
i:=pos(upCase(chr(i and $ff)),a);
if (i<1) then goto f1;
userChoose:=i;
End;

Function ReadLine:String;
Label f0,f1;
Var
  a:String;
  w:Word;
Begin;
f0:
a:='';
Write(#13'>');
f1:
w:=ReadKey;
if (w and $fe00=0) then begin;{simple key}
  w:=w and $ff;
  if (w in [0,255,13,10,8,9]) then w:=ord(' ');
  if (length(a)>250) then goto f1;
  a:=a+chr(w);
  write(chr(w));
  goto f1;
  end;
case w of
  $8001:begin;{redraw}
    clrscr;
    goto f0;
    end;
  $8003:begin;{backspace}
    if (a='') then goto f1;
    Write(#8' '#8);
    a:=copy(a,1,length(a)-1);
    goto f1;
    end;
  $8004:begin;{enter}
    WriteLn('');
    ReadLine:=a;
    exit;
    end;
  $8005:begin;{escape}
    WriteLn('');
    goto f0;
    end;
  end;
goto f1;
End;

Procedure writeQuest(a:String);
Begin;
textColor($0f);
Write(a);
textColor($07);
WriteLn('');
End;



Label f1;
Var
  a,b:String;
  i,o:LongInt;
  f:xFile;
  t:xtText;
BEGIN;
WriteLn('network installer v1.0, done by Mc at '#%date' '#%time'.');

if (xChDir(imageDrive)=0) then goto f1;
xChDir(copy(GetMyFullFileName,1,3));

writeQuest('what would you like to do?');
WriteLn('s - get a shell...');
WriteLn('i - install system from network...');
i:=userChoose('SI');
if (i<>2) then begin;
  simpleRun('\utils\shell.code','');
  halt(0);
  end;

b:=tempDrive+'netCfg.scr';
simpleRun('\system\installer\networkConfig.code',b);
if (xOpen(f,b,xGenFilMod_rw)<>0) then immErr('error opening result!');
a:=';';
xBlockWrite(f,a[1],1);
xClose(f);
writeQuest('starting network...');
simpleRun('\system\drivers\console\sysload.code',b);
simpleRun('\utils\shell.code','del '+tempDrive+'*');

b:='http://bugos.nop.hu/';
writeQuest('current server: '+b);
Write('do you want to use this server? (y/n) ');
i:=userChoose('YN');
if (i=1) then WriteLn('yes, use this server!') else begin;
  WriteLn('no, change server!');
  writeQuest('enter the path of install image:');
  repeat b:=readLine; until (b<>'');
  writeQuest('current server: '+b);
  end;

writeQuest('downloading image...');
a:=tempDrive+'netGet.cmd';
xErase(a);
xCreate(a);
if (xtOpen(t,a,false)<>0) then immErr('error opening file!');
xtWriteLn(t,'browser (netInstall for BugOS)');
xtWriteLn(t,'automode 1');
xtWriteLn(t,'get3 '+tempDrive+imageName+' '+b+imageName);
xtWriteLn(t,'quit');
xtClose(t);
simpleRun('\internet\client\http.code',a);
if (xOpen(f,tempDrive+imageName,xGenFilMod_r)<>0) then immErr('error opening result!');
xClose(f);

writeQuest('opening image...');
processKiller('cdemu.code');
processKiller('iso9660.code');
a:=tempDrive+'netOpn.cmd';
xErase(a);
xCreate(a);
if (xtOpen(t,a,false)<>0) then immErr('error opening file!');
xtWriteLn(t,'\system\otherfs\cdemu.code '+tempDrive+imageName);
xtWriteLn(t,'\system\otherfs\iso9660.code cdemu.code 0 '+imageDrive);
xtClose(t);
simpleRun('\system\drivers\console\sysload.code',a);

simpleRun('\utils\shell.code','del '+tempDrive+'*');
simpleRun('\utils\packer.code','x '+imageDrive+'install\instcd.pck '+tempDrive);

f1:
writeQuest('starting installer...');
simpleRun(tempDrive+'systeminstaller.code',imageDrive+'packages\');
Halt(0);
END.