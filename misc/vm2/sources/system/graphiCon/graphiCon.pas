{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc pipeline.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

{$include memory.inc}
{$include driver.inc}
{$include window.inc}
{$include emulator.inc}
{$include process.inc}
{$include main.inc}

Procedure readUpConfig(a:String);
Var
  t:xtText;
  i:LongInt;
Begin;
BugOS_MyOwnerInfo(i,loggedUid);
BugOS_MyProcessInfo(myProcId,i,i);
WriteLn('reading '+a+'...');
if (xtOpen(t,a,true)<>0) then immErr('error reading config');
a:=xtReadLn(t,666);
if (xChDir(a)<>0) then immErr('error changing directory!');
monitorOf:=xtReadLn(t,666);
monitorOn:=xtReadLn(t,666);
fontHeight:=BVal(xtReadLn(t,666));
if (fontHeight<1) or (fontHeight>16) then fontHeight:=16;
a:=xtReadLn(t,666);
WriteLn('font: '+a);
if readUpFont(a) then immErr('error reading font!');
a:=xtReadLn(t,666);
WriteLn('palette: '+a);
if readUpColor(a) then immErr('error reading palette!');
loginProc:=xtReadLn(t,666);
askerProc:=xtReadLn(t,666);
shellProc:=xtReadLn(t,666);
cmndrProc:=xtReadLn(t,666);
prcmnProc:=xtReadLn(t,666);
xtClose(t);
kicserel('`',BStr(myProcId),loginProc);
kicserel('`',BStr(myProcId),askerProc);
kicserel('`',BStr(myProcId),shellProc);
kicserel('`',BStr(myProcId),cmndrProc);
kicserel('`',BStr(myProcId),prcmnProc);
WriteLn('login: '+loginProc);
WriteLn('passask: '+askerProc);
WriteLn('shell: '+shellProc);
WriteLn('commander: '+cmndrProc);
WriteLn('process monitor: '+prcmnProc);
End;



Procedure monitorRound;
Var
  i:LongInt;
  w:Word;
Begin;
if (monitorOf='') then exit;
pixelClear;
pixelFlush;
for i:=1 to 8 do relequish;
BugOS_SetOwnerInfo(0);
i:=findProcSep(monitorOf);
xExec(copy(monitorOf,1,i-1),copy(monitorOf,i+1,666),w);
BugOS_SetOwnerInfo(loggedUid);
while keypressed do readkey;
while not keypressed do begin;
  recvProcesses;
  relequish;
  end;
BugOS_SetOwnerInfo(0);
i:=findProcSep(monitorOn);
xExec(copy(monitorOn,1,i-1),copy(monitorOn,i+1,666),w);
BugOS_SetOwnerInfo(loggedUid);
for i:=1 to 8 do relequish;
pixelClear;
pixelFlush;
while keypressed do readkey;
End;



Label f1,f2,f3,f4;
Var
  i,o:LongInt;
  a:String;
BEGIN;
WriteLn('graphical console v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<3) then immErr('using: gfxcon.code <video-process> <key-process> <config> [<uid> <process> [parameters]]');
readUpConfig(paramStr(3));
if pixelOpen(paramStr(1)) then immErr('error opening display!');
if keybrdOpen(paramStr(2)) then immErr('error opening keyboard!');
if MemoryResize(screenSizP) then immErr('error allocating memory!');
BugOS_SignDaemoning;
a:=paramStr(4);
loggedUid:=BVal(a);
if (loggedUid=0) and (a<>'0') then loggedUid:=-1;
a:='';
for i:=5 to paramCount do a:=a+' '+paramStr(i);
a:=copy(a,2,666);
if (a='') then begin;
  WriteLn('running in multiuser mode...');
  goto f1;
  end;

BugOS_SetOwnerInfo(loggedUid);
WriteLn('running in single user (uid='+BStr(loggedUid)+') mode...');
pixelClear;
pixelFlush;
windowNum:=0;
windowSel:=1;
nextWinPos:=0;
WriteLn('starting '+a+'...');
if startProcess(a) then immErr('error starting process!');
maximizeWindow(memoryRecordData^[screenSizP+1]);
while (windowNum>0) do doMainLoop;
WriteLn('no more windows, exiting!');
Halt(0);

f1:
pixelClear;
pixelFlush;
windowNum:=0;
windowSel:=1;
nextWinPos:=0;
loggedUid:=0;
if startProcess(loginProc) then begin;
  monitorRound;
  goto f1;
  end;
i:=waitLastProc(loggedUid);
deleteProcess(windowNum);
if (i=2) then begin; monitorRound;goto f1; end;
if (i<>0) then goto f1;
BugOS_SetOwnerInfo(loggedUid);
WriteLn('uid: '+BStr(loggedUid));
nextWinPos:=0;
startProcess(shellProc);
f2:
doMainLoop;
if (windowNum=0) then goto f1;
f3:
o:=loggedUid;
loggedUid:=0;
monitorRound;
loggedUid:=o;
f4:
pixelClear;
pixelFlush;
if startProcess(askerProc) then goto f2;
i:=waitLastProc(o);
deleteProcess(windowNum);
if (i=2) then goto f3;
if (o<>loggedUid) then goto f4;
if (i=0) then goto f2;
goto f4;
END.