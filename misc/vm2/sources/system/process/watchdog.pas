{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc datetime.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Var
  procDat:array[1..128] of record
    pid:LongInt;
    nam:String[63];
    end;
  procNum:LongInt;
  t:xtText;
  ip:String;
  prt:LongInt;
  log:String;
  testInterval:LongInt;
  testLast:LongInt;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function xLevesz(a:String):String;
Var i:byte;
Begin;
i:=pos(';',a);
if (i<>0) then a:=copy(a,1,i-1);
Kicserel(#0,' ',a);
Kicserel(#255,' ',a);
Kicserel(#9,' ',a);
a:=' '+a+' ';
Kicserel('  ',' ',a);
a:=copy(a,2,length(a)-2);
xLevesz:=a;
End;




Procedure AppendLog(b:String);
Var
  a:String;
  w1,w2,w3:Word;
function x(i:longint):string;begin;a:=bstr(i);while (length(a)<2) do a:='0'+a;x:=a;end;
Begin;
WriteLn(b);
xCreate(log);
if (xtOpen(t,log,false)<>0) then exit;
xGetDate(w1,w2,w3);
xtWrite(t,x(w1)+'-'+x(w2)+'-'+x(w3)+' ');
xGetTime(w1,w2,w3);
xtWrite(t,x(w1)+':'+x(w2)+':'+x(w3)+' - ');
xtWriteLn(t,b);
xtClose(t);
End;

Procedure addProc(var a:String);
Var i:LongInt;
Begin;
i:=BugOS_findProcNam(a);
if (i=0) then begin;
  AppendLog(a+' process does not exists!');
  exit;
  end;
inc(procNum);
procDat[procNum].pid:=i;
procDat[procNum].nam:=a;
End;



Function TestTcpConnectability(var adr;prt:LongInt):Boolean;
Label f1;
Var
  i,o,p,t:LongInt;
  a:String;
Begin;
TestTcpConnectability:=True;
if (prt=0) then goto f1;
TCPbeginConnect(p,4096,adr,prt);
timer2start;
t:=currentTime;
while TCPlookConnected(p,a,i,o) do begin;
  relequish;
  timer2start;
  if (GetTimePast(t)>testInterval) then exit;
  if (p=0) then exit;
  end;
pipeLineClose(p);
f1:
TestTcpConnectability:=False;
End;


Procedure RestartComputer;
Label f1;
Var i,o,p:LongInt;
Begin;
f1:
BugOS_totalSysInfo(p,i,o);
if (p<2) then Halt(0);
for i:=p-1 downto 0 do begin;
  o:=BugOS_findProcNum(i);
  BugOS_KillProcess(o);
  end;
goto f1;
End;


Label f1;
Var
  i,o,p,q,r:LongInt;
  a,b:String;
BEGIN;
WriteLn('watchdog v1.0, done by Mc at '#%date' '#%time'.');
TCPfindProcess;
log:=paramStr(1);
if (log='') then immErr('using: watchdog.code <config>');

if (xtOpen(t,log,true)<>0) then immErr('error opening config file!');
log:=xLevesz(xtReadLn(t,255));
testInterval:=BVal(xLevesz(xtReadLn(t,255)));
ip:=xLevesz(xtReadLn(t,255));
string2ipAddr(ip,ip);
prt:=BVal(xLevesz(xtReadLn(t,255)));
b:=xLevesz(xtReadLn(t,255));
xtClose(t);
testLast:=-99999;
procNum:=0;
while (b<>'') do begin;
  i:=pos('\',b);
  if (i<1) then i:=666;
  a:=xLevesz(copy(b,1,i-1));
  b:=copy(b,i+1,255);
  if (a='') then continue;
  addProc(a);
  end;

AppendLog('watchdog started successfully!');
BugOS_SignDaemoning;

f1:
relequish;
timer2start;
if (GetTimePast(testLast)<testInterval) then goto f1;
Write('.');
if testTcpConnectability(ip,prt) then begin;
  AppendLog('failed to connect test site!');
  r:=1;
  end else r:=0;
if (xtOpen(t,log,true)=0) then xtClose(t) else begin;
  AppendLog('failed to open file!');
  r:=1;
  end;
q:=ticksPerSec*60;
for o:=1 to procNum do if not BugOS_ProcessExists(procDat[o].pid) then begin;
  AppendLog(procDat[o].nam+' died!');
  r:=1;
  end else begin;
  BugOS_ProcessStat(procDat[o].pid,i,p);
  if (i<q) then continue;
  AppendLog(procDat[o].nam+' halted!');
  r:=1;
  end;
if (r<1) then begin;
  timer2start;
  testLast:=CurrentTime;
  goto f1;
  end;
AppendLog('resetting...');
RestartComputer;
END.