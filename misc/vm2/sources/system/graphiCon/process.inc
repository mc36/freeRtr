Procedure recvOneProcess(var window);
Var
  win:oneWindowRecord absolute window;
  buf:array[1..2048] of byte;
  i,o:LongInt;
Begin;
if (win.proc=0) then exit;
o:=sizeof(buf);
if (pipeLineRecv(win.pipe,buf,o)<>0) then o:=0;
if (o>0) then begin;
  for i:=1 to o do windowGotChar(window,buf[i]);
  win.last:=currentTime;
  exit;
  end;
if BugOS_ProcessExists(win.proc) then exit;
pipeLineClose(win.pipe);
win.pipe:=0;
win.proc:=0;
windowGotStr(window,#0#3#7#0#4#254#254#254#254#13#10'process has terminated, close this window!');
End;



Procedure recvProcesses;
Var i,o:LongInt;
Begin;
o:=screenSizP+1;
for i:=1 to windowNum do begin;
  recvOneProcess(memoryRecordData^[o]);
  inc(o,sizeof(oneWindowRecord));
  end;
End;



Function startProcess(a:String):Boolean;
Var
  i,o,p:LongInt;
  b:String;

procedure save(var window);
var win:oneWindowRecord absolute window;
begin;
win.proc:=i;
win.pipe:=o;
win.last:=currentTime;
end;

Begin;
startProcess:=True;
i:=findProcSep(a);
b:=copy(a,i+1,666);
a:=copy(a,1,i-1);
timer2start;
BugOS_SetOwnerInfo(loggedUid);
if (xExecInside(a,b,i,o)<>0) then exit;
p:=windowNum*sizeof(oneWindowRecord)+screenSizP+1;
if MemoryResize(p+sizeof(oneWindowRecord)) then begin;
  BugOS_KillProcess(i);
  pipeLineClose(o);
  exit;
  end;
save(memoryRecordData^[p]);
inc(windowNum);
initWindow(memoryRecordData^[p],80,25);
startProcess:=False;
End;



Procedure deleteProcess(n:LongInt);

procedure kill(var window);
var win:oneWindowRecord absolute window;
begin;
BugOS_KillProcess(win.proc);
pipeLineClose(win.pipe);
end;

Var i:LongInt;
Begin;
if (n<1) then exit;
if (n>windowNum) then exit;
i:=(n-1)*sizeof(oneWindowRecord)+screenSizP+1;
kill(memoryRecordData^[i]);
move(memoryRecordData^[i+sizeof(oneWindowRecord)],memoryRecordData^[i],(windowNum-n)*sizeof(oneWindowRecord));
dec(windowNum);
MemoryResize(windowNum*sizeof(oneWindowRecord)+screenSizP+1);
End;



Function waitLastProc(var uid:LongInt):LongInt;
Var pn,ct,tm:LongInt;

function getLst(var window):LongInt;
var
  win:oneWindowRecord absolute window;
  i:LongInt;
begin;
i:=getTimePast(win.last);
if (win.updt<>0) then ct:=i+1;
getLst:=i;
end;

function getPrc(var window):LongInt;
var win:oneWindowRecord absolute window;
begin;
getPrc:=win.proc;
end;

function testProc(i:LongInt):Boolean;
var
  b:array[1..1024] of byte;
  o,p,q:LongInt;
begin;
testProc:=false;
p:=BugOS_findProcNum(i);
if (p=0) then exit;
o:=pn-1;
BugOS_ProcessName(p,b,q,o,i);
if (o<>pn) then exit;
BugOS_KillProcess(p);
uid:=q;
testProc:=true;
end;

Label f1;
Var i,o,p:LongInt;
Begin;
waitLastProc:=1;
p:=(windowNum-1)*sizeof(oneWindowRecord)+screenSizP+1;
{putBorder(memoryRecordData^[p]);}
requestRedraw(memoryRecordData^[p]);
timer2start;
tm:=currentTime;
ct:=-9;
pn:=getPrc(memoryRecordData^[p]);
f1:
relequish;
timer2start;
recvProcesses;
i:=getLst(memoryRecordData^[p]);
if (i<>ct) then begin;
  ct:=i;
  updateWindow(memoryRecordData^[p]);
  if (i and 1=0) then drawCursor(memoryRecordData^[p]) else requestUpdate(memoryRecordData^[p]);
  end;
pixelFlush;
if keypressed then begin;
  while keypressed do sendKeyboard(memoryRecordData^[p],readKey);
  tm:=currentTime;
  if (getPrc(memoryRecordData^[p])=0) then exit;
  end;
inc(o);
if (o<32) then goto f1;
o:=0;
if (getTimePast(tm)>60) then begin; waitLastProc:=2;exit; end;
if (getPrc(memoryRecordData^[p])=0) then goto f1;
BugOS_totalSysInfo(o,i,i);
for i:=1 to o do if testProc(i) then begin; waitLastProc:=0;exit; end;
o:=0;
goto f1;
End;
