{$heap 7k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

{$include serial.inc}
{$include \sources\internet\kernel\utils\timer2.inc}


Label f1,f2;
Var
  buf:array[1..512] of byte;
  timer,timex:LongInt;
  prog,param:string;
  term1,term2:LongInt;
  proc1,proc2:LongInt;
  a:String;
  i,o,p:LongInt;
  w:Word;
BEGIN;
WriteLn('serial gateway v1.0, done by Mc at '#%date' '#%time'.');
a:=paramStr(1);
serialProc:=BVal(a);
if (serialProc=0) then serialProc:=BugOS_findProcNam(a);
serialPort:=BVal(paramStr(2));
term2:=BugOS_findProcNam(paramStr(3));
prog:=paramStr(4);
param:='';
for i:=5 to paramCount do param:=param+' '+paramStr(i);
param:=copy(param,2,255);
if (prog='') then immErr('using: serialGateway.code <process> <port> <terminal> <program> [parameters]');
WriteLn('starting "'+prog+'" "'+param+'"...');
SerialOpen;
if (pipeLineCreate(term1,term2,4096,false)<>0) then immErr('error opening terminal emulator!');
if (pipeLineCreate(term2,term2,4096,false)<>0) then immErr('error opening terminal emulator!');
pipeLineSend(term1,term2,sizeof(term2));
if (xExecInside(prog,param,proc1,proc2)<>0) then immErr('error starting process!');

BugOS_SignDaemoning;
timer2start;
timex:=CurrentTime;
timer:=CurrentTime;
f1:
relequish;
timer2start;
if (GetTimePast(timer)>60) then begin;
  serialBuff[1]:=1;serialCmd(1);
  timer:=CurrentTime;
  if (serialBuff[5]<>0) then begin; a:='carrier detect changed!';goto f2; end;
  end;
if (GetTimePast(timex)>5*60) then begin;
  a:='inactivity timeout!';
  goto f2;
  end;
if not BugOS_ProcessExists(proc1) then begin; a:='child process terminated!';goto f2; end;
pipeLineStats(serialData,i,o,p);
if (i=0) then begin; a:='lower level closed connection!';goto f2; end;
if (p>512) then begin;
  p:=sizeof(buf);
  pipeLineRecv(term1,buf,p);
  pipeLineSend(serialData,buf,p);
  if (p>0) then timex:=CurrentTime;
  end;
pipeLineStats(term1,i,o,p);
if (i=0) then begin; a:='terminal emulator closed connection!';goto f2; end;
if (p>512) then begin;
  p:=sizeof(buf);
  pipeLineRecv(serialData,buf,p);
  pipeLineSend(term1,buf,p);
  if (p>0) then timex:=CurrentTime;
  end;
pipeLineStats(proc2,i,o,p);
if (i=0) then begin; a:='child process closed connection!';goto f2; end;
if (p>512) then begin;
  p:=sizeof(buf);
  pipeLineRecv(term2,buf,p);
  pipeLineSend(proc2,buf,p);
  if (p>0) then timex:=CurrentTime;
  end;
pipeLineStats(term2,i,o,p);
if (i=0) then begin; a:='terminal emulator closed connection!';goto f2; end;
if (p>512) then begin;
  p:=sizeof(buf);
  pipeLineRecv(proc2,buf,p);
  pipeLineSend(term2,buf,p);
  if (p>0) then timex:=CurrentTime;
  end;
goto f1;

f2:
WriteLn('killing process...');
xExec('c:\system\process\killRecursive.code',BStr(proc1),w);
WriteLn('the following error happened:');
WriteLn(a);
END.