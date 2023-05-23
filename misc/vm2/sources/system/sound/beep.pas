{$heap 7k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

Var pipe:LongInt;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Var
  buf:array[1..1024] of byte;
  i,o,p:LongInt;
  a:String;
BEGIN;
WriteLn('beeper v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<3) then immErr('using: beep.code <process> (<hz> <ms>)*n');
a:=paramStr(1);
i:=BVal(a);
if (i=0) then i:=BugOS_findProcNam(a);
if (i=0) then immErr('process not found!');

if (pipeLineCreate(pipe,i,65536,true)<>0) then immErr('error opening pipeline!');

p:=2;
while (p<paramCount) do begin;
  i:=BVal(paramStr(p));
  move(i,buf[1],sizeof(i));
  inc(p);
  i:=BVal(paramStr(p));
  move(i,buf[5],sizeof(i));
  inc(p);
  pipeLineSend(pipe,buf,8);
  end;

END.