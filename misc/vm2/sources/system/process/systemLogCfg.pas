{$heap 7k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc crt.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc param.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Label f1,f2;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
BEGIN;
WriteLn('system logger configurator v1.0, done by Mc at '#%date' '#%time'.');
a:=GetAllParameters;
if (a='') then immErr('using: logCfg.code <parameters>');

o:=BugOS_findProcNam('systemLogger.code');
if (o=0) then immErr('main process not found!');
if (pipeLineCreate(p,o,65536,false)<>0) then p:=0;
if (p=0) then immErr('error creating pipeline!');

pipeLineSend(p,a[1],length(a));
WriteLn('connected, press f10 to exit!');

f1:
relequish;
while keypressed do if (readKey=$801d) then immErr('user requested abort!');
f2:
i:=250;
if (pipeLineRecv(p,ab[1],i)<>0) then i:=0;
if (i<1) then begin;
  pipeLineStats(p,o,i,i);
  if (o<>0) then goto f1;
  WriteLn('');
  immErr('upper closed connection!');
  end;
if (i<1) then goto f1;
ab0:=i;
Write(a);
goto f2;
END.