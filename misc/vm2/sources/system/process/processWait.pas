{$heap 7k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc bugos.inc}
{$sysinc param.inc}

Label f1;
Var
  i:LongInt;
  a:String;
BEGIN;
WriteLn('process wait v1.0, done by Mc at '#%date' '#%time'.');
a:=ParamStr(1);
if (a='') then begin;
  writeln('using: processwait.code <process name/id>');
  halt(2);
  end;
i:=BVal(a);
if (i=0) then i:=BugOS_findProcNam(a);
if (i=0) then begin;
  f1:
  writeln('process not found!');
  halt(1);
  end;
Write('waiting for pid '+BStr(i)+'...');
while BugOS_ProcessExists(i) do relequish;
writeln(' died!');
halt(0);
END.