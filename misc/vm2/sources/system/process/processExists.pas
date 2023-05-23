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
WriteLn('process exists v1.0, done by Mc at '#%date' '#%time'.');
a:=ParamStr(1);
if (a='') then begin;
  writeln('using: processexists.code <process name/id>');
  halt(2);
  end;
i:=BVal(a);
if (i=0) then i:=BugOS_findProcNam(a);
if (i=0) then begin;
  f1:
  writeln('process not exists!');
  halt(1);
  end;
if not BugOS_ProcessExists(i) then goto f1;
writeln('process exists!');
halt(0);
END.