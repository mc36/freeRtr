{$heap 7k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc bugos.inc}
{$sysinc param.inc}

Var
  i,o:LongInt;
  a:String;
BEGIN;
WriteLn('kill process v1.0, done by Mc at '#%date' '#%time'.');
BugOS_MyOwnerInfo(i,o);
if (i<>0) or (o<>0) then begin;
  BugOS_dropPrivileges;
  WriteLn('privileges dropped...');
  end;
a:=ParamStr(1);
if (a='') then begin;
  writeln('using: killprocess.code <process name/id>');
  halt(1);
  end;
i:=BVal(a);
if (i=0) then i:=BugOS_findProcNam(a);
if (i=0) then begin;
  writeln('process not found!');
  halt(2);
  end;
BugOS_KillProcess(i);
WriteLn('process '+a+' ('+bstr(i)+') killed.');
END.