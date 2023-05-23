{$heap 63k}
{$stack 15k}
{$sysinc system.inc}
{$sysinc bugos.inc}
{$sysinc param.inc}

Var
  buf:array[1..1024] of byte;
  i,o,p,q:LongInt;
  a:String;

Procedure killRec(n:LongInt);
Label f1;
Begin;
Write(BStr(n)+'; ');
BugOS_KillProcess(n);
f1:
BugOS_totalSysInfo(p,i,o);
for q:=0 to p-1 do begin;
  p:=BugOS_findProcNum(q);
  BugOS_ProcessName(p,buf,i,o,i);
  if (o<>n) then continue;
  killRec(p);
  goto f1;
  end;
End;

BEGIN;
WriteLn('kill recursive v1.0, done by Mc at '#%date' '#%time'.');
BugOS_MyOwnerInfo(i,o);
if (i<>0) or (o<>0) then begin;
  BugOS_dropPrivileges;
  WriteLn('privileges dropped...');
  end;
a:=ParamStr(1);
if (a='') then begin;
  writeln('using: killrecursive.code <process name/id>');
  halt(1);
  end;
i:=BVal(a);
if (i=0) then i:=BugOS_findProcNam(a);
if (i=0) then begin;
  writeln('process not found!');
  halt(2);
  end;
Write('process '+a+' ('+bstr(i)+') [');
killRec(i);
WriteLn(#8#8'] killed.');
END.