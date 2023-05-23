{$heap 47k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc bugos.inc}

Function countProcessMemory:LongInt;
Var i,o,p,q,r,s,t:LongInt;
Begin;
s:=0;
BugOS_totalSysInfo(r,i,o);
for q:=0 to r-1 do begin;
  t:=BugOS_findProcNum(q);
  BugOS_ProcessInfo(t,i,o,p);
  inc(s,i);
  end;
countProcessMemory:=s;
End;

Var
  buf:array[1..32*1024] of char;
  i,o,p,q:longInt;
  a:String;
BEGIN;
WriteLn('system information v1.0, done by Mc at '#%date' '#%time'.');
BugOS_totalMemInfo(i,o,p);
WriteLn('memory info:');
WriteLn('    total: '+alakit(i));
WriteLn('   kernel: '+alakit(o));
WriteLn('     free: '+alakit(p));
i:=countProcessMemory;
WriteLn('processes: '+alakit(i));
WriteLn('number of:');
BugOS_totalSysInfo(i,o,p);
WriteLn(' processes: '+alakit(i));
WriteLn(' pipelines: '+alakit(o));
WriteLn('open files: '+alakit(p));
BugOS_ProcessorInfo(0,buf,q,i);
WriteLn('processors: '+alakit(q));
WriteLn('processors installed:');
for p:=0 to q-1 do begin;
  BugOS_ProcessorInfo(p,buf,i,o);
  a:='';i:=1;
  while (buf[i]<>#0) do begin;
    a:=a+buf[i];
    inc(i);
    end;
  WriteLn('#'+Bstr(p+1)+': ('+BStr(o)+') '+a);
  end;
END.