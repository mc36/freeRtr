{$heap 15k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc bugos.inc}

function conv(i,o:longint):string;
var a:string;
begin;
a:=bstr(i);
while (length(a)<o) do a:=a+' ';
conv:=a;
end;

Var
  i,o,p,q,r:LongInt;
  d:record
    nam:string;
    par:string;
    end;
BEGIN;
WriteLn('Process Lister v1.0, done by Mc at '#%date' '#%time'.');

BugOS_totalMemInfo(i,o,p);
WriteLn(' total memory: '+BStr(i));
WriteLn('kernel memory: '+Bstr(o));
WriteLn('  free memory: '+Bstr(p));

BugOS_totalSysInfo(i,o,q);
WriteLn('number of processes: '+BStr(i));
WriteLn('number of pipelines: '+BStr(o));
WriteLn('number of openfiles: '+BStr(q));
q:=i;

WriteLn('pid        mem        pipe file uid ppid       rgt tck runs        nam     par');
for r:=1 to q do begin;
  q:=BugOS_findProcNum(r-1);
  Write(conv(q,11));
  BugOS_ProcessInfo(q,i,o,p);
  write(conv(i,11)+conv(o,4)+' '+conv(p,4)+' ');
  BugOS_ProcessName(q,d,i,o,p);
  write(conv(i,3)+' '+conv(o,11)+conv(p,3)+' ');
  BugOS_ProcessStat(q,i,o);
  write(conv(i,3)+' '+conv(o,11)+' ');
  Write(d.nam+' '+d.par);
  WriteLn('');
  end;
END.