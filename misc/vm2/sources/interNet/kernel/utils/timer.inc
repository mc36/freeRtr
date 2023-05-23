Function GetTimePast(d:LongInt):LongInt;
Var i,o,p:LongInt;
Begin;
BugOS_KernelUptime(i,o,p);
o:=o-d;
o:=o div p;
if (o<0) then inc(o,86400);
GetTimePast:=o;
End;
