Var
  CurrentTime:LongInt;
  ticksPerSec:LongInt;

Procedure timer2start;
Var i:LongInt;
Begin;
BugOS_KernelUptime(i,CurrentTime,ticksPerSec);
End;

Function GetTimePast(d:LongInt):LongInt;
Begin;
d:=(CurrentTime-d) div ticksPerSec;
if (d<0) then inc(d,86400);
GetTimePast:=d;
End;
