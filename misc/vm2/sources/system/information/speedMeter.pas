{$heap 15k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc crt.inc}
{$sysinc bugos.inc}

{$include \sources\internet\kernel\utils\timer2.inc}

Procedure clearKeys;
Begin;
while keypressed do readkey;
End;


Label f1,f2;
Var
  i,o,p,q:LongInt;
  a,b,c,d:LongInt;
BEGIN;
WriteLn('speedMeter v1.0, done by Mc at '#%date' '#%time'.');

BugOS_SystemPerformance(a,b,c,d);
clearKeys;
timer2start;
WriteLn('idles shortRounds fullRounds activeRuns');

f1:
i:=CurrentTime;
f2:
relequish;
timer2start;
if (GetTimePast(i)<1) then goto f2;
BugOS_SystemPerformance(i,o,p,q);
WriteLn(BStr(i-a)+' '+BStr(o-b)+' '+BStr(p-c)+' '+BStr(q-d));
a:=i;
b:=o;
c:=p;
d:=q;
if not keypressed then goto f1;
clearKeys;
END.