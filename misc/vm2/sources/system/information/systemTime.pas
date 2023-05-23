{$heap 3k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc datetime.inc}
{$sysinc bugos.inc}

function conv(i:LongInt):String;
var a:string;
begin;
a:=bstr(i);
while (length(a)<2) do a:='0'+a;
conv:=a;
end;

const
  DayNames:array[0..6] of String[10]=('sunday','monday','tuesday','wednesday','thursday','friday','saturday');
Var
  a:String;
  i,o,p:LongInt;
  w1,w2,w3:Word;
BEGIN;
WriteLn('system time displayer v1.0, done by Mc at '#%date' '#%time'.');
BugOS_KernelUptime(i,o,p);
o:=o div p;
a:=conv(o div 3600);
o:=o mod 3600;
a:=a+':'+conv(o div 60)+':'+conv(o mod 60);
WriteLn('system is up for '+BStr(i)+' days and '+a+' seconds.');
xGetDate(w1,w2,w3);
WriteLn('local date is '+conv(w1)+'-'+conv(w2)+'-'+conv(w3)+'.');
i:=GetDayOfWeek(w1,w2,w3);
WriteLn('this is '+dayNames[i]+'.');
xGetTime(w1,w2,w3);
WriteLn('local time is '+conv(w1)+':'+conv(w2)+':'+conv(w3)+'.');
END.