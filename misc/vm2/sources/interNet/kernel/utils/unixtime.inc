Const
  unixTime_firstYear=1970;
  unixTime_lastYear=2070;
Var
  unixTime_daysTable:array[unixTime_firstYear..unixTime_lastYear] of array [1..12] of Word;



Procedure unixTime_generateTable;
Var
  i,o,p,q:LongInt;
Begin;
o:=0;
for p:=unixTime_firstYear to unixTime_lastYear do begin;
  for q:=1 to 12 do begin;
    unixTime_daysTable[p][q]:=o;
    inc(o,GetDaysInMonth(p,q));
    end;
  end;
End;


Function unixTime_convertTo(year,month,day,hour,min,sec:LongInt):LongWord;
Var i:LongInt;
Begin;
unixTime_convertTo:=0;
if (year<unixTime_firstYear) or (year>unixTime_lastYear) then exit;
if (month<1) or (month>12) then exit;
i:=unixTime_daysTable[year][month];
inc(i,day-1);
i:=i*86400;
inc(i,hour*3600);
inc(i,min*60);
inc(i,sec);
unixTime_convertTo:=i;
End;


Procedure unixTime_convertFrom(unix:LongWord;var year,month,day,hour,min,sec:Word);
Label f1,f2;
Var i,o:LongInt;
Begin;
sec:=unix mod 60;unix:=unix div 60;
min:=unix mod 60;unix:=unix div 60;
hour:=unix mod 24;unix:=unix div 24;
for o:=unixTime_lastYear downto unixTime_firstYear do
 if (unixTime_daysTable[o][1]<=unix) then goto f1;
year:=0;
month:=0;
day:=0;
exit;
f1:
year:=o;
for i:=1 to 12 do
 if (unixTime_daysTable[o][i]>unix) then goto f2;
i:=13;
f2:
month:=i-1;
dec(unix,unixTime_daysTable[year][month]);
day:=unix+1;
End;
