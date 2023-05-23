Procedure xGetDate(var year,month,day:Word);
Begin;
asm sysCall console.getDate;
asm VarAddr trg year;
asm addrLod trg [trg];
asm movW duwuw [trg] a;
asm VarAddr trg month;
asm addrLod trg [trg];
asm movW duwuw [trg] b;
asm VarAddr trg day;
asm addrLod trg [trg];
asm movW duwuw [trg] c;
End;

Procedure xGetTime(var hour,minute,second:Word);
Begin;
asm sysCall console.getTime;
asm VarAddr trg hour;
asm addrLod trg [trg];
asm movW duwuw [trg] a;
asm VarAddr trg minute;
asm addrLod trg [trg];
asm movW duwuw [trg] b;
asm VarAddr trg second;
asm addrLod trg [trg];
asm movW duwuw [trg] c;
End;


Function GetDaysInMonth(year,month:Integer):Integer;
Begin;
GetDaysInMonth:=-1;
if (year<0) or (year>5000) or (month<1) or (month>12) then Exit;
GetDaysInMonth:=0;
if (month in [1,3,5,7,8,10,12]) then begin; GetDaysInMonth:=31;Exit; end;
if (month in [4,6,9,11]) then begin; GetDaysInMonth:=30;Exit; end;
if (year mod 4<>0) then begin; GetDaysInMonth:=28;Exit; end;
if (year mod 100<>0) then begin; GetDaysInMonth:=29;Exit; end;
if (year mod 400<>0) then begin; GetDaysInMonth:=28;Exit; end;
GetDaysInMonth:=29;
End;

Function GetDayOfWeek(year,month,day:Integer):Integer;
{0=sunday, 1=monday, 2=tuesday, 3=wednesday, 4=thursday, 5=friday, 6=saturday}
{0=vas†rnap, 1=hÇtfì, 2=kedd, 3=szerda, 4=csÅtîrtîk, 5=pÇntek, 6=szombat}
Var
  n,e,h:LongInt;
Begin;
GetDayOfWeek:=-1;
if (GetDaysInMonth(year,month)<day) or (day<1) then Exit;
if (year>1992) then begin;
  n:=0;
  for e:=1993 to year-1 do For h:=1 to 12 do n:=n+GetDaysInMonth(e,h);
  for h:=1 to month-1 do n:=n+GetDaysInMonth(year,h);
  n:=n+day+4;
  n:=n mod 7;
  end else begin;
  n:=GetDaysInMonth(year,month)-day;
  for h:=month+1 to 12 do n:=n+GetDaysInMonth(year,h);
  for e:=year+1 to 1993 do for h:=1 to 12 do n:=n+GetDaysInMonth(e,h);
  n:=-n-1;
  n:=n mod 7;
  n:=6+n;
  end;
GetDayOfWeek:=n mod 7;
End;

Function getDaysPast(y1,m1,d1,y2,m2,d2:Integer):LongInt;
Var
  neg:Boolean;
  i,o,p:LongInt;
procedure xchg(var d1,d2:integer);var i:integer;begin; i:=d1;d1:=d2;d2:=i; end;
Begin;
getDaysPast:=-(1 shl 30);
if (GetDaysInMonth(y1,m1)<d1) or (d1<1) then Exit;
if (GetDaysInMonth(y2,m2)<d2) or (d2<1) then Exit;
neg:=false;
if (y1>y2) then neg:=true;
if (y1=y2) and (m1>m2) then neg:=true;
if (y1=y2) and (m1=m2) and (d1>d2) then neg:=true;
if neg then begin;
  xchg(y1,y2);
  xchg(m1,m2);
  xchg(d1,d2);
  end;
p:=0;
for o:=y1 to y2 do for i:=1 to 12 do inc(p,GetDaysInMonth(o,i));
for i:=1 to m1-1 do dec(p,GetDaysInMonth(y1,i));
for i:=m2 to 12 do dec(p,GetDaysInMonth(y2,i));
dec(p,d1);
inc(p,d2);
if neg then p:=-p;
getDaysPast:=p;
End;
