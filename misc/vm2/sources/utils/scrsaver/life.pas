{$stack 5k}
{$heap 150k}
{$sysinc system.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc random.inc}
{$sysinc crt.inc}
{$sysinc param.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Var
  MaxX,MaxY:Word;
  mono:Boolean;
Type OneMapRecord=array[1..250,1..250] of byte;



Procedure FillUpBoard(var map:OneMapRecord;p:LongInt);
Var y,x,i:LongInt;
Begin;
for y:=1 to MaxY do for x:=1 to MaxX do begin;
  i:=random(100);
  if (i<p) then i:=1 else i:=0;
  map[y][x]:=i;
  end;
End;



Function CountPoints(var map:OneMapRecord):LongInt;
Var y,x,i:LongInt;
Begin;
i:=0;
for y:=1 to MaxY do for x:=1 to MaxX do begin;
  if (map[y][x]<>0) then inc(i);
  end;
CountPoints:=i;
End;



Procedure ProcessOneRound(var old,new:OneMapRecord);
Var
  y,x:LongInt;
  i,o:LongInt;
  ce,ue,sn:LongInt;

function get(cx,cy:LongInt):LongInt;
begin;
inc(cx,x);
inc(cy,y);
if (cx<1) then cx:=MaxX-cx;
if (cy<1) then cy:=MaxY-cy;
if (cx>MaxX) then cx:=cx-MaxX;
if (cy>MaxY) then cy:=cy-MaxY;
get:=old[cy][cx];
end;

{
lifegame:
if has 2 or 3 neighbors, then stays in life, otherwise dies...
if has 3 neighbors, then new borns...
}
Begin;
old:=new;
for y:=1 to MaxY do for x:=1 to MaxX do begin;
  ce:=get(0,0);
  sn:=get(-1,-1)+get(-1,0)+get(-1,+1)+get(+1,-1)+get(+1,0)+get(+1,+1)+get(0,-1)+get(0,+1);
  ue:=0;
  if (ce=1) and (sn=2) then ue:=1;
  if (sn=3) then ue:=1;
  new[y][x]:=ue;
  end;
End;



Procedure DisplayOne(var d:OneMapRecord);
Const ch:array[0..3] of byte=(32,223,220,219);
Var
  y,x:byte;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
ab0:=MaxX;
y:=1;
while (y<MaxY) do begin;
  for x:=1 to MaxX do ab[x]:=ch[(d[y+1][x]*2)+d[y][x]];
  GotoXY(1,(y shr 1)+1);
  Write(a);
  inc(y,2);
  end;
End;



Label f1,f2,f3;
Var
  curr,old:OneMapRecord;
  num,rnd:Word;
  time,trnd,brnd,gams:longInt;
  i:Word;
BEGIN;
WriteLn('life game v1.0, done by Mc at '#%date' '#%time'.');
Randomize;
mono:=(GetAllParameters<>'');
fillchar(curr,sizeof(curr),0);
brnd:=0;
trnd:=0;
gams:=0;

f1:
TextColor(7);clrscr;
ConsoleSize(maxX,maxY);
dec(maxX);
inc(maxY,maxY);
if (trnd>brnd) then brnd:=trnd;
inc(gams);
FillUpBoard(curr,20);
num:=CountPoints(curr)+1;
rnd:=2;
timer2start;time:=currentTime;
trnd:=0;

f2:
if (trnd and 7=0) then begin;
  if mono then textColor(15) else textColor(random(15)+1);
  end;
DisplayOne(curr);
while keypressed do if (readKey<>$8001) then goto f3 else goto f1;
while (getTimePast(time)<1) do begin; relequish;timer2start; end;
time:=currentTime;
i:=CountPoints(curr);
if (num=i) then inc(rnd) else begin; num:=i;rnd:=0; end;
if (rnd>64) then goto f1;
ProcessOneRound(old,curr);
inc(trnd);
goto f2;

f3:
textColor(7);
clrscr;
writeln(' total games: '+BStr(gams));
writeln('longest game: '+BStr(brnd));
END.