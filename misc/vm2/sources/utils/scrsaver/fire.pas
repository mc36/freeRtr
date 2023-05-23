{$sysinc system.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}
{$sysinc crt.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Const
  MaxX=80;
  MaxY=26;
Type oneflamedatrec=Array[-2..MaxY+3,-2..MaxX+3] of Byte;
Var dat,old:oneflamedatrec;


{palette generatin code:
for i:=0 to  7 do putcol(0  , 0, i*2);        0....7     0-->1
for i:=0 to  7 do putcol(i*2, 0, 16-2*i);     8....15    1-->4
for i:=0 to 16 do putcol(16+47*i/16, 0, 0);  16....32    4-->12
for i:=0 to 24 do putcol(63, 21*i/8, 0);     33....57   12-->14
for i:=0 to 24 do putcol(63, 63, 21*i/8);    58....82   14-->15
}

Function ConvertDatNum2Col(i:LongInt):LongInt;
Var c:LongInt;
Begin;
c:=0;
if (i>3) then c:=1;
if (i>11) then c:=4;
if (i>24) then c:=12;
if (i>45) then c:=14;
if (i>70) then c:=15;
ConvertDatNum2Col:=c;
End;


Procedure ClearBuffer;
Begin;
fillchar(dat,SizeOf(dat),0);
fillchar(old,sizeof(old),255);
End;


Procedure CreateRandomStuff;
Var i,o:LongInt;
Begin;
o:=0;
for i:=5 to MaxX-5 do begin;
  if (Random(10)<4) then o:=Random(2)*82;
  dat[MaxY][i]:=o;
  end;
End;


Procedure CalculateNewBuf;
Var y,x,d:LongInt;
Begin;
for y:=1 to MaxY do for x:=1 to MaxX do begin;
  d:=dat[y][x]+dat[y][x-1]+dat[y][x+1]+dat[y+1][x];
  d:=d*4 div 18;
  dat[y-1][x]:=d;
  end;
End;






Procedure FreshScr;
Var x,y,ox,oc,b:LongInt;
Begin;
oc:=1234;
for y:=1 to MaxY-1 do begin;
  ox:=1234;
  for x:=1 to MaxX-1 do begin;
    b:=dat[y][x];
    if (b=old[y][x]) then continue;
    b:=ConvertDatNum2Col(b);
    if (ox<>x) then GotoXY(x,y);
    if (oc<>b) then TextColor(b);
    write('Û');
    ox:=x+1;
    oc:=b;
    end;
  end;
old:=dat;
End;



Procedure waitTicks(n:LongInt);
Var i,o:LongInt;
Begin;
for o:=1 to n do begin;
  i:=currentTime;
  repeat
    relequish;
    timer2start;
    until (i<>currentTime);
  end;
End;

Label f1,f2;
BEGIN;
Randomize;
ClearBuffer;
timer2start;
TextColor(7);clrscr;
f1:
CreateRandomStuff;
CalculateNewBuf;
FreshScr;
waitTicks(ticksPerSec shr 3);
while keypressed do if (readKey<>$8001) then goto f2;
goto f1;
f2:
TextColor(7);
WriteLn('');
END.