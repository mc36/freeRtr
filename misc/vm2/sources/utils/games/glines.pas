{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}
{$sysinc crt.inc}

Function dup(n:Byte;c:Char):String;
Var a:String;i:Byte;
Begin;
a:='';
for i:=1 to n do a:=a+c;
dup:=a;
End;

const
  maxX=10;
  maxY=10;
  MaxB=7;
  MaxN=3;
const
  begX=10;
  begY=6;
type oneTableRec=array[1..maxY,1..maxX] of byte;
var
  table,table2:oneTableRec;
  nextD:array[1..maxN] of byte;
  curX,curY:shortint;
  balls:longint;
  lines:longint;
  moves:longint;
const
  figureD:array[0..MaxB] of char=('ú','O','O','O','O','O','O','O');
  figureC:array[0..MaxB] of byte=(7,9,11,14,13,6,10,12);


function getScores(m,l,b:longint):longint;
begin;
getScores:=m*1+l*2+b*3;
end;


procedure putTable;
var i,o,d:word;
begin;
GotoXY(begX,begY);textColor(7);
Write('Ú'+dup(2*maxX+1,'Ä')+'¿');
for i:=1 to maxY do begin;
  GotoXY(begX,begY+i);
  Write('³'+dup(2*maxX+1,' ')+'³');
  end;
GotoXY(begX,begY+maxY+1);
Write('À'+dup(2*maxX+1,'Ä')+'Ù');
for o:=1 to maxY do for i:=1 to maxX do begin;
  d:=table[o][i];
  if (d>maxB) then d:=0;
  GotoXY(i*2+begX,o+begY);
  textColor(figureC[d]);
  Write(figureD[d]);
  end;
end;

procedure putScores;
begin;
textColor(15);
GotoXY(9,1);Write(BStr(moves));
GotoXY(9,2);Write(BStr(lines));
GotoXY(9,3);Write(BStr(balls));
GotoXY(9,4);Write(BStr(getScores(moves,lines,balls)));
end;

procedure putNext;
var i,o:byte;
begin;
gotoXY(20,4);
for i:=1 to maxN do begin;
  o:=nextD[i];
  textColor(figureC[o]);
  write(figureD[o]);
  end;
end;

procedure clrTable;
begin;
fillchar(table,sizeof(table),0);
fillchar(nextD,sizeof(nextD),0);
curX:=1;
curY:=1;
balls:=0;
lines:=0;
moves:=0;
end;

function availTable:word;
var
  i:word;
  x,y:byte;
begin;
i:=0;
for y:=1 to maxY do for x:=1 to maxX do if (table[y][x]=0) then inc(i);
availTable:=i;
end;

procedure killOneDirection(x,y,dx,dy:integer);
label f1,f2;
var
  c,n:byte;
begin;
c:=table2[y][x];
if (c=0) then exit;
n:=0;
f1:
inc(n);
if (x<1) or (x>maxX) then goto f2;
if (y<1) or (y>maxY) then goto f2;
if (table2[y][x]<>c) then goto f2;
inc(x,dx);
inc(y,dy);
goto f1;
f2:
dec(n);
if (n<5) then exit;
inc(balls,n);
inc(lines,1);
for c:=1 to n do begin;
  dec(x,dx);
  dec(y,dy);
  table[y][x]:=0;
  end;
end;

procedure killLines;
var x,y:byte;
begin;
table2:=table;
for y:=1 to maxY do for x:=1 to maxX do begin;
  killOneDirection(x,y,1,+1);
  killOneDirection(x,y,1,-1);
  killOneDirection(x,y,1,0);
  killOneDirection(x,y,0,1);
  end;
end;

procedure rndTable;
var
  i,o,x,y:byte;
begin;
for i:=1 to maxN do begin;
  if (availTable<1) then exit;
  o:=0;
  repeat
    inc(o);
    if (o and $f=0) then Randomize;
    x:=random(maxX)+1;
    y:=random(maxY)+1;
    until (table[y][x]=0);
  table[y][x]:=nextD[i];
  killLines;
  end;
for i:=1 to maxN do nextD[i]:=random(maxB)+1;
end;

function ProcessMoving:boolean;
label f1,f2;
var w:word;
begin;
ProcessMoving:=false;
f1:
gotoXY(curX*2+begX,curY+begY);
w:=ReadKey;
case w of
  $8005:begin; ProcessMoving:=true;exit; end;
  $8004:exit;
  $800c:dec(curY);
  $800d:inc(curY);
  $800e:dec(curX);
  $800f:inc(curX);
  end;
if (curX<1) then curX:=maxX;
if (curY<1) then curY:=maxY;
if (curX>maxX) then curX:=1;
if (curY>maxY) then curY:=1;
goto f1;
end;

function DoOneRound:Boolean;
label f1,f2;

procedure warn(a:string);
begin;
textColor($8c);
GotoXY(1,24);
Write(a);
ReadKey;
GotoXY(1,24);
textColor(7);
Write(dup(40,' '));
end;

function testOneNeigh(x,y:byte):byte;
begin;
testOneNeigh:=0;
if (x<1) or (x>maxX) then exit;
if (y<1) or (y>maxY) then exit;
if (table2[y][x]<>0) then exit;
table2[y][x]:=255;
testOneNeigh:=1;
end;

function testAround(x,y:byte):byte;
begin;
if (table2[y][x]<>255) then begin; testAround:=0;exit; end;
testAround:=+testOneNeigh(x+0,y+1)+testOneNeigh(x+0,y-1)
            +testOneNeigh(x+1,y+0)+testOneNeigh(x-1,y+0);
end;

procedure fillRoute;
label f1;
var
  x,y:byte;
  i:word;
begin;
f1:
i:=0;
for x:=1 to maxX do for y:=1 to maxY do inc(i,testAround(x,y));
if (i<>0) then goto f1;
end;

var
  oldX,oldY:shortint;
  o:longint;
begin;
DoOneRound:=true;

putScores;
putNext;
if (availTable<1) then exit;

f1:
putTable;
if ProcessMoving then exit;
if (table[curY][curX]=0) then begin;
  warn('this isn''t a ball');
  goto f1;
  end;
GotoXY(curX*2+begX,curY+begY);
textColor($80+figureC[table[curY][curX]]);
Write(figureD[table[curY][curX]]);
oldX:=curX;oldY:=curY;
f2:
if ProcessMoving then goto f1;
if (table[curY][curX]<>0) then begin;
  warn('this isn''t an empty point');
  goto f2;
  end;

table2:=table;
table2[oldY][oldX]:=255;
fillRoute;
if (table2[curY][curX]<>255) then begin;
  warn('no route to this point');
  goto f2;
  end;
table[curY][curX]:=table[oldY][oldX];
table[oldY][oldX]:=0;
inc(moves);
o:=balls;
killLines;
if (balls=o) then rndTable;

DoOneRound:=false;
Exit;
end;


Label f1;
Var
  i:word;
  c:char;
BEGIN;
clrscr;
Randomize;
f1:
for i:=1 to 25 do begin;
  GotoXY(1,i);
  textColor(7);
  Write(dup(40,' '));
  end;

textColor(7);
gotoXY(1,1);
WriteLn(' moves:');
WriteLn(' lines:');
WriteLn(' balls:');
WriteLn('scores:');
gotoXY(14,4);WriteLn('next:');

clrTable;
for i:=1 to 3 do rndTable;
repeat until DoOneRound;
putTable;
GotoXY(1,24);
END.