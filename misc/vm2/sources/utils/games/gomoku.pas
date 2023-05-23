{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc crt.inc}

Const
  MaxX=30;
  MaxY=19;
Type
  OneTableRecord=array[1..MaxY,1..MaxX] of byte;

Procedure PutOnePoint(var tab:OneTableRecord;usr,x,y:byte);
Begin;
tab[y][x]:=usr;
End;


Function CountPoints(var tab:OneTableRecord;usr:byte;xc,yc:longint):LongInt;
Var sc:LongInt;

function get(x,y:longint):byte;
begin;
get:=5;
if (x<1) then exit;
if (y<1) then exit;
if (x>maxX) then exit;
if (y>maxY) then exit;
get:=tab[y][x];
end;

procedure cnt(xd,yd:longint);
const PointsTable:array[0..5] of longint=(0,1,3,10,101,11111);
var
  x,y:longint;
  point,empty,frend:longint;
  i:longint;
  o:LongInt;
begin;
point:=0;empty:=1;frend:=0;
x:=xc;y:=yc;
while (get(x,y)=usr) do begin;
  dec(x,xd);
  dec(y,yd);
  end;
while (get(x,y)=0) do begin;
  inc(empty);
  dec(x,xd);
  dec(y,yd);
  end;
inc(x,empty*xd);
inc(y,empty*yd);
dec(empty);
if (empty<>0) then inc(frend);
while (get(x,y)=usr) do begin;
  inc(point);
  inc(x,xd);
  inc(y,yd);
  end;
i:=empty;
while (get(x,y)=0) do begin;
  inc(empty);
  inc(x,xd);
  inc(y,yd);
  end;
if (empty<>i) then inc(frend);
if (point+empty<5) then point:=0;
if (point>5) then point:=5;
o:=PointsTable[point];
if (frend<2) then dec(o,o div 3);
inc(sc,o);
end;


Begin;
CountPoints:=0;
if (get(xc,yc)<>0) then exit;
sc:=0;
tab[yc][xc]:=usr;
cnt(+1,+1);
cnt(+1,+0);
cnt(+1,-1);
cnt(+0,-1);
tab[yc][xc]:=0;
inc(sc);
CountPoints:=sc;
End;


Procedure FindBestMove(var tab:OneTableRecord;usr:Byte;Var xb,yb:Byte);
Var
  xc,yc:Byte;
  pb,pc:LongInt;
Begin;
xb:=maxX shr 1;
yb:=maxY shr 1;
if (tab[yb][xb]=0) then pb:=10 else pb:=0;
for yc:=1 to maxY do for xc:=1 to maxX do begin;
  pc:=CountPoints(tab,usr,xc,yc);
  inc(pc,pc div 3);
  inc(pc,CountPoints(tab,3-usr,xc,yc));
  if (pc>pb) then begin;
    pb:=pc;
    xb:=xc;
    yb:=yc;
    end;
  end;
End;


Function DetectWinner(var tab:OneTableRecord):Byte;
Var
  usr:byte;
  xc,yc:Byte;

procedure cnt(xd,yd:longint);
label f1;
var
  x,y:longint;
  i:byte;
begin;
if (usr<>0) then exit;
x:=xc;y:=yc;
usr:=tab[y][x];
if (usr=0) then goto f1;
for i:=1 to 5 do begin;
  if (x<1) then goto f1;
  if (y<1) then goto f1;
  if (x>maxX) then goto f1;
  if (y>maxY) then goto f1;
  if (usr<>tab[y][x]) then goto f1;
  inc(x,xd);
  inc(y,yd);
  end;
exit;
f1:
usr:=0;
end;

Begin;
usr:=0;
for yc:=1 to maxY do for xc:=1 to maxX do begin;
  cnt(+1,+1);
  cnt(+1,+0);
  cnt(+1,-1);
  cnt(+0,-1);
  end;
DetectWinner:=usr;
End;

Function dup(n:Byte;c:Char):String;
Var a:String;i:Byte;
Begin;
a:='';
for i:=1 to n do a:=a+c;
dup:=a;
End;

procedure clrEol;
begin;
write(#13'                                       '#13);
end;

procedure clearLastLn;
begin;
while keypressed do readkey;
textColor(7);
gotoxy(1,25);
clrEol;
end;

Procedure displayTable(var table:OneTableRecord);
Const
  begX=3;
  begY=3;
  figureD:array[0..2] of char=('ú','X','O');
  figureC:array[0..2] of byte=(7,12,10);
Var
  i,o,d:Byte;
Begin;
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
  if (d>2) then d:=0;
  GotoXY(i*2+begX,o+begY);
  textColor(figureC[d]);
  Write(figureD[d]);
  end;
End;

Label f1,f2,f3;
Var
  CurrTable:OneTableRecord;
  userNames:array[1..2] of String;
  i:word;
  user:byte;
  x,y:byte;
  c:char;

function getUserName(n:byte):string;
var a:string;
begin;
a:=userNames[n];
if (a='') then a:='computer';
getUserName:=a;
end;

function askUser(var x,y:byte):boolean;
label f1,f2;
var w:Word;
begin;
askUser:=true;
f1:
gotoxy(x*2+3,y+3);
w:=readkey;
case w of
  $8005:exit;
  $8004:begin;
    if (CurrTable[y][x]=0) then goto f2;
    gotoxy(1,25);
    textColor($8c);
    write('this point is not empty!');
    readkey;
    clearLastLn;
    end;
  $800c:dec(y);
  $800d:inc(y);
  $800e:dec(x);
  $800f:inc(x);
  end;
if (x<1) then x:=maxX;
if (y<1) then y:=maxY;
if (x>maxX) then x:=1;
if (y>maxY) then y:=maxY;
goto f1;
f2:
askUser:=false;
exit;
end;

procedure putUserName;
Const
  cols:array[1..2] of byte=(10,12);
  chrs:array[1..2] of char=('O','X');
begin;
gotoxy(1,1);
textColor(7);
clrEol;
Write('player: ');
textColor(cols[user]);
Write(chrs[user]);
Write(' - ');
Write(getUserName(user));
end;


BEGIN;
clrscr;
for x:=1 to 2 do userNames[x]:=ParamStr(x);
if (userNames[1]='') then begin;
  WriteLn('using: gomoku.code <user1> [user2]');
  exit;
  end;

f1:
fillchar(currTable,sizeof(currTable),0);
user:=1;
x:=maxX shr 1;
y:=maxY shr 1;
f2:
displayTable(currTable);
putUserName;
if (userNames[user]='') then begin;
  FindBestMove(currTable,user,x,y);
  end else begin;
  if askUser(x,y) then goto f3;
  end;
PutOnePoint(currTable,user,x,y);
i:=DetectWinner(currTable);
if (i<>0) then begin;
  displayTable(currTable);
  gotoxy(1,25);
  textColor($8c);
  write(getUserName(i)+' won!');
  readkey;
  clearLastLn;
  goto f3;
  end;
user:=3-user;
goto f2;
f3:
gotoxy(1,25);
END.