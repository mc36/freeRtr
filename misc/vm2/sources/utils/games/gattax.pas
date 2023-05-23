{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc crt.inc}

Function dup(n:word;c:Char):String;
Var a:String;i:word;
Begin;
a:='';
for i:=1 to n do a:=a+c;
dup:=a;
End;

const
  maxX=8;
  maxY=8;
const
  begX=10;
  begY=6;
var
  table:array[1..maxY,1..maxX] of byte;
  curX,curY:longint;
  users:array[1..2] of record
    n:String[15];
    t:byte; {0-computer, 1-user}
    end;
const
  figureD:array[0..2] of char=('ú','X','O');
  figureC:array[0..2] of byte=(7,12,10);

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
  if (d>2) then d:=0;
  GotoXY(i*2+begX,o+begY);
  textColor(figureC[d]);
  Write(figureD[d]);
  end;
end;

procedure clrTable;
begin;
fillchar(table,sizeof(table),0);
table[maxY][1]:=2;
table[1][maxX]:=2;
table[1][1]:=1;
table[maxY][maxX]:=1;
curX:=1;
curY:=1;
end;

function ProcessMoving:boolean;
label f1,f2;
var w:Word;
begin;
ProcessMoving:=false;
f1:
gotoXY(2*curX+begX,curY+begY);
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

procedure suggestMove(plr:byte;var bcx,bcy,bmx,bmy:longint);
var
  bnm,nm:longint;
  cx,cy,mx,my:longint;

function calc(mx,my:longint):longint;
var x,y,i:longint;

procedure proc;
begin;
if (x<1) then exit;
if (y<1) then exit;
if (x>maxX) then exit;
if (y>maxY) then exit;
if (table[y][x]<>3-plr) then exit;
inc(i);
end;

begin;
calc:=-2;
inc(mx,cx);
inc(my,cy);
if (mx<1) then exit;
if (my<1) then exit;
if (mx>maxX) then exit;
if (my>maxY) then exit;
if (table[cy][cx]<>plr) then exit;
if (table[my][mx]<>0) then exit;
i:=1;
if (abs(cx-mx)>1) or (abs(cy-my)>1) then i:=0;
for y:=my-1 to my+1 do for x:=mx-1 to mx+1 do proc;
calc:=i;
end;

begin;
bnm:=-9;
bcx:=0;
bcy:=0;
bmx:=0;
bmy:=0;
for cy:=1 to maxY do for cx:=1 to maxX do begin;
  for my:=-2 to 2 do for mx:=-2 to 2 do begin;
    nm:=calc(mx,my);
    if (nm>bnm) then begin;
      bnm:=nm;
      bcx:=cx;
      bcy:=cy;
      bmx:=mx;
      bmy:=my;
      end;
    end;
  end;
inc(bmx,bcx);
inc(bmy,bcy);
end;

function DoOneRound(plr:byte):boolean;
label f1,f2,f3;

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

procedure putPnt(x,y:longint);
begin;
if (x<1) then exit;
if (y<1) then exit;
if (x>maxX) then exit;
if (y>maxY) then exit;
if (table[y][x]=0) then exit;
table[y][x]:=plr;
end;

procedure CountBalls(x,y,p:byte);
var i,o,a:byte;
begin;
a:=0;
for o:=1 to maxY do for i:=1 to maxX do if (table[o][i]=p) then inc(a);
GotoXY(x,y);
textColor(figureC[p]);
Write(figureD[p]+' has '+bstr(a)+' points');
end;

var
  oldX,oldY:longint;
  a,b,c,d:longint;
begin;
DoOneRound:=true;

countBalls(1,22,1);
countBalls(1,23,2);
putTable;
GotoXY(10,1);
textColor(figureC[plr]);
Write(figureD[plr]+' - '+copy(users[plr].n+dup(40,' '),1,32));

suggestMove(plr,a,b,c,d);
if (a+b<0) or (c+d<0) then begin;
  suggestMove(3-plr,a,b,c,d);
  if (a+b<0) or (c+d<0) then begin;
    warn('this is the end of the game!');
    Exit;
    end;
  warn('this user must pass a round!');
  DoOneRound:=false;
  Exit;
  end;

if (users[plr].t=0) then begin;
  warn('press any key to let computer move!');
  suggestMove(plr,oldX,oldY,curX,curY);
  goto f3;
  end;

f1:
putTable;
if ProcessMoving then exit;
if (table[curY][curX]<>plr) then begin;
  warn('this isn''t your point');
  goto f1;
  end;
GotoXY(2*curX+begX,curY+begY);
textColor($80+figureC[plr]);
Write(figureD[plr]);
oldX:=curX;oldY:=curY;
f2:
if ProcessMoving then goto f1;
if (table[curY][curX]<>0) then begin;
  warn('this isn''t an empty point');
  goto f2;
  end;
if (abs(oldX-curX)>2) or (abs(oldY-curY)>2) then begin;
  warn('you can''t not there');
  goto f2;
  end;

f3:
if (abs(oldX-curX)>1) or (abs(oldY-curY)>1) then table[oldY][oldX]:=0;
table[curY][curX]:=plr;
for oldY:=-1 to 1 do for oldX:=-1 to 1 do putPnt(curX+oldX,curY+oldY);

DoOneRound:=false;
Exit;
end;

Label f1;
Var
  i:word;
  b:Boolean;
  c:char;
BEGIN;
clrscr;
for i:=1 to 2 do begin;
  users[i].n:=ParamStr(i);
  b:=(users[i].n='');
  Users[i].t:=1-byte(b);
  if b then users[i].n:='Computer';
  end;
if (users[1].t=0) then begin;
  WriteLn('using: gattax.code <user1> [user2]');
  Exit;
  end;
f1:
for i:=1 to 25 do begin;
  GotoXY(1,i);
  textColor(7);
  Write(dup(40,' '));
  end;

GotoXY(2,1);
textColor(7);
Write('player:');
clrTable;
putTable;
i:=0;
repeat
  i:=(i and 1)+1
  until DoOneRound(i);
GotoXY(1,24);
END.