{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc crt.inc}

Function dup(n:Byte;c:Char):String;
Var a:String;i:Byte;
Begin;
a:='';
for i:=1 to n do a:=a+c;
dup:=a;
End;

const
  MaxX=8;
  MaxY=8;
const
  BegX=10;
  BegY=5;
  figureD:array[0..2] of char=('ú','X','O');
  figureC:array[0..2] of byte=(7,12,10);
var
  table:array[1..MaxY,1..MaxX] of byte;
  users:array[1..2] of record
    n:String[15];
    t:byte; {0-computer, 1-user}
    end;
  curX,curY:shortint;

procedure putTable;
var i,o,d:word;
begin;
GotoXY(begX,begY);textColor(7);
Write('Ú'+dup(2*maxX,'Ä')+'¿');
for i:=1 to maxY do begin;
  GotoXY(begX,begY+i);
  Write('³'+dup(2*maxX,' ')+'³');
  end;
GotoXY(begX,begY+maxY+1);
Write('À'+dup(2*maxX,'Ä')+'Ù');
for o:=1 to maxY do for i:=1 to maxX do begin;
  d:=table[o][i];
  if (d>2) then d:=0;
  GotoXY(i*2+begX,o+begY);
  textColor(figureC[d]);
  Write(figureD[d]);
  end;
end;

procedure clrTable;
var i,o:Byte;
begin;
fillChar(table,sizeof(table),0);
table[MaxY div 2+0][MaxX div 2+0]:=1;
table[MaxY div 2+1][MaxX div 2+1]:=1;
table[MaxY div 2+1][MaxX div 2+0]:=2;
table[MaxY div 2+0][MaxX div 2+1]:=2;
curX:=1;
curY:=1;
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

function selectDir(plr,bx,by,mx,my:shortint;justtest:boolean):byte;
Label f1,f2;
Var
  cx,cy:byte;
  i,o:byte;
begin;
selectDir:=0;
cx:=bx;cy:=by;
o:=0;
f1:
inc(cx,mx);
inc(cy,my);
if (cx<1) or (cx>MaxX) then exit;
if (cy<1) or (cy>MaxY) then exit;
i:=table[cy][cx];
if (i=plr) then goto f2;
if (i=0) then exit;
inc(o);
goto f1;
f2:
selectDir:=o;
if justtest then exit;
if (o<1) then exit;
cx:=bx;cy:=by;
for i:=0 to o do begin;
  table[cy][cx]:=plr;
  inc(cx,mx);
  inc(cy,my);
  end;
end;

function selectMove(plr,x,y:shortint;tst:boolean):byte;
var i:byte;
begin;
if (table[y][x]<>0) then begin; selectMove:=0;exit; end;
selectMove:=
+selectDir(plr,x,y,+1,+1,tst)
+selectDir(plr,x,y,+1, 0,tst)
+selectDir(plr,x,y,+1,-1,tst)
+selectDir(plr,x,y,-1,+1,tst)
+selectDir(plr,x,y,-1, 0,tst)
+selectDir(plr,x,y,-1,-1,tst)
+selectDir(plr,x,y, 0,+1,tst)
+selectDir(plr,x,y, 0,-1,tst)
;
end;


procedure suggestMove(plr:byte;var bx,by:shortint);
var
  bv:shortint;
  x,y:shortint;
  i:byte;
begin;
bx:=-1;
by:=-1;
bv:=0;
for y:=1 to MaxY do for x:=1 to MaxX do begin;
  i:=selectMove(plr,x,y,true);
  if (i>bv) then begin; bx:=x;by:=y;bv:=i; end;
  end;
end;

function DoOneRound(plr:byte):boolean;
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

procedure CountBalls(x,y,p:byte);
var i,o,a:byte;
begin;
a:=0;
for o:=1 to maxY do for i:=1 to maxX do if (table[o][i]=p) then inc(a);
GotoXY(x,y);
textColor(figureC[p]);
Write(figureD[p]+' has '+bstr(a)+' points');
end;


Var
  i:byte;
  a,b:shortint;
begin;
DoOneRound:=true;

countBalls(1,22,1);
countBalls(1,23,2);
putTable;
GotoXY(10,1);
textColor(figureC[plr]);
Write(figureD[plr]+' - '+copy(users[plr].n+dup(40,' '),1,32));


suggestMove(plr,a,b);
if (a+b<0) then begin;
  suggestMove(3-plr,a,b);
  if (a+b<0) then begin;
    warn('this is the end of the game!');
    Exit;
    end;
  warn('this user must pass a round!');
  DoOneRound:=false;
  Exit;
  end;

if (users[plr].t=0) then begin;
  warn('press any key to let computer move!');
  suggestMove(plr,curX,curY);
  goto f2;
  end;

f1:
putTable;
if ProcessMoving then exit;
if (table[curY][curX]<>0) then begin;
  warn('this isn''t an empty point');
  goto f1;
  end;
f2:
i:=selectMove(plr,curX,curY,false);
if (i=0) then begin;
  warn('did not jumped over any point');
  goto f1;
  end;

DoOneRound:=false;
end;




Label f1;
Var
  i:word;
  b:Boolean;
  c:char;
Begin;
clrscr;
for i:=1 to 2 do begin;
  users[i].n:=ParamStr(i);
  b:=(users[i].n='');
  Users[i].t:=1-byte(b);
  if b then users[i].n:='Computer';
  end;
if (users[1].t=0) then begin;
  WriteLn('using: lagno.code <user1> [user2]');
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
putTable;
GotoXY(1,24);
End.