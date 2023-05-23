Const screenMax=4096;
Type
  screen_OneCharacter=record c:byte;a:byte; end;
  screen_wholeData=array[0..screenMax-1] of screen_OneCharacter;
  screen_bufferRec=record
    x:LongInt;
    y:LongInt;
    d:screen_wholeData;
    end;
Var
  screenSizX:Word;
  screenSizY:Word;
  screenSize:LongInt;
  oscreenBuf:screen_wholeData;
  onScrnPosX:LongInt;
  onScrnPosY:LongInt;
  onScrnColr:LongInt;
  currentBuf:screen_wholeData;
  currntPosX:LongInt;
  currntPosY:LongInt;




Function dup(c:LongInt;a:String):String;
Var
  b:String;
  i:LongInt;
Begin;
b:='';
for i:=1 to c do b:=b+a;
dup:=b;
End;

Function FindNextWordStop(a:String;p:Word):Word;
Var i,o:Word;
Begin;
o:=length(a)+1;
for i:=length(a) downto p+1 do if (a[i]=' ') then o:=i;
while (copy(a,o,1)=' ') do inc(o);
FindNextWordStop:=o;
End;

Function FindPrevWordStop(a:String;p:Word):Word;
Var i,o:Word;
Begin;
if (p<1) then p:=1;
while (copy(a,p,1)=' ') and (p>1) do dec(p);
o:=0;
for i:=1 to p-1 do if (a[i]=' ') then o:=i;
FindPrevWordStop:=o+1;
End;



Procedure scr_clear;
Var i:LongInt;
Begin;
ConsoleSize(screenSizX,screenSizY);
screenSize:=screenSizY*screenSizX;
TextColor(7);
clrscr;
for i:=0 to screenSize-1 do begin; oscreenBuf[i].c:=$20;oscreenBuf[i].a:=$07; end;
onScrnColr:=7;
onScrnPosX:=0;
onScrnPosY:=0;
currentBuf:=oscreenBuf;
currntPosX:=0;
currntPosY:=0;
End;

Procedure scr_save(var d:screen_bufferRec);
Begin;
d.d:=currentBuf;
d.x:=currntPosX;
d.y:=currntPosY;
End;

Procedure scr_load(var d:screen_bufferRec);
Begin;
currentBuf:=d.d;
currntPosX:=d.x;
currntPosY:=d.y;
End;

Procedure scr_fresh;
Var
  x,y,p:LongInt;
  b:byte;
  c:char absolute b;
Begin;
p:=screenSize-1;
oscreenBuf[p]:=currentBuf[p];
p:=-1;
for y:=0 to screenSizY-1 do for x:=0 to screenSizX-1 do begin;
  inc(p);
  if (oscreenBuf[p].a=currentBuf[p].a) and (oscreenBuf[p].c=currentBuf[p].c) then continue;
  if (onScrnPosX<>x) or (onScrnPosY<>y) then gotoXY(x+1,y+1);
  if (onScrnColr<>currentBuf[p].a) then TextColor(currentBuf[p].a);
  b:=currentBuf[p].c;
  if (b<32) then BugOS_WriteCustomChar(c) else write(c);
  onScrnPosX:=x+1;
  onScrnPosY:=y;
  onScrnColr:=currentBuf[p].a;
  oscreenBuf[p]:=currentBuf[p];
  end;
if (onScrnPosX<>currntPosX) or (onScrnPosY<>currntPosY) then gotoXY(currntPosX+1,currntPosY+1);
onScrnPosX:=currntPosX;
onScrnPosY:=currntPosY;
End;

Procedure scr_write(x,y,c:LongInt;a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
inc(x,y*screenSizX);
for i:=1 to ab0 do begin;
  currentBuf[x].c:=ab[i];
  currentBuf[x].a:=c;
  inc(x);
  end;
End;

Procedure scr_color(x,y,c,s:LongInt);
Var i:LongInt;
Begin;
inc(x,y*screenSizX);
for i:=1 to s do begin;
  currentBuf[x].a:=c;
  inc(x);
  end;
End;

Function scr_read(var st:string;x,y,c,xs:LongInt):Boolean;
Label f1,f2,f3;
Var
  a:String;
  xb,xp:LongInt;
  w,ww:Word;
Begin;
scr_read:=True;
xb:=0;
xp:=length(st)+1;
currntPosY:=y;
goto f3;
f1:
a:=copy(st,xb+1,xs);
a:=copy(a+dup(xs,' '),1,xs);
scr_write(x,y,c,a);
currntPosX:=x+xp-xb-1;
scr_fresh;
f2:
w:=ReadKey;
if (w and $fe00=0) then begin;{simple key}
  w:=w and $ff;
  if (w in [0,255,13,10,8,9]) then w:=ord(' ');
  insert(chr(w),st,xp);
  inc(xp);
  end;
ww:=xp;
case w of
  $8001:exit;
  $8007:begin;{delete}
    delete(st,xp,1);
    end;
  $8003:if (xp>1) then begin;{backspace}
    dec(xp);
    delete(st,xp,1);
    end;
  $8004:begin; scr_read:=False;exit; end;{enter}
  $8005:exit;{esc}
  $8008:xp:=1;{home}
  $8009:xp:=length(st)+1;{end}
  $800e:dec(xp);{left}
  $800f:inc(xp);{right}
  $820e:if (xp>1) then xp:=FindPrevWordStop(st,xp-1);{ctrl+left}
  $820f:if (xp<=length(a)) then xp:=FindNextWordStop(st,xp);{ctrl+right}
  end;
if keypressed then goto f2;
f3:
if (xp<1) then xp:=1;
w:=length(st)+1;
if (xp>w) then xp:=w;
if (xp<=xb) then xb:=xp-1;
if (xp>xb+xs) then xb:=xp-xs;
goto f1;
End;

Procedure scr_window(var x,y:LongInt;t:String;xs,ys,bc,tc:LongInt);
Var
  i,o:LongInt;
  a:String;
Begin;
x:=(screenSizX-xs) shr 1;
y:=(screenSizY-ys-1) shr 1;
a:=' Ё '+dup(xs,' ')+' Ё ';
o:=x-3;
for i:=y to y+ys-1 do scr_write(o,i,bc,a);
scr_write(o,y-1,bc,' зд'+dup(xs,'д')+'д© ');
scr_write(o,y+ys,bc,' юд'+dup(xs,'д')+'ды ');
o:=x+xs+3;
for i:=y to y+ys do scr_color(o,i,8,2);
scr_color(x-1,y+ys+1,8,xs+6);
o:=(screenSizX-length(t)) shr 1;
scr_write(o-1,y-1,bc,'╢'+dup(length(t),' ')+'ц');
scr_write(o,y-1,tc,t);
End;
