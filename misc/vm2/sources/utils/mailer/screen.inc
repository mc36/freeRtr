Var
  ScrSizX,ScrSizY:Word;
  RefreshScr:byte; {32-current, 64-list, 128-screen}

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

Procedure PutOutWindow(var xp,yp:word;xs,ys:word;a:string;c1:Byte);
Var i:Word;
Begin;
xp:=(ScrSizX-xs-4) div 2;
yp:=(ScrSizY-ys+0) div 2;
textColor(c1);
GotoXY(xp,yp);Write(' É'+dup(xs+2,'Í')+'» ');
i:=xs-length(a)+4;
GotoXY(xp+(i div 2),yp);Write('µ'+a+'Æ');
a:=' º'+dup(xs+2,' ')+'º ';
for i:=1 to ys do begin;
  GotoXY(xp,yp+i);
  Write(a);
  end;
GotoXY(xp,yp+ys+1);Write(' È'+dup(xs+2,'Í')+'¼ ');
inc(xp,3);
inc(yp,1);
End;

Function AskUserForText(var st:string;x,y,xs:word):Boolean;
Label kiir,f1,f2;
Var
  a:String;
  fresh:boolean;
  xb,xp:LongInt;
  w,ww:Word;
Begin;
AskUserForText:=True;
xb:=0;
xp:=length(st)+1;
fresh:=true;
goto f2;
kiir:
a:=copy(st,xb+1,xs);
a:=copy(a+dup(xs,' '),1,xs);
GotoXY(x,y);
Write(a);
GotoXY(x+xp-xb-1,y);
fresh:=false;
f1:
w:=ReadKey;
if (w and $fe00=0) then begin;{simple key}
  w:=w and $ff;
  if (w in [0,255,13,10,8,9]) then w:=ord(' ');
  insert(chr(w),st,xp);
  inc(xp);
  fresh:=true;
  end;
ww:=xp;
case w of
  $8007:begin;{delete}
    delete(st,xp,1);
    fresh:=true;
    end;
  $8003:if (xp>1) then begin;{backspace}
    dec(xp);
    delete(st,xp,1);
    fresh:=true;
    end;
  $8001:begin; RefreshScr:=$ff;exit; end;
  $8004:begin; AskUserForText:=False;exit; end;{enter}
  $8005:exit;{esc}
  $8008:xp:=1;{home}
  $8009:xp:=length(st)+1;{end}
  $800e:dec(xp);{left}
  $800f:inc(xp);{right}
  $820e:if (xp>1) then xp:=FindPrevWordStop(st,xp-1);{ctrl+left}
  $820f:if (xp<=length(a)) then xp:=FindNextWordStop(st,xp);{ctrl+right}
  end;
if (xp<>ww) then fresh:=true;
if keypressed then goto f1;
f2:
if (xp<1) then xp:=1;
w:=length(st)+1;
if (xp>w) then xp:=w;
if (xp<=xb) then xb:=xp-1;
if (xp>xb+xs) then xb:=xp-xs;
if fresh then goto kiir;
goto f1;
End;


Function AskForOneText(win,txt:String;var st:String):Boolean;
Var xp,yp,xs,ys:Word;
Begin;
AskForOneText:=True;
xs:=ScrSizX-12;
ys:=2;
PutOutWindow(xp,yp,xs,ys,win,$71);TextColor($70);
GotoXY(xp,yp);
Write(txt);
win:=st;
if AskUserForText(st,xp,yp+1,xs) then begin;
  st:=win;
  exit;
  end;
AskForOneText:=False;
End;






Procedure ProcessHelpScreen;
Label kiir,key;
Var
  TxtBuf:array[1..4*1024] of char;
  TxtSiz:Word;
  PosBuf:array[1..256] of Word;
  PosSiz:Word;
  xp,yp,xs,ys:Word;
  bg:LongInt;
  i,o:Word;
  a:String;

Procedure addLin(a:String);
Begin;
inc(PosSiz);
PosBuf[PosSiz]:=TxtSiz+1;
move(a,TxtBuf[TxtSiz+1],length(a)+1);
inc(TxtSiz,length(a)+1);
End;

Begin;
TxtSiz:=0;
PosSiz:=0;
addLin(' commands');
AddLin('~~~~~~~~~~');
addLin('f1         - help');
addLin('f10        - leave the mailer');
addLin('enter      - read message');
addLin('ctrl+enter - select one message part');
addLin('d          - mark for deletion');
addLin('r          - reply to message');
addLin('f          - save originator to friends');
addLin('c          - compose a new message');
addLin('h          - display header of message');
addLin('');
AddLin(' moving');
AddLin('~~~~~~~~');
AddLin('up   - move up one entry');
AddLin('down - move down one entry');
AddLin('home - move to the first entry');
AddLin('end  - move to the last entry');
AddLin('pgup - move up some entries');
AddLin('pgdn - move down some entries');
addLin('');
addLin('');
AddLin(' part viewer');
AddLin('~~~~~~~~~~~~~');
addLin('f1    - help');
addLin('f10   - leave the mailer');
addLin('r     - reply to current part');
addLin('w     - extract current part');
addLin('enter - read current part');
xs:=ScrSizX-12;
ys:=ScrSizY-7;
if (ys>PosSiz) then ys:=PosSiz;
if (xs>200) then xs:=200;
PutOutWindow(xp,yp,xs,ys,'help',$71);TextColor($70);
bg:=0;
kiir:
for i:=1 to ys do begin;
  GotoXY(xp,yp+i-1);
  move(TxtBuf[PosBuf[i+bg]],a,sizeof(a));
  while (length(a)<xs) do a:=a+' ';
  a:=copy(a,1,xs);
  Write(a);
  end;
GotoXY(xp,yp);
key:
o:=bg;
i:=ReadKey;
case i of
  $8001:begin; RefreshScr:=$ff;exit; end;
  $800c:dec(bg);{up}
  $800d:inc(bg);{down}
  $800a:dec(bg,ys shr 1);{pgup}
  $800b:inc(bg,ys shr 1);{pgdn}
  $8005:exit;{esc}
  $8014:exit;{f1}
  end;
if (bg<0) then bg:=0;
i:=PosSiz-ys;
if (bg>i) then bg:=i;
if (bg<>o) then goto kiir;
goto key;
End;


Function ReadChrKey:Char;
Var w:Word;
Begin;
ReadChrKey:=#0;
w:=ReadKey;
if (w=$8001) then begin;
  RefreshScr:=$ff;
  exit;
  end;
if (w and $fe00<>0) then exit;
ReadChrKey:=UpCase(chr(w));
End;

Function ShowErrorMessage(err:Word;act:String):Boolean;
Var xp,yp,xs,ys:Word;
Begin;
ShowErrorMessage:=False;
if (err=0) then exit;
xs:=ScrSizX-8;
ys:=5;
PutOutWindow(xp,yp,xs,ys,'error',$47);TextColor($4f);
GotoXY(xp,yp+0);Write('error happened while tried to do the following:');
GotoXY(xp,yp+1);Write(act);
GotoXY(xp,yp+2);Write('the error was the following:');
GotoXY(xp,yp+3);Write(xGetErrorName(err));
GotoXY(xp,yp+4);Write('press any key to continue!');
ReadChrKey;
ShowErrorMessage:=True;
End;
