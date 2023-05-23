Var
  ScrSizX,ScrSizY:Word;
  CurrFileName:String;
  RefreshScr:Byte; {1-line, 2-text, 4-status, 128-screen}
  BegX,BegY:LongInt;
  CurX,CurY:LongInt;
  InsertMode:Boolean;
  VerticalMode:Boolean;
  DrawingMode:Byte; {0-none, 1-single, 2-double}
  SelBX,SelBY,SelEX,SelEY:LongInt;
  FindTxt1,FindTxt2:String;


Procedure ClearDocument;
Begin;
ResizeMemory(1);
WriteLine(1,'');
BegX:=0;
BegY:=0;
CurX:=1;
CurY:=1;
CurrFileName:='';
MemoryChanged:=False;
InsertMode:=True;
VerticalMode:=False;
DrawingMode:=0;
SelBX:=0;
SelBY:=0;
SelEX:=0;
SelEY:=0;
FindTxt1:='';
FindTxt2:='';
RefreshScr:=0;
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

Function DoFindNextWordInText:Boolean;
Label vege;
Var
  a,b:String;
  i,o:LongInt;
Begin;
DoFindNextWordInText:=True;
b:=kicsi(FindTxt1);
a:=ReadLine(CurY);
delete(a,1,CurX);
a:=kicsi(a);
i:=pos(b,a);
if (i<>0) then begin;
  inc(CurX,i+length(b));
  goto vege;
  end;
for o:=CurY+1 to MemoryLines do begin;
  a:=ReadLine(o);
  a:=kicsi(a);
  i:=pos(b,a);
  if (i<1) then continue;
  CurY:=o;
  CurX:=i+length(b);
  goto vege;
  end;
exit;
Vege:
DoFindNextWordInText:=False;
End;

Function DoFindPrevWordInText:Boolean;
Label vege;
Var
  a,b:String;
  i,o:LongInt;

function x:boolean;
label f1;
begin;
x:=false;
i:=pos(b,a);
if (i<1) then exit;
CurY:=o;
CurX:=-length(b)+1;
x:=true;
f1:
i:=pos(b,a);
if (i=0) then exit;
delete(a,1,i+length(b)-1);
inc(CurX,i+length(b)-1);
goto f1;
end;

Begin;
DoFindPrevWordInText:=True;
b:=kicsi(FindTxt1);
a:=ReadLine(CurY);
a:=copy(a,1,CurX-1);
a:=kicsi(a);
o:=CurY;
if x then goto vege;
for o:=CurY-1 downto 1 do begin;
  a:=ReadLine(o);
  a:=kicsi(a);
  if x then goto vege;
  end;
exit;
Vege:
DoFindPrevWordInText:=False;
End;






Procedure StatusLineClear;
Begin;
gotoXY(1,ScrSizY);
textColor(ColStat);
Write(dup(ScrSizX-1,'Ä'));
End;

Procedure StatusLineWrite(a:String;c:Byte);
Begin;
StatusLineClear;
gotoXY(4,ScrSizY);
a:=copy(a,1,ScrSizX-8);
textColor(ColStat);Write('´');
textColor(c);Write(a);
textColor(ColStat);Write('Ã');
gotoXY(5,ScrSizY);
RefreshScr:=RefreshScr or 4;
End;

Procedure StatusLineFilNam;
Var a:String;
Begin;
a:=xFileName(CurrFileName,2)+xFileName(CurrFileName,3);
a:=copy(a,1,32);
gotoXY(ScrSizX-length(a)-1,ScrSizY);
textColor(ColStat);Write('´');
textColor(ColSttP);Write(a);
End;

Procedure StatusLineCurPos;
Var a:String;
Begin;
gotoXY(4,ScrSizY);
a:=BStr(CurY);
textColor(ColStat);
Write(dup(8-length(a),'Ä'));
Write('´');
textColor(ColSttP);
Write(a);
Write(':');
a:=BStr(CurX);
Write(a);
textColor(ColStat);
Write('Ã');
Write(dup(8-length(a),'Ä'));
Write('´');
textColor(ColSttP);
if MemoryChanged then a:=#15 else a:=#249;
Write(a);
if InsertMode then a:='i' else a:='o';
Write(a);
case DrawingMode of
  0:if VerticalMode then a:=#18 else a:=#29;
  1:a:='Å';
  2:a:='Î';
  else a:='?';
  end;
Write(a);
textColor(ColStat);
Write('Ã');
End;

Procedure WriteOneLine(n:Word);
Var
  a:String;
  bg,ed:LongInt;
Begin;
gotoXY(1,n);
a:=ReadLine(BegY+n);
a:=copy(a,BegX+1,ScrSizX);
while (length(a)<ScrSizX) do a:=a+' ';
inc(n,BegY);
bg:=1;
ed:=1;
if VerticalMode then begin;
  if (SelBY<=n) and (SelEY>=n) then begin;
    bg:=SelBX-BegX;
    ed:=SelEX-BegX;
    end;
  end else begin;
  if (SelBY<n) and (SelEY>n) then begin; bg:=1;ed:=ScrSizX+1; end;
  if (SelBY=n) then begin; bg:=SelBX-BegX;ed:=ScrSizX+1; end;
  if (SelEY=n) then begin; bg:=1;ed:=SelEX-BegX; end;
  if (SelBY=n) and (SelEY=n) then begin; bg:=SelBX-BegX;ed:=SelEX-BegX; end;
  end;
if (bg<1) then bg:=1;
if (ed<1) then ed:=1;
if (bg>=ed) then begin;
  textColor(ColNorm);
  Write(a);
  Exit;
  end;
if (bg>1) then begin;
  textColor(ColNorm);
  Write(copy(a,1,bg-1));
  end;
textColor(ColSelc);
Write(copy(a,bg,ed-bg));
if (ed<=ScrSizX) then begin;
  textColor(ColNorm);
  Write(copy(a,ed,255));
  end;
End;

Procedure WriteTheText;
Var i:Word;
Begin;
for i:=1 to ScrSizY-1 do WriteOneLine(i);
End;

Procedure PutTheCursor;
Begin;
gotoXY(CurX-BegX,CurY-BegY);
End;



Procedure PutOutWindow(var xp,yp:word;xs,ys:word;a:string);
Var i:Word;
Begin;
RefreshScr:=RefreshScr or 2;
xp:=(ScrSizX-xs-4) div 2;
yp:=(ScrSizY-ys+0) div 2;
textColor(ColWinB);
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
textColor(ColWinT);
End;

Function SelectAsciiChar(a:String):LongInt;
Label f1,f2;
Var
  xp,yp,xs,ys:Word;
  x,y,c:LongInt;
  cc:char absolute c;

Procedure putBrackets(y:LongInt;s:String);
Var x:LongInt;
Begin;
x:=(y and 15)*3+xp;
y:=(y shr 4)+yp;
GotoXY(x-1,y);write(s[1]);
GotoXY(x+1,y);write(s[2]);
GotoXY(x,y);
End;

Var
  i,o:Word;
Begin;
SelectAsciiChar:=-1;
xs:=48;
ys:=18;
PutOutWindow(xp,yp,xs,ys,a);
inc(xp);
c:=0;
for y:=0 to 15 do for x:=0 to 15 do begin;
  GotoXY(xp+x*3,yp+y);
  BugOS_WriteCustomChar(cc);
  inc(c);
  end;
GotoXY(xp,yp+17);
Write('char=X  bin=12345678  oct=123  dec=123  hex=12');
x:=0;y:=0;
f1:
c:=y*16+x;
gotoXY(xp+5,yp+17);
BugOS_WriteCustomChar(cc);
gotoXY(xp+12,yp+17);
Write(byte2bintype(c));
gotoXY(xp+26,yp+17);
Write(BStr(c shr 6)+BStr((c shr 3) and 7)+BStr(c and 7));
gotoXY(xp+35,yp+17);
Write(BStr(c div 100)+BStr((c div 10) mod 10)+BStr(c mod 10));
gotoXY(xp+44,yp+17);
Write(byte2hextype(c));
putBrackets(c,'[]');
f2:
i:=ReadKey;
case i of
  $8001:begin; RefreshScr:=$ff;exit; end;
  $8005:exit;{esc}
  $8004:begin;{enter}
    SelectAsciiChar:=c;
    exit;
    end;
  $800c:dec(y);{up}
  $800d:inc(y);{down}
  $800e:dec(x);{left}
  $800f:inc(x);{right}
  else if (i and $fe00=0) then begin;
    x:=i and 15;
    y:=i shr 4;
    end;
  end;
x:=x and 15;
y:=y and 15;
if (c<>y*16+x) then begin;
  putBrackets(c,'  ');
  goto f1;
  end;
goto f2;
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
RefreshScr:=RefreshScr or 2;
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


Function AskForOneText(win,tit:String;var st:String):Boolean;
Var xp,yp,xs,ys:Word;
Begin;
AskForOneText:=True;
xs:=ScrSizX-12;
ys:=2;
PutOutWindow(xp,yp,xs,ys,win);
GotoXY(xp,yp);
WRite(tit);
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
addLin('f1       - help');
addLin('f2       - save current file');
addLin('f3       - reload current file');
addLin('f4       - toggle single/double line mode');
addLin('f5       - toggle vertical/horizontal mode');
addLin('f6       - duplicate current line');
addLin('f7       - find in text');
addLin('shift+f7 - find again');
addLin('alt+f7   - reversed search');
addLin('f8       - replace in text');
addLin('shift+f8 - replace again');
addLin('alt+f8   - reversed replace');
addLin('f9       - go to line number');
addLin('insert   - toggle insert/overwrite mode');
addLin('f10      - leave the editor');
addLin('');
AddLin(' moving');
AddLin('~~~~~~~~');
AddLin('up         - move up one line');
AddLin('down       - move down one line');
AddLin('left       - move left one column');
AddLin('right      - move right one column');
AddLin('ctrl+left  - move to next word');
AddLin('ctrl+right - move to previous word');
AddLin('home       - move beginning of line');
AddLin('end        - move ending of line');
AddLin('pgup       - move up some lines');
AddLin('pgdn       - move down some lines');
AddLin('ctrl+home  - move beginning of screen');
AddLin('ctrl+end   - move ending of screen');
AddLin('ctrl+pgup  - move beginning of document');
AddLin('ctrl+pgdn  - move ending of document');
addLin('');
AddLin(' editing');
AddLin('~~~~~~~~~');
addLin('delete    - delete next character');
addLin('backspace - delete previous character');
addLin('ctrl+q    - insert current date');
addLin('ctrl+w    - insert current time');
addLin('ctrl+y    - delete current line');
addLin('ctrl+n    - insert new line');
addLin('ctrl+t    - delete to next word');
addLin('alt+l     - align vertical block to left');
addLin('alt+r     - align vertical block to right');
addLin('alt+c     - align vertical block to center');
addLin('alt+j     - align vertical block to justify');
addLin('alt+[     - uppercase the selected text');
addLin('alt+]     - lowercase the selected text');
addLin('alt+\     - capitalize the selected text');
addLin('alt+a     - insert ascii character');
addLin('');
AddLin(' clipboard');
AddLin('~~~~~~~~~~~');
AddLin('shift+moving - select text');
AddLin('shift+delete - cut selected text');
AddLin('shift+insert - paste from clipboard');
AddLin('ctrl+delete  - clear selected text to clipboard');
AddLin('ctrl+insert  - copy selected text to clipboard');
xs:=ScrSizX-12;
ys:=ScrSizY-7;
if (ys>PosSiz) then ys:=PosSiz;
if (xs>200) then xs:=200;
PutOutWindow(xp,yp,xs,ys,'help');
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

Function TestForSavedFile(a:String):Boolean;
Label vege;
Var
  xp,yp:Word;
  w:Word;
Begin;
TestForSavedFile:=True;
if not MemoryChanged then goto vege;
PutOutWindow(xp,yp,37,3,'warning');
GotoXY(xp,yp+0);Write('you are going to '+a+',');
GotoXY(xp,yp+1);Write('but the contents of text has changed!');
GotoXY(xp,yp+2);Write('are you sure you want to continue?');
GotoXY(xp,yp);
w:=readkey;
w:=w and $feff;
if (w=ord('y')) then goto vege;
if (w=ord('Y')) then goto vege;
exit;
vege:
TestForSavedFile:=False;
End;
