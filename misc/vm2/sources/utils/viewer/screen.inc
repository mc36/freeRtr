Var
  ScrSizX,ScrSizY:Word;
  ScrSizM,ScrSizB,ScrSizH:Word;
  RefreshScr:byte; {16-cursor, 32-header, 64-listing, 128-screen}
  filScr,filPos:LongInt;
  filBin:Boolean;  {binary/hexa toggle}
  filHex:Boolean;  {false=high, true=low nibble of hex digit}
  selBeg,selEnd:LongInt;


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

Procedure PutOutWindow(var xp,yp:word;xs,ys:word;a:string);
Var i:Word;
Begin;
RefreshScr:=RefreshScr or 1;
xp:=(ScrSizX-xs-4) div 2;
yp:=(ScrSizY-ys+0) div 2;
textColor($71);
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
textColor($70);
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
PutOutWindow(xp,yp,xs,ys,win);
GotoXY(xp,yp);
Write(txt);
win:=st;
if AskUserForText(st,xp,yp+1,xs) then begin;
  st:=win;
  exit;
  end;
AskForOneText:=False;
End;







Procedure FreshScreen;
Var
  a:String;
  i,o,p,q,r:LongInt;
  w:Word;
Begin;
if (RefreshScr=0) then exit;
if (RefreshScr and $80<>0) then begin;
  ConsoleSize(ScrSizX,ScrSizY);
  ScrSizM:=0;
  while (ScrSizX>=ScrSizM*4+15+ScrSizM div 4) do inc(ScrSizM);
  {o:=0;
  for i:=0 to 8 do if (1 shl i<=ScrSizM) then o:=i;
  ScrSizM:=1 shl o;}
  ScrSizM:=ScrSizM and $fffe;
  ScrSizB:=ScrSizX-ScrSizM+1;
  i:=ScrSizB-9-ScrSizM*3-ScrSizM div 4;
  ScrSizH:=10+i div 2;
  textColor($07);clrscr;
  textColor($1f);
  a:='pos=$00000000/$00000000    000,000,000,000/000,000,000,000';
  while (length(a)<ScrSizX) do a:=a+' ';
  GotoXY(1,1);Write(a);
  a:='f1=help';
  while (length(a)<ScrSizX-1) do a:=a+' ';
  GotoXY(1,ScrSizY);Write(a);
  RefreshScr:=$fe;
  end;
if (RefreshScr and $01<>0) then begin;
  textColor($07);
  a:='';while (length(a)<ScrSizX) do a:=a+' ';
  for i:=2 to ScrSizY-1 do begin;
    GotoXY(1,i);
    Write(a);
    end;
  RefreshScr:=RefreshScr or $40;
  end;
if (RefreshScr and $40<>0) then begin;
  q:=filScr;
  for o:=1 to ScrSizY-2 do begin;
     textColor($07);
     GotoXY(1,o+1);
     Write(conv2hex(q));
     for i:=0 to ScrSizM-1 do begin;
       if (q<selBeg) or (q>selEnd) then r:=$00 else r:=$80;
       if (q<0) or (q>=filSiz) then begin; a:='  ';w:=ord(' ') end else begin;
         w:=readCharacter(q);
         a:=byte2hextype(w);
         end;
       textColor(r+$02);
       GotoXY(ScrSizB+i,o+1);
       BugOS_WriteCustomChar(chr(w));
       textColor(r+$03);
       GotoXY(ScrSizH+i*3+i div 4,o+1);
       Write(a);
       inc(q);
       end;
     end;
  RefreshScr:=RefreshScr or $10;
  end;
if (RefreshScr and $20<>0) then begin;
  textColor($1f);
  GotoXY(6,1);Write(conv2hex(filPos));
  GotoXY(16,1);Write(conv2hex(filSiz));
  a:=alakit(filPos);
  while (length(a)<15) do a:=' '+a;
  GotoXY(28,1);Write(a);
  a:=alakit(filSiz);
  while (length(a)<15) do a:=a+' ';
  GotoXY(44,1);Write(a);
  RefreshScr:=RefreshScr or $10;
  end;
if (RefreshScr and $10<>0) then begin;
  p:=filPos-filScr;
  i:=p mod ScrSizM;
  if filBin then i:=ScrSizB+i else begin;
    i:=ScrSizH+i*3+i div 4;
    if filHex then inc(i);
    end;
  gotoXY(i,p div ScrSizM+2);
  end;
RefreshScr:=0;
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
addLin('f1     - help');
addLin('f5     - go to position');
addLin('f10    - leave the viewer');
addLin('alt+a  - place ascii character');
addLin('alt+t  - truncate file');
addLin('insert - insert some space');
addLin('delete - delete some space');
addLin('');
AddLin(' moving');
AddLin('~~~~~~~~');
AddLin('tab       - toggle hex/bin mode');
AddLin('up        - move up one entry');
AddLin('down      - move down one entry');
AddLin('home      - move to the beginning');
AddLin('end       - move to the ending');
AddLin('pgup      - move up some entries');
AddLin('pgdn      - move down some entries');
AddLin('ctrl+pgup - move up some entries');
AddLin('ctrl+pgdn - move down some entries');
AddLin('ctrl+home - move up more entries');
AddLin('ctrl+end  - move down more entries');
addLin('');
addLin(' selecting');
AddLin('~~~~~~~~~~~');
addLin('alt+b - mark beginning of selection');
addLin('alt+e - mark ending of selection');
addLin('alt+f - fill selected area');
addLin('alt+i - paste from clipboard');
addLin('alt+o - copy to clipboard');
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
