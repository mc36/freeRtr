Var
  ScrSizX,ScrSizY:Word;
  RefreshScr:byte; {1-prompt, 2-currcontent, 4-currpanel, 32-2contents,
    64-2panels, 128-screen}
  PromptString:String;

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

Procedure PutOutWindow(var xp,yp:word;xs,ys:word;a:string;c1,c2:Byte);
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
textColor(c2);
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
PutOutWindow(xp,yp,xs,ys,win,ColWinB,ColWinT);
GotoXY(xp,yp);
Write(txt);
win:=st;
if AskUserForText(st,xp,yp+1,xs) then begin;
  st:=win;
  exit;
  end;
AskForOneText:=False;
End;




Procedure PanelResizeAll;
Const max=250;
Var i,o:Word;
Begin;
ConsoleSize(ScrSizX,ScrSizY);
if (ScrSizX>max) then ScrSizX:=max;
if (ScrSizY>max) then ScrSizY:=max;
if CfgVert then begin;
  i:=ScrSizX div 2;
  PanelDat[1].ScrBX:=1;
  PanelDat[2].ScrBX:=i+1;
  for o:=1 to 2 do begin;
    PanelDat[o].ScrBY:=1;
    PanelDat[o].ScrSX:=i-2;
    PanelDat[o].ScrSY:=ScrSizY-3;
    end;
  end else begin;
  i:=(ScrSizY-1) div 2;
  PanelDat[1].ScrBY:=1;
  PanelDat[2].ScrBY:=i+1;
  for o:=1 to 2 do begin;
    PanelDat[o].ScrBX:=1;
    PanelDat[o].ScrSX:=scrSizX-2;
    PanelDat[o].ScrSY:=i-2;
    end;
  end;
if not CfgTwpn then begin;
  PanelDat[1].ScrBX:=1;
  PanelDat[1].ScrBY:=1;
  PanelDat[2].ScrBX:=1;
  PanelDat[2].ScrBY:=1;
  i:=ScrSizX-2;
  o:=ScrSizY-3;
  PanelDat[1].ScrSX:=i;
  PanelDat[1].ScrSY:=o;
  PanelDat[2].ScrSX:=i;
  PanelDat[2].ScrSY:=o;
  end;
for o:=1 to 2 do if (PanelDat[o].ScrSX<8) or (ScrSizY<5) then begin;
  WriteLn('too little display!');
  Halt(1);
  end;
for o:=1 to 2 do begin;
  i:=PanelDat[o].ScrSX div 4;
  if (i<4) then i:=4;
  PanelDat[o].ScrSS:=i;
  PanelDat[o].ScrSN:=PanelDat[o].ScrSX-i-1;
  end;
End;

Procedure PanelDrawOutOne(var d:OnePanelRecord);
Var
  i:Word;
  a:String;
Begin;
TextColor(ColPanB);
GotoXY(d.ScrBX,d.ScrBY);
Write('É'+dup(d.ScrSN,'Í')+'Ñ'+dup(d.ScrSS,'Í')+'»');
a:='º'+dup(d.ScrSN,' ')+'³'+dup(d.ScrSS,' ')+'º';
for i:=1 to d.ScrSY do begin;
  GotoXY(d.ScrBX,d.ScrBY+i);
  Write(a);
  end;
GotoXY(d.ScrBX,d.ScrBY+d.ScrSY+1);
Write('È'+dup(d.ScrSX,'Í')+'¼');
GotoXY(d.ScrBX,d.ScrBY+d.ScrSY-3);
Write('Ç'+dup(d.ScrSN,'Ä')+'Á'+dup(d.ScrSS,'Ä')+'¶');
a:=d.pat;
i:=d.ScrSX-4;
if (length(a)>i) then a:='®'+right(a,i-1);
i:=(d.ScrSX-length(a)) div 2;
GotoXY(d.ScrBX+i,d.ScrBY);
Write('µ');
TextColor(ColPanD);
Write(a);
TextColor(ColPanB);
Write('Æ');
End;

Procedure PanelPutOutLine(var d:OnePanelRecord;n:LongInt);
Var
  i:Word;
  o:LongInt;
  bug,lnd:boolean;
  dd:OneFileRecord;
  a:String;
Begin;
GotoXY(d.ScrBX+1,d.ScrBY+n-d.beg);
bug:=(n<1) or (n>d.max);
if bug then begin;
  a:='';
  i:=ColPanB;
  end else begin;
  dd:=entriesBuf^[d.mem+n-1];
  a:=dd.nam;
  if (dd.flg and xRights_Directory<>0) then begin;
    a:=nagy(a);
    i:=ColPanD;
    end else begin;
    a:=kicsi(a);
    i:=ColPanF;
    if (dd.flg and (xRights_OwnExec or xRights_AnyExec)<>0) then i:=ColPanE;
    end;
  if dd.sel then i:=ColPanS;
  end;
if (d.cur=n) then i:=ColPanC;
while (length(a)<d.ScrSN) do a:=a+' ';
lnd:=(length(a)>d.ScrSN);
a:=copy(a,1,d.ScrSN);
TextColor(i);
Write(a);
TextColor(ColPanB);
if lnd then a:='¯' else a:='³';
Write(a);
TextColor(i);
if bug then a:='' else begin;
  o:=dd.siz;
  a:=BStr(o);
  if (length(a)>d.ScrSS) then begin;
    o:=o div 1024;
    a:=BStr(o)+'k';
    end;
  if (length(a)>d.ScrSS) then begin;
    o:=o div 1024;
    a:=BStr(o)+'m';
    end;
  end;
while (length(a)<d.ScrSS) do a:=' '+a;
a:=copy(a,1,d.ScrSS);
Write(a);
End;

Procedure PanelPutOutWhole(var d:OnePanelRecord);
Var i,o:LongInt;
Begin;
o:=d.beg;
for i:=1 to d.ScrSY-4 do begin;
  inc(o);
  PanelPutOutLine(d,o);
  end;
End;

Procedure PanelPutOutCurrnt(var d:OnePanelRecord);
Var
  dd:OneFileRecord;
  i,o,p:LongInt;
  a:String;
Begin;
TextColor(ColPanB);
i:=abs(d.cur);
if (i<1) or (i>d.max) then a:='' else begin;
  dd:=entriesBuf^[d.mem+i-1];
  a:=dd.nam;
  end;
while (length(a)<d.ScrSX) do a:=a+' ';
a:=copy(a,1,d.ScrSX);
GotoXY(d.ScrBX+1,d.ScrBY+d.ScrSY-2);Write(a);
a:=ConvertFileMode2string(dd.flg)+'  '+Alakit(dd.siz);
while (length(a)<d.ScrSX) do a:=a+' ';
a:=copy(a,1,d.ScrSX);
GotoXY(d.ScrBX+1,d.ScrBY+d.ScrSY-1);Write(a);
PanelCountSelected(d,i,o,p,true,true);
inc(o,p);
if (o=0) then begin;
  PanelCountSelected(d,i,o,p,false,false);
  inc(o,p);
  a:='total: '+alakit(i)+' bytes, '+bstr(o)+' entries';
  end else begin;
  a:=alakit(i)+' bytes, '+bstr(o)+' entries selected';
  end;
while (length(a)<d.ScrSX) do a:=a+' ';
a:=copy(a,1,d.ScrSX);
GotoXY(d.ScrBX+1,d.ScrBY+d.ScrSY-0);Write(a);
End;

Procedure PutOutPrompt;
Var
  a:string;
  i:Word;
Begin;
TextColor(ColNorm);
GotoXY(1,ScrSizY);
i:=ScrSizX-2;
a:=PromptString;
if (length(a)>i) then a:='®'+copy(a,length(a)-i+2,255);
while (length(a)<i) do a:=a+' ';
Write('>'+a);
End;

Procedure PutOutCursor;
Var i:Word;
Begin;
i:=length(PromptString)+2;
if (i>ScrSizX) then i:=ScrSizX;
GotoXY(i,ScrSizY);
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
addLin('f2     - compare');
addLin('f3     - view current file');
addLin('f4     - edit current file');
addLin('f5     - copy');
addLin('f6     - rename');
addLin('f7     - create directory');
addLin('f8     - erase');
addLin('f9     - user menu');
addLin('f10    - leave the commander');
addLin('alt+f1 - change disk in left panel');
addLin('alt+f2 - change disk in right panel');
addLin('alt+f5 - make link to current entry');
addLin('alt+f6 - rename current entry');
addLin('alt+r  - reread the current panel');
addLin('alt+f  - information about current entry');
addLin('alt+d  - information about current disk');
addLin('alt+a  - set rights of current entry');
addLin('');
AddLin(' moving');
AddLin('~~~~~~~~');
AddLin('tab       - change between panels');
AddLin('up        - move up one entry');
AddLin('down      - move down one entry');
AddLin('home      - move to the first entry');
AddLin('end       - move to the last entry');
AddLin('pgup      - move up some entries');
AddLin('pgdn      - move down some entries');
AddLin('ctrl+pgup - change to parent directory');
AddLin('ctrl+pgdn - enter the current directory');
AddLin('ctrl+\    - change to root directory');
addLin('');
addLin(' selecting');
AddLin('~~~~~~~~~~~');
addLin('insert - select current entry');
addLin('*      - invert selectionion (on files)');
addLin('ctrl+* - invert selectionion (on all entries)');
addLin('+      - select group (by mask)');
addLin('-      - unselect group (by mask)');
addLin('');
addLin(' prompt');
AddLin('~~~~~~~~');
addLin('enter            - execute current command/prompt');
addLin('ctrl+enter       - place current entry name on prompt');
addLin('shift+ctrl+enter - place current entry pathname on prompt');
addLin('ctrl+[           - place left panel path on prompt');
addLin('ctrl+]           - place right panel path on prompt');
addLin('keys             - type into the prompt line');
addLin('backspace        - clear last character from prompt');
addLin('escape           - clear the whole prompt line');
xs:=ScrSizX-12;
ys:=ScrSizY-7;
if (ys>PosSiz) then ys:=PosSiz;
if (xs>200) then xs:=200;
PutOutWindow(xp,yp,xs,ys,'help',ColWinB,ColWinT);
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
PutOutWindow(xp,yp,xs,ys,'error',ColErrB,ColErrT);
GotoXY(xp,yp+0);Write('error happened while tried to do the following:');
GotoXY(xp,yp+1);Write(act);
GotoXY(xp,yp+2);Write('the error was the following:');
GotoXY(xp,yp+3);Write(xGetErrorName(err));
GotoXY(xp,yp+4);Write('press any key to continue!');
ReadChrKey;
ShowErrorMessage:=True;
End;
