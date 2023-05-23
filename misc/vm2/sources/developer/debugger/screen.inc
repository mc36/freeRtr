Const
  RegNameSize=3;
Var
  ScrSizX,ScrSizY:Word;
  RefreshScr:byte; {1=cursor, 32=content, 64=panel, 128=screen}
  windowNum:LongInt; {0=mixed, 1=inst, 2=reg, 3=flg, 4=mem, 5=stk}
  mixedPos:LongInt; {1=inst, 2=reg, 3=flg, 4=mem, 5=stk}
  instBeg,instCur:LongInt;
  regsBeg,regsCur:LongInt;
  flgsBeg,flgsCur:LongInt;
  dumpBeg,dumpCur,dumpAct:LongInt;
  stckBeg,stckCur:LongInt;


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

Procedure ShowErrorMessage(act,err:String);
Var xp,yp,xs,ys:Word;
Begin;
xs:=ScrSizX-8;
ys:=2;
PutOutWindow(xp,yp,xs,ys,'error',ColErrB,ColErrT);
GotoXY(xp,yp+0);Write(act);
GotoXY(xp,yp+1);Write(err);
ReadKey;
End;



Procedure PanelResizeAll;
Const max=250;
Var i,o:Word;
Begin;
ConsoleSize(ScrSizX,ScrSizY);
dec(ScrSizX);
if (ScrSizX<64) or (ScrSizY<16) then begin;
  WriteLn('too little display!');
  Halt(1);
  end;
if (ScrSizX>max) then ScrSizX:=max;
if (ScrSizY>max) then ScrSizY:=max;
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
addLin(' general commands');
AddLin('~~~~~~~~~~~~~~~~~~');
addLin('f1        - help');
addLin('alt+1     - mixed page');
addLin('alt+2     - instructions page');
addLin('alt+3     - registers page');
addLin('alt+4     - flags page');
addLin('alt+5     - memory dump page');
addLin('alt+6     - stack dump page');
addLin('f4        - run to cursor');
addLin('f7        - run next instruction');
addLin('f8        - run to next instruction');
addLin('f9        - run until abort');
addLin('f10       - leave the debugger');
AddLin('');
AddLin(' moving');
AddLin('~~~~~~~~');
AddLin('up        - move up one entry');
AddLin('down      - move down one entry');
AddLin('left      - move left one entry');
AddLin('right     - move right one entry');
AddLin('pgup      - move up some entries');
AddLin('pgdn      - move down some entries');
AddLin('ctrl+pgup - move up more entries');
AddLin('ctrl+pgdn - move down more entries');
AddLin('home      - move up more entries');
AddLin('end       - move down more entries');
AddLin('ctrl+home - move to first screen entry');
AddLin('ctrl+end  - move to last screen entry');
AddLin('');
AddLin(' mixed');
AddLin('~~~~~~~~');
AddLin('tab       - move inside mixed panel');
AddLin('shift+tab - move inside mixed panel');
AddLin('');
AddLin(' instructions');
AddLin('~~~~~~~~~~~~~~');
AddLin('alt+o - move to current');
AddLin('alt+n - set current');
AddLin('alt+g - goto');
AddLin('alt+a - insert opcode');
AddLin('alt+d - disasm code');
AddLin('');
AddLin(' registers');
AddLin('~~~~~~~~~~~');
AddLin('+       - increment');
AddLin('-       - decrement');
AddLin('shift++ - increment more');
AddLin('shift+- - decrement more');
AddLin('ctrl++  - increment more');
AddLin('ctrl+-  - decrement more');
AddLin('alt++   - increment more');
AddLin('alt+-   - decrement more');
AddLin('alt+c   - change value');
AddLin('');
AddLin(' flags');
AddLin('~~~~~~~');
AddLin('+         - increment');
AddLin('-         - decrement');
AddLin('space     - toggle lower bit');
AddLin('0..9,a..f - change');
AddLin('');
AddLin(' dump');
AddLin('~~~~~~');
AddLin('alt+g - goto');
AddLin('alt+t - toggle hex/bin');
AddLin('');
AddLin(' stack');
AddLin('~~~~~~~');
AddLin('alt+o - move to current');
AddLin('alt+n - set current');
AddLin('alt+g - goto');
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



Procedure DisplayMixedPanel;
Var
  i:LongInt;
  a:String;
Begin;
TextColor(ColPanB);
gotoXY(1,1);
Write('É'+dup(ScrSizX-2*RegNameSize-13,'Í')+'Ñ'+dup(RegNameSize+9,'Í')+'Ñ'+dup(RegNameSize,'Í')+'»');
a:='º'+dup(ScrSizX-2*RegNameSize-13,' ')+'³'+dup(RegNameSize+9,' ')+'³'+dup(RegNameSize,' ')+'º';
for i:=2 to ScrSizY-6 do begin;
  gotoXY(1,i);
  Write(a);
  end;
gotoXY(1,ScrSizY-5);
Write('Ç'+dup(ScrSizX-2*RegNameSize-13,'Ä')+'Å'+dup(RegNameSize+9,'Ä')+'Á'+dup(RegNameSize,'Ä')+'¶');
a:='º'+dup(ScrSizX-2*RegNameSize-13,' ')+'³'+dup(RegNameSize*2+10,' ')+'º';
for i:=ScrSizY-4 to ScrSizY-1 do begin;
  gotoXY(1,i);
  Write(a);
  end;
gotoXY(1,ScrSizY);
Write('È'+dup(ScrSizX-2*RegNameSize-13,'Í')+'Ï'+dup(RegNameSize*2+10,'Í')+'¼');
End;

Procedure DisplaySimplePanel(noAddr:Boolean);
Var
  i:LongInt;
  a:String;
Begin;
TextColor(ColPanB);
gotoXY(1,1);
Write('É'+dup(ScrSizX-2,'Í')+'»');
a:='º'+dup(ScrSizX-2,' ')+'º';
for i:=2 to ScrSizY-1 do begin;
  gotoXY(1,i);
  Write(a);
  end;
gotoXY(1,ScrSizY);
Write('È'+dup(ScrSizX-2,'Í')+'¼');
if noAddr then exit;
gotoXY(5,ScrSizY);
Write('µ        Æ');
End;






Procedure FillMixedPanel;
Var
  buf:array[0..1024] of byte;
  d:OneIndexRecord;
  i,o,p,q,r,s:Longint;
  a:String;
Begin;
{instructions}
q:=ScrSizX-2*RegNameSize-13;
r:=registerDat.d[instrRegIdx].val;
p:=instBeg-1;
for i:=2 to ScrSizY-6 do begin;
  if (p>=0) and (p<indxMax) then begin;
    xSeek(indxFil,p*sizeof(d));
    xBlockRead(indxFil,d,sizeof(d));
    xtSetPos(listFil,d.p);
    a:=xtReadLn(listFil,255);
    end else begin; fillchar(d,sizeof(d),0);a:=''; end;
  a:=copy(a+dup(q,' '),1,q);
  inc(p);
  gotoXY(2,i);
  o:=ColPanN;
  if (r=d.o) then o:=ColPanI;
  if (mixedPos=1) and (instCur=p) then o:=ColPanS;
  TextColor(o);
  Write(a);
  end;
{registers}
p:=regsBeg;
q:=ScrSizX-2*RegNameSize-10;
for i:=2 to ScrSizY-6 do begin;
  o:=ColPanN;
  if (p>0) and (p<=registerDat.n) then begin;
    if (registerDat.d[p].val<>registerOld.d[p].val) then o:=ColPanI;
    if (mixedPos=2) and (regsCur=p) then o:=ColPanS;
    a:=copy(registerDat.d[p].nam+dup(RegNameSize,' '),1,RegNameSize)+' '+toHex(registerDat.d[p].val,registerDat.d[p].siz);
    end else a:='';
  a:=copy(a+dup(RegNameSize+16,' '),1,RegNameSize+9);
  gotoXY(q,i);
  TextColor(o);
  Write(a);
  inc(p);
  end;
{flags}
p:=flgsBeg;
q:=ScrSizX-RegNameSize;
for i:=2 to ScrSizY-6 do begin;
  o:=ColPanN;
  if (p>0) and (p<=flagsRegDat.n) then begin;
    if (flagsRegDat.d[p].val<>0) then o:=ColPanI;
    if (mixedPos=3) and (flgsCur=p) then o:=ColPanS;
    a:=flagsRegDat.d[p].nam;
    end else a:='';
  a:=copy(a+dup(RegNameSize,' '),1,RegNameSize);
  gotoXY(q,i);
  TextColor(o);
  Write(a);
  inc(p);
  end;
{memory}
fillchar(buf,sizeof(buf),0);
debug_readMem(dumpBeg,sizeof(buf),buf);
p:=dumpBeg;
q:=ScrSizY-4;
for i:=0 to 4*8-1 do begin;
  o:=ColPanN;
  if (mixedPos=4) and (dumpCur=p) then o:=ColPanS;
  TextColor(o);
  o:=buf[i];
  gotoXY((i and 7)*3+((i shr 2) and 1)+10,(i shr 3)+q);
  write(byte2hextype(o));
  gotoXY((i and 7)+36,(i shr 3)+q);
  BugOS_WriteCustomChar(chr(o));
  inc(p);
  end;
p:=dumpBeg;
TextColor(ColPanN);
for i:=0 to 3 do begin;
  gotoXY(2,q+i);
  Write(toHex(p,3));
  inc(p,8);
  end;
{stack}
r:=registerDat.d[stackRegIdx].siz;
s:=registerDat.d[stackRegIdx].val;
fillchar(buf,sizeof(buf),0);
debug_readMem(stckBeg*r,sizeof(buf),buf);
q:=0;
p:=stckBeg*r;
for i:=0 to 3 do begin;
  a:=toHex(p,3)+' '+toHex(getNextVal(buf,q,r),r);
  o:=ColPanN;
  if (s=p) then o:=ColPanI;
  if (mixedPos=5) and (stckCur=stckBeg+i) then o:=ColPanS;
  TextColor(o);
  gotoXY(ScrSizX-2*RegNameSize-10,ScrSizY+i-4);
  a:=copy(a+dup(RegNameSize+16,' '),1,RegNameSize+13);
  Write(a);
  inc(p,r);
  end;
End;



Procedure FillInstructions;
Var
  d:OneIndexRecord;
  i,o,p,r:LongInt;
  a:String;
Begin;
p:=instBeg-1;
r:=registerDat.d[instrRegIdx].val;
for i:=2 to ScrSizY-1 do begin;
  if (p>=0) and (p<indxMax) then begin;
    xSeek(indxFil,p*sizeof(d));
    xBlockRead(indxFil,d,sizeof(d));
    xtSetPos(listFil,d.p);
    a:=xtReadLn(listFil,255);
    end else begin; fillchar(d,sizeof(d),0);a:=''; end;
  a:=copy(a+dup(ScrSizX,' '),1,ScrSizX-2);
  inc(p);
  gotoXY(2,i);
  o:=ColPanN;
  if (r=d.o) then o:=ColPanI;
  if (instCur=p) then o:=ColPanS;
  TextColor(o);
  Write(a);
  end;
xSeek(indxFil,(instCur-1)*sizeof(d));
xBlockRead(indxFil,d,sizeof(d));
gotoXY(6,ScrSizY);
TextColor(ColPanN);
Write(toHex(d.o,4));
End;



Procedure FillRegisters;
Var
  i,o,p:LongInt;
  a:String;
Begin;
p:=regsBeg;
for i:=2 to ScrSizY-1 do begin;
  o:=ColPanN;
  if (p>0) and (p<=registerDat.n) then begin;
    if (registerDat.d[p].val<>registerOld.d[p].val) then o:=ColPanI;
    if (regsCur=p) then o:=ColPanS;
    a:=copy(registerDat.d[p].nam+dup(ScrSizX,' '),1,ScrSizX shr 1)+' '+toHex(registerDat.d[p].val,registerDat.d[p].siz);
    end else a:='';
  a:=copy(a+dup(ScrSizX,' '),1,ScrSizX-2);
  gotoXY(2,i);
  TextColor(o);
  Write(a);
  inc(p);
  end;
End;



Procedure FillFlagsPanel;
Var
  i,o,p:LongInt;
  a:String;
Begin;
p:=flgsBeg;
for i:=2 to ScrSizY-1 do begin;
  o:=ColPanN;
  if (p>0) and (p<=flagsRegDat.n) then begin;
    if (flagsRegDat.d[p].val<>0) then o:=ColPanI;
    if (flgsCur=p) then o:=ColPanS;
    a:=copy(flagsRegDat.d[p].nam+dup(ScrSizX,' '),1,ScrSizX shr 1)+' '+toHex(flagsRegDat.d[p].val,4);
    end else a:='';
  a:=copy(a+dup(ScrSizX,' '),1,ScrSizX-2);
  gotoXY(2,i);
  TextColor(o);
  Write(a);
  inc(p);
  end;
End;



Procedure FillDumpPanel;
Var
  buf:array[0..1024] of byte;
  i,o,p:LongInt;
  a:String;
Begin;
fillchar(buf,sizeof(buf),0);
debug_readMem(dumpBeg,sizeof(buf),buf);
p:=dumpBeg;
for i:=0 to (ScrSizY-2)*8-1 do begin;
  o:=ColPanN;
  if (dumpCur=p) then o:=ColPanS;
  TextColor(o);
  o:=buf[i];
  gotoXY((i and 7)*3+((i shr 2) and 1)+13,(i shr 3)+2);
  write(byte2hextype(o));
  gotoXY((i and 7)+39,(i shr 3)+2);
  BugOS_WriteCustomChar(chr(o));
  inc(p);
  end;
p:=dumpBeg;
TextColor(ColPanN);
for i:=2 to ScrSizY-1 do begin;
  gotoXY(2,i);
  Write(toHex(p,4));
  inc(p,8);
  end;
gotoXY(6,ScrSizY);
Write(toHex(dumpCur,4));
End;



Procedure FillStackPanel;
Var
  buf:array[1..1024] of byte;
  i,o,p,q,r,s:LongInt;
  a:String;
Begin;
r:=registerDat.d[stackRegIdx].siz;
s:=registerDat.d[stackRegIdx].val;
fillchar(buf,sizeof(buf),0);
debug_readMem(stckBeg*r,sizeof(buf),buf);
q:=0;
p:=stckBeg*r;
for i:=0 to ScrSizY-3 do begin;
  a:=toHex(p,4)+' '+toHex(getNextVal(buf,q,r),r);
  o:=ColPanN;
  if (s=p) then o:=ColPanI;
  if (stckCur=stckBeg+i) then o:=ColPanS;
  TextColor(o);
  gotoXY(2,i+2);
  a:=copy(a+dup(ScrSizX,' '),1,ScrSizX-2);
  Write(a);
  inc(p,r);
  end;
TextColor(ColPanN);
gotoXY(6,ScrSizY);
Write(toHex(stckCur,4));
End;





Procedure CursorMixedPanel;
Var
  i,o:LongInt;
Begin;
case mixedPos of
  1:gotoXY(2,instCur-instBeg+2);
  2:gotoXY(ScrSizX-2*RegNameSize-10,regsCur-regsBeg+2);
  3:gotoXY(ScrSizX-RegNameSize,flgsCur-flgsBeg+2);
  4:begin;
    i:=dumpCur-dumpBeg;
    o:=i and 7;
    i:=(i shr 3)+ScrSizY-4;
    if (dumpAct=2) then gotoXY(o+36,i) else gotoXY(o*3+((o shr 2) and 1)+dumpAct+10,i);
    end;
  5:gotoXY(ScrSizX-2*RegNameSize-10,ScrSizY+stckCur-stckBeg-4);
  end;
End;

Procedure CursorInstructions;
Begin;
gotoXY(2,instCur-instBeg+2);
End;

Procedure CursorRegisters;
Begin;
gotoXY(2,regsCur-regsBeg+2);
End;

Procedure CursorFlagsPanel;
Begin;
gotoXY(2,flgsCur-flgsBeg+2);
End;

Procedure CursorDumpPanel;
Var i,o:LongInt;
Begin;
i:=dumpCur-dumpBeg;
o:=i and 7;
i:=(i shr 3)+2;
if (dumpAct=2) then gotoXY(o+39,i) else gotoXY(o*3+((o shr 2) and 1)+dumpAct+13,i);
End;

Procedure CursorStackPanel;
Begin;
gotoXY(2,stckCur-stckBeg+2);
End;
