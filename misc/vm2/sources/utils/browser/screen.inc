Type characterSetRecord=array[0..255] of byte;
Const charSetAnsiDos:characterSetRecord=(0,1,2,3,4,5,6,7,8,9,10,11,
  12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
  30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,
  48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,
  66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,
  84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,
  102,103,104,105,106,107,108,109,110,111,112,113,114,115,
  116,117,118,119,120,121,122,123,124,125,126,127,128,129,
  130,131,132,133,134,135,136,137,138,139,140,141,142,143,
  144,145,146,147,148,149,150,151,152,153,154,155,156,157,
  158,159,160,161,162,163,164,165,166,167,168,169,170,171,
  172,173,174,175,176,177,178,179,180,181,182,183,184,185,
  186,187,188,189,190,191,192,193,194,195,196,197,198,199,
  200,201,202,203,204,205,206,207,208,209,210,211,212,213,
  214,215,216,217,218,219,220,221,222,223,224,225,226,227,
  228,229,230,231,232,233,234,235,236,237,238,239,240,241,
  242,243,244,245,246,247,248,249,250,251,252,253,254,255);
Const charSetIso8859_1:characterSetRecord=(0,1,2,3,4,5,6,7,8,9,10,11,
  12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
  30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,
  48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,
  66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,
  84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,
  102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,
  117,118,119,120,121,122,123,124,125,126,127,46,46,46,46,
  46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,
  46,46,46,46,46,46,46,46,46,46,46,33,155,156,46,46,46,
  21,46,46,46,174,46,45,46,46,248,241,253,46,46,230,227,
  249,46,46,46,175,172,171,46,168,46,143,46,46,142,143,46,
  46,46,144,46,46,46,141,46,46,46,46,46,149,46,167,153,
  120,46,46,151,152,154,46,46,46,46,160,131,46,132,134,46,
  46,46,130,136,137,46,161,140,139,46,46,46,162,147,147,
  148,246,149,46,163,150,129,46,46,152);
Const charSetIso8859_2:characterSetRecord=(0,1,2,3,4,5,6,7,8,9,10,11,
  12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
  30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,
  48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,
  66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,
  84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,
  102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,
  117,118,119,120,121,122,123,124,125,126,127,46,46,46,46,
  46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,
  46,46,46,46,46,46,46,46,46,46,46,46,46,157,46,46,151,
  21,46,230,184,155,141,240,166,189,248,46,46,136,239,46,
  152,46,46,231,173,46,171,241,167,190,232,181,182,198,142,
  149,143,128,172,144,46,211,183,214,215,210,209,227,213,
  224,226,138,153,120,252,222,233,235,154,237,221,46,234,
  160,131,46,132,150,134,135,159,130,46,137,216,161,140,46,
  208,228,229,162,147,139,148,246,253,133,163,251,129,236,
  238,248);
Var
  currCharSet:characterSetRecord;

Procedure convCharSetStr(var a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
for i:=1 to ab0 do begin;
  o:=currCharSet[ab[i]];
  if (o in [0..31,255]) then o:=46;
  ab[i]:=o;
  end;
End;

Procedure convCharSetFile(a:String);
Var
  f:xFile;
  buf:array[1..1024] of byte;
  p,s,i,o:LongInt;
Begin;
if (xOpen(f,a,xGenFilMod_rw)<>0) then immErr('error opening file!');
s:=xFileSize(f);
p:=0;
while (p<s) do begin;
  o:=s-p;
  if (o>sizeof(buf)) then o:=sizeof(buf);
  xSeek(f,p);xBlockRead(f,buf,o);
  for i:=1 to o do buf[i]:=currCharSet[buf[i]];
  xSeek(f,p);xBlockWrite(f,buf,o);
  inc(p,o);
  end;
xClose(f);
End;

Procedure rotateCharacterSet(var d:characterSetRecord);
Var
  dd:characterSetRecord;
  i:LongInt;
Begin;
for i:=0 to 255 do dd[i]:=32;
for i:=255 downto 0 do dd[d[i]]:=i;
d:=dd;
End;



Procedure PutOutWindow(var xp,yp:word;xs,ys:word;a:string);
Var i:Word;
Begin;
xp:=(screenSizX-xs-4) div 2;
yp:=(screenSizY-ys+0) div 2;
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
  $8001:exit;
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
xs:=screenSizX-12;
ys:=2;
textColor($71);
PutOutWindow(xp,yp,xs,ys,win);
GotoXY(xp,yp);
textColor($70);
Write(tit);
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
addLin('f10      - leave the browser');
addLin('f7       - find in document');
addLin('shift+f7 - find again');
addLin('alt+f7   - reversed search');
addLin('alt+g    - change current url');
addLin('ctrl+g   - enter new url');
addLin('alt+e    - change encoding');
addLin('alt+d    - download current link');
addLin('alt+r    - reload current document');
addLin('alt+u    - change authentication');
addLin('alt+p    - change proxy server');
addLin('enter    - load link/change entry');
addLin('alt+a    - bookmark current page');
addLin('alt+h    - view history');
addLin('alt+b    - view bookmarks');
addLin('');
AddLin(' moving');
AddLin('~~~~~~~~');
AddLin('up         - move up one line');
AddLin('down       - move down one line');
AddLin('left       - select previous entry');
AddLin('right      - select next entry');
AddLin('home       - move beginning of document');
AddLin('end        - move ending of document');
AddLin('pgup       - move up one screen');
AddLin('pgdn       - move down one screen');
AddLin('shift+pgup - move up some lines');
AddLin('shift+pgdn - move down some lines');
AddLin('ctrl+pgup  - move up more lines');
AddLin('ctrl+pgdn  - move down more lines');
AddLin('alt+pgup   - move up much more lines');
AddLin('alt+pgdn   - move down much more lines');
xs:=screenSizX-12;
ys:=screenSizY-6;
if (ys>PosSiz) then ys:=PosSiz;
if (xs>200) then xs:=200;
textColor($71);
PutOutWindow(xp,yp,xs,ys,'help');
textColor($70);
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
  $8001:exit;
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





Procedure decodeCurrentLine;
Label f1;
Var
  col,ps,ops:LongInt;
  a,b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;

procedure put(a:string;c:longint);
var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
begin;
for i:=1 to ab0 do begin;
  inc(LineListNum);
  LineListDat[LineListNum].c:=ab[i];
  LineListDat[LineListNum].a:=c;
  end;
end;

procedure lnk(siz:LongInt);
var d:oneLinkRecord;
begin;
if (LinkListNum>=LinkListMax) then exit;
if (siz<1) then exit;
d.beg:=LineListNum+1;
d.siz:=siz-1;
d.ps:=ops;
inc(LinkListNum);
LinkListDat[LinkListNum]:=d;
end;

Begin;
col:=$07;
ps:=0;
LineListNum:=0;
LinkListNum:=0;
f1:
ops:=ps;
a:=outputGetTag(ps);
if (ab0=0) then exit;
case ab[1] of
  1:begin;
    col:=ab[2];
    goto f1;
    end;
  2:begin;
    put(copy(a,2,666),col);
    goto f1;
    end;
  $83:begin;
    lnk(ab0-5);
    put(copy(a,6,666),10);
    goto f1;
    end;
  $8b,$8c:begin;
    lnk(ab0-5);
    move(a[2],i,sizeof(i));
    b:=outputGetDat(i+sizeof(a));
    if (b[1]=#0) then i:=32 else i:=42;
    ab[7]:=i;
    put(copy(a,6,666),11);
    goto f1;
    end;
  $84..$8a,$8d..$90:begin;
    lnk(ab0-5);
    put(copy(a,6,666),11);
    goto f1;
    end;
  end;
writeln('bug!');
goto f1;
End;

Function getCurrentLineAscii:String;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
ab0:=0;
for i:=1 to LineListNum do begin;
  inc(ab0);
  ab[ab0]:=LineListDat[i].c;
  end;
getCurrentLineAscii:=a;
End;


Procedure selectOneEntry(i:LongInt);
Var d:oneLinkRecord;
Begin;
d:=LinkListDat[i];
for i:=d.beg to d.beg+d.siz do inc(LineListDat[i].a,$10);
End;



Procedure displayDecodedLine;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,c:LongInt;
Begin;
c:=LineListDat[1].a;
TextColor(c);
ab0:=0;
for i:=1 to LineListNum do begin;
  if (LineListDat[i].a<>c) then begin;
    convCharSetStr(a);
    write(a);
    ab0:=0;
    c:=LineListDat[i].a;
    TextColor(c);
    end;
  inc(ab0);
  ab[ab0]:=LineListDat[i].c;
  end;
convCharSetStr(a);
if (ab0>0) then write(a);
ab0:=screenSizX-LineListNum;
if (ab0<1) then exit;
fillchar(a[1],ab0,32);
TextColor(7);
write(a);
End;
