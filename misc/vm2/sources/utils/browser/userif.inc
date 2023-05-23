FUNCTION doUserInterface:Boolean;
Var
  lineBeg:LongInt;
  slctLin:LongInt;
  slctNum:LongInt;
  ob,ol,on:LongInt;



Procedure rangeCheck;
Var i:LongInt;
Begin;
i:=outputLin-screenSizY+2;
if (lineBeg>i) then lineBeg:=i;
if (lineBeg<0) then lineBeg:=0;
if (slctLin<lineBeg) or (slctLin>=lineBeg+screenSizY-2) then begin;
  slctLin:=-1;
  slctNum:=0;
  end;
End;



Procedure repaintLn(i:LongInt);
Begin;
gotoXY(1,i-lineBeg+2);
outputReadBuf(i);
decodeCurrentLine;
if (i=slctLin) then selectOneEntry(slctNum);
displayDecodedLine;
End;



Procedure repaintSt;
Label f1;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
a:='';
if (slctNum<1) then goto f1;
outputReadBuf(slctLin);
decodeCurrentLine;
i:=LinkListDat[slctNum].ps;
a:=outputGetTag(i);
move(a[2],i,sizeof(i));
case ab[1] of
  $83:a:=outputGetDat(i);
  $84:a:=copy(outputGetDat(i),3,666);
  $86,$87,$88:a:=outputGetDat(i);
  $89,$8a:a:=outputGetDat(i);
  $8b,$8c:a:=outputGetDat(i);
  $8d:a:=copy(outputGetDat(i),2,666);
  $90:a:=outputGetDat(i);
  else a:='???';
  end;
f1:
convCharSetStr(a);
fillUpString(a,screenSizX-1);
gotoXY(1,screenSizY);
TextColor($1f);
write(a);
End;



Procedure findStopPrev;
Label f1;
Begin;
rangeCheck;
if (slctLin<0) then begin;
  slctLin:=lineBeg+screenSizY-3;
  slctNum:=LinkListMax+1;
  end;
f1:
rangeCheck;
if (slctLin<0) then exit;
outputReadBuf(slctLin);
decodeCurrentLine;
if (slctNum>LinkListNum) then slctNum:=LinkListNum+1;
dec(slctNum);
if (slctNum<1) then begin;
  slctNum:=LinkListMax+1;
  dec(slctLin);
  goto f1;
  end;
End;



Procedure findStopNext;
Label f1;
Begin;
rangeCheck;
if (slctLin<0) then begin;
  slctLin:=lineBeg;
  slctNum:=0;
  end;
f1:
rangeCheck;
if (slctLin<0) then exit;
outputReadBuf(slctLin);
decodeCurrentLine;
inc(slctNum);
if (slctNum>LinkListNum) then begin;
  slctNum:=0;
  inc(slctLin);
  goto f1;
  end;
End;



Procedure doSelecterRound;
Label f1,f2,f3,f4,f5;
Var
  f:xfile;
  a,b:String;
  i,o,p,q,r:LongInt;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  xp,yp,xs,ys:Word;
  max,cur:LongInt;
Begin;
a:=dataPath+TemporarySlct;
xErase(a);
xCreate(a);
if (xOpen(f,a,xGenFilMod_rw)<>0) then immErr('error opening file!');
p:=slctLin;
outputReadBuf(p);
decodeCurrentLine;
q:=LinkListDat[slctNum].ps;
a:=outputGetTag(q);
move(ab[2],i,sizeof(i));
b:=copy(outputGetDat(i),2,255);
cur:=1;
max:=0;
f1:
if (p>outputLin) then goto f3;
outputReadBuf(p);
f2:
a:=outputGetTag(q);
if (ab0=0) then begin;
  q:=0;
  inc(p);
  goto f1;
  end;
if (ab[1] in [$8d,$8e,$85]) then goto f3;
if (ab[1]<>$8f) then goto f2;
move(ab[2],i,sizeof(i));
inc(i,sizeof(a));
a:=outputGetDat(i);
a:='1234'+a;
move(i,ab[1],sizeof(i));
xBlockWrite(f,a,sizeof(a));
inc(max);
if (ab[5]=1) then cur:=max;
goto f2;
f3:
if (max<1) then begin; xClose(f);exit; end;
xs:=screenSizX-12;
ys:=screenSizY-6;
if (ys>200) then ys:=200;
if (xs>200) then xs:=200;
if (ys and 1=0) then dec(ys);
textColor($71);
PutOutWindow(xp,yp,xs,ys,'select for '+b);
gotoXY(xp-1,yp+(ys shr 1));Write('>');
gotoXY(xp+xs,yp+(ys shr 1));Write('<');
textColor($70);
f4:
o:=cur-(ys shr 1)-1;
for r:=0 to ys-1 do begin;
  gotoXY(xp,yp+r);
  if (o>=0) and (o<max) then begin;
    xSeek(f,o*sizeof(a));
    xBlockRead(f,a,sizeof(a));
    if (ab[5] and 1<>0) then i:=42 else i:=32;
    a:=copy(a,4,255);
    ab[1]:=i;
    ab[2]:=32;
    end else ab0:=0;
  convCharSetStr(a);
  fillUpString(a,xs);
  write(a);
  inc(o);
  end;
gotoXY(xp,yp+(ys shr 1));
p:=cur;
f5:
i:=readkey;
case i of
  $8004,$8005:begin; {enter, escape}
    xClose(f);
    exit;
    end;
  $0020:begin; {space}
    xSeek(f,(cur-1)*sizeof(a));
    xBlockRead(f,a,sizeof(a));
    if (ab[5]>1) then goto f5;
    ab[5]:=(ab[5]+1) and 1;
    xSeek(f,(cur-1)*sizeof(a));
    xBlockWrite(f,a,sizeof(a));
    move(ab[1],i,sizeof(i));
    a:=outputGetDat(i);
    ab[1]:=(ab[1]+1) and 1;
    outputPutDat(i,a);
    goto f4;
    end;
  $800c:dec(cur); {up}
  $800d:inc(cur); {down}
  $8008:cur:=0; {hone}
  $8009:cur:=max; {end}
  $800a:dec(cur,ys shr 1); {pgup}
  $800b:inc(cur,ys shr 1); {pgdn}
  $810a:dec(lineBeg,ys); {shift+pgup}
  $810b:inc(lineBeg,ys); {shift+pgdn}
  $820a:dec(lineBeg,ys*2); {ctrl+pgup}
  $820b:inc(lineBeg,ys*2); {ctrl+pgdn}
  $840a:dec(lineBeg,ys*4); {alt+pgup}
  $840b:inc(lineBeg,ys*4); {alt+pgdn}
  end;
if (cur<1) then cur:=1;
if (cur>max) then cur:=max;
if keypressed then goto f5;
if (cur=p) then goto f5;
goto f4;
End;



Procedure doChangeEncoding;
Label f1;
Var
  xp,yp,xs,ys:Word;
  i:LongInt;
Begin;
xs:=19;
ys:=4;
textColor($71);
PutOutWindow(xp,yp,xs,ys,'change encoding');
textColor($70);
gotoXY(xp,yp+1);Write('0 - us-ascii');
gotoXY(xp,yp+2);Write('1 - iso-8859-1');
gotoXY(xp,yp+3);Write('2 - iso-8859-2');
gotoXY(xp,yp);Write('choose encoding:');
f1:
i:=readkey;
case i of
  $8004,$8005:exit; {enter, escape}
  $0030:begin; currCharSet:=charSetAnsiDos;exit; end; {0}
  $0031:begin; currCharSet:=charSetIso8859_1;exit; end; {1}
  $0032:begin; currCharSet:=charSetIso8859_2;exit; end; {2}
  end;
goto f1;
End;


Procedure doAskUserPass;
Label f1;
Var
  xp,yp,xs,ys:Word;
  i:LongInt;
  a:String;
Begin;
xs:=screenSizX-12;
ys:=4;
textColor($71);
PutOutWindow(xp,yp,xs,ys,'change authentication');
textColor($70);
gotoXY(xp,yp+0);Write('name of user:');
gotoXY(xp,yp+2);Write('password:');
a:=currentUsr;
if AskUserForText(a,xp,yp+1,xs) then exit;
currentUsr:=a;
a:=currentPwd;
if AskUserForText(a,xp,yp+3,xs) then exit;
currentPwd:=a;
End;



Function doAskForOverwrite:LongInt; {0=cancel, 1=continue, 2-overwrite}
Label f1;
Var
  xp,yp,xs,ys:Word;
  i:LongInt;
Begin;
doAskForOverwrite:=0;
xs:=23;
ys:=4;
textColor($47);
PutOutWindow(xp,yp,xs,ys,'file already exists');
textColor($4f);
gotoXY(xp,yp+1);Write('c - continue file');
gotoXY(xp,yp+2);Write('o - overwrite file');
gotoXY(xp,yp+3);Write('s - skip file');
gotoXY(xp,yp);Write('what do you want to do:');
f1:
i:=readkey;
case i and $feff of
  $73,$53:i:=0; {s}
  $63,$43:i:=1; {c}
  $6f,$4f:i:=2; {o}
  else goto f1;
  end;
doAskForOverwrite:=i;
End;



Function doSubmitButton:Boolean;
Label f1,f2,f3,f4;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p,q:LongInt;
  encMet:Boolean; {false=url, true=mime}
  encFil:xtText;
  encSep:String;
  selNam:String;

Procedure add2url(a:String);
Var
  b:String;
  bb:array[0..1] of byte absolute b;
  bb0:byte absolute b;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
bb0:=0;
for o:=1 to ab0 do begin;
  i:=ab[o];
  if (bb0>250) then break;
  if (i=32) then begin;
    inc(bb0);
    bb[bb0]:=43;
    continue;
    end;
  if not (i in [0..31,255,58,47,38,43,61,63,35]) then begin;
    inc(bb0);
    bb[bb0]:=i;
    continue;
    end;
  inc(bb0);bb[bb0]:=37;
  b:=b+byte2hextype(i);
  end;
currentUrl:=currentUrl+b;
End;

Procedure addOneText(nam,val:String);
Begin;
if (nam='') then exit;
if not encMet then begin;
  add2url(nam);
  if (val<>'') then begin;
    currentUrl:=currentUrl+'=';
    add2url(val);
    end;
  currentUrl:=currentUrl+'&';
  exit;
  end;
xtWriteLn(encFil,encSep);
xtWrite(encFil,'Content-Disposition: form-data; name="');
xtWrite(encFil,nam);
xtWriteLn(encFil,'"');
xtWriteLn(encFil,'');
xtWriteLn(encFil,val);
End;

Procedure addOneFile(nam,val:String;asFile:Boolean);
Var
  f:xFile;
  buf:array[1..1024] of byte;
  i,o,p,s:LongInt;
Begin;
if (nam='') then exit;
if (xOpen(f,val,xGenFilMod_r)<>0) then exit;
s:=xFileSize(f);
p:=0;
if not encMet then begin;
  if (s>255) then s:=255;
  xBlockRead(f,val[1],s);
  val[0]:=chr(s);
  xClose(f);
  addOneText(nam,val);
  exit;
  end;
xtWriteLn(encFil,encSep);
xtWrite(encFil,'Content-Disposition: form-data; name="');
xtWrite(encFil,nam);
xtWrite(encFil,'"');
val:=xFileName(val,2)+xFileName(val,3);
if asFile then begin;
  xtWrite(encFil,'; filename="');
  xtWrite(encFil,val);
  xtWrite(encFil,'"');
  end;
xtWriteLn(encFil,'');
xtWriteLn(encFil,'');
while (p<s) do begin;
  o:=s-p;
  if (o>sizeof(buf)) then o:=sizeof(buf);
  xBlockRead(f,buf,o);
  inc(p,o);
  for i:=1 to o do xtPutOneChar(encFil,buf[i]);
  end;
xtWriteLn(encFil,'');
xClose(f);
End;

Begin;
doSubmitButton:=True;
xErase(dataPath+TemporaryPost);
outputReadBuf(slctLin);
decodeCurrentLine;
if (slctNum<1) or (slctNum>LinkListNum) then exit;
i:=LinkListDat[slctNum].ps;
a:=outputGetTag(i);
move(ab[2],o,sizeof(o));
case ab[1] of
  $83:begin; {link}
    currentUrl:=convertUrlString(outputGetDat(o));
    downloadMethod:=1;
    doSubmitButton:=False;
    exit;
    end;
  $86:; {submit}
  else exit;
  end;
q:=slctLin+1;
f1:
dec(q);
if (q<0) then exit;
outputReadBuf(q);
p:=0;
repeat
  a:=outputGetTag(p);
  if (ab0=0) then goto f1;
  until (ab[1]=$84); {formBeg}
move(ab[2],o,sizeof(o));
a:=outputGetDat(o);
downloadMethod:=ab[1];
encMet:=(ab[2]=2);
currentUrl:=convertUrlString(copy(a,3,666));
ab0:=32;
for i:=1 to ab0 do ab[i]:=random(26)+$61;
encSep:='--'+BStr($7fffffff)+a+BStr($7fffffff);
if encMet then begin;
  a:=dataPath+TemporaryPost;
  xCreate(a);
  if (xtOpen(encFil,a,false)<>0) then immErr('error creating file!');
  end else begin;
  currentUrl:=currentUrl+'?';
  end;
selNam:='';
goto f3;
f2:
inc(q);
outputReadBuf(q);
p:=0;
if (q>outputLin) then goto f4;
f3:
o:=p;
a:=outputGetTag(p);
if (ab0=0) then goto f2;
move(ab[2],i,sizeof(i));
case ab[1] of
  $84:goto f4; {formBeg}
  $85:goto f4; {formEnd}
  $86:begin; {submit}
    if (q<>slctLin) then goto f3;
    decodeCurrentLine;
    if (LinkListDat[slctNum].ps<>o) then goto f3;
    addOneText(outputGetDat(i),outputGetDat(i+sizeof(a)));
    goto f3;
    end;
  $89:begin; {input}
    addOneText(outputGetDat(i),outputGetDat(i+sizeof(a)));
    goto f3;
    end;
  $8a:begin; {file}
    addOneFile(outputGetDat(i),outputGetDat(i+sizeof(a)),true);
    goto f3;
    end;
  $8b,$8c:begin; {checkbox, radio}
    a:=outputGetDat(i+sizeof(a));
    if (ab[1]=1) then addOneText(outputGetDat(i),copy(a,2,666));
    goto f3;
    end;
  $8d:begin; {selectBeg}
    selNam:=copy(outputGetDat(i),2,666);
    goto f3;
    end;
  $8e:begin; {selectEnd}
    selNam:='';
    goto f3;
    end;
  $8f:begin; {selectDat}
    a:=outputGetDat(i+sizeof(a));
    if (ab[1]=1) then addOneText(selNam,outputGetDat(i));
    goto f3;
    end;
  $90:begin; {textarea}
    addOneFile(outputGetDat(i),dataPath+outputGetDat(i+sizeof(a)),false);
    goto f3;
    end;
  end;
goto f3;
f4:
if encMet then begin;
  xtWriteLn(encFil,encSep+'--');
  xtClose(encFil);
  end else begin;
  currentUrl:=copy(currentUrl,1,length(currentUrl)-1);
  end;
doSubmitButton:=False;
End;


Function searchString(mov:LongInt):Boolean;
Label f1;
Var
  lin:LongInt;
  a:String;
Begin;
searchString:=True;
lin:=lineBeg;
a:=kicsi(currentFnd);
f1:
inc(lin,mov);
if (lin<0) then exit;
if (lin>=outputLin) then exit;
outputReadBuf(lin);
decodeCurrentLine;
if (pos(a,kicsi(getCurrentLineAscii))<1) then goto f1;
lineBeg:=lin;
searchString:=False;
End;





LABEL cls,scr,drw,key,kynt;
VAR
  a,b,c:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
  w:Word;
BEGIN;
doUserInterface:=false;
lineBeg:=0;
slctLin:=-1;
slctNum:=0;
cls:
ConsoleSize(screenSizX,screenSizY);
TextColor(7);
clrscr;
scr:
a:=currentTit;
convCharSetStr(a);
fillUpString(a,screenSizX);
gotoXY(1,1);
TextColor($1f);
write(a);
drw:
rangeCheck;
o:=lineBeg;
for i:=2 to screenSizY-1 do begin;
  gotoXY(1,i);
  outputReadBuf(o);
  decodeCurrentLine;
  if (o=slctLin) then selectOneEntry(slctNum);
  displayDecodedLine;
  inc(o);
  end;
repaintSt;
key:
ob:=lineBeg;
ol:=slctLin;
on:=slctNum;
kynt:
i:=ReadKey;
case i of
  $8001:goto cls; {redraw}
  $8014:begin; ProcessHelpScreen;goto scr; end; {f1}
  $801d:exit; {f10}
  $801a:begin; {f7}
    a:=currentFnd;
    if AskForOneText('find','enter text to find:',a) then goto drw;
    currentFnd:=a;
    searchString(1);
    goto drw;
    end;
  $811a:begin; {shift+f7}
    if searchString(1) then goto key;
    goto drw;
    end;
  $841a:begin; {alt+f7}
    if searchString(-1) then goto key;
    goto drw;
    end;
  $8008:lineBeg:=0; {hone}
  $8009:lineBeg:=outputLin; {end}
  $800a:dec(lineBeg,screenSizY-3); {pgup}
  $800b:inc(lineBeg,screenSizY-3); {pgdn}
  $810a:dec(lineBeg,screenSizY*2); {shift+pgup}
  $810b:inc(lineBeg,screenSizY*2); {shift+pgdn}
  $820a:dec(lineBeg,screenSizY*4); {ctrl+pgup}
  $820b:inc(lineBeg,screenSizY*4); {ctrl+pgdn}
  $840a:dec(lineBeg,screenSizY*8); {alt+pgup}
  $840b:inc(lineBeg,screenSizY*8); {alt+pgdn}
  $800c:dec(lineBeg); {up}
  $800d:inc(lineBeg); {down}
  $800e:findStopPrev; {left}
  $800f:findStopNext; {right}
  $0467,$0267:begin; {alt+g  ctrl+g}
    if (i and $ff00>$200) then b:=currentUrl else b:='';
    a:=b;
    if AskForOneText('change url','enter url to go to:',a) then a:='';
    if (a='') or (a=b) then goto drw;
    if (pos('://',a)<1) then a:='http://'+a;
    a:=convertUrlString(a);
    currentUrl:=a;
    xErase(dataPath+TemporaryPost);
    downloadMethod:=1;
    doUserInterface:=true;
    repaintSt;
    exit;
    end;
  $8004:begin; {enter}
    outputReadBuf(slctLin);
    decodeCurrentLine;
    if (slctNum<1) or (slctNum>LinkListNum) then goto key;
    i:=LinkListDat[slctNum].ps;
    a:=outputGetTag(i);
    move(ab[2],p,sizeof(p));
    case ab[1] of
      $83:begin; {link}
        if doSubmitButton then goto key;
        doUserInterface:=true;
        exit;
        end;
      $84:begin; {formBeg}
        a:=outputGetDat(p);
        for i:=1 to 2 do inc(ab[i],$30);
        if AskForOneText('change form','enter form to send to:',a) then a:='';
        if (a='') then goto drw;
        a:=convertUrlString(a);
        for i:=1 to 2 do ab[i]:=(ab[i]-$30) and 1;
        outputPutDat(p,a);
        end;
      $86:begin; {submit}
        if doSubmitButton then goto key;
        doUserInterface:=true;
        exit;
        end;
      $89:begin; {text}
        b:=outputGetDat(p);
        inc(p,sizeof(b));
        a:=outputGetDat(p);
        convCharSetStr(a);
        if AskForOneText('change value','enter value of '+b+':',a) then goto drw;
        move(currCharSet,outputBuf,sizeof(currCharSet));
        rotateCharacterSet(currCharSet);
        convCharSetStr(a);
        move(outputBuf,currCharSet,sizeof(currCharSet));
        outputPutDat(p,a);
        end;
      $8a:begin; {file}
        b:=outputGetDat(p);
        inc(p,sizeof(b));
        a:=outputGetDat(p);
        if AskForOneText('select file','enter file for '+b+':',a) then goto drw;
        if (a='') then outputPutDat(p,a);
        if fileExists(a) then outputPutDat(p,a);
        end;
      $8b,$8c:begin; {checkbox, radio}
        inc(p,sizeof(b));
        a:=outputGetDat(p);
        ab[1]:=(ab[1]+1) and 1;
        outputPutDat(p,a);
        end;
      $8d:doSelecterRound; {select}
      $90:begin; {textarea}
        inc(p,sizeof(a));
        a:=dataPath+outputGetDat(p);
        TextColor(7);
        WriteLn('');
        Writeln('decoding text...');
        convCharSetFile(a);
        xExec(myFavoritEditor,a,w);
        Writeln('encoding text...');
        move(currCharSet,outputBuf,sizeof(currCharSet));
        rotateCharacterSet(currCharSet);
        convCharSetFile(a);
        move(outputBuf,currCharSet,sizeof(currCharSet));
        goto cls;
        end;
      end;
    goto scr;
    end;
  $0465:begin; {alt+e}
    doChangeEncoding;
    goto scr;
    end;
  $0464:begin; {alt+d}
    b:=currentUrl;
    if doSubmitButton then goto key;
    a:=currentUrl;
    c:=currentUrl;
    currentUrl:=b;
    o:=666;
    for i:=1 to ab0 do if (ab[i]=47) then o:=i;
    a:=dataPath+copy(a,o+1,666);
    if AskForOneText('download','enter name of file',a) then goto drw;
    if fileExists(a) then case doAskForOverwrite of
      1:;
      2:xErase(a);
      else goto drw;
      end;
    repaintSt;
    b:=currentUrl;
    currentUrl:=c;
    c:=currentUsr+#13+currentPwd;
    doDownloadOneFile(a);
    currentUrl:=b;
    i:=pos(#13,c);
    currentUsr:=copy(c,1,i-1);
    currentPwd:=copy(c,i+1,666);
    goto cls;
    end;
  $0472:begin; {alt+r}
    doUserInterface:=true;
    exit;
    end;
  $0461:begin; {alt+a}
    append2bookmark(BookmarkFileName,currentTit,currentUrl);
    end;
  $0462:begin; {alt+b}
    currentUrl:='about://bookmark';
    doUserInterface:=true;
    exit;
    end;
  $0468:begin; {alt+h}
    currentUrl:='about://history';
    doUserInterface:=true;
    exit;
    end;
  $0475:begin; {alt+u}
    doAskUserPass;
    goto drw;
    end;
  $0470:begin; {alt+p}
    a:=currentPrx;
    if AskForOneText('proxy','enter host [port] of proxy:',a) then goto drw;
    currentPrx:=a;
    goto drw;
    end;
  end;
rangeCheck;
if keypressed then goto kynt;
if (ob<>lineBeg) then goto drw;
if (ol=slctLin) and (on=slctNum) then goto key;
if (slctLin<0) then begin;
  repaintLn(ol);
  repaintSt;
  goto key;
  end;
if (ol<0) then ol:=slctLin;
if (ol<>slctLin) then repaintLn(ol);
repaintLn(slctLin);
repaintSt;
goto key;
END;
