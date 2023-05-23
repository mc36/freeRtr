Const
  defaultColor=$03;
Var
  currentCol:LongInt;
  commandSiz:LongInt;
  commandDat:array[1..300] of byte;
  commandMax:LongInt;
  insertMode:Boolean;



{$include scroll.inc}

Procedure startEmulator;
Var i:LongInt;
Begin;
outputSize:=0;
for i:=0 to screenMaxX*screenMaxY-1 do begin;
  screenData[i].a:=defaultColor;
  screenData[i].c:=$20;
  end;
screenPosX:=0;
screenPosY:=0;
currentCol:=defaultColor;
commandSiz:=0;
insertMode:=False;
End;




Procedure testForScrolling;
Var
  i,o:LongInt;
Begin;
if (screenPosX>=screenMaxX) then begin;
  screenPosX:=0;
  inc(screenPosY);
  end;
if (screenPosY<screenMaxY) then exit;
o:=(screenMaxY-1)*screenMaxX;
move(screenData[screenMaxX],screenData,o*2);
dec(screenPosY);
for i:=1 to screenMaxX do begin;
  screenData[o].c:=$20;
  screenData[o].a:=currentCol;
  inc(o);
  end;
End;



Procedure appendOutputStr(a:String);
Begin;
if (length(a)+outputSize>sizeof(outputData)) then exit;
move(a[1],outputData[outputSize+1],length(a));
inc(outputSize,length(a));
End;

Procedure appendOutputChr(i:LongInt);
Begin;
if (outputSize>=sizeof(outputData)) then exit;
inc(outputSize);
outputData[outputSize]:=i;
End;



Procedure putOneCharacter(c:byte);
Var
  i,o:LongInt;
Begin;
o:=(screenPosY*screenMaxX)+screenPosX;
if insertMode then begin;
  move(screenData[o],screenData[o+1],(screenMaxX-screenPosX-1)*2);
  end;
screenData[o].c:=c;
screenData[o].a:=currentCol;
inc(screenPosX);
End;




Procedure gotRemoteChar(chb:Byte);
Label f1,f2,vege;
Var
  chc:Char absolute chb;
  b:Boolean;
  i,o,p:LongInt;
Begin;
if (commandSiz=0) then goto f1;
inc(commandSiz);
commandDat[commandSiz]:=chb;
if (commandSiz<commandMax) then exit;
case commandDat[1] of
  25:begin; {^y char count}
    o:=commandDat[2];
    for i:=1 to commandDat[3] do begin;
      putOneCharacter(o);
      testForScrolling;
      end;
    goto f2;
    end;
  22:; {^v...}
  else goto f2;
  end;
b:=insertMode;
insertMode:=False;
case commandDat[2] of
  1:begin; {^v^a attr}
    commandMax:=3;
    if (commandSiz<3) then exit;
    currentCol:=commandDat[3];
    goto f2;
    end;
  2:begin; {^v^b}
    currentCol:=currentCol or $80;
    goto f2;
    end;
  3:begin; {^v^c}
    dec(screenPosY);
    if (screenPosY<0) then screenPosY:=0;
    goto f2;
    end;
  4:begin; {^v^d}
    inc(screenPosY);
    if (screenPosY>=screenMaxY) then screenPosY:=screenMaxY-1;
    goto f2;
    end;
  5:begin; {^v^e}
    dec(screenPosX);
    if (screenPosX<0) then screenPosX:=0;
    goto f2;
    end;
  6:begin; {^v^f}
    inc(screenPosX);
    if (screenPosX>=screenMaxX) then screenPosX:=screenMaxX-1;
    goto f2;
    end;
  7:begin; {^v^g}
    for i:=screenPosY*screenMaxX+screenPosX+1 to (screenPosY+1)*screenMaxX-1 do begin;
      screenData[i].c:=$20;
      screenData[i].a:=currentCol;
      end;
    goto f2;
    end;
  8:begin; {^v^h y x}
    commandMax:=4;
    if (commandSiz<4) then exit;
    screenPosY:=commandDat[3]-1;
    screenPosX:=commandDat[4]-1;
    if (screenPosX<0) then screenPosX:=0;
    if (screenPosY<0) then screenPosY:=0;
    if (screenPosX>=screenMaxX) then screenPosX:=screenMaxX-1;
    if (screenPosY>=screenMaxY) then screenPosY:=screenMaxY-1;
    goto f2;
    end;
  9:begin; {^v^i}
    insertMode:=True;
    goto f2;
    end;
  10:begin; {^v^j ln y1 x1 y2 x2}
    commandMax:=7;
    if (commandSiz<7) then exit;
    i:=commandDat[3];
    if (i<1) then i:=255;
    doScrollUp(commandDat[5]-1,commandDat[4]-1,commandDat[7]-1,commandDat[6]-1,i,$20,currentCol);
    goto f2;
    end;
  11:begin; {^v^k ln y1 x1 y2 x2}
    commandMax:=7;
    if (commandSiz<7) then exit;
    i:=commandDat[3];
    if (i<1) then i:=255;
    doScrollDown(commandDat[5]-1,commandDat[4]-1,commandDat[7]-1,commandDat[6]-1,i,$20,currentCol);
    goto f2;
    end;
  12:begin; {^v^l attr y x}
    commandMax:=5;
    if (commandSiz<5) then exit;
    currentCol:=commandDat[3];
    doFillArea(screenPosX,screenPosY,screenPosX+commandDat[5],screenPosY+commandDat[4],$20,currentCol);
    goto f2;
    end;
  13:begin; {^v^m attr char y x}
    commandMax:=6;
    if (commandSiz<6) then exit;
    currentCol:=commandDat[3];
    doFillArea(screenPosX,screenPosY,screenPosX+commandDat[6],screenPosY+commandDat[5],commandDat[4],currentCol);
    goto f2;
    end;
  14:begin; {^v^n}
    doScrollLeft(screenPosX,screenPosY,screenMaxX,screenPosY,1,$20,currentCol);
    goto f2;
    end;
  15:begin; {^v^o}
    goto f2;
    end;
  16:begin; {^v^p}
    goto f2;
    end;
  17:begin; {^v^q}
    commandMax:=3;
    if (commandSiz<3) then exit;
    goto f2;
    end;
  18:begin; {^v^r}
    currentCol:=defaultColor;
    goto f2;
    end;
  19:begin; {^v^s tone octave duration}
    commandMax:=5;
    if (commandSiz<5) then exit;
    goto f2;
    end;
  20:begin; {^v^t color}
    commandMax:=3;
    if (commandSiz<3) then exit;
    screenData[screenPosY*screenMaxX+screenPosX].a:=commandDat[3];
    goto f2;
    end;
  21:begin; {^v^u window color}
    commandMax:=4;
    if (commandSiz<4) then exit;
    o:=commandDat[4];
    for i:=0 to screenMaxX*screenMaxY-1 do screenData[i].a:=o;
    goto f2;
    end;
  22:begin; {^v^v window color y1 x1 y2 x2}
    commandMax:=8;
    if (commandSiz<8) then exit;
    goto f2;
    end;
  23:begin; {^v^w window}
    commandMax:=3;
    if (commandSiz<3) then exit;
    goto f2;
    end;
  25:begin; {^v^y size datastream count}
    insertMode:=b;
    commandMax:=3;
    if (commandSiz<3) then exit;
    commandMax:=commandDat[3]+4;
    if (commandSiz<commandMax) then exit;
    for o:=1 to commandDat[commandMax] do for i:=4 to commandDat[3]+4 do begin;
      putOneCharacter(commandDat[i]);
      testForScrolling;
      end;
    goto f2;
    end;
  33:begin; {^v ! char attr y x}
    commandMax:=6;
    if (commandSiz<6) then exit;
    i:=commandDat[5]-1;
    o:=commandDat[6]-1;
    if (i<0) then goto f2;
    if (o<0) then goto f2;
    if (i>=screenMaxY) then goto f2;
    if (o>=screenMaxX) then goto f2;
    inc(i,o*screenMaxX);
    screenData[i].c:=commandDat[2];
    screenData[i].a:=commandDat[3];
    goto f2;
    end;
  39:begin; {^v ' mode}
    commandMax:=3;
    if (commandSiz<3) then exit;
    goto f2;
    end;
  42:begin; {^v ' 1/10 seconds}
    commandMax:=3;
    if (commandSiz<3) then exit;
    goto f2;
    end;
  43:begin; {^v +}
    doScrollDown(0,screenPosY,screenMaxX,screenMaxY,1,$20,currentCol);
    goto f2;
    end;
  44:begin; {^v ,}
    doScrollRight(screenPosX,0,screenMaxX,screenMaxY,1,$20,currentCol);
    goto f2;
    end;
  45:begin; {^v -}
    doScrollUp(0,screenPosY,screenMaxX,screenPosY,1,$20,currentCol);
    goto f2;
    end;
  46:begin; {^v .}
    doScrollLeft(screenPosX,0,screenMaxX,screenMaxY,1,$20,currentCol);
    goto f2;
    end;
  47:begin; {^v / mode}
    commandMax:=3;
    if (commandSiz<3) then exit;
    goto f2;
    end;
  48:begin; {^v 0 attr}
    commandMax:=3;
    if (commandSiz<3) then exit;
    o:=screenPosY*screenMaxX+screenPosX;
    p:=commandDat[3];
    for i:=screenPosX to screenMaxX-1 do screenData[i].a:=p;
    goto f2;
    end;
  49:begin; {^v 1 attr}
    commandMax:=3;
    if (commandSiz<3) then exit;
    o:=screenPosY*screenMaxX;
    p:=commandDat[3];
    for i:=0 to screenPosX do screenData[i].a:=p;
    goto f2;
    end;
  58:begin; {^v : mode}
    commandMax:=3;
    if (commandSiz<3) then exit;
    appendOutputStr(#22':0');
    goto f2;
    end;
  60:begin; {^v < ln y1 x1 y2 x2}
    commandMax:=7;
    if (commandSiz<7) then exit;
    i:=commandDat[3];
    if (i<1) then i:=255;
    doScrollLeft(commandDat[5]-1,commandDat[4]-1,commandDat[7]-1,commandDat[6]-1,i,$20,currentCol);
    goto f2;
    end;
  61:begin; {^v = mode}
    commandMax:=3;
    if (commandSiz<3) then exit;
    goto f2;
    end;
  62:begin; {^v > ln y1 x1 y2 x2}
    commandMax:=7;
    if (commandSiz<7) then exit;
    i:=commandDat[3];
    if (i<1) then i:=255;
    doScrollRight(commandDat[5]-1,commandDat[4]-1,commandDat[7]-1,commandDat[6]-1,i,$20,currentCol);
    goto f2;
    end;
  63:begin; {^v ? y x}
    commandMax:=4;
    if (commandSiz<4) then exit;
    i:=commandDat[3]-1;
    o:=commandDat[4]-1;
    if (i<0) then goto f2;
    if (o<0) then goto f2;
    if (i>=screenMaxY) then goto f2;
    if (o>=screenMaxX) then goto f2;
    p:=(o*screenMaxX)+i;
    appendOutputStr(#22'!');
    appendOutputChr(screenData[p].c);
    appendOutputChr(screenData[p].a);
    appendOutputChr(i);
    appendOutputChr(o);
    goto f2;
    end;
  end;

f2:
commandSiz:=0;
exit;
f1:
case chb of
  22:begin; {^v}
    commandMax:=2;
    commandSiz:=1;
    commandDat[1]:=chb;
    exit;
    end;
  25:begin; {^y}
    commandMax:=3;
    commandSiz:=1;
    commandDat[1]:=chb;
    exit;
    end;
  13:begin; {cr}
    screenPosX:=0;
    goto vege;
    end;
  10:begin; {lf}
    inc(screenPosY);
    goto vege;
    end;
  12:begin; {clrscr}
    for i:=0 to screenMaxX*screenMaxY-1 do begin;
      screenData[i].a:=defaultColor;
      screenData[i].c:=$20;
      end;
    screenPosX:=0;
    screenPosY:=0;
    currentCol:=defaultColor;
    goto vege;
    end;
  7:begin; {bell}
    goto vege;
    end;
  8:begin; {backspace}
    dec(screenPosX);
    if (screenPosX>=0) then goto vege;
    screenPosX:=0;
    goto vege;
    end;
  9:begin; {tab}
    screenPosX:=(screenPosX and $fffffff8)+8;
    goto vege;
    end;
  end;
putOneCharacter(chb);
vege:
testForScrolling;
End;





Procedure gotLocalKey(w:Word);
Const
  keyCodes:array[0..25] of longInt=(30,48,46,32,18,33,34,35,23,36,37,38,50,
    49,24,25,16,19,31,20,22,47,17,45,21,44);
Begin;
if (w and $fe00=$0000) then begin; {letter}
  appendOutputChr(w);
  exit;
  end;
if (w and $fe00=$0200) then begin; {ctrl+letter}
  appendOutputChr(w and $1f);
  exit;
  end;
case w of
  $8002:begin; appendOutputChr(9);exit; end;  {tab}
  $8003:begin; appendOutputChr(8);exit; end;  {backspace}
  $8004:begin; appendOutputChr(13);exit; end; {enter}
  $8204:begin; appendOutputChr(10);exit; end; {ctrl+enter}
  $8005:begin; appendOutputChr(27);exit; end; {escape}
  $8014..$801d:w:=w-$8014+59; {f1..f10}
  $8114..$811d:w:=w-$8114+84; {shfit+f1..shfit+f10}
  $8214..$821d:w:=w-$8214+94; {ctrl+f1..ctrl+f10}
  $8414..$841d:w:=w-$8414+104; {alt+f1..alt+f10}
  $8006:w:=82;  {insert}
  $8007:w:=83;  {delete}
  $8010:w:=114; {printscreen}
  $800c:w:=72;  {up}
  $800d:w:=80;  {down}
  $800e:w:=75;  {left}
  $800f:w:=77;  {right}
  $8008:w:=71;  {home}
  $8009:w:=79;  {end}
  $800a:w:=73;  {pgup}
  $800b:w:=81;  {pgdn}
  $820e:w:=115; {ctrl+left}
  $820f:w:=116; {ctrl+right}
  $8208:w:=119; {ctrl+home}
  $8209:w:=117; {ctrl+end}
  $820a:w:=132; {ctrl+pgup}
  $820b:w:=118; {ctrl+pgdn}
  $8102:w:=15;  {shift+tab}
  $0461..$047a:w:=keyCodes[w-$0461]; {alt+letter}
  $0431..$0439:w:=w-$0431+120; {alt+number}
  $0430:w:=129; {alt+0}
  $042d:w:=130; {alt+-}
  $043d:w:=131; {alt+=}
  $801e..$801f:w:=w-$801e+133; {f11..f12}
  $811e..$811f:w:=w-$811e+135; {shift+f11..shift+f12}
  $821e..$821f:w:=w-$821e+137; {ctrl+f11..ctrl+f12}
  $841e..$841f:w:=w-$841e+139; {alt+f11..alt+f12}
  $045b:w:=26;  {alt+[}
  $045c:w:=27;  {alt+\}
  $045d:w:=39;  {alt+]}
  $043b:w:=40;  {alt+;}
  $0427:w:=41;  {alt+'}
  $0460:w:=43;  {alt+`}
  $042c:w:=51;  {alt+,}
  $042e:w:=52;  {alt+.}
  $8403:w:=14;  {alt+backspace}
  $8402:w:=165; {alt+tab}
  $8202:w:=148; {ctrl+tab}
  $8404:w:=28;  {alt+enter}
  $840c:w:=152; {alt+up}
  $840d:w:=160; {alt+down}
  $840e:w:=155; {alt+left}
  $840f:w:=157; {alt+right}
  $8406:w:=162; {alt+insert}
  $8407:w:=163; {alt+delete}
  $8408:w:=151; {alt+home}
  $8409:w:=159; {alt+end}
  $840a:w:=153; {alt+pgup}
  $840b:w:=161; {alt+pgdn}
  $042f:w:=53;  {alt+/}
  $042a:w:=55;  {alt+*}
  $042d:w:=74;  {alt+-}
  $042b:w:=78;  {alt++}
  $022f:w:=149; {ctrl+/}
  $022a:w:=150; {ctrl+*}
  $022d:w:=142; {ctrl+-}
  $022b:w:=144; {ctrl++}
  $8206:w:=146; {ctrl+insert}
  $8207:w:=147; {ctrl+delete}
  else exit;
  end;
appendOutputChr(0);
appendOutputChr(w);
End;
