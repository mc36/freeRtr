Var
  screenSavX:LongInt;
  screenSavY:LongInt;
  currentCol:LongInt;
  commandSiz:LongInt;
  commandDat:array[1..256] of byte;


{$include scroll.inc}

Procedure startEmulator;
Var i:LongInt;
Begin;
outputSize:=0;
for i:=0 to screenMaxX*screenMaxY-1 do begin;
  screenData[i].a:=$07;
  screenData[i].c:=$20;
  end;
screenPosX:=0;
screenPosY:=0;
currentCol:=$07;
commandSiz:=0;
screenSavX:=0;
screenSavY:=0;
End;




Function getNextParam(var p:LongInt;def:LongInt):LongInt;
Begin;
getNextParam:=def;
inc(p);
if (p>=commandSiz) then exit;
if not (commandDat[p] in [$30..$39]) then exit;
def:=0;
while (p<commandSiz) and (commandDat[p] in [$30..$39]) do begin;
  def:=def*10;
  inc(def,commandDat[p]-$30);
  inc(p);
  end;
getNextParam:=def;
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


Procedure gotRemoteChar(chb:Byte);
Label f1,f2,vege;
Const colorCodes:array[0..7] of longint=(0,4,2,6,1,5,3,7);
Var
  chc:Char absolute chb;
  i,o,p,q,r,s,t:LongInt;

Function decodeColor(currentCol:LongInt):LongInt;
Begin;
while (p<commandSiz) do begin;
  i:=getNextParam(p,0);
  case i of
    30..37:begin; {foreground}
      currentCol:=(currentCol and $f8) or colorCodes[i-30];
      continue;
      end;
    40..47:begin; {background}
      currentCol:=(currentCol and $8f) or (colorCodes[i-40] shl 4);
      continue;
      end;
    0:begin; {no attribs}
      currentCol:=$07;
      continue;
      end;
    1:begin; {bold on}
      currentCol:=currentCol or $08;
      continue;
      end;
    4:begin; {underline on}
      continue;
      end;
    5:begin; {blink on}
      currentCol:=currentCol or $80;
      continue;
      end;
    7:begin; {reverse on}
      currentCol:=currentCol xor $77;
      continue;
      end;
    22:begin; {bold off}
      currentCol:=currentCol and $f7;
      continue;
      end;
    24:begin; {underline off}
      continue;
      end;
    25:begin; {blink off}
      currentCol:=currentCol and $7f;
      continue;
      end;
    26:begin; {reverse off}
      currentCol:=currentCol xor $77;
      continue;
      end;
    end;
  end;
decodeColor:=currentCol;
End;

Begin;
if (commandSiz=0) then goto f1;
inc(commandSiz);
if (commandSiz>sizeof(commandDat)) then goto f2;
commandDat[commandSiz]:=chb;
if (commandSiz=2) then begin;
  if (chb=$5b) then exit;
  goto f2;
  end;
if (chb in [$30..$39,$3b])  then exit;
p:=2;
case chb of
  $6d:begin; {m - set graphic rendition}
    currentCol:=decodeColor(currentCol);
    goto f2;
    end;
  $41:begin; {A - cursor up}
    dec(screenPosY,getNextParam(p,1));
    if (screenPosY<0) then screenPosY:=0;
    goto f2;
    end;
  $42:begin; {B - cursor down}
    inc(screenPosY,getNextParam(p,1));
    if (screenPosY>=screenMaxY) then screenPosY:=screenMaxY-1;
    goto f2;
    end;
  $43:begin; {C - cursor forward}
    inc(screenPosX,getNextParam(p,1));
    if (screenPosX>=screenMaxX) then screenPosX:=screenMaxX-1;
    goto f2;
    end;
  $44:begin; {D - cursor backward}
    dec(screenPosX,getNextParam(p,1));
    if (screenPosX<0) then screenPosX:=0;
    goto f2;
    end;
  $48,$66:begin; {H,f - cursor position}
    screenPosY:=getNextParam(p,1)-1;
    screenPosX:=getNextParam(p,1)-1;
    if (screenPosX<0) then screenPosX:=0;
    if (screenPosY<0) then screenPosY:=0;
    if (screenPosX>=screenMaxX) then screenPosX:=screenMaxX-1;
    if (screenPosY>=screenMaxY) then screenPosY:=screenMaxY-1;
    goto f2;
    end;
  $73:begin; {s - save cursor position}
    screenSavX:=screenPosX;
    screenSavY:=screenPosY;
    goto f2;
    end;
  $75:begin; {u - restore cursor position}
    screenPosX:=screenSavX;
    screenPosY:=screenSavY;
    goto f2;
    end;
  $6e:begin; {n - device status report}
    case getNextParam(p,0) of
      6:appendOutputStr(#27'['+BStr(screenPosY+1)+';'+BStr(screenPosX+1)+'R');
      end;
    goto f2;
    end;
  $4a:begin; {J - erase in display}
    case getNextParam(p,0) of
      0:begin; {ending of screen}
        for i:=screenPosY*screenMaxX+screenPosX to screenMaxX*screenMaxY-1 do begin;
          screenData[i].c:=$20;
          screenData[i].a:=currentCol;
          end;
        end;
      1:begin; {beginning of screen}
        for i:=0 to screenPosY*screenMaxX+screenPosX do begin;
          screenData[i].c:=$20;
          screenData[i].a:=currentCol;
          end;
        end;
      2:begin; {whole screen}
        for i:=0 to screenMaxX*screenMaxY-1 do begin;
          screenData[i].c:=$20;
          screenData[i].a:=currentCol;
          end;
        end;
      end;
    goto f2;
    end;
  $4b:begin; {K - erase in line}
    case getNextParam(p,0) of
      0:begin; {ending of line}
        for i:=screenPosY*screenMaxX+screenPosX to (screenPosY+1)*screenMaxX-1 do begin;
          screenData[i].c:=$20;
          screenData[i].a:=currentCol;
          end;
        end;
      1:begin; {beginning of line}
        for i:=screenPosY*screenMaxX to screenPosY*screenMaxX+screenPosX do begin;
          screenData[i].c:=$20;
          screenData[i].a:=currentCol;
          end;
        end;
      2:begin; {whole line}
        for i:=screenPosY*screenMaxX to (screenPosY+1)*screenMaxX-1 do begin;
          screenData[i].c:=$20;
          screenData[i].a:=currentCol;
          end;
        end;
      end;
    goto f2;
    end;
  $4c:begin; {L - insert lines}
    doScrollDown(0,screenPosY,screenMaxX,screenMaxY,getNextParam(p,1),$20,currentCol);
    goto f2;
    end;
  $4d:begin; {M - delete lines}
    doScrollUp(0,screenPosY,screenMaxX,screenMaxY,getNextParam(p,1),$20,currentCol);
    goto f2;
    end;
  $40:begin; {@ - insert characters}
    doScrollRight(screenPosX,screenPosY,screenMaxX,screenPosY,getNextParam(p,1),$20,currentCol);
    goto f2;
    end;
  $50:begin; {P - delete characters}
    doScrollLeft(screenPosX,screenPosY,screenMaxX,screenPosY,getNextParam(p,1),$20,currentCol);
    goto f2;
    end;
  $58:begin; {X - erase characters}
    doFillArea(screenPosX,screenPosY,screenPosX+getNextParam(p,1)-1,screenPosY,$20,currentCol);
    goto f2;
    end;
  $7e:begin; {~ - delete columns}
    doScrollLeft(screenPosX,0,screenMaxX,screenMaxY,getNextParam(p,1),$20,currentCol);
    goto f2;
    end;
  $7d:begin; {] - insert columns}
    doScrollRight(screenPosX,0,screenMaxX,screenMaxY,getNextParam(p,1),$20,currentCol);
    goto f2;
    end;
  $7a:begin; {z - erase area}
    o:=getNextParam(p,1);
    i:=getNextParam(p,1);
    q:=getNextParam(p,0);
    p:=getNextParam(p,0);
    doFillArea(i,o,p,q,$20,currentCol);
    goto f2;
    end;
  $78:begin; {z - fill area}
    r:=getNextParam(p,32);
    o:=getNextParam(p,1);
    i:=getNextParam(p,1);
    q:=getNextParam(p,0);
    p:=getNextParam(p,0);
    doFillArea(i,o,p,q,r,-1);
    goto f2;
    end;
  $72,$74:begin; {r,t - color area}
    o:=getNextParam(p,1);
    i:=getNextParam(p,1);
    q:=getNextParam(p,0);
    p:=getNextParam(p,0);
    r:=decodeColor(currentCol);
    doFillArea(i,o,p,q,-1,r);
    goto f2;
    end;

  end;

f2:
commandSiz:=0;
exit;
f1:
case chb of
  27:begin; {esc}
    commandSiz:=1;
    commandDat[commandSiz]:=27;
    goto vege;
    end;
  13:begin; {cr}
    screenPosX:=0;
    goto vege;
    end;
  10:begin; {lf}
    inc(screenPosY);
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
  7:begin; {bell}
    goto vege;
    end;
  end;
i:=(screenPosY*screenMaxX)+screenPosX;
screenData[i].c:=chb;
screenData[i].a:=currentCol;
inc(screenPosX);
vege:
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





Procedure gotLocalKey(w:Word);
Const
  pgdnup:array[0..5] of longInt=(2,3,1,4,5,6);
  f1f15:array[0..19] of longInt=(11,12,13,14,15,17,18,19,20,21,23,24,25,26,28,29,31,32,33,34);
Var
  os:LongInt;
Begin;
os:=outputSize;
if (w and $400<>0) then begin;
  appendOutputChr(27);
  w:=w and $fbff;
  end;
if (w and $fe00=$0000) then begin; {letter}
  appendOutputChr(w);
  exit;
  end;
if (w and $fe00=$0200) then begin; {ctrl+letter}
  appendOutputChr(w and $1f);
  exit;
  end;
case w of
  $8002:appendOutputChr(9);   {tab}
  $8003:appendOutputChr(8);   {backspace}
  $8005:appendOutputChr(27);  {escape}
  $8004:appendOutputChr(13);  {enter}
  $8207:appendOutputChr(127); {ctrl+delete}
  $8204:appendOutputChr(10);  {ctrl+enter}
  $800c:appendOutputStr(#27'[A'); {up}
  $800d:appendOutputStr(#27'[B'); {down}
  $800e:appendOutputStr(#27'[D'); {left}
  $800f:appendOutputStr(#27'[C'); {right}
  $8006..$800b:appendOutputStr(#27'['+BStr(pgdnup[w-$8006])+'~'); {ins..pgdn}
  $8114..$811d:appendOutputStr(#27'O'+chr(w-$8114+80)); {shift+f1..shift+f10}
  $8014..$801d:appendOutputStr(#27'['+BStr(f1f15[w-$8014])+'~'); {f1..f15}
  $8208:appendOutputStr(#27'[H'); {ctrl+home}
  $8209:appendOutputStr(#27'[K'); {ctrl+end}
  else outputSize:=os;
  end;
End;
