Var
  currentCol:LongInt;
  commandSiz:LongInt;
  commandDat:array[1..256] of byte;
  currentSet:LongInt;


Procedure startEmulator;
Var i:LongInt;
Begin;
outputSize:=0;
for i:=0 to screenMaxX*screenMaxY-1 do begin;
  screenData[i].a:=$03;
  screenData[i].c:=$20;
  end;
screenPosX:=0;
screenPosY:=0;
currentCol:=$07;
commandSiz:=0;
currentSet:=0;
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
Const graph_chars:array[0..255] of byte=(0,1,2,3,4,5,6,7,8,9,10,11,12,
  13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,
  36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,
  59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,
  82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,
  104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,
  121,122,123,124,125,126,127,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
  17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,
  40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,
  63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,
  86,87,88,89,90,91,92,93,94,32,4,176,177,178,177,176,248,241,220,223,
  217,191,218,192,197,126,196,196,196,95,195,180,193,194,179,243,242,227,
  216,156,249,32);
Var
  chc:Char absolute chb;
  i,o,p:LongInt;
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
        end;
      end;
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
  $63:begin; {c - device attributes}
    appendOutputStr(#27'[?1;7c');
    goto f2;
    end;
  $6e:begin; {n - device status report}
    case getNextParam(p,0) of
      5:appendOutputStr(#27'[0n');
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
  14:begin; {so}
    currentSet:=$80;
    goto vege;
    end;
  15:begin; {so}
    currentSet:=$00;
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
i:=(screenPosY*screenMaxX)+screenPosX;
screenData[i].c:=graph_chars[chb xor currentSet];
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
  $8002:appendOutputChr(9);   {tab}
  $8003:appendOutputChr(8);   {backspace}
  $8005:appendOutputChr(27);  {escape}
  $8007:appendOutputChr(127); {delete}
  $8004:appendOutputChr(13);  {enter}
  $8204:appendOutputChr(10);  {ctrl+enter}
  $8404:appendOutputStr(#27'?M'); {alt+enter}
  $800c:appendOutputStr(#27'[A'); {up}
  $800d:appendOutputStr(#27'[B'); {down}
  $800e:appendOutputStr(#27'[D'); {left}
  $800f:appendOutputStr(#27'[C'); {right}
  $820c:appendOutputStr(#27'OA'); {ctrl+up}
  $820d:appendOutputStr(#27'OB'); {ctrl+down}
  $820e:appendOutputStr(#27'OD'); {ctrl+left}
  $820f:appendOutputStr(#27'OC'); {ctrl+right}
  $8430..$8439:appendOutputStr(#27'O'+chr(w-$8430+112)); {alt+0..alt+9}
  $042c..$042e:appendOutputStr(#27'O'+chr(w-$042c+108)); {alt+,..alt+.}
  else exit;
  end;
End;
