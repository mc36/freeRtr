Var
  commandSiz:LongInt;
  commandDat:array[1..256] of byte;



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
commandSiz:=0;
End;







Procedure appendOutputChr(i:LongInt);
Begin;
if (outputSize>=sizeof(outputData)) then exit;
inc(outputSize);
outputData[outputSize]:=i;
End;




Procedure gotRemoteChar(chb:Byte);
Label f1,f2,vege;
Var
  chc:Char absolute chb;
  i,o:LongInt;
Begin;
if (commandSiz=0) then goto f1;
inc(commandSiz);
case commandDat[2] of
  $41:begin; {A - cursor up}
    dec(screenPosY);
    if (screenPosY<0) then screenPosY:=0;
    goto f2;
    end;
  $42:begin; {B - cursor down}
    inc(screenPosY);
    if (screenPosY>=screenMaxY) then screenPosY:=screenMaxY-1;
    goto f2;
    end;
  $43:begin; {C - cursor forward}
    inc(screenPosX);
    if (screenPosX>=screenMaxX) then screenPosX:=screenMaxX-1;
    goto f2;
    end;
  $44:begin; {D - cursor backward}
    dec(screenPosX);
    if (screenPosX<0) then screenPosX:=0;
    goto f2;
    end;
  $49:begin; {I - reverse line feed}
    dec(screenPosY);
    if (screenPosY>=0) then goto f2;
    o:=(screenMaxY-1)*screenMaxX;
    move(screenData,screenData[screenMaxX],o*2);
    for i:=0 to screenMaxX-1 do begin;
      screenData[i].c:=$20;
      inc(i);
      end;
    screenPosY:=0;
    goto f2;
    end;
  $48:begin; {H - cursor home}
    screenPosX:=0;
    screenPosY:=0;
    goto f2;
    end;
  $59:begin; {Y - direct cursor addressing}
    if (commandSiz<4) then exit;
    screenPosX:=commandDat[4];
    screenPosY:=commandDat[3];
    if (screenPosX<0) then screenPosX:=0;
    if (screenPosY<0) then screenPosY:=0;
    if (screenPosX>=screenMaxX) then screenPosX:=screenMaxX-1;
    if (screenPosY>=screenMaxY) then screenPosY:=screenMaxY-1;
    goto f2;
    end;
  $4b:begin; {K - erase to end of line}
    for i:=screenPosY*screenMaxX+screenPosX to (screenPosY+1)*screenMaxX-1 do screenData[i].c:=$20;
    goto f2;
    end;
  $4a:begin; {J - erase to end of screen}
    for i:=screenPosY*screenMaxX+screenPosX to screenMaxX*screenMaxY-1 do screenData[i].c:=$20;
    goto f2;
    end;
  $5a:begin; {Z - identify device}
    appendOutputChr(27);
    appendOutputChr(47);
    appendOutputChr(67);
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
    commandSiz:=2;
    commandDat[1]:=27;
    commandDat[2]:=$59; {H-gotoxy}
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
screenData[(screenPosY*screenMaxX)+screenPosX].c:=chb;
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
  inc(o);
  end;
End;





Procedure gotLocalKey(w:Word);
Const functKeys:array[1..12] of LongInt=(113,114,115,116,117,118,119,120,121,112,110,77);
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
  $8002:begin; appendOutputChr(9);exit; end;   {tab}
  $8003:begin; appendOutputChr(8);exit; end;   {backspace}
  $8004:begin; appendOutputChr(13);exit; end;  {enter}
  $8204:begin; appendOutputChr(10);exit; end;  {ctrl+enter}
  $8005:begin; appendOutputChr(27);exit; end;  {escape}
  $8007:begin; appendOutputChr(127);exit; end; {delete}
  $800c:w:=65; {up}
  $800d:w:=66; {down}
  $800e:w:=68; {left}
  $800f:w:=67; {right}
  $8006:w:=80; {insert}
  $8008:w:=81; {home}
  $800a:w:=82; {pgup}
  $800b:w:=83; {pgup}
  $8014..$801f:begin; {f1..f12}
    appendOutputChr(27);
    appendOutputChr(63);
    appendOutputChr(functKeys[w-$8014]);
    exit;
    end;
  else exit;
  end;
appendOutputChr(27);
appendOutputChr(w);
End;
