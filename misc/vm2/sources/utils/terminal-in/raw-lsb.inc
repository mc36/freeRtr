Var
  currentCol:LongInt;
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
currentCol:=$03;
commandSiz:=0;
End;





Procedure appendOutputChr(i:LongInt);
Begin;
if (outputSize>=sizeof(outputData)) then exit;
inc(outputSize);
outputData[outputSize]:=i;
End;





Procedure gotRemoteChar(chb:Byte);
Label f1,f2,kiir,vege;
Var
  chc:Char absolute chb;
  i,o,p:LongInt;
Begin;
if (commandSiz=0) then goto f1;
inc(commandSiz);
commandDat[commandSiz]:=chb;
case commandDat[2] of
  0:begin; {no operation}
    goto f2;
    end;
  1:begin; {report console resolution}
    appendOutputChr($01);
    appendOutputChr($80);
    appendOutputChr(ScreenMaxX);
    appendOutputChr(ScreenMaxX shr 8);
    appendOutputChr(ScreenMaxY);
    appendOutputChr(ScreenMaxY shr 8);
    goto f2;
    end;
  2:begin; {clear screen to color}
    for i:=0 to screenMaxX*screenMaxY-1 do begin;
      screenData[i].a:=currentCol;
      screenData[i].c:=$20;
      end;
    screenPosX:=0;
    screenPosY:=0;
    goto f2;
    end;
  3:begin; {set current color}
    if (commandSiz<3) then exit;
    currentCol:=commandDat[3];
    goto f2;
    end;
  4:begin; {set cursor position}
    if (commandSiz<6) then exit;
    i:=commandDat[3];
    o:=commandDat[4];
    screenPosX:=(o*256)+i-1;
    i:=commandDat[5];
    o:=commandDat[6];
    screenPosY:=(o*256)+i-1;
    if (screenPosX<0) then screenPosX:=0;
    if (screenPosY<0) then screenPosY:=0;
    if (screenPosX>=screenMaxX) then screenPosX:=screenMaxX-1;
    if (screenPosY>=screenMaxY) then screenPosY:=screenMaxY-1;
    goto f2;
    end;
  5:begin; {write special char}
    if (commandSiz<3) then exit;
    chb:=commandDat[3];
    commandSiz:=0;
    goto kiir;
    end;
  6:begin; {report capabilities}
    appendOutputChr($00);
    appendOutputChr($80);
    appendOutputChr($00);
    appendOutputChr($00);
    goto f2;
    end;
  end;


f2:
commandSiz:=0;
exit;
f1:
case chb of
  0:begin; {null}
    commandSiz:=1;
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
  end;
kiir:
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
Begin;
if (w=$8000) then exit;
if (w=$8001) then exit;
appendOutputChr(w);
appendOutputChr(w shr 8);
End;
