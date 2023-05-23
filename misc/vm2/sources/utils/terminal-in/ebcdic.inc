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
End;





Procedure gotRemoteChar(chb:Byte);
Label vege;
Var
  chc:Char absolute chb;
  i,o:LongInt;
Begin;
chb:=converter_ebcdic2ascii[chb];
case chb of
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
Begin;
if (outputSize>=outputMax) then exit;
if (w and $fe00=$0000) then begin; {letter}
  inc(outputSize);
  outputData[outputSize]:=converter_ascii2ebcdic[w];
  exit;
  end;
if (w and $fe00=$0200) then begin; {ctrl+letter}
  inc(outputSize);
  outputData[outputSize]:=converter_ascii2ebcdic[w and $1f];
  exit;
  end;
case w of
  $8002:w:=9;  {tab}
  $8003:w:=8;  {backspace}
  $8004:w:=13; {enter}
  $8204:w:=10; {ctrl+enter}
  $8005:w:=27; {escape}
  else exit;
  end;
inc(outputSize);
outputData[outputSize]:=converter_ascii2ebcdic[w];
End;
