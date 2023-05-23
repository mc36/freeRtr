Var
  lineMax:LongInt;
  linePos:LongInt;
  txtBase:LongInt;



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

lineMax:=(screenMaxX-5) div 4;
txtBase:=screenMaxX-lineMax-2;
linePos:=0;
End;





Procedure gotRemoteChar(chb:Byte);
Const
  hexDigits:array[0..15] of LongInt=($30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$41,$42,$43,$44,$45,$46);
Var
  chc:Char absolute chb;
  i,o:LongInt;
Begin;
i:=screenPosY*screenMaxX;
screenData[i+txtBase+linePos].c:=chb;
inc(i,linePos*3);
screenData[i+2].c:=hexDigits[chb shr 4];
screenData[i+3].c:=hexDigits[chb and $f];
linePos:=(linePos+1) mod lineMax;
screenPosX:=(linePos*3)+2;
if (linePos>0) then exit;
inc(screenPosY);
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
  outputData[outputSize]:=w;
  exit;
  end;
if (w and $fe00=$0200) then begin; {ctrl+letter}
  inc(outputSize);
  outputData[outputSize]:=w and $1f;
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
outputData[outputSize]:=w;
End;
