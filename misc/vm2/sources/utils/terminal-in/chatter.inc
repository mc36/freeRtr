Var
  windowBeg:array[1..2] of LongInt;
  windowPos:array[1..2] of LongInt;
  windowSiz:LongInt;


{$include scroll.inc}



Procedure startEmulator;
Var
  i,o:LongInt;
  a:String;

Procedure put(p:LongInt;a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
case p of
  0:p:=o;
  1:p:=((screenMaxX-ab0) shr 1)+o;
  2:p:=screenMaxX-ab0+o;
  else exit;
  end;
for i:=1 to ab0 do begin;
  screenData[p].c:=ab[i];
  inc(p);
  end;
End;


Begin;
outputSize:=0;
for i:=0 to screenMaxX*screenMaxY-1 do begin;
  screenData[i].a:=$03;
  screenData[i].c:=$20;
  end;
screenPosX:=0;
screenPosY:=0;
windowSiz:=(screenMaxY-1) shr 1;
o:=windowSiz*screenMaxX;
for i:=o to o+screenMaxX-1 do begin;
  screenData[i].a:=$17;
  screenData[i].c:=$20;
  end;
put(0,'remote /\');
put(1,'chatter');
put(2,'\/ local');
windowBeg[1]:=0;
windowBeg[2]:=windowSiz+1;
for i:=1 to 2 do windowPos[i]:=0;
End;



Procedure gotOneChar(win,chb:LongInt);
Label scroll;
Begin;
case chb of
  13:goto scroll; {cr}
  10:goto scroll; {lf}
  8:begin; {backspace}
    if (windowPos[win]<1) then exit;
    dec(windowPos[win]);
    screenPosY:=windowBeg[win]+windowSiz-1;
    screenPosX:=windowPos[win];
    screenData[screenPosY*screenMaxX+screenPosX].c:=$20;
    exit;
    end;
  end;
screenPosY:=windowBeg[win]+windowSiz-1;
screenData[screenPosY*screenMaxX+windowPos[win]].c:=chb;
inc(windowPos[win]);
screenPosX:=windowPos[win];
if (windowPos[win]<screenMaxX) then exit;
scroll:
screenPosY:=windowBeg[win]+windowSiz-1;
doScrollUp(0,windowBeg[win],screenMaxX,screenPosY,1,$20,$03);
windowPos[win]:=0;
screenPosX:=0;
End;






Procedure gotRemoteChar(chb:Byte);
Begin;
gotOneChar(1,chb);
End;

Procedure gotLocalKey(w:Word);
Begin;
if (outputSize>=outputMax) then exit;
if (w and $fe00=$0000) then begin; {letter}
  inc(outputSize);
  outputData[outputSize]:=w;
  gotOneChar(2,w);
  exit;
  end;
if (w and $fe00=$0200) then begin; {ctrl+letter}
  w:=w and $1f;
  inc(outputSize);
  outputData[outputSize]:=w;
  gotOneChar(2,w);
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
gotOneChar(2,w);
End;
