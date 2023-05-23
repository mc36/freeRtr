Const
  txBeg='tx: ';
  rxBeg='rx: ';
Var
  bufferDat:array[1..256] of byte;
  bufferSiz:LongInt;


Procedure gotBasicChar(chb:Byte);
Label vege;
Var i,o:LongInt;
Begin;
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



Procedure sendString(a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
for i:=1 to ab0 do begin;
  inc(outputSize);
  outputData[outputSize]:=ab[i];
  end;
End;

Procedure displayString(a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
a:=a+#13#10;
for i:=1 to ab0 do gotBasicChar(ab[i]);
End;


Procedure gotCommand;
Var
  a,b,c:String;
  i:LongInt;
Begin;
case bufferDat[2] of
  251:begin; a:='will';  i:=254;c:='don''t'; end;
  252:begin; a:='won''t';i:=254;c:='don''t'; end;
  253:begin; a:='do';    i:=252;c:='won''t'; end;
  254:begin; a:='don''t';i:=252;c:='won''t'; end;
  else exit;
  end;
case bufferDat[3] of
  0  :begin; i:=-1;b:='binary'; end;
  1  :begin; i:=-1;b:='echo'; end;
  3  :begin; i:=-1;b:='suppress-go-ahead'; end;
  5  :b:='status';
  6  :b:='timing-mark';
  20 :b:='data-entry-terminal';
  23 :b:='send-location';
  24 :b:='terminal-type';
  25 :b:='end-of-record';
  26 :b:='tacacs-uid';
  27 :b:='outmrk';
  28 :b:='ttyloc';
  29 :b:='3270-regime';
  31 :b:='window-size';
  32 :b:='terminal-speed';
  33 :b:='toggle-flow-control';
  34 :b:='linemode';
  35 :b:='x-display-location';
  36 :b:='environ';
  37 :b:='authentication';
  38 :b:='encrypt';
  39 :b:='new-environ';
  42 :b:='charset';
  44 :b:='com-port-option';
  47 :b:='kermit';
  255:b:='extended-options-list';
  else b:='? ('+BStr(bufferDat[3]);
  end;
displayString(rxBeg+a+' '+b);
if (i<0) then exit;
displayString(txBeg+c+' '+b);
sendString(#255+chr(i)+chr(bufferDat[3]));
End;




Procedure startEmulator;
Var i:LongInt;
Begin;
outputSize:=0;
bufferSiz:=0;
for i:=0 to screenMaxX*screenMaxY-1 do begin;
  screenData[i].a:=$03;
  screenData[i].c:=$20;
  end;
screenPosX:=0;
screenPosY:=0;
sendString(#255#251#0);displayString(txBeg+'will binary');
sendString(#255#253#0);displayString(txBeg+'do binary');
sendString(#255#252#1);displayString(txBeg+'won''t echo');
sendString(#255#253#1);displayString(txBeg+'do echo');
sendString(#255#252#3);displayString(txBeg+'won''t suppress-go-ahead');
sendString(#255#253#3);displayString(txBeg+'do suppress-go-ahead');
End;





Procedure gotRemoteChar(chb:Byte);
Begin;
if (bufferSiz>0) then begin;
  inc(bufferSiz);
  bufferDat[bufferSiz]:=chb;
  if (bufferSiz<3) then exit;
  gotCommand;
  bufferSiz:=0;
  exit;
  end;
if (chb=255) then begin;
  bufferSiz:=1;
  bufferDat[1]:=255;
  exit;
  end;
gotBasicChar(chb);
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
