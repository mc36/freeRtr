Const
  TerminalIniter=#12#13#32#13;
Var
  inputS:LongInt;
  OutputS:LongInt;
  inputB:array[1..2048] of byte;
  OutputB:array[1..1024] of Word;
Type
  OneDataRecord=record
    inpP:LongInt;               {input pipeline}
    inpS:LongInt;               {input buffer size}
    inpB:array[1..64] of byte;  {input buffer data}
    inpT:LongInt;               {input last time}
    inpM:Boolean;               {input mode}
    color:Byte;                 {current color}
    outP:LongInt;               {output pipeline}
    outS:LongInt;               {output buffer size}
    outB:array[1..64] of byte;  {output buffer data}
    maxX:LongInt;               {screen x size}
    maxY:LongInt;               {screen y size}
    end;



Procedure ProcessInputChar(var con:OneDataRecord;ch:LongInt);
Var
  a:String;
  i,o,p:LongInt;
Begin;
case ch of
  8:begin; {backspace}
    inc(outputS);
    outputB[outputS]:=$8003;
    exit;
    end;
  9:begin; {tab}
    inc(outputS);
    outputB[outputS]:=$8002;
    exit;
    end;
  13:begin; {enter}
    inc(outputS);
    outputB[outputS]:=$8004;
    exit;
    end;
  10:begin; {ctrl+enter}
    inc(outputS);
    outputB[outputS]:=$8204;
    exit;
    end;
  27:begin; {escape}
    inc(outputS);
    outputB[outputS]:=$8005;
    exit;
    end;
  0..31:begin; {ctrl+key}
    inc(outputS);
    outputB[outputS]:=ch+$0260;
    exit;
    end;
  else begin; {any key}
    inc(outputS);
    outputB[outputS]:=ch;
    exit;
    end;
  end;
End;




Procedure ProcessOutputChar(var con:OneDataRecord;ch:LongInt);
Var
  i,o,p:LongInt;
  w:Word;
  a:String;
Begin;
if (con.outS=0) then begin;
  if (ch<>0) then begin;
    inc(inputS);
    inputB[inputS]:=ch;
    exit;
    end;
  con.outS:=1;
  con.outB[1]:=0;
  exit;
  end;
inc(con.outS);
con.outB[con.outS]:=ch;
case con.outB[2] of
  $00:begin; {no operation}
    con.outS:=0;
    exit;
    end;
  $01:begin; {report console resolution in a redraw, x, y sequence}
    inc(outputS);
    outputB[outputS]:=$8001;
    inc(outputS);
    outputB[outputS]:=con.maxX;
    inc(outputS);
    outputB[outputS]:=con.maxY;
    con.outS:=0;
    exit;
    end;
  $02:begin; {clear the screen to the current color}
    con.outS:=0;
    exit;
    end;
  $03:begin; {set current color, the next byte contains the color}
    if (con.outS<3) then exit;
    con.outS:=0;
    exit;
    end;
  $04:begin; {set cursor position, the next words are: x, y}
    if (con.outS<6) then exit;
    con.outS:=0;
    exit;
    end;
  $05:begin; {write any special character}
    if (con.outS<3) then exit;
    con.outS:=0;
    exit;
    end;
  $06:begin; {report capabilities in a noop; 0}
    inc(outputS);
    outputB[outputS]:=$8000;
    inc(outputS);
    outputB[outputS]:=0;
    con.outS:=0;
    exit;
    end;
  else con.outS:=0;
  end;
End;





Function doConn(var con:OneDataRecord):Boolean;
Label f1;
Var
  buf:array[1..64] of byte;
  i,o,p:LongInt;
Begin;
doConn:=False;
OutputS:=0;
InputS:=0;
pipeLineStats(con.inpP,i,o,p);
if (i=0) then begin; doConn:=true;exit; end;
while (1=1) do begin;
  if (p-inputS<512) then break;
  o:=sizeof(buf);
  pipeLineRecv(con.outP,buf,o);
  if (o<1) then break;
  for i:=1 to o do ProcessOutputChar(con,buf[i]);
  end;
pipeLineStats(con.outP,i,o,p);
if (i=0) then begin; doConn:=true;exit; end;
while (1=1) do begin;
  if (p-outputS*2<512) then break;
  o:=sizeof(buf);
  pipeLineRecv(con.inpP,buf,o);
  if (o<1) then break;
  for i:=1 to o do ProcessInputChar(con,buf[i]);
  end;
if (con.inpS=0) then goto f1;
if con.inpM then goto f1;
if (GetTimePast(con.inpT)<1) then goto f1;
con.inpS:=0;
f1:
pipeLineSend(con.inpP,inputB,inputS);
pipeLineSend(con.outP,outputB,outputS*2);
End;
