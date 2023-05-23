Const
  TerminalIniter={#27'[?7l'+} #27'[0m'+ #27'[2J' +#27'[1;1H';
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
Const
  keys1:array[0..3] of byte=(12,13,15,14);
  keys2:array[1..6] of byte=(8,6,7,9,10,11);
  keys3:array[1..14] of byte=(20,21,22,23,24,0,25,26,27,28,29,0,30,31);
Var
  a:String;
  i,o,p:LongInt;
Begin;
if (con.inpS=0) then case ch of
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
  27:begin; {escape}
    inc(con.inpS);
    con.inpB[1]:=27;
    con.inpM:=False;
    con.inpT:=CurrentTime;
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
if (con.inpS=1) then case ch of
  8:begin; {backspace}
    inc(outputS);
    outputB[outputS]:=$8403;
    con.inpS:=0;
    exit;
    end;
  9:begin; {tab}
    inc(con.inpS);
    con.inpB[2]:=ch;
    con.inpM:=True;
    exit;
    end;
  13:begin; {enter}
    inc(outputS);
    outputB[outputS]:=$8404;
    con.inpS:=0;
    exit;
    end;
  27,91:begin; {escape,'['}
    inc(con.inpS);
    con.inpB[2]:=ch;
    con.inpT:=CurrentTime;
    exit;
    end;
  1..31:begin; {ctrl+key}
    inc(outputS);
    outputB[outputS]:=ch+$0660;
    con.inpS:=0;
    exit;
    end;
  else begin;
    inc(outputS);
    outputB[outputS]:=ch+$0400;
    con.inpS:=0;
    exit;
    end;
  end;
inc(con.inpS);
if (con.inpS>sizeof(con.inpB)) then begin;
  con.inpS:=0;
  exit;
  end;
con.inpB[con.inpS]:=ch;
if con.inpM then case ch of
  8:begin; {backspace}
    dec(con.inpS,2);
    if (con.inpS<2) then con.inpS:=2;
    exit;
    end;
  27:begin; {escape}
    con.inpS:=2;
    exit;
    end;
  13:begin; {enter}
    move(con.inpB,a[1],255);
    move(con.inpS,a,1);
    con.inpS:=0;
    a:=copy(a,3,length(a)-3);
    i:=keyNamesCnv(a);
    if (i and $ff000000=$81000000) then begin;
      con.maxX:=(i shr 12) and $fff;
      con.maxY:=i and $fff;
      exit;
      end;
    if (i<0) then exit;
    inc(outputS);
    outputB[outputS]:=i;
    if (i<>$8001) then exit;
    inc(outputS);
    outputB[outputS]:=con.maxX;
    inc(outputS);
    outputB[outputS]:=con.maxY;
    exit;
    end;
  else exit;
  end;
if (ch in [$30..$39,$5b]) then exit;
move(con.inpB,a[1],255);
move(con.inpS,a,1);
con.inpS:=0;
if (copy(a,1,3)=#27#27'[') then begin; a:=copy(a,2,255);p:=$400; end else p:=0;
a:=copy(a,3,length(a)-3);
case ch of
  65..68:begin; {A..D}
    inc(outputS);
    outputB[outputS]:=p+keys1[ch-65]+$8000;
    exit;
    end;
  126:begin; {'~'}
    i:=BVal(a);
    if (i in [1..6]) then begin;
      inc(outputS);
      outputB[outputS]:=p+keys2[i]+$8000;
      exit;
      end;
    if (i in [11..24]) then begin;
      inc(outputS);
      outputB[outputS]:=p+keys3[i-10]+$8000;
      exit;
      end;
    exit;
    end;
  else exit;
  end;
End;




Procedure ProcessOutputChar(var con:OneDataRecord;ch:LongInt);
Const
  ColorCodes:array[0..7] of string[1]=('0','4','2','6','1','5','3','7');
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
    a:=#27'[1;1H'#27'[2J';
    move(a[1],inputB[inputS+1],length(a));
    inc(inputS,length(a));
    con.outS:=0;
    exit;
    end;
  $03:begin; {set current color, the next byte contains the color}
    if (con.outS<3) then exit;
    con.outS:=0;
    p:=con.outB[3];
    o:=con.color;
    if (o=p) then exit;
    con.color:=p;
    a:='';
    if ((o and $80<>0) and (p and $80=0)) or ((o and 8<>0) and (p and 8=0)) then begin;
      a:='0;';
      o:=(p and $77) xor $77;
      end;
    if (p and $80<>0) and (o and $80=0) then a:=a+'5;';
    if (p and 8<>0) and (o and 8=0) then a:=a+'1;';
    if (o and $70<>p and $70) then a:=a+'4'+ColorCodes[(p shr 4) and 7]+';';
    if (o and 7<>p and 7) then a:=a+'3'+ColorCodes[p and 7]+';';
    a:=#27'['+copy(a,1,length(a)-1)+'m';
    move(a[1],inputB[inputS+1],length(a));
    inc(inputS,length(a));
    exit;
    end;
  $04:begin; {set cursor position, the next words are: x, y}
    if (con.outS<6) then exit;
    i:=0;o:=0;
    move(con.outB[5],w,sizeof(w));
    i:=w;
    move(con.outB[3],w,sizeof(w));
    o:=w;
    a:=#27'['+BStr(i)+';'+BStr(o)+'H';
    move(a[1],inputB[inputS+1],length(a));
    inc(inputS,length(a));
    con.outS:=0;
    exit;
    end;
  $05:begin; {write any special character}
    if (con.outS<3) then exit;
    i:=con.outB[3];
    if (i<32) then i:=46;
    inc(inputS);
    inputB[inputS]:=i;
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
if (con.inpS=1) then begin;
  inc(outputS);
  outputB[outputS]:=$8005;
  end;
if (con.inpS=2) then begin;
  if (con.inpB[2]=$5b) then i:=$5b else i:=5;
  inc(outputS);
  outputB[outputS]:=$8400+i;
  end;
con.inpS:=0;
f1:
pipeLineSend(con.inpP,inputB,inputS);
pipeLineSend(con.outP,outputB,outputS*2);
End;
