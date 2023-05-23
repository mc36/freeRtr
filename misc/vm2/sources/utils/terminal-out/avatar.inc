Const
  TerminalIniter=#22#10#255#1#1#255#255#22#8#1#1;
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
  0:begin; {null}
    inc(con.inpS);
    con.inpB[1]:=0;
    con.inpM:=False;
    con.inpT:=CurrentTime;
    exit;
    end;
  1..31:begin; {ctrl+key}
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
if (con.inpS=1) then begin;
  if (ch=165) then begin; {alt+tab}
    inc(con.inpS);
    con.inpB[2]:=ch;
    con.inpM:=True;
    exit;
    end;
  con.inpS:=0;
  case ch of
    59..68:i:=ch-59+$8014; {f1..f10}
    84..93:i:=ch-84+$8114; {shfit+f1..shfit+f10}
    94..103:i:=ch-94+$8214; {ctrl+f1..ctrl+f10}
    104..113:i:=ch-104+$8414; {alt+f1..alt+f10}
    120..128:i:=ch-120+$0431; {alt+numbers}
    30:i:=$0461;  {alt+a}
    48:i:=$0461;  {alt+b}
    46:i:=$0461;  {alt+c}
    32:i:=$0461;  {alt+d}
    18:i:=$0461;  {alt+e}
    33:i:=$0461;  {alt+f}
    34:i:=$0461;  {alt+g}
    35:i:=$0461;  {alt+h}
    23:i:=$0461;  {alt+i}
    36:i:=$0461;  {alt+j}
    37:i:=$0461;  {alt+k}
    38:i:=$0461;  {alt+l}
    50:i:=$0461;  {alt+m}
    49:i:=$0461;  {alt+n}
    24:i:=$0461;  {alt+o}
    25:i:=$0461;  {alt+p}
    16:i:=$0461;  {alt+q}
    19:i:=$0461;  {alt+r}
    31:i:=$0461;  {alt+s}
    20:i:=$0461;  {alt+t}
    22:i:=$0461;  {alt+u}
    47:i:=$0461;  {alt+v}
    17:i:=$0461;  {alt+w}
    45:i:=$0461;  {alt+x}
    21:i:=$0461;  {alt+y}
    44:i:=$0461;  {alt+z}
    53:i:=$042f;  {alt+/}
    55:i:=$042a;  {alt+*}
    74:i:=$042d;  {alt+-}
    78:i:=$042b;  {alt++}
    82:i:=$8006;  {insert}
    83:i:=$8007;  {delete}
    72:i:=$800c;  {up}
    80:i:=$800d;  {down}
    75:i:=$800e;  {left}
    77:i:=$800f;  {right}
    71:i:=$8008;  {home}
    79:i:=$8009;  {end}
    73:i:=$800a;  {pgup}
    81:i:=$800b;  {pgdn}
    15:i:=$8102;  {shift+tab}
    26:i:=$045b;  {alt+[}
    27:i:=$045c;  {alt+\}
    39:i:=$045d;  {alt+]}
    40:i:=$043b;  {alt+;}
    41:i:=$0427;  {alt+'}
    43:i:=$0460;  {alt+`}
    51:i:=$042c;  {alt+,}
    52:i:=$042e;  {alt+.}
    14:i:=$8403;  {alt+backspace}
    28:i:=$8404;  {alt+enter}
    133:i:=$801e; {f11}
    134:i:=$801f; {f12}
    135:i:=$811e; {shift+f11}
    136:i:=$811f; {shift+f12}
    137:i:=$821e; {ctrl+f11}
    138:i:=$821f; {ctrl+f12}
    139:i:=$841e; {alt+f11}
    140:i:=$841f; {alt+f12}
    114:i:=$8010; {printscreen}
    115:i:=$820e; {ctrl+left}
    116:i:=$820f; {ctrl+right}
    119:i:=$8208; {ctrl+home}
    117:i:=$8209; {ctrl+end}
    132:i:=$820a; {ctrl+pgup}
    118:i:=$820b; {ctrl+pgdn}
    129:i:=$0430; {alt+0}
    130:i:=$042d; {alt+-}
    131:i:=$043d; {alt+=}
    165:i:=$8402; {alt+tab}
    148:i:=$8202; {ctrl+tab}
    152:i:=$840c; {alt+up}
    160:i:=$840d; {alt+down}
    155:i:=$840e; {alt+left}
    157:i:=$840f; {alt+right}
    162:i:=$8406; {alt+insert}
    163:i:=$8407; {alt+delete}
    151:i:=$8408; {alt+home}
    159:i:=$8409; {alt+end}
    153:i:=$840a; {alt+pgup}
    161:i:=$840b; {alt+pgdn}
    149:i:=$022f; {ctrl+/}
    150:i:=$022a; {ctrl+*}
    142:i:=$022d; {ctrl+-}
    144:i:=$022b; {ctrl++}
    146:i:=$8206; {ctrl+insert}
    147:i:=$8207; {ctrl+delete}
    end;
  inc(outputS);
  outputB[outputS]:=i;
  exit;
  end;
inc(con.inpS);
if (con.inpS>sizeof(con.inpB)) then begin;
  con.inpS:=0;
  exit;
  end;
con.inpB[con.inpS]:=ch;
case ch of
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
End;




Procedure ProcessOutputChar(var con:OneDataRecord;ch:LongInt);
Const
  ColorCodes:array[0..7] of string[1]=('0','4','2','6','1','5','3','7');
Var
  i,o,p:LongInt;
  w:Word;
  a:String;

Procedure put;
Begin;
if (ch>=32) then begin;
  inc(inputS);
  inputB[inputS]:=ch;
  exit;
  end;
inc(inputS);
inputB[inputS]:=25;
inc(inputS);
inputB[inputS]:=ch;
inc(inputS);
inputB[inputS]:=1;
End;

Begin;
if (con.outS=0) then begin;
  if (ch<>0) then begin;
    if (ch in [8,10,13]) then begin;
      inc(inputS);
      inputB[inputS]:=ch;
      end else put;
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
    a:=#22#10#255#1#1#255#255#22#8#1#1;
    move(a[1],inputB[inputS+1],length(a));
    inc(inputS,length(a));
    con.outS:=0;
    exit;
    end;
  $03:begin; {set current color, the next byte contains the color}
    if (con.outS<3) then exit;
    con.outS:=0;
    p:=con.outB[3];
    if (p=con.color) then exit;
    con.color:=p;
    a:=#22#1+chr(p and $7f);
    if (p and $80<>0) then a:=a+#22#2;
    move(a[1],inputB[inputS+1],length(a));
    inc(inputS,length(a));
    exit;
    end;
  $04:begin; {set cursor position, the next words are: x, y}
    if (con.outS<6) then exit;
    a:=#22#8+chr(con.outB[5])+chr(con.outB[3]);
    move(a[1],inputB[inputS+1],length(a));
    inc(inputS,length(a));
    con.outS:=0;
    exit;
    end;
  $05:begin; {write any special character}
    if (con.outS<3) then exit;
    con.outS:=0;
    ch:=con.outB[3];
    put;
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
