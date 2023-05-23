Const
  modemVersion='v1.0';
  modemProggy='modemEmu';
Type
  modemProfileRecord=record
    commandEcho:Boolean;        {echo input commands}
    connectEcho:Boolean;        {echo input data}
    verbose:Boolean;            {verbose output}
    errorOk:Boolean;            {generate replies}
    connRing:Boolean;           {connect, ring replies}
    autoAnswer:Byte;            {rings before answer counter}
    ringCounter:Byte;           {number of rings sawn}
    escapeChar:Byte;            {escape sequence character}
    carrigeChar:Char;           {carrige return character}
    linefeedChar:Char;          {line feed character}
    backspaceChar:Char;         {backspace character}
    connectMsg:Byte;            {connect msg: 1=spd, 4=crr, 8=prt, 32=cmprs}
    pulseDial:Boolean;          {pulse dialing}
    carrierAlways:Boolean;      {carrier always on}
    dtrLowAction:Byte;          {dtr goes low do: 0=none, 1=cmd, 2=hangup}
    flowControl:Boolean;        {rts/ctr flow control for connection}
    end;
  modemStateRecord=record
    dataPipe:LongInt;           {data pipeline}
    ctrlPipe:LongInt;           {control pipeline}
    linkName:String;            {name of link}
    lastSign:LongInt;           {last signal status}
    state:LongInt;              {state: 0=cmd, 1=dial, 2=answer, 3=connect}
    connected:Boolean;          {connected state}
    curr:modemProfileRecord;    {current profile}
    register:Byte;              {register selected}
    lastCmd:String;             {last command issued}
    rxBuf:String;               {rx buffer}
    txSiz:LongInt;              {tx buffer size}
    disconnReason:Byte;         {disconnect reason: 0=none, 1=remote, 2=local}
    txBuf:array[1..1024] of char;               {tx buffer}
    phoneBook:array[0..3] of string[63];        {phonebook entries}
    prof:array[0..1] of modemProfileRecord;     {profiles}
    end;

Var ignoreBadCommands:Boolean;

Procedure modemInitialize(var d:modemProfileRecord);
Begin;
fillchar(d,sizeof(d),0);
d.commandEcho:=True;
d.verbose:=True;
d.errorOk:=True;
d.connRing:=True;
d.escapeChar:=43;
d.carrigeChar:=#13;
d.linefeedChar:=#10;
d.backspaceChar:=#8;
d.connectMsg:=1;
d.dtrLowAction:=2;
d.flowControl:=True;
End;

Function modemAddString(var d:modemStateRecord;a:String):Boolean;
Var i:LongInt;
Begin;
modemAddString:=True;
i:=length(a);
if (d.txSiz+i>sizeof(d.txBuf)) then exit;
move(a[1],d.txBuf[d.txSiz+1],i);
inc(d.txSiz,i);
modemAddString:=False;
End;

Function modemAddResponse(var d:modemStateRecord;n:LongInt):Boolean;
Var a:String;
Begin;
modemAddResponse:=True;
case n of
  0:a:='OK';
  1:a:='CONNECT';
  2:a:='RING';
  3:a:='NO CARRIER';
  4:a:='ERROR';
  6:a:='NO DIALTONE';
  7:a:='BUSY';
  8:a:='NO ANSWER';
  15:a:='CONNECT 14400';
  52:a:='CARRIER 14400';
  67:a:='COMPRESSION: V.42BIS';
  77:a:='PROTOCOL: LAP-M';
  else exit;
  end;
if not d.curr.verbose then begin;
  a:=BStr(n)+d.curr.carrigeChar;
  end else begin;
  a:=a+d.curr.carrigeChar+d.curr.linefeedChar;
  end;
modemAddResponse:=modemAddString(d,a);
End;

Procedure modemAddRingConn(var d:modemStateRecord;stat:byte);
Begin;
case stat of
  0:begin; {ok}
    if not d.curr.errorOk then exit;
    if d.curr.commandEcho then modemAddString(d,d.curr.carrigeChar);
    if d.curr.verbose then modemAddString(d,d.curr.linefeedChar);
    modemAddResponse(d,0);
    end;
  1:begin; {connect}
    if not d.curr.connRing then exit;
    if (d.curr.connectMsg and $04<>0) then modemAddResponse(d,52);
    if (d.curr.connectMsg and $08<>0) then modemAddResponse(d,77);
    if (d.curr.connectMsg and $20<>0) then modemAddResponse(d,67);
    if (d.curr.connectMsg and $01<>0) then modemAddResponse(d,15) else modemAddResponse(d,1);
    end;
  2:begin; {ring}
    if not d.curr.connRing then exit;
    modemAddResponse(d,2);
    end;
  3:begin; {no carrier}
    if not d.curr.connRing then exit;
    modemAddResponse(d,3);
    end;
  end;
End;




Procedure modemProcessChar(var d:modemStateRecord;c:char);

Procedure getCh;
Begin;
c:=d.rxBuf[1];
if (d.rxBuf='') then c:=#0;
End;

Procedure delCh;
Begin;
d.rxBuf:=copy(d.rxBuf,2,255);
End;

Function getNum:LongInt;
Var a:String;
Label f1;
Begin;
a:='';
f1:
getCh;
if (c in ['0'..'9']) then begin;
  a:=a+c;
  delCh;
  goto f1;
  end;
getNum:=BVal(a);
End;

Function getDial(var n:LongInt):String;
Var
  a:String;
  r:Byte;
Label f1;
Begin;
if d.curr.pulseDial then a:='p' else a:='t';
getDial:='';
n:=-1;
r:=1;
f1:
getCh;
c:=lowCase(c);
if (c=#0) then begin;
  n:=r and $7f;
  delCh;
  getDial:=a;
  exit;
  end;
if (c in ['0'..'9','a'..'d','#','*']) then begin;
  a:=a+c;
  delCh;
  goto f1;
  end;
if (c in ['p','t']) then begin; delCh;goto f1; end;
if (c in ['(',')','-',' ']) then begin; delCh;goto f1; end;
if (c in ['w',',','@','!','^']) then begin; delCh;goto f1; end;
if (c=';') then begin;
  n:=r or $80;
  delCh;
  getDial:=a;
  exit;
  end;
if (c='s') then begin;
  delCh;
  getCh;
  if (c<>'=') then exit;
  delCh;
  getCh;
  if (c<'0') or (c>'3') then exit;
  delCh;
  a:=a+d.phoneBook[BVal(c)];
  goto f1;
  end;
if (c='r') then begin;
  delCh;
  r:=3-r;
  goto f1;
  end;
exit;
End;

Procedure writeReg(var p:modemProfileRecord;r,v:Byte);
Begin;
case r of
  000:p.autoAnswer:=v;
  001:p.ringCounter:=v;
  002:p.escapeChar:=v;
  003:p.carrigeChar:=chr(v);
  004:p.linefeedChar:=chr(v);
  005:p.backspaceChar:=chr(v);
  014:begin;
    p.connectEcho:=(v and $01<>0);
    p.commandEcho:=(v and $02<>0);
    p.errorOk:=(v and $04=0);
    p.verbose:=(v and $08<>0);
    p.connRing:=(v and $10=0);
    p.pulseDial:=(v and $20<>0);
    end;
  021:begin;
    p.carrierAlways:=(v and $20=0);
    v:=(v shr 3) and $03;
    if (v>2) then v:=2;
    p.dtrLowAction:=v;
    end;
  039:begin;
    p.flowControl:=((v and $07) in [1,3]);
    end;
  095:p.connectMsg:=v;
  end;
End;

Function readReg(var p:modemProfileRecord;r:Byte):Byte;
Begin;
case r of
  000:r:=p.autoAnswer; {ring to answer}
  001:r:=p.ringCounter; {ring count}
  002:r:=p.escapeChar; {escape sequence character}
  003:r:=ord(p.carrigeChar); {carriage return character}
  004:r:=ord(p.linefeedChar); {line feed character}
  005:r:=ord(p.backspaceChar); {backspace character}
  006:r:=2; {wait before blind dialing}
  007:r:=45; {wait for carrier after dialing}
  008:r:=2; {duration of delay for comma dial modifier}
  009:r:=6; {carrier detect response time}
  010:r:=14; {delay between lost carrier and hang up}
  011:r:=95; {multi-frequency tone duration}
  012:r:=50; {escape sequence guard time}
  014:begin; {general bit mapped options status}
    r:=$80;
    if p.connectEcho then inc(r,$01);
    if p.commandEcho then inc(r,$02);
    if not p.errorOk then inc(r,$04);
    if p.verbose then inc(r,$08);
    if not p.connRing then inc(r,$10);
    if p.pulseDial then inc(r,$20);
    end;
  016:r:=0; {general bit mapped test options status}
  018:r:=0; {modem test timer}
  019:r:=0; {autosync bit mapped options}
  020:r:=0; {autosync hdlc address or bsc sync character}
  021:begin; {v24/general bit mapped options status}
    r:=(p.dtrLowAction shl 3) or $04;
    if not p.carrierAlways then inc(r,$20);
    end;
  022:r:=117; {speaker/results bit mapped options status}
  023:r:=62; {general bit mapped options status}
  024:r:=0; {sleep inactivity timer}
  025:r:=5; {dtr detection}
  026:r:=1; {rts to cts interval}
  027:r:=73; {bit mapped options status}
  028:r:=0; {bit mapped options status}
  029:r:=70; {flash dial modifier time}
  030:r:=0; {inactivity timeout}
  031:r:=194; {bit mapped options status}
  032:r:=17; {xon character}
  033:r:=19; {xoff character}
  036:r:=7; {negotiation failure treatment}
  037:r:=0; {desired dce line speed}
  038:r:=20; {delay before forced hang up}
  039:begin; {flow control bit mapped options status}
    r:=0;
    if p.flowControl then inc(r,$03);
    end;
  040:r:=104; {general bit mapped options status}
  041:r:=195; {general bit mapped options status}
  044:r:=3; {asynchronous framing technique selection}
  046:r:=138; {error-control protocol selection}
  048:r:=7; {enabling/disabling feature negotiation}
  049:r:=8; {asb buffer size lower limit}
  050:r:=16; {asb buffer size upper limit}
  053:r:=0; {global pad configuration}
  063:r:=0; {leased line carrier level}
  069:r:=15; {link layer window size}
  070:r:=10; {maximum number of retransmissions}
  071:r:=20; {link layer timeout}
  072:r:=30; {loss of flag idle timeout}
  073:r:=5; {no activity timeout}
  074:r:=0; {minimum incoming logical channel number high}
  075:r:=1; {minimum incoming logical channel number low}
  076:r:=40; {maximum incoming logical channel number high}
  077:r:=95; {maximum incoming logical channel number high}
  078:r:=0; {outgoing logical channel number high}
  079:r:=16; {outgoing logical channel number low}
  080:r:=1; {packet layer n20 parameter}
  081:r:=18; {packet layer t20 parameter}
  082:r:=128; {break signaling technique}
  084:r:=130; {adaptive start up negotiation (asu)}
  085:r:=129; {asu negotiation report}
  086:r:=0; {connection failure cause}
  091:r:=10; {pstn transmit attenuation level}
  092:r:=10; {fax transmit attenuation level}
  093:r:=9; {v25bis dte interface speed}
  094:r:=0; {command mode selector}
  095:r:=p.connectMsg; {negotiation message options}
  097:r:=30; {v32 automdode v22/v22bis probe timing}
  else r:=0;
  end;
readReg:=r;
End;

Function conv2reg(b:byte):string;
Var a:String;
Begin;
a:=BStr(b);
while (length(a)<3) do a:='0'+a;
conv2reg:=a;
End;

Function composeLine(var p:modemProfileRecord;dat:String):String;
Var
  a:String;
  i,o:LongInt;
Begin;
a:='';
while (dat<>'') do begin;
  o:=ord(dat[1]);
  i:=readReg(p,o);
  dat:=copy(dat,2,255);
  a:=a+' S'+copy(conv2reg(o),2,255)+':'+conv2reg(i);
  end;
composeLine:=copy(a,2,255);
End;

Procedure showProf(var p:modemProfileRecord);
Var a,b:String;
Begin;
b:=d.curr.carrigeChar+d.curr.linefeedChar;
modemAddString(d,'B1 ');
if p.commandEcho then a:='1' else a:='0';
modemAddString(d,'E'+a+' ');
if p.connectEcho then a:='0' else a:='1';
modemAddString(d,'F'+a+' L1 M1 N1 ');
a:='0';
if not p.errorOk then a:='1';
if not p.connRing then a:='2';
modemAddString(d,'Q'+a+' ');
if not p.pulseDial then a:='T' else a:='P';
modemAddString(d,a+' ');
if p.verbose then a:='1' else a:='0';
modemAddString(d,'V'+a+' ');
if (p.connectMsg<>0) then a:='1' else a:='0';
modemAddString(d,'W'+a+' X0 Y0 ');
if p.carrierAlways then a:='0' else a:='1';
modemAddString(d,'&C'+a+' &D'+BStr(p.dtrLowAction)+' &G0 ');
if p.flowControl then a:='3' else a:='0';
modemAddString(d,'&K'+a+b);
modemAddString(d,'&L0 &M0 &Q5 &R1 &S0 &X0 &Y0 %C3 %E2 \A3 \K5 \N6 \V1'+b);
modemAddString(d,composeLine(p,#0#1#2#3#4#5#6#7#8#9)+b);
modemAddString(d,composeLine(p,#10#11#12#18#25#26#36#37#38#46)+b);
modemAddString(d,composeLine(p,#48#95)+b);
modemAddString(d,b);
End;

Procedure showPhon;
Var i:LongInt;
Begin;
modemAddString(d,'TELEPHONE NUMBERS:'+d.curr.carrigeChar+d.curr.linefeedChar);
for i:=0 to 3 do
 modemAddString(d,BStr(i)+'='+d.phoneBook[i]+d.curr.carrigeChar+d.curr.linefeedChar);
End;


Label f1,f2,f3;
Var
  i,o:LongInt;
  a:String;
Begin;
if (d.state<>0) then begin;
  if (d.state>2) then exit;
  if d.curr.connRing then modemAddResponse(d,3);
  d.state:=0;
  d.rxBuf:='';
  exit;
  end;
i:=length(d.rxBuf);
if (i=0) then begin;
  if (lowCase(c)<>'a') then exit;
  if d.curr.commandEcho then modemAddString(d,c);
  d.rxBuf:='a';
  exit;
  end;
if (i=1) then begin;
  if (c='/') then begin;
    if d.curr.commandEcho then modemAddString(d,c);
    d.rxBuf:=d.lastCmd;
    c:=d.curr.carrigeChar;
    goto f3;
    end;
  if (lowCase(c)<>'t') then exit;
  if d.curr.commandEcho then modemAddString(d,c);
  d.rxBuf:='at';
  exit;
  end;
if (c>#31) then begin;
  if (i>128) then exit;
  if d.curr.commandEcho then modemAddString(d,c);
  d.rxBuf:=d.rxBuf+c;
  exit;
  end;
if (c=d.curr.backspaceChar) then begin;
  if (i<=2) then exit;
  if d.curr.commandEcho then modemAddString(d,c);
  d.rxBuf:=copy(d.rxBuf,1,i-1);
  exit;
  end;
if (c<>d.curr.carrigeChar) then exit;
f3:
WriteLn(d.linkName+': '+d.rxBuf);
d.lastCmd:=d.rxBuf;
d.rxBuf:=copy(d.rxBuf,3,255);
if d.curr.commandEcho then modemAddString(d,d.curr.carrigeChar);
if d.curr.verbose then modemAddString(d,d.curr.linefeedChar);
f1:
if (d.rxBuf='') then begin;
  if d.curr.errorOk then modemAddResponse(d,0);
  exit;
  end;
getCh;
c:=lowCase(c);
if (c=' ') then begin;
  delCh;
  goto f1;
  end;
if (c='a') then begin; {answer command}
  if d.connected then goto f2;
  delCh;
  d.state:=2;
  exit;
  end;
if (c='b') then begin; {select communication standard}
  delCh;
  getNum;
  goto f1;
  end;
if (c='c') then begin; {carrier control selection}
  delCh;
  getNum;
  goto f1;
  end;
if (c='d') then begin; {dial command}
  if d.connected then goto f2;
  delCh;
  a:=getDial(i);
  if (i<0) then goto f2;
  d.state:=i and $7f;
  if (i and $80=0) then exit;
  goto f1;
  end;
if (c='e') then begin; {command state character echo selection}
  delCh;
  i:=getNum;
  d.curr.commandEcho:=(i and 1<>0);
  goto f1;
  end;
if (c='f') then begin; {on-line state character echo selection}
  delCh;
  i:=getNum;
  d.curr.connectEcho:=(i and 1=0);
  goto f1;
  end;
if (c='h') then begin; {hook command options}
  delCh;
  i:=getNum;
  if (i and 1<>0) then goto f1;
  d.connected:=false;
  d.state:=0;
  goto f1;
  end;
if (c='i') then begin; {internal memory tests}
  delCh;
  i:=getNum;
  case i of
    0:modemAddString(d,'144'+d.curr.carrigeChar+d.curr.linefeedChar); {display product code}
    1:modemAddString(d,'255'+d.curr.carrigeChar+d.curr.linefeedChar); {display ROM checksum}
    2:modemAddResponse(d,0); {perform ROM checksum test}
    3:modemAddString(d,modemVersion+'-V34_DLS'+d.curr.carrigeChar+d.curr.linefeedChar); {firmware version}
    4:begin; {reports OEM defined identifier string}
      modemAddString(d,'MODEM CHIP : '+modemProggy+' '+modemVersion+d.curr.carrigeChar+d.curr.linefeedChar);
      modemAddString(d,'DATA + NO FAX + NO VOICE'+d.curr.carrigeChar+d.curr.linefeedChar);
      end;
    5:modemAddString(d,'022'+d.curr.carrigeChar+d.curr.linefeedChar); {reports country code}
    6:modemAddString(d,modemProggy+'144DPF-PPL Rev '+modemVersion+d.curr.carrigeChar+d.curr.linefeedChar); {reports data pump}
    7:modemAddString(d,'000'+d.curr.carrigeChar+d.curr.linefeedChar); {reports the DAA code}
    else goto f2;
    end;
  goto f1;
  end;
if (c='l') then begin; {speaker volume level selection}
  delCh;
  getNum;
  goto f1;
  end;
if (c='m') then begin; {speaker on/off selection}
  delCh;
  getNum;
  goto f1;
  end;
if (c='n') then begin; {negotiation of handshake options}
  delCh;
  getNum;
  goto f1;
  end;
if (c='o') then begin; {on-line command}
  if not d.connected then goto f2;
  delCh;
  getNum;
  d.state:=3;
  if d.curr.connRing then modemAddResponse(d,1);
  exit;
  end;
if (c='p') then begin; {select pulse dialing method}
  d.curr.pulseDial:=True;
  delCh;
  goto f1;
  end;
if (c='q') then begin; {result code display options}
  delCh;
  i:=getNum;
  d.curr.errorOk:=(i<1);
  d.curr.connRing:=(i<2);
  goto f1;
  end;
if (c='s') then begin; {addresses an s register}
  delCh;
  d.register:=getNum;
  goto f1;
  end;
if (c='=') then begin; {write to an s register}
  delCh;
  i:=getNum;
  writeReg(d.curr,d.register,i);
  goto f1;
  end;
if (c='?') then begin; {read an s register}
  delCh;
  i:=readReg(d.curr,d.register);
  modemAddString(d,conv2reg(i)+d.curr.carrigeChar+d.curr.linefeedChar);
  goto f1;
  end;
if (c='t') then begin; {select tone dialing method}
  d.curr.pulseDial:=False;
  delCh;
  goto f1;
  end;
if (c='v') then begin; {result code format options}
  delCh;
  i:=getNum;
  d.curr.verbose:=(i and 1<>0);
  goto f1;
  end;
if (c='w') then begin; {negotiation progress message selection}
  delCh;
  i:=getNum;
  if (i=1) then i:=63 else i:=0;
  d.curr.connectMsg:=i;
  goto f1;
  end;
if (c='x') then begin; {call progress options}
  delCh;
  getNum;
  goto f1;
  end;
if (c='y') then begin; {long space disconnect options}
  delCh;
  getNum;
  goto f1;
  end;
if (c='z') then begin; {soft reset command}
  delCh;
  i:=getNum and $01;
  d.curr:=d.prof[i];
  d.connected:=false;
  d.state:=0;
  goto f1;
  end;
if (c='&') then begin; {extended commands}
  delCh;
  getCh;
  c:=lowCase(c);
  if (c='h') then begin; {help}
    delCh;
    getNum;
    modemAddString(d,'supported commands: a d e f h i o p q s = ? t v w z &c &d &k &v &w &z'+d.curr.carrigeChar+d.curr.linefeedChar);
    goto f1;
    end;
  if (c='b') then begin; {v32 auto retrain options}
    delCh;
    getNum;
    goto f1;
    end;
  if (c='c') then begin; {data carrier detect options}
    delCh;
    i:=getNum;
    d.curr.carrierAlways:=(i and 1=0);
    goto f1;
    end;
  if (c='d') then begin; {data terminal ready options}
    delCh;
    i:=getNum;
    if (i>2) then i:=2;
    d.curr.dtrLowAction:=i;
    goto f1;
    end;
  if (c='f') then begin; {recall factory profile}
    delCh;
    modemInitialize(d.curr);
    goto f1;
    end;
  if (c='g') then begin; {guard tone selection}
    delCh;
    getNum;
    goto f1;
    end;
  if (c='j') then begin; {jack type selection}
    delCh;
    getNum;
    goto f1;
    end;
  if (c='k') then begin; {local flow control options}
    delCh;
    i:=getNum;
    d.curr.flowControl:=(i in [1,3]);
    goto f1;
    end;
  if (c='l') then begin; {line type selection}
    delCh;
    getNum;
    goto f1;
    end;
  if (c='m') then begin; {select direct asynchronous mode}
    delCh;
    getNum;
    goto f1;
    end;
  if (c='p') then begin; {set pulse dial make/break}
    delCh;
    getNum;
    goto f1;
    end;
  if (c='q') then begin; {communications mode options}
    delCh;
    getNum;
    goto f1;
    end;
  if (c='r') then begin; {rts/ctr options}
    delCh;
    getNum;
    goto f1;
    end;
  if (c='s') then begin; {data set ready options}
    delCh;
    getNum;
    goto f1;
    end;
  if (c='t') then begin; {test options}
    delCh;
    getNum;
    goto f1;
    end;
  if (c='u') then begin; {trellis coding options}
    delCh;
    getNum;
    goto f1;
    end;
  if (c='v') then begin; {view configuration profiles}
    delCh;
    i:=getNum;
    if (i=0) then begin;
      modemAddString(d,'ACTIVE PROFILE:'+d.curr.carrigeChar+d.curr.linefeedChar);
      showProf(d.curr);
      showPhon;
      modemAddString(d,d.curr.carrigeChar+d.curr.linefeedChar);
      goto f1;
      end;
    if (i=1) then begin;
      case d.disconnReason of
        0:a:='NONE';
        1:a:='REMOTE';
        2:a:='LOCAL';
        else a:='UNKNOWN';
        end;
      if d.connected then a:='CONNECTED';
      modemAddString(d,'TERMINATION REASON.......... '+a+d.curr.carrigeChar+d.curr.linefeedChar);
      modemAddString(d,'LAST TX rate................ 14400'+d.curr.carrigeChar+d.curr.linefeedChar);
      modemAddString(d,'HIGHEST TX rate............. 14400'+d.curr.carrigeChar+d.curr.linefeedChar);
      modemAddString(d,'LAST RX rate................ 14400'+d.curr.carrigeChar+d.curr.linefeedChar);
      modemAddString(d,'HIGHEST RX rate............. 14400'+d.curr.carrigeChar+d.curr.linefeedChar);
      modemAddString(d,'PROTOCOL.................... LAP-M'+d.curr.carrigeChar+d.curr.linefeedChar);
      modemAddString(d,'COMPRESSION................. V.42BIS'+d.curr.carrigeChar+d.curr.linefeedChar);
      modemAddString(d,'Line QUALITY................ 015'+d.curr.carrigeChar+d.curr.linefeedChar);
      modemAddString(d,'RX LEVEL.................... 043'+d.curr.carrigeChar+d.curr.linefeedChar);
      modemAddString(d,'Highest RX State............ FF'+d.curr.carrigeChar+d.curr.linefeedChar);
      modemAddString(d,'Highest TX State............ FF'+d.curr.carrigeChar+d.curr.linefeedChar);
      modemAddString(d,'EQM Sum..................... FFFF'+d.curr.carrigeChar+d.curr.linefeedChar);
      modemAddString(d,'Min Distance................ 005'+d.curr.carrigeChar+d.curr.linefeedChar);
      modemAddString(d,'RBS Pattern................. FF'+d.curr.carrigeChar+d.curr.linefeedChar);
      modemAddString(d,'Rate Drop................... FF'+d.curr.carrigeChar+d.curr.linefeedChar);
      modemAddString(d,'Digital Loss................ 00'+d.curr.carrigeChar+d.curr.linefeedChar);
      modemAddString(d,'Local Rtrn Count............ 00'+d.curr.carrigeChar+d.curr.linefeedChar);
      modemAddString(d,'Remote Rtrn Count........... 00'+d.curr.carrigeChar+d.curr.linefeedChar);
      modemAddString(d,'Flex fail'+d.curr.carrigeChar+d.curr.linefeedChar);
      modemAddString(d,d.curr.carrigeChar+d.curr.linefeedChar);
      goto f1;
      end;
    if (i=2) then begin;
      for i:=0 to 1 do begin;
        modemAddString(d,'STORED PROFILE '+BStr(i)+':'+d.curr.carrigeChar+d.curr.linefeedChar);
        showProf(d.prof[i]);
        end;
      goto f1;
      end;
    goto f2;
    end;
  if (c='w') then begin; {write active profile to memory}
    delCh;
    i:=getNum and $01;
    d.prof[i]:=d.curr;
    goto f1;
    end;
  if (c='x') then begin; {synchronous transmit clock source}
    delCh;
    getNum;
    goto f1;
    end;
  if (c='y') then begin; {select stored profile for hard reset}
    delCh;
    getNum;
    goto f1;
    end;
  if (c='z') then begin; {store telephone number}
    delCh;
    o:=getNum and $03;
    getCh;
    if (c<>'=') then goto f2;
    delCh;
    a:=getDial(i);
    if (i<0) then goto f2;
    d.phoneBook[o]:=a;
    goto f1;
    end;
  goto f2;
  end;
if (c='%') then begin; {extended commands}
  delCh;
  getCh;
  c:=lowCase(c);
  if (c='c') then begin; {enable/disable data compression}
    delCh;
    getNum;
    goto f1;
    end;
  if (c='e') then begin; {line quality monitor and auto retrain}
    delCh;
    getNum;
    goto f1;
    end;
  if (c='l') then begin; {line signal level}
    delCh;
    modemAddString(d,'043'+d.curr.carrigeChar+d.curr.linefeedChar);
    goto f1;
    end;
  if (c='q') then begin; {line signal quality}
    delCh;
    modemAddString(d,'015'+d.curr.carrigeChar+d.curr.linefeedChar); {reports country code}
    goto f1;
    end;
  goto f2;
  end;
if (c='\') then begin; {extended commands}
  delCh;
  getCh;
  c:=lowCase(c);
  if (c='a') then begin; {select maximum mnp block size}
    delCh;
    getNum;
    goto f1;
    end;
  if (c='b') then begin; {transmit break to remote}
    delCh;
    getNum;
    goto f1;
    end;
  if (c='k') then begin; {break control}
    delCh;
    getNum;
    goto f1;
    end;
  if (c='n') then begin; {error correting mode}
    delCh;
    getNum;
    goto f1;
    end;
  if (c='v') then begin; {single line connect message}
    delCh;
    getNum;
    goto f1;
    end;
  goto f2;
  end;
if (c=')') then begin; {extended commands}
  delCh;
  getCh;
  c:=lowCase(c);
  if (c='m') then begin; {enable cellular power level adjustment}
    delCh;
    getNum;
    goto f1;
    end;
  goto f2;
  end;
if (c='*') then begin; {extended commands}
  delCh;
  getCh;
  c:=lowCase(c);
  if (c='h') then begin; {link negotiation speed}
    delCh;
    getNum;
    goto f1;
    end;
  if (c='b') then begin; {display blacklisted numbers}
    delCh;
    modemAddString(d,'NO. - PHONE NUMBER -'+d.curr.carrigeChar+d.curr.linefeedChar);
    modemAddString(d,'------ ------------------------------'+d.curr.carrigeChar+d.curr.linefeedChar);
    goto f1;
    end;
  if (c='d') then begin; {display delaied numbers}
    delCh;
    modemAddString(d,'NO. - PHONE NUMBER -'+d.curr.carrigeChar+d.curr.linefeedChar);
    modemAddString(d,'------ ------------------------------'+d.curr.carrigeChar+d.curr.linefeedChar);
    goto f1;
    end;
  goto f2;
  end;
if (c='@') then begin; {extended commands}
  delCh;
  getCh;
  c:=lowCase(c);
  if (c='m') then begin; {initial cellular power level setting}
    delCh;
    getNum;
    goto f1;
    end;
  goto f2;
  end;
if (c=':') then begin; {extended commands}
  delCh;
  getCh;
  c:=lowCase(c);
  if (c='e') then begin; {compromise equalizer enable command}
    delCh;
    getNum;
    goto f1;
    end;
  goto f2;
  end;
if (c='-') then begin; {extended commands}
  delCh;
  getCh;
  c:=lowCase(c);
  if (c='k') then begin; {mnp extended services}
    delCh;
    getNum;
    goto f1;
    end;
  if (c='q') then begin; {enable fallback to v22bis/v22}
    delCh;
    getNum;
    goto f1;
    end;
  goto f2;
  end;
if (c='#') then begin; {extended commands}
  delCh;
  getCh;
  c:=lowCase(c);
  goto f2;
  end;
if (c='+') then begin; {extended commands}
  delCh;
  getCh;
  c:=lowCase(c);
  goto f2;
  end;

f2:
if ignoreBadCommands then i:=0 else i:=4;
if d.curr.errorOk then modemAddResponse(d,i);
d.rxBuf:='';
End;
