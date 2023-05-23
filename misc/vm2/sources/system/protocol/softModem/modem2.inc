Procedure modemGotCommand(b:String);
Var
  register:LongInt;
  c:Char;
  a:String;
  i,o:LongInt;

Procedure getCh;
Begin;
c:=b[1];
if (b='') then c:=#0;
End;

Procedure delCh;
Begin;
b:=copy(b,2,255);
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

Function getDial(var r:LongInt):String;
Label f1;
Var a:String;
Begin;
getDial:=#13;
a:='';
r:=1;
f1:
getCh;
c:=lowCase(c);
if (c=#0) then begin;
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
if (c='r') then begin;
  delCh;
  r:=3-r;
  goto f1;
  end;
exit;
End;

Procedure writeReg(r,v:Byte);
Begin;
case r of
  014:begin;
    modemCmdEcho:=(v and $02<>0);
    modemVerbRsp:=(v and $08<>0);
    end;
  021:begin;
    modemCDforced:=(v and $20=0);
    modemDTRignor:=((v shr 3)=0);
    end;
  end;
End;

Function readReg(r:Byte):Byte;
Begin;
case r of
  000:r:=0; {ring to answer}
  001:r:=0; {ring count}
  002:r:=0; {escape sequence character}
  003:r:=13; {carriage return character}
  004:r:=10; {line feed character}
  005:r:=8; {backspace character}
  006:r:=2; {wait before blind dialing}
  007:r:=45; {wait for carrier after dialing}
  008:r:=2; {duration of delay for comma dial modifier}
  009:r:=6; {carrier detect response time}
  010:r:=14; {delay between lost carrier and hang up}
  011:r:=95; {multi-frequency tone duration}
  012:r:=50; {escape sequence guard time}
  014:begin; {general bit mapped options status}
    r:=$81;
    if modemCmdEcho then inc(r,$02);
    if modemVerbRsp then inc(r,$08);
    end;
  016:r:=0; {general bit mapped test options status}
  018:r:=0; {modem test timer}
  019:r:=0; {autosync bit mapped options}
  020:r:=0; {autosync hdlc address or bsc sync character}
  021:begin; {v24/general bit mapped options status}
    if modemDTRignor then r:=0 else r:=1;
    if not modemCDforced then inc(r,$20);
    inc(r,$04);
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
  039:r:=0; {flow control bit mapped options status}
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
  095:r:=1; {negotiation message options}
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


Label f1,f2;
Begin;
register:=0;
f1:
if (b='') then begin;
  modemAddResp(0);
  exit;
  end;
getCh;
c:=lowCase(c);
if (c=' ') then begin;
  delCh;
  goto f1;
  end;
if (c='a') then begin; {answer command}
  delCh;
  if movemVoipMode then goto f1;
  i:=modemRxSiz;
  connectStart(true);
  modemRxSiz:=i;
  exit;
  end;
if (c='b') then begin; {select communication standard}
  delCh;
  case getNum of
    0:begin; modemFreqOrg:=1080;modemFreqAns:=1750; end; {ccitt}
    1:begin; modemFreqOrg:=1170;modemFreqAns:=2125; end; {bell}
    end;
  goto f1;
  end;
if (c='c') then begin; {carrier control selection}
  delCh;
  getNum;
  goto f1;
  end;
if (c='d') then begin; {dial command}
  delCh;
  a:=getDial(o);
  if (a=#13) then goto f2;
  i:=modemRxSiz;
  connectStart(o=2);
  modemRxSiz:=i;
  modemCommand:=a;
  modemConnected:=-1;
  exit;
  end;
if (c='e') then begin; {command state character echo selection}
  delCh;
  i:=getNum;
  modemCmdEcho:=(i and 1<>0);
  goto f1;
  end;
if (c='f') then begin; {on-line state character echo selection}
  delCh;
  getNum;
  goto f1;
  end;
if (c='h') then begin; {hook command options}
  delCh;
  getNum;
  goto f1;
  end;
if (c='i') then begin; {internal memory tests}
  delCh;
  i:=getNum;
  case i of
    0:modemAdd2rx('144'#13#10); {display product code}
    1:modemAdd2rx('255'#13#10); {display ROM checksum}
    2:modemAddResp(0); {perform ROM checksum test}
    3:modemAdd2rx(modemVersion+'-V21'#13#10); {firmware version}
    4:begin; {reports OEM defined identifier string}
      modemAdd2rx('MODEM CHIP : '+modemProggy+' '+modemVersion+#13#10);
      modemAdd2rx('DATA + NO FAX + VOICE'+#13#10);
      end;
    5:modemAdd2rx('022'#13#10);
    6:modemAdd2rx(modemProggy+'300FSK Rev '+modemVersion+#13#10); {reports data pump}
    7:modemAdd2rx('000'+#13#10); {reports the DAA code}
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
  goto f2;
  end;
if (c='p') then begin; {select pulse dialing method}
  delCh;
  goto f1;
  end;
if (c='q') then begin; {result code display options}
  delCh;
  getNum;
  goto f1;
  end;
if (c='s') then begin; {addresses an s register}
  delCh;
  register:=getNum;
  goto f1;
  end;
if (c='=') then begin; {write to an s register}
  i:=getNum;
  writeReg(register,i);
  goto f1;
  end;
if (c='?') then begin; {read an s register}
  delCh;
  i:=readReg(register);
  modemAdd2rx(conv2reg(i)+#13#19);
  goto f1;
  end;
if (c='t') then begin; {select tone dialing method}
  delCh;
  goto f1;
  end;
if (c='v') then begin; {result code format options}
  delCh;
  i:=getNum;
  modemVerbRsp:=(i and 1<>0);
  goto f1;
  end;
if (c='w') then begin; {negotiation progress message selection}
  delCh;
  getNum;
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
  getNum;
  i:=modemRxSiz;
  modemResetParam;
  modemRxSiz:=i;
  goto f1;
  end;
if (c='&') then begin; {extended commands}
  delCh;
  getCh;
  c:=lowCase(c);
  if (c='h') then begin; {help}
    delCh;
    getNum;
    modemAdd2rx('supported commands:');
    modemAdd2rx(' a d e i s = ? v z');
    modemAdd2rx(' &c &d &f &v');
    modemAdd2rx(' +fclass +vts +vrx +vtx +vtr');
    modemAdd2rx(' #cls #vts #vrx #vtx #vtr');
    modemAdd2rx(' +x-ans +x-org +x-prc +x-sqr +x-dal +x-hng');
    modemAdd2rx(#13#10);
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
    modemCDforced:=(i and 1=0);
    goto f1;
    end;
  if (c='d') then begin; {data terminal ready options}
    delCh;
    i:=getNum;
    modemDTRignor:=(i=0);
    goto f1;
    end;
  if (c='f') then begin; {recall factory profile}
    delCh;
    i:=modemRxSiz;
    modemResetParam;
    modemRxSiz:=i;
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
    getNum;
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
    getNum;
    if modemCmdEcho then a:='1' else a:='0';
    modemAdd2rx('E'+a+' T ');
    if modemVerbRsp then a:='1' else a:='0';
    modemAdd2rx('V'+a+' ');
    if modemCDforced then a:='0' else a:='1';
    modemAdd2rx('&C'+a+' ');
    if modemDTRignor then a:='0' else a:='1';
    modemAdd2rx('&D'+a+' ');
    modemAdd2rx('+X-ANS='+BStr(modemFreqAns)+' ');
    modemAdd2rx('+X-ORG='+BStr(modemFreqOrg)+' ');
    modemAdd2rx('+X-PRC='+BStr(modemPrecisity)+' ');
    modemAdd2rx('+X-SQR='+BStr(modemSquareOut)+' ');
    modemAdd2rx(#13#10);
    goto f1;
    end;
  if (c='w') then begin; {write active profile to memory}
    delCh;
    getNum;
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
  goto f2;
  end;
if (c in ['#','+']) then begin; {extended commands}
  i:=pos('=',b);
  if (i<1) then i:=666;
  a:=kicsi(copy(b,1,i-1));
  b:=copy(b,i+1,666);
  if (a='+x-ans') then begin;
    i:=getNum;
    if (i<10) then goto f2;
    modemFreqAns:=i;
    goto f1;
    end;
  if (a='+x-org') then begin;
    i:=getNum;
    if (i<10) then goto f2;
    modemFreqOrg:=i;
    goto f1;
    end;
  if (a='+x-prc') then begin;
    i:=getNum;
    if (i<1) or (i>100) then goto f2;
    modemPrecisity:=i;
    goto f1;
    end;
  if (a='+x-sqr') then begin;
    modemSquareOut:=getNum;
    goto f1;
    end;
  if (a='+x-dal') then begin;
    if soundDial(b) then i:=0 else i:=4;
    modemAddResp(i);
    exit;
    end;
  if (a='+x-hng') then begin;
    if soundHangup then i:=0 else i:=4;
    modemAddResp(i);
    exit;
    end;
  if (a='+fclass') or (a='#cls') then begin;
    movemVoipMode:=(getNum=8);
    goto f1;
    end;
  if not movemVoipMode then goto f2;
  if (a='+vip') then goto f1; {reset voice parameters}
  if (a='+vcid') then begin; {caller id support}
    getNum;
    goto f1;
    end;
  if (a='+vdr') then begin; {distinctive ringing & cadence report}
    getNum;
    goto f1;
    end;
  if (a='+vgt') then begin; {speaker volume control}
    getNum;
    goto f1;
    end;
  if (a='+vgr') then begin; {receive gain}
    getNum;
    goto f1;
    end;
  if (a='+vem') then begin; {error reporting}
    getNum;
    goto f1;
    end;
  if (a='+vit') then begin; {inactivity timer}
    getNum;
    goto f1;
    end;
  if (a='+vnh') then begin; {automatic hangup}
    getNum;
    goto f1;
    end;
  if (a='+vls') then begin; {analog source/destination selection}
    getNum;
    goto f1;
    end;
  if (a='+vtd') then begin; {beep tone duration timer}
    getNum;
    goto f1;
    end;
  if (a='+vsd') then begin; {silence detection}
    getNum;
    goto f1;
    end;
  if (a='+vsm') then begin; {compression method}
    getNum;
    goto f1;
    end;
  if (a='+vra') then begin; {ringback goes away timer}
    getNum;
    goto f1;
    end;
  if (a='+vrn') then begin; {ringback never appeared}
    getNum;
    goto f1;
    end;
  if (a='+vpr') then begin; {protected reporting interval}
    getNum;
    goto f1;
    end;
  if (a='#vbr') then begin; {select baud rate}
    getNum;
    goto f1;
    end;
  if (a='#cid') then begin; {caller id}
    getNum;
    goto f1;
    end;
  if (a='#tl') then begin; {transmit level}
    getNum;
    goto f1;
    end;
  if (a='#rg') then begin; {record gain}
    getNum;
    goto f1;
    end;
  if (a='#vbs') then begin; {bit rate select}
    getNum;
    goto f1;
    end;
  if (a='#vbt') then begin; {beep tone timer}
    getNum;
    goto f1;
    end;
  if (a='#vls') then begin; {voice line select}
    getNum;
    goto f1;
    end;
  if (a='#vra') then begin; {ringback goes away timer}
    getNum;
    goto f1;
    end;
  if (a='#vrn') then begin; {ringback never came}
    getNum;
    goto f1;
    end;
  if (a='#vsd') then begin; {silence detection tuner}
    getNum;
    goto f1;
    end;
  if (a='#vsp') then begin; {silence detection period}
    getNum;
    goto f1;
    end;
  if (a='#vss') then begin; {silence detection sensitivity}
    getNum;
    goto f1;
    end;
  if (a='#vsk') then begin; {buffer skid settings}
    getNum;
    goto f1;
    end;
  if (a='#vsr') then begin; {sampling rate}
    getNum;
    goto f1;
    end;
  if (a='#vtd') then begin; {tone detection}
    getNum;
    goto f1;
    end;
  if (a='+vts') or (a='#vts') then begin; {dtmf and tone generation}
    i:=modemRxSiz;
    connectStart(true);
    modemRxSiz:=i;
    i:=pos(',',b);
    if (i>0) then b:=copy(b,1,i-1);
    modemCommand:=b;
    modemConnected:=-2;
    exit;
    end;
  if (a='+vtr') then begin; {start rx and tx}
    i:=modemRxSiz;
    connectStart(true);
    modemRxSiz:=i;
    modemConnected:=-5;
    modemAddResp(1);
    exit;
    end;
  if (a='+vrx') then begin; {start rx}
    i:=modemRxSiz;
    connectStart(true);
    modemRxSiz:=i;
    modemConnected:=-3;
    modemAddResp(1);
    exit;
    end;
  if (a='+vtx') then begin; {start tx}
    i:=modemRxSiz;
    connectStart(true);
    modemRxSiz:=i;
    modemConnected:=-4;
    modemAddResp(1);
    exit;
    end;
  if (a='#vtr') then begin; {start rx and tx}
    i:=modemRxSiz;
    connectStart(true);
    modemRxSiz:=i;
    modemConnected:=-5;
    modemAddResp(100001);
    exit;
    end;
  if (a='#vrx') then begin; {start rx}
    i:=modemRxSiz;
    connectStart(true);
    modemRxSiz:=i;
    modemConnected:=-3;
    modemAddResp(100001);
    exit;
    end;
  if (a='#vtx') then begin; {start tx}
    i:=modemRxSiz;
    connectStart(true);
    modemRxSiz:=i;
    modemConnected:=-4;
    modemAddResp(100001);
    exit;
    end;
  goto f2;
  end;

f2:
if modemIgnoreBad then i:=0 else i:=4;
modemAddResp(i);
End;
