Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Function getHostName:String;
Var t:xtText;
Begin;
getHostName:='localHost';
if (xtOpen(t,'c:\system\localHost.text',true)<>0) then exit;
getHostName:=xtReadLn(t,666);
xtClose(t);
End;

Var
  cnBeg:LongInt;
  cnNum:String;
  prAdr:OneTCPaddressRecord;
  prPrt:LongInt;
  udpPipe:LongInt;
  uprPipe:LongInt;
  txBuf:array[1..2*1024] of byte;
  txSiz:LongInt;
  txCnt:LongInt;
  txTim:LongInt;
  rxTim:LongInt;
  txSeq:LongInt;
  rxSeq:LongInt;
  doAck:Boolean;
  SeqDat:LongInt;
  tuneL:LongInt;
  tuneR:LongInt;
  sessL:LongInt;
  sessR:LongInt;
  msgTyp:LongInt;
  msgErr:String;
  msgVer:LongInt;
  msgFrm:LongInt;
  msgBer:LongInt;
  msgVnd:String;
  msgHst:String;
  msgWnd:LongInt;
  msgBps:LongInt;
  msgNum:String;



Procedure gotOnePacket(var data;size:LongInt);
Label f1;
Const maxSiz=255;
Var
  buf:array[1..1] of byte absolute data;
  a:String;
  ab:array[0..128] of byte absolute a;
  ab0:byte absolute a;
  hd,ps:LongInt;
  i,o,p:LongInt;
Begin;
msgTyp:=-1;
msgErr:='';
msgVer:=$10;
msgFrm:=0;
msgBer:=0;
msgVnd:='';
msgHst:='';
msgWnd:=-1;
msgBps:=0;
msgNum:='';
doAck:=false;
ps:=2;
hd:=readWordMSB(buf);
if (hd and 15<>2) then begin;
  WriteLn('got invalid version');
  exit;
  end;
if (hd and $4000<>0) then begin;
  i:=readWordMSB(buf[ps+1]);
  inc(ps,2);
  if (i>size) then begin;
    WriteLn('got short packet');
    exit;
    end;
  size:=i;
  end;
i:=readWordMSB(buf[ps+1]);
if (i<>tuneL) then begin;
  WriteLn('got invalid tunnel');
  exit;
  end;
i:=readWordMSB(buf[ps+3]);
if (i<>sessL) and (i<>0) then begin;
  WriteLn('got invalid session');
  exit;
  end;
inc(ps,4);
i:=-1;
o:=-1;
if (hd and $800<>0) then begin;
  i:=readWordMSB(buf[ps+1]);
  o:=readWordMSB(buf[ps+3]);
  inc(ps,4);
  end;
if (hd and $8000=0) then begin; i:=-1;o:=-1; end;
if (o>=0) then begin;
  doAck:=true;
  if ((o-1) and $ffff=txSeq) then begin;
    txSeq:=(txSeq+1) and $ffff;
    txSiz:=0;
    txCnt:=0;
    end;
  end;
if (i>=0) then begin;
  doAck:=true;
  if ((rxSeq-i-1) and $ffff<$7000) then begin;
    WriteLn('got old packet again');
    exit;
    end;
  if (rxSeq<>i) then begin;
    WriteLn('got bad sequence number');
    exit;
    end;
  rxSeq:=i;
  end;
if (hd and $200<>0) then begin;
  i:=readWordMSB(buf[ps+1]);
  inc(ps,2);
  inc(ps,i);
  end;
if (ps>size) then begin;
  WriteLn('got bad header');
  exit;
  end;
rxTim:=currentTime;
if (ps=size) then begin;
  doAck:=false;
  exit;
  end;
if (hd and $8000=0) then begin;
  doAck:=false;
  pipeLineSend(uprPipe,buf[ps+1],size-ps);
  exit;
  end;
rxSeq:=(rxSeq+1) and $ffff;
f1:
if (ps>=size) then exit;
o:=readWordMSB(buf[ps+1]);
i:=readWordMSB(buf[ps+3]);
p:=readWordMSB(buf[ps+5]);
inc(ps,6);
hd:=(o and $3ff)-6;
if (hd<0) then begin;
  WriteLn('got bad avp');
  goto f1;
  end;
if (hd>=maxSiz) then ab0:=maxSiz else ab0:=hd;
move(buf[ps+1],ab[1],ab0);
inc(ps,hd);
if (ps>size) then begin;
  WriteLn('got half avp');
  goto f1;
  end;
if (i<>0) then begin;
  WriteLn('got vendor specific avp');
  goto f1;
  end;
if (o and $4000<>0) then begin;
  WriteLn('got hidden avp');
  goto f1;
  end;
case p of
  0:msgTyp:=readWordMSB(ab[1]);
  36:;{random vector}
  1:begin;
    if (msgErr<>'') then msgErr:=msgErr+' ';
    i:=readWordMSB(ab[1]);
    msgErr:=msgErr+'err='+BStr(i);
    if (ab0>2) then begin;
      i:=readWordMSB(ab[3]);
      msgErr:=msgErr+' res='+BStr(i);
      end;
    if (ab0>4) then msgErr:=msgErr+' text='+copy(a,5,666);
    end;
  2:msgVer:=readWordMSB(ab[1]);
  3:msgFrm:=readLongMSB(ab[1]);
  4:msgBer:=readLongMSB(ab[1]);
  5:;{tie breaker}
  6:begin;
    a:='firm=v'+BStr(ab[1])+'.'+BStr(ab[2]);
    if (msgVnd='') then msgVnd:=a else msgVnd:=msgVnd+'; '+a;
    end;
  7:msgHst:=a;
  8:if (msgVnd='') then msgVnd:=a else msgVnd:=a+'; '+msgVnd;
  9:tuneR:=readWordMSB(ab[1]);
  10:msgWnd:=readWordMSB(ab[1]);
  11:WriteLn('got chap-req');
  13:WriteLn('got chap-rsp');
  12:begin;
    if (msgErr<>'') then msgErr:=msgErr+' ';
    i:=readLongMSB(ab[1]) shr 8;
    msgErr:=msgErr+'cauC='+BStr(i);
    if (ab0>3) then begin;
      i:=readWordMSB(ab[4]);
      msgErr:=msgErr+' cauM='+BStr(i);
      end;
    if (ab0>4) then msgErr:=msgErr+' text='+copy(a,5,666);
    end;
  14:sessR:=readWordMSB(ab[1]);
  16,17:msgBps:=(msgBps+readLongMSB(ab[1])) shr 1;
  18:msgBer:=readLongMSB(ab[1]);
  19:msgFrm:=readLongMSB(ab[1]);
  21:msgNum:=a;
  22:msgNum:=a;
  23:msgNum:=a;
  24,38:msgBps:=(msgBps+readLongMSB(ab[1])) shr 1;
  39:SeqDat:=0;
  34:;{call errors}
  35:;{accm}
  else WriteLn('got invalid avp ('+BStr(p)+')');
  end;
goto f1;
End;



Procedure placeOneAVP(var data;var size:LongInt;t:LongInt;a:String);
Var
  buf:array[1..1] of byte absolute data;
  ab:array[0..128] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
WriteWordMSB(buf[size+1],ab0+$8006);
WriteWordMSB(buf[size+3],0);
WriteWordMSB(buf[size+5],t);
inc(size,6);
move(ab[1],buf[size+1],ab0);
inc(size,ab0);
End;

Procedure beginOneTx;
Begin;
if (txSiz>0) then immErr('remote did not acked!');
txCnt:=0;
doAck:=True;
txSiz:=0;
End;

Function releq2conn(learn:Boolean):Boolean;
Label f1;
Var
  buf:array[1..2*1024] of byte;
  a:String;
  i,o:LongInt;
Begin;
releq2conn:=True;
if (txSiz<1) then if (GetTimePast(rxTim)>60) then begin;
  WriteLn('sending keepalive packet...');
  beginOneTx;
  placeOneAVP(txBuf,txSiz,0,#0#6); {message type}
  end;
if doAck then goto f1;
if (txSiz>0) then if (GetTimePast(txTim)>10) then begin;
  WriteLn('timeout while sending packet!');
  inc(txCnt);
  if (txCnt>10) then immErr('retry counter reached maximum!');
  f1:
  o:=txSiz+12;
  WriteWordMSB(buf[1],$c802);
  WriteWordMSB(buf[3],o);
  WriteWordMSB(buf[5],tuneR);
  WriteWordMSB(buf[7],sessR);
  WriteWordMSB(buf[9],txSeq);
  WriteWordMSB(buf[11],rxSeq);
  move(txBuf,buf[13],txSiz);
  UDPsendPacket(udpPipe,prAdr,prPrt,buf,o);
  txTim:=currentTime;
  doAck:=false;
  end;
o:=sizeof(buf);
if UDPreceivePacket(udpPipe,a,i,buf,o) then exit;
if learn then prPrt:=i;
if not TCPcompareAddress(a,prAdr) or (prPrt<>i) then begin;
  WriteLn('got packet from '+ipAddr2string(a)+' '+BStr(i));
  exit;
  end;
gotOnePacket(buf,o);
if (msgTyp<0) then exit;
case msgTyp of
  6:exit; {hello}
  4:immErr('got stop control connection notification');
  14:immErr('got call disconnect notify');
  end;
releq2conn:=False;
End;

Function releq2upper:Boolean;
Var
  buf:array[1..2*1024] of byte;
  a:String;
  i,o:LongInt;
Begin;
releq2upper:=false;
if (SeqDat<0) then i:=8 else i:=12;
o:=sizeof(buf)-i;
if (pipeLineRecv(uprPipe,buf[i+1],o)<>0) then o:=0;
if (o<2) then begin;
  pipeLineStats(uprPipe,o,i,i);
  if (o<>0) then exit;
  WriteLn('upper layer exited!');
  releq2upper:=true;
  exit;
  end;
inc(o,i);
if (SeqDat<0) then i:=$4002 else i:=$4802;
WriteWordMSB(buf[1],i);
WriteWordMSB(buf[3],o);
WriteWordMSB(buf[5],tuneR);
WriteWordMSB(buf[7],sessR);
if (SeqDat>=0) then begin;
  WriteWordMSB(buf[9],SeqDat);
  WriteWordMSB(buf[11],0);
  SeqDat:=(SeqDat+1) and $ffff;
  end;
UDPsendPacket(udpPipe,prAdr,prPrt,buf,o);
End;

Procedure wait4flush;
Begin;
while (txSiz>0) do begin;
  releq2conn(false);
  relequish;
  timer2start;
  end;
End;

Procedure wait4packet;
Begin;
while releq2conn(false) do begin;
  relequish;
  timer2start;
  end;
End;




Procedure openTunnelConn;
Var a:String;
Begin;
timer2start;
tuneL:=random($fff8)+4;
sessL:=random($fff8)+4;
tuneR:=0;
sessR:=0;
txSiz:=0;
txSeq:=0;
rxSeq:=0;
SeqDat:=-1;
WriteLn('sending start control connection request...');
beginOneTx;
placeOneAVP(txBuf,txSiz,0,#0#1); {message type}
placeOneAVP(txBuf,txSiz,2,#1#0); {protocol version}
placeOneAVP(txBuf,txSiz,7,getHostName); {host name}
placeOneAVP(txBuf,txSiz,3,#0#0#0#3); {framing capabilities}
a[0]:=#2;WriteWordMSB(a[1],tuneL);
placeOneAVP(txBuf,txSiz,9,a); {assigned tunnel id}
placeOneAVP(txBuf,txSiz,4,#0#0#0#3); {bearer capabilities}
placeOneAVP(txBuf,txSiz,10,#0#1); {receive window size}
placeOneAVP(txBuf,txSiz,8,proggyName); {vendor name}
while releq2conn(true) do begin;
  relequish;
  timer2start;
  end;
if (msgTyp<>2) then immErr('got invalid packet!');
WriteLn('got start control connection reply...');
wait4flush;
WriteLn('sending start control connection connected...');
beginOneTx;
placeOneAVP(txBuf,txSiz,0,#0#3); {message type}
wait4flush;
WriteLn('tunnel opened with '+ipAddr2string(prAdr)+' '+BStr(prPrt)+'...');
End;



Procedure openSessionConn;
Var a:String;
Begin;
wait4flush;
WriteLn('sending outgoing/incoming call request...');
beginOneTx;
placeOneAVP(txBuf,txSiz,0,#0+chr(cnBeg)); {message type}
a[0]:=#2;WriteWordMSB(a[1],sessL);
placeOneAVP(txBuf,txSiz,14,a); {assigned session id}
placeOneAVP(txBuf,txSiz,15,#0#0#0#1); {call serial number}
if (cnBeg<>10) then begin;
  placeOneAVP(txBuf,txSiz,16,#0#0#0#1); {minimum bps}
  placeOneAVP(txBuf,txSiz,17,#127#255#255#255); {maximum bps}
  placeOneAVP(txBuf,txSiz,18,#0#0#0#1); {bearer type}
  placeOneAVP(txBuf,txSiz,21,cnNum); {called number}
  end;
wait4packet;
if (msgTyp<>cnBeg+1) then immErr('got invalid packet!');
WriteLn('got outgoing/incoming call reply...');
if (cnBeg<>10) then begin;
  wait4packet;
  if (msgTyp<>cnBeg+2) then immErr('got invalid packet!');
  WriteLn('got outgoing call connected...');
  end else begin;
  wait4flush;
  WriteLn('sending incoming call connected...');
  beginOneTx;
  placeOneAVP(txBuf,txSiz,0,#0+chr(cnBeg+2)); {message type}
  placeOneAVP(txBuf,txSiz,24,#127#255#255#255); {tx connect speed}
  placeOneAVP(txBuf,txSiz,19,#0#0#0#2); {framing type}
  placeOneAVP(txBuf,txSiz,38,#127#255#255#255); {rx connect speed}
  wait4flush;
  end;
WriteLn('connection successfully estabilished!');
End;


Procedure closeTunnelConn;
Var a:String;
Begin;
wait4flush;
WriteLn('sending stop control connection notification...');
beginOneTx;
placeOneAVP(txBuf,txSiz,0,#0#4); {message type}
a[0]:=#2;WriteWordMSB(a[1],tuneL);
placeOneAVP(txBuf,txSiz,9,a); {assigned tunnel id}
placeOneAVP(txBuf,txSiz,1,#0#1); {result code}
wait4flush;
End;



Procedure WaitForUpperLayer;
Var
  i,o,p:LongInt;
  a:String;
Begin;
Write('waiting for upper level...');
BugOS_SignDaemoning;
pipeLineBegListen;
while (pipeLineGetIncoming(uprPipe)<>0) do relequish;
pipeLineEndListen;
a:='l2tp with '+ipAddr2string(prAdr)+' '+BStr(prPrt);
BugOS_MyProcessInfo(i,o,p);
i:=i xor p xor o;
p:=((i shr 24) xor (i shr 16) xor (i shr 8) xor i) and $ff;
a:='12341234'#0#0#0#0#0#0#0#0+chr(p)+#255+a+#0;
i:=1;move(i,a[1],sizeof(i));
i:=1400;move(i,a[5],sizeof(i));
pipeLineSend(uprPipe,a[1],length(a));
WriteLn(' done!');
End;


Procedure doOneConnection;
Label f1;
Begin;
f1:
relequish;
timer2start;
releq2conn(false);
if releq2upper then exit;
goto f1;
End;
