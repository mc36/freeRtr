Function decodeBearerType(i:LongInt):String;
Var a:String;
Begin;
a:='('+BStr(i)+')';
if (i and 1<>0) then a:=a+' digital';
if (i and 2<>0) then a:=a+' analog';
decodeBearerType:=a;
End;

Function decodeFramingType(i:LongInt):String;
Var a:String;
Begin;
a:='('+BStr(i)+')';
if (i and 1<>0) then a:=a+' sync';
if (i and 2<>0) then a:=a+' async';
decodeFramingType:=a;
End;

Procedure placeOneAVP(var con:OneConnectionRecord;t:LongInt;a:String);
Var
  ab:array[0..128] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
WriteWordMSB(con.bufD[con.bufS+1],ab0+$8006);
WriteWordMSB(con.bufD[con.bufS+3],0);
WriteWordMSB(con.bufD[con.bufS+5],t);
move(ab[1],con.bufD[con.bufS+7],ab0);
inc(con.bufS,ab0+6);
End;

Function beginOneTx(var con:OneConnectionRecord):Boolean;
Begin;
beginOneTx:=True;
if (con.bufS>0) then exit;
con.doAck:=1;
con.bufS:=0;
con.bufC:=0;
con.bufP:=con.sessR;
con.bufT:=currentTime;
beginOneTx:=False;
End;

Procedure sendDisconnect(var con:OneConnectionRecord);
Var a:String;
Begin;
pipeLineClose(con.pipe);
con.pipe:=0;
if (con.stat=3) then exit;
con.stat:=3;
if beginOneTx(con) then exit;
con.bufP:=0;
con.bufC:=7;
placeOneAVP(con,0,#0#4); {message type}
a[0]:=#2;WriteWordMSB(a[1],con.tuneL);
placeOneAVP(con,9,a); {assigned tunnel id}
placeOneAVP(con,1,#0#1); {result code}
End;

Procedure startUpperLayer(var con:OneConnectionRecord);
Var
  a:String;
  i,o:LongInt;
Begin;
if (con.stat=4) then exit;
con.stat:=4;
a:=con.cmnd;
{$ifdef debug}
WriteLn('starting '+a+'...');
{$endif}
i:=pos(' ',a);
if (i<1) then i:=666;
if (xExecBgnd(copy(a,1,i-1),copy(a,i+1,255),o)<>0) then o:=0;
if (o<>0) then exit;
WriteLn('failed to start upper process!');
con.stat:=3;
End;



Procedure gotOnePacket(var addr;port:LongInt;var data;size:LongInt);
Label f1,f2,f3,f4,f5;
Const maxSiz=255;
Var
  buf:array[1..1] of byte absolute data;
  a:String;
  ab:array[0..128] of byte absolute a;
  ab0:byte absolute a;
  hd,ps:LongInt;
  rxSeq:LongInt;
  txSeq:LongInt;
  msgTyp:LongInt;
  msgFrm:LongInt;
  msgBer:LongInt;
  msgSeq:Boolean;
  tuneL:LongInt;
  tuneR:LongInt;
  sessL:LongInt;
  sessR:LongInt;
  con:OneConnectionRecord;
  usr:LongInt;
  i,o,p:LongInt;
Begin;
ps:=2;
hd:=readWordMSB(buf);
if (hd and 15<>2) then begin;
  WriteLn('got invalid version from '+ipAddr2string(addr)+' '+BStr(port));
  exit;
  end;
if (hd and $4000<>0) then begin;
  i:=readWordMSB(buf[ps+1]);
  inc(ps,2);
  if (i>size) then begin;
    WriteLn('got short packet from '+ipAddr2string(addr)+' '+BStr(port));
    exit;
    end;
  size:=i;
  end;
tuneL:=readWordMSB(buf[ps+1]);
sessL:=readWordMSB(buf[ps+3]);
inc(ps,4);
txSeq:=-1;
rxSeq:=-1;
if (hd and $800<>0) then begin;
  txSeq:=readWordMSB(buf[ps+1]);
  rxSeq:=readWordMSB(buf[ps+3]);
  inc(ps,4);
  end;
if (hd and $200<>0) then begin;
  i:=readWordMSB(buf[ps+1]);
  inc(ps,2);
  inc(ps,i);
  end;
if (ps>size) then begin;
  WriteLn('got bad header from '+ipAddr2string(addr)+' '+BStr(port));
  exit;
  end;
if (hd and $8000=0) then begin; {data packet}
  usr:=FindConnectByID(tuneL);
  if (usr<=0) then begin;
    f1:
    WriteLn('got bad tunnel id from '+ipAddr2string(addr)+' '+BStr(port));
    exit;
    end;
  con:=ConnectionDat^[usr];
  if (con.sessL<>sessL) then goto f1;
  if (con.port<>port) then goto f1;
  if not TCPcompareAddress(con.addr,addr) then goto f1;
  pipeLineSend(con.pipe,buf[ps+1],size-ps);
  exit;
  end;
if (ps=size) then size:=0;

{$ifdef debug}
WriteLn('got command packet from '+ipAddr2string(addr)+' '+BStr(port));
WriteLn('tunnel='+BStr(tuneL)+' session='+BStr(sessL)+' tx='+BStr(txSeq)+' rx='+BStr(rxSeq));
{$endif}

if (txSeq<0) or (rxSeq<0) then begin;
  WriteLn('got command without sequence from '+ipAddr2string(addr)+' '+BStr(port));
  exit;
  end;

msgTyp:=-1;
msgFrm:=0;
msgBer:=0;
msgSeq:=false;
tuneR:=-1;
sessR:=-1;
f2:
if (ps>=size) then goto f3;
o:=readWordMSB(buf[ps+1]);
i:=readWordMSB(buf[ps+3]);
p:=readWordMSB(buf[ps+5]);
inc(ps,6);
hd:=(o and $3ff)-6;
if (hd<0) then begin;
  WriteLn('got bad avp from '+ipAddr2string(addr)+' '+BStr(port));
  goto f2;
  end;
if (hd>=maxSiz) then ab0:=maxSiz else ab0:=hd;
move(buf[ps+1],ab[1],ab0);
inc(ps,hd);
if (ps>size) then begin;
  WriteLn('got half avp from '+ipAddr2string(addr)+' '+BStr(port));
  goto f2;
  end;
if (i<>0) then begin;
  WriteLn('got vendor specific avp from '+ipAddr2string(addr)+' '+BStr(port));
  goto f2;
  end;
if (o and $4000<>0) then begin;
  WriteLn('got hidden avp from '+ipAddr2string(addr)+' '+BStr(port));
  goto f2;
  end;
case p of
  0:begin;
    msgTyp:=readWordMSB(ab[1]);
    {$ifdef debug}
    case msgTyp of
       1:a:='start control connection request';
       2:a:='start control connection reply';
       3:a:='start control connection connected';
       4:a:='stop control connection notification';
       6:a:='hello';
       7:a:='outgoing call request';
       8:a:='outgoing call reply';
       9:a:='outgoing call connected';
      10:a:='incoming call request';
      11:a:='incoming call reply';
      12:a:='incoming call connected';
      14:a:='call disconnect notify';
      15:a:='wan error notify';
      16:a:='set link info';
      else a:='unknown; '+BStr(msgTyp);
      end;
    WriteLn('message type: '+a);
    {$endif}
    end;
  3:begin;
    msgFrm:=readLongMSB(ab[1]);
    {$ifdef debug}
    WriteLn('framing capability: '+decodeFramingType(msgFrm));
    {$endif}
    end;
  4:begin;
    msgBer:=readLongMSB(ab[1]);
    {$ifdef debug}
    WriteLn('bearer capability: '+decodeBearerType(msgBer));
    {$endif}
    end;
  9:begin;
    tuneR:=readWordMSB(ab[1]);
    {$ifdef debug}
    WriteLn('remote tunnel: '+BStr(tuneR));
    {$endif}
    end;
  14:begin;
    sessR:=readWordMSB(ab[1]);
    {$ifdef debug}
    WriteLn('remote session: '+BStr(sessR));
    {$endif}
    end;
  18:begin;
    msgBer:=readLongMSB(ab[1]);
    {$ifdef debug}
    WriteLn('bearer type: '+decodeBearerType(msgBer));
    {$endif}
    end;
  19:begin;
    msgFrm:=readLongMSB(ab[1]);
    {$ifdef debug}
    WriteLn('framing type: '+decodeFramingType(msgFrm));
    {$endif}
    end;
  39:begin;
    msgSeq:=true;
    {$ifdef debug}
    WriteLn('sequencing required');
    {$endif}
    end;
  1:begin;
    {$ifdef debug}
    WriteLn('result code: '+BStr(readWordMSB(ab[1])));
    WriteLn('error code: '+BStr(readWordMSB(ab[3])));
    WriteLn('error message: '+copy(a,5,666));
    {$endif}
    end;
  2:begin;
    {$ifdef debug}
    WriteLn('message version: '+BStr(ab[1])+'.'+BStr(ab[2]));
    {$endif}
    end;
  5:begin;
    {$ifdef debug}
    WriteLn('tie breaker');
    {$endif}
    end;
  6:begin;
    {$ifdef debug}
    WriteLn('firmware version: '+BStr(readWordMSB(ab[1])));
    {$endif}
    end;
  7:begin;
    {$ifdef debug}
    WriteLn('host name: '+a);
    {$endif}
    end;
  8:begin;
    {$ifdef debug}
    WriteLn('vendor name: '+a);
    {$endif}
    end;
  10:begin;
    {$ifdef debug}
    WriteLn('window size: '+BStr(readWordMSB(ab[1])));
    {$endif}
    end;
  11:begin;
    {$ifdef debug}
    WriteLn('challenge request');
    {$endif}
    end;
  13:begin;
    {$ifdef debug}
    WriteLn('challenge response');
    {$endif}
    end;
  12:begin;
    {$ifdef debug}
    WriteLn('cause code: '+BStr(readWordMSB(ab[1])));
    WriteLn('cause message: '+BStr(ab[3])+' '+copy(a,5,666));
    {$endif}
    end;
  15:begin;
    {$ifdef debug}
    WriteLn('call serial number: '+BStr(readLongMSB(ab[1])));
    {$endif}
    end;
  16:begin;
    {$ifdef debug}
    WriteLn('minimum bps: '+BStr(readLongMSB(ab[1])));
    {$endif}
    end;
  17:begin;
    {$ifdef debug}
    WriteLn('maximum bps: '+BStr(readLongMSB(ab[1])));
    {$endif}
    end;
  21:begin;
    {$ifdef debug}
    WriteLn('called number: '+a);
    {$endif}
    end;
  22:begin;
    {$ifdef debug}
    WriteLn('calling number: '+a);
    {$endif}
    end;
  23:begin;
    {$ifdef debug}
    WriteLn('sub-address: '+a);
    {$endif}
    end;
  24:begin;
    {$ifdef debug}
    WriteLn('tx connect speed: '+BStr(readLongMSB(ab[1])));
    {$endif}
    end;
  25:begin;
    {$ifdef debug}
    WriteLn('physical channel id: '+BStr(readLongMSB(ab[1])));
    {$endif}
    end;
  26:begin;
    {$ifdef debug}
    WriteLn('initial rx lcp confreq');
    {$endif}
    end;
  27:begin;
    {$ifdef debug}
    WriteLn('last tx lcp confreq');
    {$endif}
    end;
  28:begin;
    {$ifdef debug}
    WriteLn('last rx lcp confreq');
    {$endif}
    end;
  29:begin;
    {$ifdef debug}
    WriteLn('proxy auth type: '+BStr(readWordMSB(ab[1])));
    {$endif}
    end;
  30:begin;
    {$ifdef debug}
    WriteLn('proxy auth name: '+a);
    {$endif}
    end;
  31:begin;
    {$ifdef debug}
    WriteLn('proxy auth challenge');
    {$endif}
    end;
  32:begin;
    {$ifdef debug}
    WriteLn('proxy auth id: '+BStr(readWordMSB(ab[1])));
    {$endif}
    end;
  33:begin;
    {$ifdef debug}
    WriteLn('proxy auth response');
    {$endif}
    end;
  34:begin;
    {$ifdef debug}
    WriteLn('error counters:');
    WriteLn('crc: '+BStr(readLongMSB(ab[3])));
    WriteLn('framing: '+BStr(readLongMSB(ab[7])));
    WriteLn('hw ovr: '+BStr(readLongMSB(ab[11])));
    WriteLn('buffer ovr: '+BStr(readLongMSB(ab[15])));
    WriteLn('timeout: '+BStr(readLongMSB(ab[19])));
    WriteLn('alignment: '+BStr(readLongMSB(ab[23])));
    {$endif}
    end;
  35:begin;
    {$ifdef debug}
    WriteLn('tx accm: '+BStr(readLongMSB(ab[3])));
    WriteLn('rx accm: '+BStr(readLongMSB(ab[7])));
    {$endif}
    end;
  36:begin;
    {$ifdef debug}
    WriteLn('random vector');
    {$endif}
    end;
  37:begin;
    {$ifdef debug}
    WriteLn('private group id');
    {$endif}
    end;
  38:begin;
    {$ifdef debug}
    WriteLn('rx connect speed: '+BStr(readLongMSB(ab[1])));
    {$endif}
    end;
  else WriteLn('got invalid avp ('+BStr(p)+') from '+ipAddr2string(addr)+' '+BStr(port));
  end;
goto f2;
f3:

if (msgTyp=1) then begin; {sccr}
  usr:=FindConnectByStat(0);
  if (usr<=0) then begin;
    WriteLn('out of connection slots!');
    exit;
    end;
  if (tuneR=0) or (sessL<>0) or (tuneL<>0) or (rxSeq<>0) or (txSeq<>0) then begin;
    WriteLn('got bad connreq from '+ipAddr2string(addr)+' '+BStr(port));
    exit;
    end;
  o:=0;
  for i:=1 to ConnectionNum do begin;
    if (ConnectionDat^[i].tuneR<>tuneR) then continue;
    if (ConnectionDat^[i].port<>port) then continue;
    if TCPcompareAddress(ConnectionDat^[i].addr,addr) then o:=i;
    end;
  if (o>0) then begin;
    WriteLn('got duplicated connreq from '+ipAddr2string(addr)+' '+BStr(port));
    ConnectionDat^[o].doAck:=1;
    exit;
    end;
  fillchar(con,sizeof(con),0);
  con.cmnd:=ConnectionDat^[usr].cmnd;
  con.tuneR:=tuneR;
  repeat
    tuneL:=random($fff8)+4;
    until (FindConnectByID(tuneL)=0);
  con.tuneL:=tuneL;
  move(addr,con.addr,sizeof(OneTCPaddressRecord));
  con.rxSeq:=1;
  con.port:=port;
  con.stat:=1;
  con.txSeq:=0;
  if beginOneTx(con) then exit;
  placeOneAVP(con,0,#0#2); {message type}
  placeOneAVP(con,2,#1#0); {protocol version}
  placeOneAVP(con,3,#0#0#0#3); {framing capabilities}
  placeOneAVP(con,4,#0#0#0#3); {bearer capabilities}
  placeOneAVP(con,7,servName); {host name}
  a[0]:=#2;WriteWordMSB(a[1],tuneL);
  placeOneAVP(con,9,a); {assigned tunnel id}
  placeOneAVP(con,10,#0#1); {receive window size}
  placeOneAVP(con,8,proggyName); {vendor name}
  WriteLn('connection accepted from '+ipAddr2string(addr)+' '+BStr(port));
  goto f4;
  end;

usr:=FindConnectByID(tuneL);
if (usr<=0) then goto f1;
con:=ConnectionDat^[usr];
if (con.port<>port) then goto f1;
if not TCPcompareAddress(con.addr,addr) then goto f1;

con.doAck:=1;
if ((rxSeq-1) and $ffff=con.txSeq) then begin;
  con.txSeq:=(con.txSeq+1) and $ffff;
  con.bufS:=0;
  con.bufC:=0;
  end;
if ((con.rxSeq-txSeq-1) and $ffff<$7000) then begin;
  WriteLn('got old packet again from '+ipAddr2string(addr)+' '+BStr(port));
  goto f4;
  end;
if (con.rxSeq<>txSeq) then begin;
  WriteLn('got future packet from '+ipAddr2string(addr)+' '+BStr(port));
  goto f4;
  end;
{$ifdef debug}
WriteLn('got good control packet from '+ipAddr2string(addr)+' '+BStr(port));
{$endif}
if msgSeq then con.datSeq:=0;
con.time:=currentTime;
if (size<1) then goto f4;
con.rxSeq:=(txSeq+1);

case msgTyp of
  1:; {start control connection request}
  2:begin; {start control connection reply}
    end;
  3:begin; {start control connection connected}
    end;
  4:begin; {stop control connection notification}
    WriteLn('got close tunnel from '+ipAddr2string(addr)+' '+BStr(port));
    sendDisconnect(con);
    end;
  6:begin; {hello}
    end;
  7:begin; {outgoing call request}
    if (con.sessL<>0) then begin;
      f5:
      WriteLn('got second session open from '+ipAddr2string(addr)+' '+BStr(port));
      if beginOneTx(con) then goto f4;
      placeOneAVP(con,0,#0#14); {message type}
      repeat
        i:=random($fff0)+1;
        until (i<>con.sessL);
      a[0]:=#2;WriteWordMSB(a[1],i);
      placeOneAVP(con,14,a); {assigned session id}
      placeOneAVP(con,1,#0#1#0#0'only one session permitted'); {result code}
      goto f4;
      end;
    con.sessL:=random($fff8)+4;
    con.sessR:=sessR;
    if beginOneTx(con) then goto f4;
    placeOneAVP(con,0,#0#8); {message type}
    a[0]:=#2;WriteWordMSB(a[1],con.sessL);
    placeOneAVP(con,14,a); {assigned session id}
    con.stat:=5;
    end;
  8:begin; {outgoing call reply}
    end;
  9:begin; {outgoing call connected}
    end;
  10:begin; {incoming call request}
    if (con.sessL<>0) then goto f5;
    con.sessL:=random($fff8)+4;
    con.sessR:=sessR;
    if beginOneTx(con) then goto f4;
    placeOneAVP(con,0,#0#11); {message type}
    a[0]:=#2;WriteWordMSB(a[1],con.sessL);
    placeOneAVP(con,14,a); {assigned session id}
    end;
  11:begin; {incoming call reply}
    end;
  12:begin; {incoming call connected}
    if (con.tuneL<>tuneL) then goto f4;
    startUpperLayer(con);
    end;
  14:begin; {call disconnect notify}
    if (con.tuneL<>tuneL) then goto f4;
    WriteLn('got close session from '+ipAddr2string(addr)+' '+BStr(port));
    sendDisconnect(con);
    end;
  15:begin; {wan error notify}
    end;
  16:begin; {set link info}
    end;
  else WriteLn('got unknown control from '+ipAddr2string(addr)+' '+BStr(port));
  end;

f4:
ConnectionDat^[usr]:=con;
End;




Procedure relequishConn(var con:OneConnectionRecord);
Label f1,f2;
Var
  buf:array[1..2*1024] of byte;
  i,o:LongInt;
  a:String;
Begin;
if (con.stat=0) then exit;
if (con.bufS<=0) then begin;
  if (con.stat=3) then goto f2;
  if (con.stat=5) then begin;
    if beginOneTx(con) then exit;
    placeOneAVP(con,0,#0#9); {message type}
    placeOneAVP(con,24,#0#0#112#128); {tx connect speed}
    placeOneAVP(con,19,#0#0#0#2); {framing type}
    placeOneAVP(con,38,#0#0#112#128); {tx connect speed}
    startUpperLayer(con);
    goto f1;
    end;
  end;
if (con.bufS<=0) then if (GetTimePast(con.time)>60) then begin;
  WriteLn('sending keepalive to '+ipAddr2string(con.addr)+' '+BStr(con.port));
  if beginOneTx(con) then exit;
  con.bufP:=0;
  placeOneAVP(con,0,#0#6); {message type}
  goto f1;
  end;
if (con.doAck<>0) then goto f1;
if (con.bufS>0) then if (GetTimePast(con.bufT)>10) then begin;
  WriteLn('timeout while sending to '+ipAddr2string(con.addr)+' '+BStr(con.port));
  inc(con.bufC);
  if (con.bufC<=10) then goto f1;
  WriteLn('connection lost with '+ipAddr2string(con.addr)+' '+BStr(con.port));
  f2:
  pipeLineClose(con.pipe);
  con.pipe:=0;
  con.stat:=0;
  exit;
  f1:
  o:=con.bufS+12;
  WriteWordMSB(buf[1],$c802);
  WriteWordMSB(buf[3],o);
  WriteWordMSB(buf[5],con.tuneR);
  WriteWordMSB(buf[7],con.bufP);
  WriteWordMSB(buf[9],con.txSeq);
  WriteWordMSB(buf[11],con.rxSeq);
  move(con.bufD,buf[13],con.bufS);
  UDPsendPacket(servPipe,con.addr,con.port,buf,o);
  con.bufT:=currentTime;
  con.doAck:=0;
  end;
if (con.stat<>2) then exit;

if (con.datSeq<0) then i:=8 else i:=12;
o:=sizeof(buf)-i;
if (pipeLineRecv(con.pipe,buf[i+1],o)<>0) then o:=0;
if (o=0) then begin;
  pipeLineStats(con.pipe,o,i,i);
  if (o<>0) then exit;
  WriteLn('upper terminated with '+ipAddr2string(con.addr)+' '+BStr(con.port));
  sendDisconnect(con);
  exit;
  end;
inc(o,i);
if (con.datSeq<0) then i:=$4002 else i:=$4802;
WriteWordMSB(buf[1],i);
WriteWordMSB(buf[3],o);
WriteWordMSB(buf[5],con.tuneR);
WriteWordMSB(buf[7],con.sessR);
if (con.datSeq>=0) then begin;
  WriteWordMSB(buf[9],con.datSeq);
  WriteWordMSB(buf[11],0);
  con.datSeq:=(con.datSeq+1) and $ffff;
  end;
UDPsendPacket(servPipe,con.addr,con.port,buf,o);
End;
