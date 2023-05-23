Procedure skinnySendPacket(var buffer;siz,typ:LongInt);
Var
  hdr:OneSkinnyHeaderRecord absolute buffer;
  buf:array[1..1] of byte absolute buffer;
Begin;
WriteLn('sending '+GetMessageName(typ)+'; size='+BStr(siz)+'...');
move(buf,buf[sizeof(hdr)+1],siz);
WriteLongLSB(hdr.typ,typ);
WriteLongLSB(hdr.siz,siz+4);
hdr.res:=0;
pipeLineSend(serverPipe,buf,siz+sizeof(hdr));
lastSent:=currentTime;
End;

Procedure skinnySendEmpty(typ:LongInt);
Begin;
skinnySendPacket(typ,0,typ);
End;

Procedure skinnySendString(typ:LongInt;dat:String);
Var
  buf:array[1..512] of byte;
  datS:byte absolute dat;
Begin;
move(dat[1],buf,datS);
skinnySendPacket(buf,datS,typ);
End;

Procedure skinnyRecvPacket(var buffer;var siz,typ:LongInt);
Var i,o,p:LongInt;
Begin;
siz:=-1;
typ:=-1;
pipeLineStats(serverPipe,p,o,i);
if (p=0) then immErr('remote closed connection!');
if (lastHeader.siz=0) then begin;
  if (o<sizeof(lastHeader)) then exit;
  o:=sizeof(lastHeader);
  pipeLineRecv(serverPipe,lastHeader,o);
  if (o<>sizeof(lastHeader)) then immErr('bad number of bytes received!');
  exit;
  end;
i:=ReadLongLSB(lastHeader.siz)-4;
if (i<0) or (i>1024) then immErr('invalid packet size!');
if (o<i) then exit;
o:=i;
pipeLineRecv(serverPipe,buffer,o);
if (o<>i) then immErr('bad number of bytes received!');
siz:=o;
typ:=ReadLongLSB(lastHeader.typ);
fillchar(lastHeader,sizeof(lastHeader),0);
WriteLn('received '+GetMessageName(typ)+'; size='+BStr(siz)+'...');
End;



Procedure skinnySendUnreg;
Begin;
skinnySendEmpty($27);
End;

Procedure skinnySendAlarm;
Begin;
skinnySendString($20,#2#0#0#0+'ciscoATAemulator'+
  #0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0
  #0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0
  #0#0#0#0#0#0#0#0);
End;

Procedure skinnySendReg;
Begin;
skinnySendString(1,getStationName+#0 #0#0#0#0 #1#0#0#0+getStationAddr
  +#12#0#0#0#1#0#0#0 #2#0#0#0#4#0#0#128);
End;

Procedure skinnySendCapaRep;
Var buf:array[1..1024] of byte;
Begin;
fillchar(buf,sizeof(buf),0);
WriteLongLSB(buf[1],1);
WriteLongLSB(buf[5],2);
WriteLongLSB(buf[9],24);
skinnySendPacket(buf,292,$10);
End;

Procedure skinnySendButtonTempReq;
Begin;
skinnySendEmpty($0e);
End;

Procedure skinnySendSoftKeyTempReq;
Begin;
skinnySendEmpty($28);
End;

Procedure skinnySendCfgStatReq;
Begin;
skinnySendEmpty($0c);
End;

Procedure skinnySendSoftKeySetReq;
Begin;
skinnySendEmpty($25);
End;

Procedure skinnySendDateTimeReq;
Begin;
skinnySendEmpty($0d);
End;

Procedure skinnySendLineStatReq;
Begin;
skinnySendString($0b,#1#0#0#0);
End;

Procedure skinnySendFwdStatReq;
Begin;
skinnySendString($09,#1#0#0#0);
End;

Procedure skinnySendAvilLnsReq;
Begin;
skinnySendString($2d,#1#0#0#0);
End;

Procedure skinnySendKeepalive;
Begin;
skinnySendEmpty($00);
End;

Procedure skinnySendKeepAck;
Begin;
skinnySendEmpty($100);
End;

Procedure skinnySendStimulus;
Begin;
skinnySendString($05,#9#0#0#0#1#0#0#0);
End;

Procedure skinnySendButton(i:LongInt);
Var a:String;
Begin;
a:='1234';
WriteLongLSB(a[1],i);
skinnySendString($03,a);
End;

Procedure skinnySendSoftKey(cid,but:LongInt);
Var a:String;
Begin;
a:='1234'#1#0#0#0'1234';
WriteLongLSB(a[1],but);
WriteLongLSB(a[9],cid);
skinnySendString($26,a);
End;

Procedure skinnySendStatistic(m:String);
Var a:String;
Begin;
while (length(a)<32) do a:=a+#0;
a:=copy(a,1,32)+'1234123412341234'#0#0#0#0#0#0#0#0#0#0#0#0;
if (ReadLongLSB(m[29])=0) then begin;
  sentPck:=0;
  sentOct:=0;
  rcvdPck:=0;
  rcvdOct:=0;
  end;
WriteLongLSB(a[33],sentPck);
WriteLongLSB(a[37],sentOct);
WriteLongLSB(a[41],rcvdPck);
WriteLongLSB(a[45],rcvdOct);
skinnySendString($23,a);
End;

Procedure skinnySendRcvChnAck(a:String;p,t:LongInt);
Begin;
a:=#0#0#0#0+copy(a,length(IPv4addressPrefix)+1,4)+'12341234';
WriteLongLSB(a[9],p);
WriteLongLSB(a[13],t);
skinnySendString($22,a);
End;
