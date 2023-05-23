Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Var
  prAdr,myAdr:OneTCPaddressRecord;
  prPrt,myPrt:LongInt;
  udpPipe:LongInt;
  upAdrMy,upAdrPr:OneTCPaddressRecord;
  uprPipe:LongInt;
  reqAddr:Byte;
  userPass:String;
  lastSent:LongInt;
  lastRcvd:LongInt;


Procedure openTunnelConn;
Var
  client4,client6:OneTCPaddressRecord;
  server4,server6:OneTCPaddressRecord;
  buf:array[1..1024*2] of byte;
  i,o:LongInt;
  a,b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;

Procedure pktXchg(name:String);
Label f1,f2;
Var retry:LongInt;
Begin;
retry:=10;
f1:
dec(retry);
if (retry<0) then immErr('remote not responding!');
WriteLn('sending '+name+'...');
UDPsendPacket(udpPipe,prAdr,prPrt,b[1],length(b));
timer2start;
lastSent:=currentTime;
f2:
o:=sizeof(buf);
if (UDPreceivePacket(udpPipe,a,i,buf,o)<>0) then o:=0;
if (o<1) then begin;
  relequish;
  timer2start;
  if (getTimePast(lastSent)>=5) then goto f1;
  goto f2;
  end;
if (i<>prPrt) then goto f2;
if not TCPcompareAddress(a,prAdr) then goto f2;
i:=o;
if (i>255) then i:=255;
ab0:=i;
move(buf,a[1],ab0);
if (copy(a,1,8)<>copy(b,1,8)) then begin;
  WriteLn('got bad sequence number!');
  goto f1;
  end;
a:=copy(a,9,666);
dec(o,8);
move(buf[9],buf,o);
End;

Procedure PutLen;
Begin;
b:='Content-length: '+BStr(length(b))+#13#10+b;
End;

Procedure getLine;
Begin;
i:=o;
if (i>255) then i:=255;
ab0:=i;
move(buf,a[1],ab0);
ab0:=pos(#13,a);
dec(o,ab0);
move(buf[ab0+1],buf,o);
kicserel(#13,' ',a);
kicserel(#10,' ',a);
kicserel(#0,' ',a);
kicserel(#9,' ',a);
kicserel(#255,' ',a);
parseXMLcut(a);
a:=kicsi(a);
End;

Procedure adder(i:longint);
Var b,c:String;
Begin;
if parseXMLadd(i,b,c) then exit;
if (copy(b,length(b)-12,666)=#13'address'#13'type') then begin;
  parseXMLtag(c);
  exit;
  end;
if (b=#13'tunnel'#13'client'#13'address'#13'ipv4') then string2ipAddr(c,client4);
if (b=#13'tunnel'#13'client'#13'address'#13'ipv6') then string2ipAddr(c,client6);
if (b=#13'tunnel'#13'server'#13'address'#13'ipv4') then string2ipAddr(c,server4);
if (b=#13'tunnel'#13'server'#13'address'#13'ipv6') then string2ipAddr(c,server6);
End;


Begin;
b:=#0#0#0#240#0#0#0#0'VERSION=2.0.0';
pktXchg('version request');
b:='CAPABILITY ';
if (copy(a,1,length(b))<>b) then immErr('got bad packet: '+a);

b:=#1#0#0#240#0#0#0#0'AUTHENTICATE PLAIN';
pktXchg('authenticate request');
if (a<>'') then immErr('got bad packet: '+a);

b:=#2#0#0#240#0#0#0#0+userPass;
pktXchg('user information');
b:='200 ';
if (copy(a,1,length(b))<>b) then immErr('got bad packet: '+a);

if isAddressIPv4mask(prAdr) then i:=4 else i:=6;
b:='<tunnel action="create" type="v'+BStr(reqAddr)+'udpv'+BStr(i)+'" proxy="no"><client>'+
   '<address type="ipv'+BStr(i)+'">'+ipAddr2string(myAdr)+'</address>'+
   '<keepalive interval="30"></keepalive>'+
   '</client></tunnel>'#13#10;
PutLen;
b:=#3#0#0#240#0#0#0#0+b;
pktXchg('tunnel creation');
getLine;
i:=pos(':',a);
if (i<1) then i:=ab0+1;
b:=copy(a,i+1,666);
a:=copy(a,1,i-1);
parseXMLcut(a);
parseXMLcut(b);
if (a<>'content-length') then immErr('got bad packet: '+a);
i:=BVal(b);
if (i>o) then immErr('got truncated packet: got '+BStr(o)+', said '+BStr(i));
o:=i;
getLine;
b:='200 ';
if (copy(a,1,length(b))<>b) then immErr('got bad status: '+a);

fillchar(upAdrMy,sizeof(upAdrMy),0);
fillchar(upAdrPr,sizeof(upAdrPr),0);
client4:=upAdrMy;
client6:=upAdrMy;
server4:=upAdrPr;
server6:=upAdrPr;
parseXMLinit;
for i:=1 to o do adder(buf[i]);
WriteLn('client ipv4 address: '+ipAddr2string(client4));
WriteLn('client ipv6 address: '+ipAddr2string(client6));
WriteLn('server ipv4 address: '+ipAddr2string(server4));
WriteLn('server ipv6 address: '+ipAddr2string(server6));

b:='<tunnel action="accept"></tunnel>'#13#10;
PutLen;
b:=#4#0#0#240#0#0#0#0+b;
pktXchg('tunnel accept');
if (a<>'') then immErr('got bad packet: '+a);
timer2start;

if TCPcompareAddress(client4,upAdrMy) then client4:=client6;
if TCPcompareAddress(client6,upAdrMy) then client6:=client4;
if TCPcompareAddress(server4,upAdrPr) then server4:=server6;
if TCPcompareAddress(server6,upAdrPr) then server6:=server4;
if (reqAddr=4) then begin;
  upAdrMy:=client4;
  upAdrPr:=server4;
  end else begin;
  upAdrMy:=client6;
  upAdrPr:=server6;
  end;
lastSent:=currentTime;
lastRcvd:=currentTime;
WriteLn('connection successfully estabilished!');
WriteLn('  my address: '+ipAddr2string(upAdrMy));
WriteLn('peer address: '+ipAddr2string(upAdrPr));
End;




Procedure releq2tunnel;
Var
  buf:array[1..1024*2] of byte;
  i,o:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;

Procedure doUpp;
Label f1,f2;
Var
  pip:LongInt;
  i,o,p:LongInt;
  a,b:String;
Begin;
f1:
if (pipeLineGetIncoming(pip)<>0) then exit;
pipeLineStats(pip,p,i,o);
BugOS_ProcessName(p,buf,i,i,o);
if (o and $40=0) then goto f2;
i:=128;
if (pipeLineRecv(pip,b[1],i)<>0) then i:=0;
b[0]:=chr(i);
a:=kicsi(copy(b,1,8));
b:=copy(b,9,255);
case reqAddr of
  4:begin;
    if (a='arpadd--') then begin;
      a:='ok';
      goto f2;
      end;
    if (a='arpread-') then begin;
      a:='arpdata'#0#0#0#0;
      goto f2;
      end;
    if (a='param---') then begin;
      b[0]:=#4;
      move(upAdrMy[13],b[1],4);
      a:='param'+b;
      move(upAdrPr[13],b[1],4);a:=a+b;
      a:=a+#255#255#255#255;
      goto f2;
      end;
    if (a='data----') and (uprPipe=0) then begin;
      a:='data';
      pipeLineSend(pip,a[1],length(a));
      uprPipe:=pip;
      goto f1;
      end;
    end;
  6:begin;
    if (a='adradd--') then begin;
      a:='ok';
      goto f2;
      end;
    if (a='adrread-') then begin;
      a:='adrdata'#0#0#0#0;
      goto f2;
      end;
    if (a='param6--') then begin;
      a:='param6'#0#0#0+#$fe#$80#0#0#0#0#0#0#0#0#0#0#0#0#0#0;
      b[0]:=#16;
      move(upAdrMy[1],b[1],16);a:=a+b;
      move(upAdrPr[1],b[1],16);a:=a+b;
      a:=a+#255#255#255#255#255#255#255#255#255#255#255#255#255#255#255#255;
      goto f2;
      end;
    if (a='data6---') and (uprPipe=0) then begin;
      a:='data6';
      pipeLineSend(pip,a[1],length(a));
      uprPipe:=pip;
      goto f1;
      end;
    end;
  end;

a:='error';
f2:
pipeLineSend(pip,a[1],length(a));
pipeLineClose(pip);
goto f1;
End;

Begin;
relequish;
timer2start;
if (getTimePast(lastSent)>30) then begin;
  a:='ping';
  UDPsendPacket(udpPipe,upAdrPr,7,a[1],length(a));
  lastSent:=currentTime;
  end;
if (getTimePast(lastRcvd)>60*5) then immErr('remote does not responding!');

doUpp;
if (uprPipe=0) then exit;
o:=sizeof(buf);
if (pipeLineRecv(uprPipe,buf,o)<>0) then o:=0;
if (o<1) then begin;
  pipeLineStats(uprPipe,o,i,i);
  if (o=0) then immErr('upper level closed connection!');
  end else begin;
  if (reqAddr=4) then begin;
    dec(o,4);
    move(buf[5],buf,o);
    end else begin;
    move(i,buf,sizeof(i));
    if (i<>0) then exit;
    dec(o,20);
    move(buf[21],buf,o);
    end;
  if (o<4) then exit;
  UDPsendPacket(udpPipe,prAdr,prPrt,buf,o);
  lastSent:=currentTime;
  end;

o:=sizeof(buf);
if (UDPreceivePacket(udpPipe,a,i,buf,o)<>0) then o:=0;
if (o<1) then exit;
if (i<>prPrt) then exit;
if not TCPcompareAddress(a,prAdr) then exit;
lastRcvd:=currentTime;
if (reqAddr=4) then begin;
  move(buf,buf[5],o);
  inc(o,4);
  move(upAdrPr[13],buf,4);
  end else begin;
  move(buf,buf[21],o);
  inc(o,20);
  i:=0;
  move(i,buf,4);
  move(upAdrPr[1],buf[5],16);
  end;
pipeLineSend(uprPipe,buf,o);
End;
