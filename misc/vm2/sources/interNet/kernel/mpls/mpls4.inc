Procedure relequish2udp;
Label f1;
Var
  buf:array[1..4096] of byte;
  hdr:OneLDPmessageHeader absolute buf;
  i,o,p:LongInt;
  a:String;
Begin;
f1:
p:=sizeof(buf);
if UDPreceivePacket(localPipe,a,o,buf,p) then exit;
o:=convert2ip4addr(a);
i:=findOnePeerAddr(o);
if (i<1) then begin;
  WriteLn('got udp from invalid host '+ipAddr2string(a));
  goto f1;
  end;
if (ReadWordMSB(hdr.ver)<>1) then begin;
  WriteLn('got invalid version from '+ipAddr2string(a));
  goto f1;
  end;
dec(p,sizeof(hdr));
o:=ReadWordMSB(hdr.len)-6;
if (o>p) then begin;
  WriteLn('got truncated from '+ipAddr2string(a));
  goto f1;
  end;
gotOneConnPacket(ConnectionDat[i],buf[sizeof(hdr)+1],o,1);
goto f1;
End;



Procedure gotNewIncomingConnection(pipe:LongInt);
Label f1;
Var
  i,o,p:LongInt;
  a,b:String;
Begin;
pipeLineStats(pipe,o,i,i);
if (o=TCPprocessId) then begin;
  currTcpPipe:=pipe;
  exit;
  end;
for i:=1 to SessionNum do if (SessionDat[i].pipe=0) then goto f1;
pipeLineClose(pipe);
exit;
f1:
SessionDat[i].pipe:=pipe;
a:=convertFromIP4addr(SessionDat[i].addr);
a:=ipAddr2string(a[1])+' '+BStr(SessionDat[i].vcid);
WriteLn('upper with '+a+' logged in!');
a:='mpls with '+a;
if (SessionDat[i].mode in [4,5]) then begin;
  b:='';
  for i:=1 to sizeof(virtMacAd) do b:=b+chr(virtMacAd[i]);
  for i:=1 to sizeof(virtMacAd) do b:=b+#255;
  i:=sizeof(virtMacAd);
  end else begin;
  b:=chr(virtMacAd[sizeof(virtMacAd)])+#255;
  i:=1;
  end;
a:='12341234'#0#0#0#0#0#0#0#0+b+a+#0;
move(i,a[1],sizeof(i));
i:=1400;move(i,a[5],sizeof(i));
pipeLineSend(pipe,a[1],length(a));
End;



Procedure relequish2conn(var con:OneConnectionRecord);
Label f1;
Var
  buf:array[1..4096] of byte;
  hdr:OneLDPmessageHeader absolute buf;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
Begin;
if (getTimePast(con.timH)>15) then sendOneConnHello(con);
case con.stat of
  3:begin; {ready}
    if (getTimePast(con.timK)>30) then sendOneConnKeepalive(con);
    if (con.bufS<1) then begin;
      pipeLineStats(con.pipe,p,i,o);
      if (p=0) then goto f1;
      if (i<sizeof(hdr)) then exit;
      o:=sizeof(hdr);
      pipeLineRecv(con.pipe,hdr,o);
      if (o<>sizeof(hdr)) then goto f1;
      if (ReadWordMSB(hdr.ver)<>1) then begin;
        WriteLn('got invalid version!');
        goto f1;
        end;
      i:=ReadWordMSB(hdr.len)-6;
      if (i<1) or (i>sizeof(buf)) then begin;
        WriteLn('got invalid size!');
        goto f1;
        end;
      con.bufS:=i;
      end;
    pipeLineStats(con.pipe,p,i,o);
    if (p=0) then goto f1;
    if (i<con.bufS) then exit;
    o:=con.bufS;
    pipeLineRecv(con.pipe,buf,o);
    if (o<>con.bufS) then goto f1;
    con.bufS:=0;
    gotOneConnPacket(con,buf,o,0);
    end;
  0:begin; {free}
    if (getTimePast(con.timK)<60) then exit;
    if (con.rcvN<3) then exit;
    if (getTimePast(con.rcvT)>30) then exit;
    con.timK:=currentTime;
    clearOneConn(con);
    if (compareIp4addrs(localAddr,con.addr)<1) then exit;
    a:=convertFromIP4addr(con.addr);
    WriteLn('connecting to '+ipAddr2string(a[1]));
    TCPbeginConnect(con.pipe,65536,a[1],localPort);
    con.stat:=1;
    end;
  1:begin; {opening}
    if not TCPlookConnected(con.pipe,a,i,o) then con.stat:=2;
    if (con.pipe=0) then goto f1;
    end;
  2:begin; {opened}
    if (con.conn=1) then begin;
      getNextHopMacAddr;
      o:=0;
      for i:=1 to sizeof(firstHopM) do inc(o,firstHopM[i]);
      if (o=0) then goto f1;
      con.labl:=0;
      end;
    con.sndM:=1;p:=0; {init}
    addOneTLV(buf,p,$500,#0#1#0#$b4#0#0#4#0+convertLong2str(con.addr)+#0#0);
    addOneMsgHeader(buf,p,$200,con.sndM);
    sendOneConnPacket(con,buf,p,0);
    sendOneConnKeepalive(con);
    inc(con.sndM);p:=0; {address}
    addOneTLV(buf,p,$101,#0#1+convertLong2str(localAddr));
    addOneMsgHeader(buf,p,$300,con.sndM);
    sendOneConnPacket(con,buf,p,0);
    inc(con.sndM);p:=0; {label}
    addOneTLV(buf,p,$100,#2#0#1+convertIp4prefix(localAddr,32));
    WriteLongMSB(i,labelLocalAddr);
    addOneTLV(buf,p,$200,convertLong2str(i));
    addOneMsgHeader(buf,p,$400,con.sndM);
    sendOneConnPacket(con,buf,p,0);
    con.stat:=3;
    end;
  else begin; f1:clearOneConn(con); end;
  end;
End;
