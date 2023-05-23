Type
  OneProtocolHeader=record
    chkSum:Word;                {checksum}
    length:Word;                {size of packet}
    ctrl:Byte;                  {transport control}
    prot:Byte;                  {packet type}
    trgNet:LongWord;            {target network}
    trgNod:OneHrdwrAddrRecord;  {target node address}
    trgSck:Word;                {target socket}
    srcNet:LongWord;            {source network}
    srcNod:OneHrdwrAddrRecord;  {source node address}
    srcSck:Word;                {source socket}
    end;




Procedure IPXgotOnePacket(var pck:OnePacketRecord);
Var
  buf:record
    tcp:OneTCPcommandHeader;
    dat:array[1..4*1024] of byte;
    end;
  tcp:OneTCPcommandHeader absolute buf;
  inetAddr:OneTCPaddressRecord;
  hdr:OneProtocolHeader;
  i,o,p:LongInt;
Begin;
if (pck.s<sizeof(hdr)) then begin;
  WriteLn('ipx got too short packet!');
  exit;
  end;
move(pck,hdr,sizeof(hdr));
i:=ReadWordMSB(hdr.length);
if (i>pck.s) then begin;
  WriteLn('ipx got truncated packet!');
  exit;
  end;
if (i<sizeof(hdr)) then begin;
  WriteLn('ipx got buggy packet!');
  exit;
  end;
pck.s:=i-sizeof(hdr);
i:=ReadWordMSB(hdr.trgSck);
p:=FindOneLocalSocket(i);
if (p<1) then begin;
  WriteLn('ipx got for socket '+BStr(i)+'!');
  exit;
  end;
ConnectionDat^[p].time:=currentTime;
move(pck.d[sizeof(hdr)+1],buf.dat,pck.s);
fillchar(tcp,sizeof(tcp),0);
tcp.cmd:=5;
move(pck.a,inetAddr[sizeof(inetAddr)-addressSize+1],addressSize);
tcp.adr:=inetAddr;
tcp.prt:=hdr.srcSck;
tcp.dat:=0;
pipeLineSend(ConnectionDat^[p].pipe,buf,sizeof(tcp)+pck.s);
End;




Function relequish2connection(num:LongInt;var con:OneConnectionRecord):Boolean;
Label f1,f2;
Var
  buf:record
    tcp:OneTCPcommandHeader;
    dat:array[1..4*1024] of byte;
    end;
  inetAddr:OneTCPaddressRecord;
  phyAddr:OneHrdwrAddrRecord;
  hdr:OneProtocolHeader;
  pck:OnePacketRecord;
  tcp:OneTCPcommandHeader absolute buf;
  i,o,p:LongInt;
Begin;
if (GetTimePast(con.time)>5*60) then goto f1;
relequish2connection:=false;
case con.stat of
  2:begin; {connection estabilished}
    i:=sizeof(buf);
    if (pipeLineRecv(con.pipe,tcp,i)<>0) then i:=0;
    if (i<sizeof(tcp)) then begin;
      pipeLineStats(con.pipe,i,o,p);
      if (i=0) then goto f1;
      exit;
      end;
    if (tcp.cmd<>5) then exit;
    p:=i-sizeof(tcp);
    inetAddr:=tcp.adr;
    if TCPcompareAddress(broadcastAddr,inetAddr) then phyAddr:=bcastAddress else
     move(inetAddr[sizeof(inetAddr)-sizeof(phyAddr)+1],phyAddr,sizeof(phyAddr));
    fillchar(hdr,sizeof(hdr),0);
    hdr.chkSum:=$ffff;
    WriteWordMSB(hdr.length,sizeof(hdr)+p);
    hdr.ctrl:=0;
    hdr.prot:=4;
    hdr.trgNet:=0;
    hdr.trgNod:=phyAddr;
    WriteWordMSB(hdr.trgSck,tcp.prt);
    hdr.srcNet:=0;
    hdr.srcNod:=localAddress;
    WriteWordMSB(hdr.srcSck,con.prt);
    pck.s:=sizeof(hdr)+p;
    pck.a:=phyAddr;
    move(hdr,pck.d,sizeof(hdr));
    move(buf.dat,pck.d[sizeof(hdr)+1],p);
    SendPacket(pck);
    con.time:=CurrentTime;
    end;
  1:begin; {wait for udp command}
    pipeLineStats(con.pipe,i,o,p);
    if (i=0) then goto f1;
    if (o<sizeof(tcp)) then exit;
    i:=sizeof(tcp);
    if (pipeLineRecv(con.pipe,tcp,i)<>0) then i:=0;
    if (i<>sizeof(tcp)) then exit;
    con.time:=CurrentTime;
    if (tcp.cmd<>4) then goto f1;
    BugOS_ProcessName(con.proc,buf.dat,i,i,o);
    p:=tcp.prt;
    if (o and $40=0) then p:=0;
    f2:
    i:=FindOneLocalSocket(p);
    if (p=0) or (p=$ffff) then i:=1;
    if (i<>0) then begin;
      p:=$a000+random($3000);
      goto f2;
      end;
    con.prt:=p;
    con.stat:=2;
    inetAddr:=tcp.adr;
    move(inetAddr[sizeof(inetAddr)-addressSize+1],con.adr[addressBegin],addressSize);
    fillchar(tcp,sizeof(tcp),0);
    tcp.cmd:=3;
    fillchar(inetAddr,sizeof(inetAddr),0);
    move(localAddress,inetAddr[sizeof(inetAddr)-addressSize+1],addressSize);
    tcp.adr:=inetAddr;
    tcp.prt:=con.prt;
    tcp.dat:=0;
    pipeLineSend(con.pipe,tcp,sizeof(tcp));
    end;
  else begin; f1:con.stat:=0;relequish2connection:=true; end;
  end;
End;
