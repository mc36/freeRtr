Type
  OneRealTimeProtocolHeaderRecord=record
    vers:Byte;          {version information}
    prot:Byte;          {protocol type}
    seq:Word;           {sequence number}
    time:LongInt;       {timestamp}
    ssrc:LongInt;       {synchronization source}
    end;
  OneRealTimeControlProtocolHeaderRecord=record
    vers:Byte;          {version information}
    prot:Byte;          {packet type}
    len:Word;           {length in dword}
    ssrc:LongInt;       {synchronization source}
    ntpM:LongInt;       {ntp time msb}
    ntpL:LongInt;       {ntp time lsb}
    time:LongInt;       {timestamp}
    pack:LongInt;       {packets sent}
    byte:LongInt;       {bytes sent}
    end;


Procedure releq2upper(prt:LongInt);
Label f1;
Var
  buf:array[1..4*1024] of byte;
  hdr:OneRealTimeProtocolHeaderRecord absolute buf;
  beg,siz,num:LongInt;
  i,o,p:LongInt;
  a:String;

Procedure mine(var con:OneConnectionRecord);
Begin;
if (con.seqn<0) then con.seqn:=(o-1) and $ffff;
if ((con.seqn-o) and $ffff<$8000) then begin;
  WriteLn('got previous packet from '+ipAddr2string(a)+' '+BStr(p));
  exit;
  end;
con.seqn:=o;
buf[beg]:=hdr.prot;
pipeLineSend(con.pipe,buf[beg],siz+1);
con.timR:=currentTime;
inc(con.recB,siz);
inc(con.recP);
End;

Begin;
o:=sizeof(buf);
if UDPreceivePacket(servPipC[prt],a,p,buf,o) then goto f1;
i:=FindOneConnection(a,p-1,prt);
if (i<1) then goto f1;
WriteLn('got report from '+ipAddr2string(a)+' '+BStr(p));
ConnectionDat^[i].timR:=currentTime;
f1:
o:=sizeof(buf);
if UDPreceivePacket(servPipD[prt],a,p,buf,siz) then exit;
num:=FindOneConnection(a,p,prt);
if (num<1) then begin;
  WriteLn('got from unknown source '+ipAddr2string(a)+' '+BStr(p));
  goto f1;
  end;
if (siz<sizeof(hdr)) then begin;
  WriteLn('got too short packet from '+ipAddr2string(a)+' '+BStr(p));
  goto f1;
  end;
beg:=sizeof(hdr);
if (hdr.vers and $c0<>$80) then begin;
  WriteLn('got bad version from '+ipAddr2string(a)+' '+BStr(p));
  goto f1;
  end;
{padding}
if (hdr.vers and $20<>0) then dec(siz,buf[siz]);
{contributing source}
inc(beg,(hdr.vers and $f) shl 2);
{extensions}
if (hdr.vers and $10<>0) then inc(beg,(ReadWordMSB(buf[beg+3])+1) shl 2);
dec(siz,beg);
if (siz<0) then begin;
  WriteLn('got truncated packet from '+ipAddr2string(a)+' '+BStr(p));
  goto f1;
  end;
o:=ReadWordMSB(hdr.seq);
mine(ConnectionDat^[num]);
goto f1;
End;



Function releq2user(var con:OneConnectionRecord):Boolean;
Label f1,f2;
Var
  buf:array[1..4*1024] of byte;
  ctr:OneRealTimeControlProtocolHeaderRecord absolute buf;
  hdr:OneRealTimeProtocolHeaderRecord absolute buf;
  tcp:OneTCPcommandHeader absolute buf;
  i,o:LongInt;
Begin;
releq2user:=False;
if (getTimePast(con.timR)>5*60) then begin;
  WriteLn('timeout with '+ipAddr2string(con.addr)+' '+BStr(con.port));
  goto f1;
  end;
if (getTimePast(con.timT)>30) then begin;
  if (con.stat<>1) then goto f1;
  con.timT:=currentTime;
  fillchar(ctr,sizeof(ctr),0);
  ctr.vers:=$80;
  ctr.prot:=200;
  WriteWordMSB(ctr.len,6);
  ctr.ssrc:=con.pipe;
  ctr.ntpM:=0;
  ctr.ntpL:=0;
  WriteLongMSB(ctr.time,con.sndB);
  WriteLongMSB(ctr.pack,con.sndP);
  WriteLongMSB(ctr.byte,con.sndB);
  UDPsendPacket(servPipC[con.locP],con.addr,con.port+1,ctr,sizeof(ctr));
  WriteLn('sent report to '+ipAddr2string(con.addr)+' '+BStr(con.port+1));
  end;
f2:
o:=sizeof(buf);
if (pipeLineRecv(con.pipe,buf,o)<>0) then o:=0;
if (o<1) then begin;
  pipeLineStats(con.pipe,o,i,i);
  if (o<>0) then exit;
  WriteLn('upper closed with '+ipAddr2string(con.addr)+' '+BStr(con.port));
  goto f1;
  end;
case con.stat of
  1:begin; {connected}
    dec(o);
    move(buf[2],buf[sizeof(hdr)+1],o);
    hdr.prot:=buf[1];
    hdr.vers:=$80;
    WriteWordMSB(hdr.seq,con.sndP);
    WriteLongMSB(hdr.time,con.sndB);
    WriteLongMSB(hdr.ssrc,con.pipe);
    inc(con.sndB,o);
    inc(con.sndP);
    UDPsendPacket(servPipD[con.locP],con.addr,con.port,buf,o+sizeof(hdr));
    end;
  0:Begin; {waiting}
    if (o<>sizeof(tcp)) then goto f1;
    if (tcp.cmd<>6) then goto f1;
    move(tcp.adr,con.addr,sizeof(con.addr));
    con.port:=tcp.prt and $7ffffffe;
    o:=0;
    for i:=1 to portNum do if (FindOneConnection(con.addr,con.port,i)=0) then begin; o:=i;break; end;
    if (o<1) then goto f1;
    con.locP:=o;
    WriteLn('connected to '+ipAddr2string(con.addr)+' '+BStr(con.port));
    fillchar(tcp,sizeof(tcp),0);
    move(servAddr,tcp.adr,sizeof(tcp.adr));
    tcp.prt:=servPort[con.locP];
    tcp.cmd:=3;
    pipeLineSend(con.pipe,tcp,sizeof(tcp));
    con.seqn:=-1;
    con.timR:=currentTime;
    con.timT:=-99999;
    con.stat:=1;
    end;
  else begin;
    f1:
    releq2user:=True;
    exit;
    end;
  end;
goto f2;
End;
