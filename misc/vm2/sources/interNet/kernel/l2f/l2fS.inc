Const defaultRetry=16;

Procedure sendOnePacket(var con:OneConnectionRecord;proto,seq,mid,size:Longint;var buffer);
Var
  buf:array[1..1] of byte absolute buffer;
  i,o:LongInt;
Begin;
{$ifdef debug}
WriteLn('sending size='+BStr(size)+' proto='+BStr(proto)+' seq='+BStr(seq)+' mid='+BStr(mid)+' to '+ipAddr2string(con.addr)+' '+BStr(con.port));
{$endif}
move(buf[1],buf[17],size);
inc(size,16);
if (seq<0) then begin; i:=$c001;seq:=0; end else i:=$d001;
writeWordMSB(buf[1],i);
buf[3]:=proto;
buf[4]:=seq;
writeWordMSB(buf[5],mid);
writeWordMSB(buf[7],con.tuneR);
writeWordMSB(buf[9],size);
writeWordMSB(buf[11],0);
writeLongMSB(buf[13],0);
UDPsendPacket(servPipe,con.addr,con.port,buf,size);
End;

Procedure sendDisconnect(var con:OneConnectionRecord);
Var buf:array[1..1024] of byte;
Begin;
pipeLineClose(con.pipe);
con.pipe:=0;
if (con.stat=0) then exit;
con.stat:=0;
buf[1]:=3;
sendOnePacket(con,1,con.cmdSeq+1,0,1,buf);
End;

Procedure startUpperLayer(var con:OneConnectionRecord);
Var
  a:String;
  i,o:LongInt;
Begin;
a:=con.cmnd;
{$ifdef debug}
WriteLn('starting '+a+'...');
{$endif}
i:=pos(' ',a);
if (i<1) then i:=666;
if (xExecBgnd(copy(a,1,i-1),copy(a,i+1,255),o)<>0) then o:=0;
if (o<>0) then exit;
WriteLn('failed to start upper process!');
End;



Procedure gotOnePacket(var addr;port:LongInt;var buffer;size:LongInt);
Label f1,f2,f3;
Var
  buf:array[1..1] of byte absolute buffer;
  a:String;
  ab:array[0..128] of byte absolute a;
  ab0:byte absolute a;
  hd,ps:LongInt;
  msgTun:LongInt;
  msgSes:LongInt;
  msgSeq:LongInt;
  msgPro:LongInt;
  con:OneConnectionRecord;
  usr:LongInt;
  i,o,p:LongInt;
Begin;
hd:=readWordMSB(buf);
if (hd and 7<>1) then begin;
  WriteLn('got invalid version from '+ipAddr2string(addr)+' '+BStr(port));
  exit;
  end;
msgPro:=buf[3];
msgSeq:=buf[4];
msgSes:=readWordMSB(buf[5]);
msgTun:=readWordMSB(buf[7]);
i:=readWordMSB(buf[9]);
if (i>size) then begin;
  WriteLn('got truncated packet from '+ipAddr2string(addr)+' '+BStr(port));
  exit;
  end;
size:=i;
ps:=10;
if (hd and $8000<>0) then begin;
  p:=ReadWordMSB(buf[ps+1]);
  inc(ps,2);
  end else p:=0;
if (hd and $4000<>0) then begin;
  i:=ReadLongMSB(buf[ps+1]);
  inc(ps,4);
  end else i:=0;
inc(ps,p);
if (ps>size) then begin;
  WriteLn('got undersized packet from '+ipAddr2string(addr)+' '+BStr(port));
  exit;
  end;
{$ifdef debug}
WriteLn('received size='+BStr(size-ps)+' proto='+BStr(msgPro)+' seq='+BStr(msgSeq)+' cid='+BStr(msgTun)+' mid='+BStr(msgSes)+' from '+ipAddr2string(addr)+' '+BStr(port));
{$endif}

if (msgPro=1) and (msgSeq=0) and (msgTun=0) and (msgSes=0) and (buf[ps+1]=1) then begin; {open}
  inc(ps);
  usr:=0;
  for i:=1 to ConnectionNum do begin;
    if (ConnectionDat^[i].tuneR<>msgTun) then continue;
    if (ConnectionDat^[i].port<>port) then continue;
    if TCPcompareAddress(ConnectionDat^[i].addr,addr) then usr:=i;
    end;
  if (usr=0) then usr:=FindConnectByStat(0) else
    WriteLn('got duplicated confreq from '+ipAddr2string(addr)+' '+BStr(port));
  if (usr<=0) then begin;
    WriteLn('out of connection slots!');
    exit;
    end;
  fillchar(con,sizeof(con),0);
  con.cmnd:=ConnectionDat^[usr].cmnd;
  repeat
    i:=random($fff8)+4;
    until (FindConnectByID(i)=0);
  con.tuneL:=i;
  move(addr,con.addr,sizeof(OneTCPaddressRecord));
  con.port:=port;
  con.stat:=1;
  con.datSeq:=0;
  p:=-1;
  while (ps<size) do begin;
    inc(ps);
    case buf[ps] of
      2:begin; {name}
        move(buf[ps+1],a,sizeof(a));
        inc(ps,ab0+1);
        end;
      3:begin; {chal}
        move(buf[ps+1],a,sizeof(a));
        inc(ps,ab0+1);
        end;
      4:begin; {clid}
        p:=readLongMSB(buf[ps+1]);
        inc(ps,5);
        end;
      else break;
      end;
    end;
  if (p and $ffff<>p) then begin;
    WriteLn('got invalid confreq from '+ipAddr2string(addr)+' '+BStr(port));
    exit;
    end;
  con.tuneR:=p;
  buf[1]:=1;    {confreq}
  buf[2]:=2;    {name};
  a:=servName;
  move(a,buf[3],sizeof(a));
  ps:=ab0+3;
  ab0:=16;
  for i:=1 to ab0 do ab[i]:=random($100);
  inc(ps);
  buf[ps]:=3;   {chal}
  inc(ps);
  move(ab0,buf[ps],sizeof(a));
  inc(ps,ab0);
  buf[ps+1]:=4; {clid}
  writeLongMSB(buf[ps+2],con.tuneL);
  inc(ps,5);
  sendOnePacket(con,1,con.cmdSeq,0,ps,buf);
  WriteLn('connection accepted from '+ipAddr2string(addr)+' '+BStr(port));
  goto f1;
  end;

usr:=FindConnectByID(MsgTun);
if (usr<=0) then begin;
  f2:
  WriteLn('got bad tunnel id from '+ipAddr2string(addr)+' '+BStr(port));
  exit;
  end;
con:=ConnectionDat^[usr];
if (con.port<>port) then goto f2;
if not TCPcompareAddress(con.addr,addr) then goto f2;

if (msgPro<>1) then begin;
  if (msgSes<>con.sessN) then begin;
    {$ifdef debug}
    WriteLn('got bad session id from '+ipAddr2string(addr)+' '+BStr(port));
    {$endif}
    sendDisconnect(con);
    exit;
    end;
  pipeLineSend(con.pipe,buf[ps+1],size-ps);
  goto f1;
  end;

if (msgSes=0) then p:=con.cmdSeq else p:=con.datSeq;
case (msgSeq-p) and $ff of
  1:;
  0:begin;
    {$ifdef debug}
    WriteLn('got previous packet from '+ipAddr2string(addr)+' '+BStr(port));
    {$endif}
    end;
  2..64:begin;
    {$ifdef debug}
    WriteLn('got future packet from '+ipAddr2string(addr)+' '+BStr(port));
    {$endif}
    end;
  else begin;
    {$ifdef debug}
    WriteLn('got old packet from '+ipAddr2string(addr)+' '+BStr(port));
    {$endif}
    exit;
    end;
  end;
if (msgSes=0) then con.cmdSeq:=msgSeq else if (msgSes=con.sessN) then con.datSeq:=msgSeq;

inc(ps);
case buf[ps] of
  1:begin;
    {$ifdef debug}
    WriteLn('got confreq from '+ipAddr2string(addr)+' '+BStr(port));
    {$endif}
    sendDisconnect(con);
    goto f1;
    end;
  2:;
  3:begin;
    WriteLn('got close from '+ipAddr2string(addr)+' '+BStr(port));
    sendDisconnect(con);
    goto f1;
    end;
  4:begin;
    {$ifdef debug}
    WriteLn('got echoreq from '+ipAddr2string(addr)+' '+BStr(port));
    {$endif}
    buf[ps]:=5;
    sendOnePacket(con,1,con.cmdSeq,0,size-ps+1,buf[ps]);
    goto f1;
    end;
  5:begin;
    {$ifdef debug}
    WriteLn('got echorep from '+ipAddr2string(addr)+' '+BStr(port));
    {$endif}
    goto f1;
    end;
  else begin;
    {$ifdef debug}
    WriteLn('got unknown command from '+ipAddr2string(addr)+' '+BStr(port));
    {$endif}
    sendDisconnect(con);
    goto f1;
    end;
  end;

{$ifdef debug}
WriteLn('got open from '+ipAddr2string(addr)+' '+BStr(port));
{$endif}
if (msgSes=0) then begin;
  ab0:=16;
  for i:=1 to ab0 do ab[i]:=0;
  buf[1]:=2;    {open}
  buf[2]:=3;    {name}
  move(a,buf[3],sizeof(a));
  sendOnePacket(con,1,con.cmdSeq,0,ab0+3,buf);
  con.stat:=2;
  goto f1;
  end;
if (con.sessN=0) then begin;
  con.sessN:=msgSes;
  startUpperLayer(con);
  con.stat:=3;
  f3:
  buf[1]:=2;    {open}
  sendOnePacket(con,1,con.datSeq,con.sessN,1,buf);
  goto f1;
  end;
if (msgSes=con.sessN) then goto f3;
buf[1]:=3;      {close}
sendOnePacket(con,1,msgSeq,msgSes,1,buf);

f1:
con.time:=currentTime;
con.retry:=defaultRetry;
ConnectionDat^[usr]:=con;
End;




Procedure relequishConn(var con:OneConnectionRecord);
Label f1;
Var
  buf:array[1..2*1024] of byte;
  i,o:LongInt;
  a:String;
Begin;
if (con.stat=0) then exit;
if (con.retry=defaultRetry) then i:=60 else i:=5;
if (getTimePast(con.time)<i) then goto f1;
dec(con.retry);
if (con.retry<0) then begin;
  WriteLn('timeout while sending to '+ipAddr2string(con.addr)+' '+BStr(con.port));
  sendDisconnect(con);
  exit;
  end;
con.time:=currentTime;
WriteLn('sending keepalive to '+ipAddr2string(con.addr)+' '+BStr(con.port));
buf[1]:=4;
sendOnePacket(con,1,con.cmdSeq+1,0,1,buf);
f1:
if (con.stat<>4) then exit;
o:=sizeof(buf);
if (pipeLineRecv(con.pipe,buf,o)<>0) then o:=0;
if (o>0) then begin;
  sendOnePacket(con,2,-1,con.sessN,o,buf);
  exit;
  end;
pipeLineStats(con.pipe,o,i,i);
if (o<>0) then exit;
WriteLn('upper terminated with '+ipAddr2string(con.addr)+' '+BStr(con.port));
sendDisconnect(con);
End;
