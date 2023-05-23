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
  prAdr:OneTCPaddressRecord;
  prPrt:LongInt;
  udpPipe:LongInt;
  uprPipe:LongInt;
  tuneL:LongInt;
  tuneR:LongInt;
  sessN:LongInt;
  cmdSeq:LongInt;
  datSeq:LongInt;
  rxSeq:LongInt;
  txKey:LongInt;
  rxKey:LongInt;
  msgKey:LongInt;
  msgSes:LongInt;




Procedure sendPacket(var buffer;size,proto,sess:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
Begin;
move(buf[1],buf[17],size);
inc(size,16);
writeWordMSB(buf,$d001);  {offset, sequence, key, ver=1}
buf[3]:=proto;
if (sess=0) then begin;
  buf[4]:=cmdSeq;
  end else begin;
  buf[4]:=datSeq;
  end;
writeWordMSB(buf[5],sess);
writeWordMSB(buf[7],tuneR);
writeWordMSB(buf[9],size);
writeWordMSB(buf[11],0);  {offset}
move(txKey,buf[13],sizeof(txKey));
UDPsendPacket(udpPipe,prAdr,prPrt,buf,size);
End;



Function recvPacket(var buffer;var size:LongInt;time:LongInt):LongInt;
Label f1,f2;
Var
  buf:array[1..1] of byte absolute buffer;
  a:String;
  begTim:LongInt;
  i,o,p:LongInt;
  msgOfs:LongInt;
  msgFlg:LongInt;
  msgPro:LongInt;

Function testSeq(var seq:LongInt):LongInt;
Begin;
testSeq:=1;
i:=(o-seq) and $ff;
if (i>64) and (i<255) then exit;
seq:=o;
testSeq:=0;
End;

Begin;
recvPacket:=-666;
timer2start;
begTim:=currentTime;
f1:
size:=2*1024;
if UDPreceivePacket(udpPipe,a,i,buf,size) then size:=0;
if (size<1) then begin;
  if (getTimePast(begTim)>time) then exit;
  relequish;
  timer2start;
  goto f1;
  end;
if not TCPcompareAddress(a,prAdr) or (prPrt<>i) then begin;
  WriteLn('got packet from '+ipAddr2string(a)+' '+BStr(i));
  goto f1;
  end;
msgFlg:=readWordMSB(buf);
if (msgFlg and 7<>1) then begin;
  WriteLn('got invalid version');
  goto f1;
  end;
msgPro:=buf[3];
msgOfs:=0;
msgKey:=0;
if (msgFlg and $1000=0) then o:=-1 else o:=buf[4];
msgSes:=readWordMSB(buf[5]);
if (msgSes<>0) and (msgSes<>sessN) then begin;
  WriteLn('got invalid session id');
  goto f1;
  end;
if (msgPro<>1) then o:=-1;
if (o>=0) then begin;
  if (msgSes=0) then i:=testSeq(cmdSeq) else i:=testSeq(datSeq);
  if (i<>0) then begin;
    WriteLn('got old packet');
    goto f1;
    end;
  end;
i:=readWordMSB(buf[7]);
if (i<>tuneL) then begin;
  WriteLn('got invalid tunnel id');
  goto f1;
  end;
i:=readWordMSB(buf[9]);
if (i>size) then begin;
  WriteLn('got truncated packet');
  goto f1;
  end;
size:=i;
p:=10;
if (msgFlg and $8000<>0) then begin;
  msgOfs:=ReadWordMSB(buf[p+1]);
  inc(p,2);
  end;
if (msgFlg and $4000<>0) then begin;
  msgKey:=ReadLongMSB(buf[p+1]);
  inc(p,4);
  end;
inc(p,msgOfs);
dec(size,p);
if (size<0) then begin;
  WriteLn('got undersized packet');
  goto f1;
  end;
move(buf[p+1],buf,size);
recvPacket:=msgPro;
if (msgPro<>1) then exit;
if (size>0) then case buf[1] of
  3:immErr('disconnect req received!');
  4:begin; {echo req}
    WriteLn('got echo request');
    buf[1]:=5;
    sendPacket(buf,size,1,0);
    end;
  5:begin; {echo reply}
    WriteLn('got echo reply');
    end;
  end;
if (msgSes=0) then cmdSeq:=(cmdSeq+1) and $ff else datSeq:=(datSeq+1) and $ff;
End;




Procedure openTunnelConn;
Label f1,f2,f3,f4,f5,f6;
Var
  buf:array[1..2*1024] of byte;
  i,o,p:LongInt;
  a:string;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  retry:LongInt;
  txChal:String;
  rxChal:String;
Begin;
cmdSeq:=0;
datSeq:=0;
tuneL:=random($fff8)+1;
tuneR:=0;
sessN:=0;
txKey:=0;
rxKey:=0;

buf[1]:=16;
for i:=2 to 17 do buf[i]:=random($100);
move(buf,txChal,sizeof(txChal));
rxChal:='';

retry:=10;
f1:
dec(retry);
if (retry<0) then immErr('remote not responding!');
WriteLn('sending req config...');
buf[1]:=1;                      {conf}
buf[2]:=2;                      {conf_name}
a:=getHostName;
move(a,buf[3],sizeof(a));
p:=ab0+3;
buf[p+1]:=3;                    {conf_chal}
inc(p);
move(txChal,buf[p+1],sizeof(txChal));
inc(p,length(txChal)+1);
buf[p+1]:=4;                    {conf_clid}
writeLongMSB(buf[p+2],tuneL);
inc(p,5);
sendPacket(buf,p,1,0);
f2:
i:=recvPacket(buf,p,5);
if (i<0) then goto f1;
if (i<>1) then begin;
  WriteLn('got invalid protocol');
  goto f2;
  end;
if (buf[1]<>1) then begin;
  WriteLn('got invalid message');
  goto f2;
  end;
WriteLn('got req config...');
o:=1;
f3:
if (o>p) then goto f4;
inc(o);
case buf[o] of
  2:begin; {name}
    move(buf[o+1],a,sizeof(a));
    inc(o,ab0+1);
    end;
  3:begin; {chal}
    move(buf[o+1],a,sizeof(a));
    inc(o,ab0+1);
    rxChal:=a;
    end;
  4:begin; {clid}
    tuneR:=readLongMSB(buf[o+1]);
    inc(o,5);
    end;
  else immErr('got invalid suboption!');
  end;
goto f3;

f4:
retry:=10;
f5:
dec(retry);
if (retry<0) then immErr('remote not responding!');

txKey:=0;
WriteLn('sending accept config...');
buf[1]:=2;                      {open}
buf[2]:=3;                      {chal_resp}
ab0:=16;
for i:=1 to ab0 do ab[i]:=0;
move(a,buf[3],sizeof(a));
p:=ab0+3;
sendPacket(buf,p,1,0);
f6:
i:=recvPacket(buf,p,5);
if (i<0) then goto f5;
if (i<>1) then begin;
  WriteLn('got invalid protocol');
  goto f6;
  end;
if (buf[1]<>2) then begin;
  WriteLn('got invalid message');
  goto f6;
  end;
WriteLn('got accept config...');
End;



Procedure openSessionConn;
Label f1,f2;
Var
  buf:array[1..2*1024] of byte;
  i,o,p:LongInt;
  a:string;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  retry:LongInt;
Begin;
sessN:=random($fff8)+1;
retry:=10;

f1:
dec(retry);
if (retry<0) then immErr('remote not responding!');

WriteLn('sending open config...');
buf[1]:=2;
buf[2]:=6;              {auth type}
buf[3]:=3;
buf[4]:=1;              {auth name}
a:=getHostName;
move(a,buf[5],sizeof(a));
p:=ab0+5;
buf[p+1]:=3;            {auth resp}
buf[p+2]:=0;
inc(p,2);
sendPacket(buf,p,1,sessN);
f2:
i:=recvPacket(buf,p,5);
if (i<0) then goto f1;
if (i<>1) then begin;
  WriteLn('got invalid protocol...');
  goto f2;
  end;
if (buf[1]<>2) then begin;
  WriteLn('got invalid message');
  goto f2;
  end;
WriteLn('got open config...');
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


Procedure closeTunnelConn;
Var b:Byte;
Begin;
b:=3;
sendPacket(b,1,1,0);
End;



Procedure doOneConnection;
Label f1;
Var
  buf:array[1..2*1024] of byte;
  i,o,p:LongInt;
  tim,ret:LongInt;
Begin;
tim:=currentTime;
ret:=16;
f1:
relequish;
i:=sizeof(buf)-16;
if (pipeLineRecv(uprPipe,buf[17],i)<>0) then i:=0;
if (i>0) then begin;
  inc(i,16);
  writeWordMSB(buf,$c001);  {offset, key, ver=1}
  buf[3]:=2;
  buf[4]:=0;
  writeWordMSB(buf[5],sessN);
  writeWordMSB(buf[7],tuneR);
  writeWordMSB(buf[9],i);
  writeWordMSB(buf[11],0);  {offset}
  move(txKey,buf[13],sizeof(txKey));
  UDPsendPacket(udpPipe,prAdr,prPrt,buf,i);
  end else begin;
  pipeLineStats(uprPipe,o,i,i);
  if (o=0) then begin;
    WriteLn('upper layer exited!');
    exit;
    end;
  end;
if (getTimePast(tim)>60) then begin;
  buf[1]:=4;
  sendPacket(buf,1,1,0);
  WriteLn('sent echo request');
  tim:=currentTime;
  dec(ret);
  if (ret<1) then begin;
    WriteLn('remote not responding!');
    exit;
    end;
  end;
i:=recvPacket(buf,p,-10);
if (i<0) then goto f1;
tim:=currentTime;
ret:=16;
if (i=1) then goto f1;
pipeLineSend(uprPipe,buf,p);
goto f1;
End;
