Type
  OneMacAddressRecord=array[1..6] of byte;
  OneConnectionRecord=record
    stat:LongInt;               {status: 0-free, 1-conning, 2-opened}
                                {3-ready}
    time:LongInt;               {time of last action}
    addr:LongInt;               {remote address}
    pipe:LongInt;               {pipeline for data}
    conn:LongInt;               {connection number}
    labl:LongInt;               {label for this connection}
    bufS:LongInt;               {buffer size}
    timH:LongInt;               {hello timer}
    timK:LongInt;               {keepalive timer}
    sndM:LongInt;               {last sent message number}
    rcvT:LongInt;               {last received message}
    rcvN:LongInt;               {number of messages got}
    end;
  OneSessionRecord=record
    stat:LongInt;               {status: 0-free, 1-ready}
    time:LongInt;               {time of last action}
    addr:LongInt;               {remote address}
    labl:LongInt;               {label for this connection}
    pipe:LongInt;               {pipeline for data}
    mode:LongInt;               {pseudowire type}
    vcid:LongInt;               {virtual circuit}
    mttu:LongInt;               {maximum transmit unit}
    conn:LongInt;               {connection number}
    sess:LongInt;               {session number}
    pack:LongInt;               {sequence number}
    end;
Var
  localAddr:LongInt;
  localPort:LongInt;
  localPipe:LongInt;
  firstHopI:LongInt;
  firstHopM:OneMacAddressRecord;
  currTcpPipe:LongInt;
  lastBcastMsg:LongInt;
  upperPipe:LongInt;
  lowerPipe:LongInt;
  lowerAddr:OneMacAddressRecord;
  lowerBrod:OneMacAddressRecord;
  lowerName:String;
  lowerDesc:String;
  upperName:String;
  virtMacAd:OneMacAddressRecord;
  ConnectionDat:array[1..32] of OneConnectionRecord;
  ConnectionNum:LongInt;
  SessionDat:array[1..64] of OneSessionRecord;
  SessionNum:LongInt;


Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;



Function convertLong2str(adr:LongInt):String;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
ab0:=sizeof(adr);
move(adr,ab[1],sizeof(adr));
convertLong2str:=a;
End;

Function convertIp4prefix(adr,len:LongInt):String;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
if (len<0) then len:=0;
if (len>32) then len:=32;
move(adr,ab[2],sizeof(adr));
ab0:=((len+7) shr 3)+1;
ab[1]:=len;
i:=(len shr 3)+2;
ab[i]:=($ff shl ((32-len) and 7)) and ab[i];
convertIp4prefix:=a;
End;

Procedure convertFromIp4prefix(var buffer;var adr,len:LongInt);
Var buf:array[1..1] of byte absolute buffer;
Begin;
len:=buf[1];
if (len<0) then len:=0;
if (len>32) then len:=32;
fillchar(adr,sizeof(adr),0);
move(buf[2],adr,(len+7) shr 3);
End;

Function convert2ip4addr(var addr):LongInt;
Var
  adr:array[1..1] of byte absolute addr;
  i:LongInt;
Begin;
convert2ip4addr:=0;
if not isAddressIPv4mask(addr) then exit;
move(adr[length(IPv4addressPrefix)+1],i,sizeof(i));
convert2ip4addr:=i;
End;

Function convertFromIP4addr(adr:LongInt):String;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
ab0:=sizeof(adr);
move(adr,ab[1],sizeof(adr));
convertFromIP4addr:=IPv4addressPrefix+a;
End;

Function compareIp4addrs(addr1,addr2:LongInt):LongInt;
Var
  a1:array[1..1] of byte absolute addr1;
  a2:array[1..1] of byte absolute addr2;
  i,o:LongInt;
Begin;
o:=0;
for i:=sizeof(o) downto 1 do begin;
  if (a1[i]<a2[i]) then o:=-1;
  if (a1[i]>a2[i]) then o:=+1;
  end;
compareIp4addrs:=o;
End;

Function convert2macAddr(var addr):String;
Var
  adr:array[1..1] of byte absolute addr;
  i:LongInt;
  a:String;
Begin;
a:='';
for i:=1 to sizeof(OneMacAddressRecord) do a:=a+'-'+byte2hextype(adr[i]);
convert2macAddr:=copy(a,2,666);
End;



Function findOnePeerAddr(adr:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do if (adr=ConnectionDat[i].addr) then goto f1;
i:=0;
f1:
findOnePeerAddr:=i;
End;

Function findOneSession(con,vc:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to SessionNum do begin;
  if (con<>SessionDat[i].conn) then continue;
  if (vc=SessionDat[i].vcid) then goto f1;
  end;
i:=0;
f1:
findOneSession:=i;
End;


Procedure getNextHopMacAddr;
Var q:LongInt;

Procedure doQuery(var a:String);
Label f1;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
Begin;
relequish;
if (pipeLineCreate(o,q,4096,true)<>0) then exit;
pipeLineSend(o,ab[1],ab0);
p:=0;
ab0:=0;
f1:
relequish;
inc(p);
if (p>16) then begin;
  pipeLineClose(o);
  exit;
  end;
i:=250;
if (pipeLineRecv(o,ab[1],i)<>0) then i:=0;
if (i<1) then goto f1;
pipeLineClose(o);
ab0:=i;
End;

Var
  i,o,p:LongInt;
  a:String;
Begin;
fillchar(firstHopM,sizeof(firstHopM),0);
q:=BugOS_findProcNam(upperName);
if (q=0) then exit;
p:=1;
o:=1;
while (o<=p) do begin;
  a:='arpread-1234';
  move(o,a[9],sizeof(o));
  doQuery(a);
  if (copy(a,1,7)<>'arpdata') then exit;
  move(a[8],p,sizeof(p));
  inc(o);
  move(a[12],i,sizeof(i));
  if (i<>firstHopI) then continue;
  move(a[16],firstHopM,sizeof(firstHopM));
  exit;
  end;
End;



Procedure clearOneSess(var ses:OneSessionRecord);
Begin;
ses.stat:=0;
ses.time:=currentTime;
ses.labl:=-1;
ses.pack:=0;
End;

Procedure clearOneConn(var con:OneConnectionRecord);
Var i:LongInt;
Begin;
for i:=1 to SessionNum do if (SessionDat[i].conn=con.conn) then clearOneSess(SessionDat[i]);
pipeLineClose(con.pipe);
con.time:=currentTime;
con.timH:=currentTime;
con.timK:=currentTime;
con.sndM:=0;
con.pipe:=0;
con.stat:=0;
con.bufS:=0;
con.rcvN:=0;
con.rcvT:=-99999;
if (con.conn<>1) then exit;
for i:=1 to ConnectionNum do ConnectionDat[i].labl:=-1;
End;
