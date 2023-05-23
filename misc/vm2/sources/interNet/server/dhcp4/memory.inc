Const ip4addrBegin=13;
Type
  OneConnectionRecord=record
    ipA:LongInt;                {ip address}
    hwA:OneTCPaddressRecord;    {hardware address}
    typ:LongInt;                {type: 0=free, 1=alloced, 2=fixed}
    tim:LongInt;                {time of last leased}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  lowLevelProc:LongInt;
  pipeCmd:LongInt;
  portCmd:LongInt;
  addrCmd:OneTCPaddressRecord;
  lastSent:LongInt;
  gateAddr:OneTCPaddressRecord;
  maskAddr:OneTCPaddressRecord;
  dnsLists:String;
  domainNam:String;
  bootServ:String;
  bootFile:String;
  leaseTime:LongInt;
  lastPerid:LongInt;
  lastCheck:LongInt;

Function ResizeMem(n:LongInt):Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
ResizeMem:=True;
i:=n*sizeof(OneConnectionRecord);
if (ExtendedMemoryResize(p,i)<i) then exit;
ConnectionNum:=n;
ConnectionDat:=p^;
ResizeMem:=False;
End;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function getWord(var a:String):String;
Var i:LongInt;
Begin;
i:=pos(' ',a);
if (i<1) then i:=666;
getWord:=copy(a,1,i-1);
a:=copy(a,i+1,255);
End;

Function FindOneHardwareAddr(ha:OneTCPaddressRecord):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do begin;
  if (ConnectionDat^[i].typ=0) then continue;
  if TCPcompareAddress(ha,ConnectionDat^[i].hwA) then goto f1;
  end;
i:=0;
f1:
FindOneHardwareAddr:=i;
End;

Function FindOneInternetAddr(ia:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do begin;
  if (ConnectionDat^[i].ipA=ia) then goto f1;
  end;
i:=0;
f1:
FindOneInternetAddr:=i;
End;

Function FindOneFreeAddress:LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do begin;
  if (ConnectionDat^[i].typ=0) then goto f1;
  end;
i:=0;
f1:
FindOneFreeAddress:=i;
End;

Procedure lowLevelQuery(var a:String);
Label f1;
Var i,o,p:LongInt;
Begin;
relequish;
if (pipeLineCreate(o,lowLevelProc,4096,true)<>0) then exit;
pipeLineSend(o,a[1],length(a));
fillchar(a,sizeof(a),0);
p:=0;
f1:
relequish;
inc(p);
if (p>16) then begin;
  pipeLineClose(o);
  fillchar(a,sizeof(a),0);
  exit;
  end;
i:=250;
if (pipeLineRecv(o,a[1],i)<>0) then i:=0;
if (i<1) then goto f1;
pipeLineClose(o);
a[0]:=chr(i);
End;


Procedure listAddressCache(typ:LongInt;a:String);
Var
  i,o:LongInt;
  ad:OneTCPaddressRecord;
Begin;
WriteLn(a);
for i:=1 to ConnectionNum do if (ConnectionDat^[i].typ=typ) then begin;
  a:=IPv4addressPrefix;
  move(a[1],ad,sizeof(ad));
  move(ConnectionDat^[i].ipA,ad[ip4addrBegin],sizeof(ConnectionDat^[i].ipA));
  Write(ipAddr2string(ad));
  a:='';
  ad:=ConnectionDat^[i].hwA;
  for o:=1 to sizeof(ad) do a:=a+':'+byte2hextype(ad[o]);
  WriteLn('  '+copy(a,2,255));
  end;
End;


Procedure DoOnePurgingRound;
Var
  i,o:LongInt;
  a:String;
Begin;
for i:=1 to ConnectionNum do if (ConnectionDat^[i].typ=1) then
 if (GetTimePast(ConnectionDat^[i].tim)>leaseTime) then
  ConnectionDat^[i].typ:=0;
inc(lastCheck);
a:='arpread-1234';
move(lastCheck,a[9],sizeof(i));
lowLevelQuery(a);
if (copy(a,1,7)<>'arpdata') then exit;
move(a[8],o,sizeof(o));
if (lastCheck>=o) then lastCheck:=0;
move(a[12],o,sizeof(o));
i:=FindOneInternetAddr(o);
if (i=0) then exit;
if (ConnectionDat^[i].typ=0) then ConnectionDat^[i].typ:=1;
a:=copy(a,16,sizeof(ConnectionDat^[i].hwA));
fillchar(ConnectionDat^[i].hwA,sizeof(ConnectionDat^[i].hwA),0);
move(a[1],ConnectionDat^[i].hwA,length(a));
ConnectionDat^[i].tim:=currentTime;
End;
