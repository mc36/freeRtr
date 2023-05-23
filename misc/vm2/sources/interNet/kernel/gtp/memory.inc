Type
  OneConnectionRecord=record
    stat:LongInt;               {status: 0-free, 1-wait, 2-upper, 3-conn}
    cmnd:String;                {command to start}
    addr:OneTCPaddressRecord;   {delegated ip address}
    mode:LongInt;               {mode: 1=ppp, 2=ipv4, 3=ipv6}
    timR:LongInt;               {time of last receive}
    timT:LongInt;               {time of last sent}
    seqC:LongInt;               {control sequence}
    seqD:LongInt;               {data sequence}
    teiL:LongInt;               {local teid}
    teiC:LongInt;               {control teid}
    teiD:LongInt;               {data teid}
    imei:String;                {ms imei number}
    isdn:String;                {ms isdn number}
    pipe:LongInt;               {upper pipeline id}
    ctrl:OneTCPaddressRecord;   {remote control address}
    data:OneTCPaddressRecord;   {remote data address}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  ctrlAdr:OneTCPaddressRecord;
  ctrlPrt:LongInt;
  ctrlPip:LongInt;
  dataAdr:OneTCPaddressRecord;
  dataPrt:LongInt;
  dataPip:LongInt;
  apnName:String;
  nextTeid:LongInt;


Function ResizeMem(n:LongInt):Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
ResizeMem:=True;
i:=n*sizeof(OneConnectionRecord);
ConnectionNum:=n;
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

Function xLevesz(a:String):String;
Begin;
a:=' '+a+' ';
kicserel('  ',' ',a);
xLevesz:=copy(a,2,length(a)-2);
End;

Procedure updateOneSGSNaddr(var addr;what:LongInt); {1-rx, 2-tx}
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do begin;
  if (ConnectionDat^[i].stat=0) then continue;
  if not TCPcompareAddress(ConnectionDat^[i].ctrl,addr) then continue;
  if (what and 1<>0) then ConnectionDat^[i].timR:=currentTime;
  if (what and 2<>0) then ConnectionDat^[i].timT:=currentTime;
  end;
End;

Function convertOneAddress(var addr):String;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
ab0:=sizeof(OneTCPaddressRecord);
move(addr,ab[1],sizeof(OneTCPaddressRecord));
if isAddressIPv4mask(addr) then a:=copy(a,length(IPv4addressPrefix)+1,666);
convertOneAddress:=a;
End;

Function FindConnectByStat(s:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do if (ConnectionDat^[i].stat=s) then goto f1;
i:=0;
f1:
FindConnectByStat:=i;
End;

Function FindConnectByTeid(s:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
if (s<1) then begin; i:=0;goto f1; end;
for i:=1 to ConnectionNum do if (ConnectionDat^[i].teiL=s) then goto f1;
i:=0;
f1:
FindConnectByTeid:=i;
End;
