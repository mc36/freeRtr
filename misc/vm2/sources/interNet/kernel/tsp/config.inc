Procedure ReadUpConfig(a:String);
Var t:xtText;

function gnl:string;
var
  i:longint;
  a:String;
begin;
a:=xtReadLn(t,255);
i:=pos(';',a);
if (i>0) then a:=copy(a,1,i-1);
a:=' '+a+' ';
kicserel('  ',' ',a);
a:=copy(a,2,length(a)-2);
gnl:=a;
end;

Function doReq(b,a:String):String;
Label f1;
Var
  prc,pip:LongInt;
  i,o:LongInt;
Begin;
prc:=BugOS_findProcNam(b);
if (prc=0) then immErr(b+': process not found!');
relequish;
if (pipeLineCreate(pip,prc,1024,true)<>0) then immErr(b+': unabled to create pipeline!');
if (pipeLineSend(pip,a[1],length(a))<>0) then immErr(b+': unabled to send data over pipeline!');
i:=0;
f1:
inc(i);
if (i>128) then immErr(b+': unabled to receive data from pipeline!');
relequish;
o:=255;
if (pipeLineRecv(pip,a[1],o)<>0) then o:=0;
if (o=0) then goto f1;
pipeLineClose(pip);
a[0]:=chr(o);
doReq:=a;
End;

Var
  d:OneConnectionRecord;
  i,o:LongInt;
  b:String;
Begin;
if (xtOpen(t,a,true)<>0) then immErr('error opening config!');
servPort:=BVal(gnl);
if string2ipAddr(gnl,ip4addr) then fillchar(ip4addr,sizeof(ip4addr),0);
if string2ipAddr(gnl,ip6addr) then fillchar(ip6addr,sizeof(ip6addr),0);
fillchar(servAddr,sizeof(servAddr),0);
if TCPcompareAddress(servAddr,ip4addr) then begin;
  a:=doReq('ip4.code','param---');
  move(a[6],ip4addr,sizeof(ip4addr));
  end;
if TCPcompareAddress(servAddr,ip6addr) then begin;
  a:=doReq('ip6.code','param---');
  move(a[6],ip6addr,sizeof(ip6addr));
  end;
ip4conf:=gnl;
ip6conf:=gnl;
upperCon:=0;
ConnectionNum:=0;
fillchar(d,sizeof(d),0);
while not xtEOF(t) do begin;
  a:=gnl;
  if (a='') then continue;
  i:=pos(' ',a);
  if string2ipAddr(copy(a,1,i-1),servAddr) then continue;
  a:=copy(a,i+1,666);
  if isAddressIPv4mask(servAddr) then i:=4 else i:=6;
  d.perV:=i;
  for o:=1 to bval(a) do begin;
    d.peer:=servAddr;
    WriteLongMSB(servAddr[13],ReadLongMSB(servAddr[13])+1);
    if ResizeMem(ConnectionNum+1) then immErr('error allocating memory!');
    ConnectionDat^[ConnectionNum]:=d;
    end;
  end;
xtClose(t);
{$ifdef debug}
WriteLn('my ip4 address: '+ipAddr2string(ip4addr));
WriteLn('my ip6 address: '+ipAddr2string(ip6addr));
WriteLn('will serve '+BStr(ConnectionNum)+' clients.');
{$endif}
End;
