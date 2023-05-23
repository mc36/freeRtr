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

Var
  b:String;
  i,o:LongInt;
Begin;
WriteLn('reading '+a+'...');
if (xtOpen(t,a,true)<>0) then immErr('error opening!');
localPort:=BVal(gnl);
if string2ipAddr(gnl,serverAddr) then immErr('error in server address!');
serverPort:=BVal(gnl);
myNumber:=gnl;
inCallPrg:=gnl;
inCallPar:=gnl;
BugOS_MyProcessInfo(o,i,i);
kicserel('"',BStr(o),inCallPar);
kicserel(' ','',myNumber);
if (myNumber='') then myNumber:='user';
a:=ipAddr2string(serverAddr);
if not isAddressIPv4mask(serverAddr) then a:='['+a+']';
kicserel('|',myNumber+'@'+a,inCallPar);
xtClose(t);
End;
