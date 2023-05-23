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

function rep(b:string):string;
begin;
if (copy(b,length(b),255)<>'\') then b:=b+'\';
rep:=b;
end;

Procedure sortUsers;

Function QuickReadOne(n:LongInt):String;
Begin;
QuickReadOne:=usersListDat[n].emil;
End;

Procedure QuickSwapOne(n1,n2:LongInt);
Begin;
usersListDat[usersListMax]:=usersListDat[n1];
usersListDat[n1]:=usersListDat[n2];
usersListDat[n2]:=usersListDat[usersListMax];
End;

{$sysinc quicksrt.inc}

Begin;
if (usersListNum>1) then QuickSort(1,usersListNum);
End;

Var
  b:String;
  i:LongInt;
Begin;
if (xtOpen(t,'c:\system\localHost.text',true)<>0) then immErr('error opening localhost.text');
domainName:=xtReadLn(t,255);
xtReadLn(t,255);
xtReadLn(t,255);
b:=xtReadLn(t,255);
xtClose(t);
TimeZone:=copy(b,1,pos(' ',b)-1);
if (xtOpen(t,a,true)<>0) then immErr('error opening config!');
serverPort:=BVal(gnl);
serverSPort:=BVal(gnl);
a:=gnl;
b:=gnl;
TempPath:=rep(gnl);
relayServer:=gnl;
relayUserPass:=gnl;
relayUserPass:=#0+relayUserPass+#0+gnl;
xtClose(t);
if (relayServer='') then relayUserPass:=#0#0;
if (xtOpen(t,a,true)<>0) then immErr('error opening relays!');
relayListNum:=0;
relaySelfAddr:=False;
while not xtEOF(t) do begin;
  a:=gnl;
  if (a='') then continue;
  if (a='-') then begin; relaySelfAddr:=true;continue; end;
  inc(relayListNum);
  string2ipAddr(getWord(a),relayListDat[relayListNum].adr);
  string2ipAddr(a,relayListDat[relayListNum].msk);
  end;
xtClose(t);
if (xtOpen(t,b,true)<>0) then immErr('error opening users!');
usersListNum:=0;
while not xtEOF(t) do begin;
  b:=gnl;
  if (b='') then continue;
  inc(usersListNum);
  usersListDat[usersListNum].emil:=kicsi(getWord(b));
  usersListDat[usersListNum].uid:=BVal(getWord(b));
  usersListDat[usersListNum].path:=rep(b);
  end;
xtClose(t);
sortUsers;
End;
