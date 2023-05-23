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
QuickReadOne:=usersListDat[n].name;
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
if (xtOpen(t,a,true)<>0) then immErr('error opening config!');
serverPort:=BVal(gnl);
serverSPort:=BVal(gnl);
a:=gnl;
xtClose(t);
if (xtOpen(t,a,true)<>0) then immErr('error opening users!');
usersListNum:=0;
while not xtEOF(t) do begin;
  b:=gnl;
  if (b='') then continue;
  inc(usersListNum);
  usersListDat[usersListNum].name:=kicsi(getWord(b));
  usersListDat[usersListNum].uid:=BVal(getWord(b));
  usersListDat[usersListNum].path:=rep(b);
  end;
xtClose(t);
sortUsers;
End;
