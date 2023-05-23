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
  i:LongInt;
Begin;
if (xtOpen(t,a,true)<>0) then immErr('error opening config!');
serverPort:=BVal(gnl);
b:=gnl;
i:=pos('%u%',b);
if (i<1) then begin;
  blockUrlBeg:='';
  blockUrlEnd:=b;
  end else begin;
  blockUrlBeg:=copy(b,1,i-1);
  blockUrlEnd:=copy(b,i+3,666);
  end;
logFileName:=gnl;
logFileLock:=(BVal(gnl)<>0);
domainListNum:=0;
while not xtEOF(t) do begin;
  a:=gnl;
  if (a='') then continue;
  b:=getWord(a);
  if (a='') then continue;
  inc(domainListNum);
  domainListDat[domainListNum].perm:=(b='+');
  domainListDat[domainListNum].name:=kicsi(a);
  end;
xtClose(t);
End;
