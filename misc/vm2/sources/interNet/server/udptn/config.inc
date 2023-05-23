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
terminalFilter:=BugOS_findProcNam(gnl);
if (terminalFilter=0) then immErr('terminal filter not found!');
startProcNam:=gnl;
startProcPar:=gnl;
userIDtoGive:=BVal(gnl);
xtClose(t);
End;
