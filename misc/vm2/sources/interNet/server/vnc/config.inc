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
if (xtOpen(t,a,true)<>0) then immErr('error opening config!');
serverPort:=BVal(gnl);
systemName:=gnl;
systemPass:=gnl;
screenSizX:=BVal(gnl);
screenSizY:=BVal(gnl);
processNam:=gnl;
processPar:=gnl;
BugOS_MyProcessInfo(o,i,i);
kicserel('`',BStr(o),processPar);
xtClose(t);
if (systemName='') then if (xtOpen(t,'c:\system\localHost.text',true)=0) then begin;
  systemName:=xtReadLn(t,666);
  xtClose(t);
  end;
if (systemName='') then systemName:='localHost';
End;
