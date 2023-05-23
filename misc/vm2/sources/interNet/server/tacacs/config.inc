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
WriteLn('reading '+a+'...');
if (xtOpen(t,a,true)<>0) then immErr('error opening!');
serverPort:=BVal(gnl);
logFileName:=gnl;
sharedSecret:=gnl;
logFileLock:=(BVal(gnl)<>0);
userPrompt:=gnl;
passPrompt:=gnl;
succPrompt:=gnl;
failPrompt:=gnl;
authenticateCurrentProcess:=gnl;
xtClose(t);
if (userPrompt='') then userPrompt:='username:';
if (passPrompt='') then passPrompt:='passcode:';
End;
