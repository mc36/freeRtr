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

Procedure addGrt(a:String);
Var ab0:byte absolute a;
Begin;
inc(greetMsgSiz);
move(a,greetMsgDat[greetMsgSiz],ab0+1);
inc(greetMsgSiz,ab0);
End;

Begin;
if (xtOpen(t,a,true)<>0) then immErr('error opening config!');
serverPort:=BVal(gnl);
a:=gnl;
xtClose(t);
if (xtOpen(t,a,true)<>0) then immErr('error opening motd!');
greetMsgSiz:=0;
while not xtEOF(t) do addGrt(xtReadLn(t,666));
xtClose(t);
if (xtOpen(t,'c:\system\localHost.text',true)<>0) then immErr('error opening localhost!');
serverName:=xtReadLn(t,666);
xtClose(t);
End;
