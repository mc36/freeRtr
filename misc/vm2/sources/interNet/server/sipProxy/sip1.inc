Procedure appendOneStr(var buffer;var siz:LongInt;a:String);
Var
  buf:array[1..1] of byte absolute buffer;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
move(ab[1],buf[siz+1],ab0);
inc(siz,ab0);
End;

Procedure appendOneLine(var buffer;var siz:LongInt;a:String);
Begin;
appendOneStr(buffer,siz,a);
appendOneStr(buffer,siz,#13#10);
End;

Function generateMyAddr:String;
Var a:String;
Begin;
a:=ipAddr2string(localAddr);
if not isAddressIPv4mask(localAddr) then a:='['+a+']';
generateMyAddr:=a+':'+BStr(localPort);
End;

Function getPhoneNumber(a:String):String;
Var i:LongInt;
Begin;
getPhoneNumber:='';
i:=pos('sip:',kicsi(a));
if (i<1) then exit;
a:=copy(a,i+4,666);
i:=pos(';',a);
if (i>0) then a:=copy(a,1,i-1);
i:=pos('>',a);
if (i>0) then a:=copy(a,1,i-1);
kicserel(' ','',a);
getPhoneNumber:=a;
End;

Function getUserName(a:String):String;
Var i:LongInt;
Begin;
a:=getPhoneNumber(a);
i:=pos('@',a);
if (i<1) then i:=666;
getUserName:=copy(a,1,i-1);
End;

Procedure releq2user(var con:OneConnectionRecord);
Label f1,f2,f3;
Var
  buf:array[1..4*1024] of byte;
  i,o,p:LongInt;
Begin;
if (con.stat<>1) then exit;
if (getTimePast(con.time)<600) then exit;
con.stat:=0;
End;
