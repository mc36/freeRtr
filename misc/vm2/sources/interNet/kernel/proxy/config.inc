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

Procedure putByte(b:byte);
Begin;
inc(EventsNum);
EventsDat[EventsNum]:=b;
End;

Procedure putStr(a:string);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
for i:=1 to ab0 do putByte(ab[i]);
End;


Var
  b:String;
  i:LongInt;
Begin;
if (xtOpen(t,a,true)<>0) then immErr('error opening config!');
if string2ipAddr(gnl,serverAddr) then immErr('error in address');
serverPort:=BVal(gnl);
if (serverPort=0) then serverPort:=1080;
EventsNum:=0;
while not xtEOF(t) do begin;
  b:=gnl;
  a:=kicsi(getWord(b));
  if (a='') then continue;
  if (a='sendaddr') then begin;
    putByte(0);
    putByte(BVal(b));
    continue;
    end;
  if (a='sendport') then begin;
    putByte(1);
    putByte(BVal(b));
    continue;
    end;
  if (a='sendtext') then begin;
    putByte(2);
    putByte(length(b));
    putStr(b);
    continue;
    end;
  if (a='sendbyte') then begin;
    a:='';
    while (b<>'') do a:=a+chr(BVal(getWord(b)));
    putByte(2);
    putByte(length(a));
    putStr(a);
    continue;
    end;
  if (a='recvbytes') then begin;
    putByte(3);
    putByte(BVal(b));
    continue;
    end;
  if (a='waitfor') then begin;
    putByte(4);
    putByte(BVal(b));
    continue;
    end;
  if (a='testnext') then begin;
    putByte(5);
    putByte(BVal(b));
    continue;
    end;
  if (a='recvpasstr') then begin;
    putByte(6);
    continue;
    end;
  if (a='recvasczstr') then begin;
    putByte(7);
    continue;
    end;

  immErr('bad line in config: '+a+' '+b);
  end;
putByte(255);
xtClose(t);
End;
