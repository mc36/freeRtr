Type
  OneConnectionRecord=record
    stat:LongInt;               {status: 0-free, 1-init, 2-command, 3-headers, 4-content, 5-upper, 100-conn}
    time:LongInt;               {time of last receive}
    cmnd:String;                {command to start}
    dele:OneTCPaddressRecord;   {delegated ip address}
    pipL:LongInt;               {lower pipeline id}
    pipU:LongInt;               {upper pipeline id}
    addr:OneTCPaddressRecord;   {remote address}
    port:LongInt;               {remote port}
    read:String;                {read buffer}
    need:LongInt;               {needed response: 1-red, 2-get, 3-post, 4-connect}
    bufS:LongInt;               {bytes waiting}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  servAddr:OneTCPaddressRecord;
  servPort:LongInt;
  servPipe:LongInt;
  myUrl:String;



Function ResizeMem(n:LongInt):Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
ResizeMem:=True;
i:=n*sizeof(OneConnectionRecord);
if (ExtendedMemoryResize(p,i)<i) then exit;
ConnectionNum:=n;
ConnectionDat:=p^;
ResizeMem:=False;
End;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function getWord(var a:String):String;
Var i:LongInt;
Begin;
i:=pos(' ',a);
if (i<1) then i:=666;
getWord:=copy(a,1,i-1);
a:=copy(a,i+1,255);
End;

Function xLevesz(a:String):String;
Begin;
a:=' '+a+' ';
kicserel('  ',' ',a);
xLevesz:=copy(a,2,length(a)-2);
End;

Function FindConnectByStat(s:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do if (ConnectionDat^[i].stat=s) then goto f1;
i:=0;
f1:
FindConnectByStat:=i;
End;
