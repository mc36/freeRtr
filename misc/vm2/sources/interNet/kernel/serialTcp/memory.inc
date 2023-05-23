Type
  OneConnectionRecord=record
    stat:LongInt;               {status: 0-free, 1-init, 2-connect}
    addr:OneTCPaddressRecord;   {address of remote}
    port:LongInt;               {port of remote}
    pipe:LongInt;               {tcp pipeline id}
    time:LongInt;               {time of last receive}
    serC:LongInt;               {serial control pipe}
    serD:LongInt;               {serial data pipe}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  servAddr:OneTCPaddressRecord;
  servPort:LongInt;
  servPipe:LongInt;

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
