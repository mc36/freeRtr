Type
  OneConnectionRecord=record
    stat:LongInt;               {status: 0-free, 1-start, 2-head, 3-data}
    time:LongInt;               {time of last action}
    cmnd:String;                {command to start}
    uprP:LongInt;               {upper pipe}
    uprC:LongInt;               {upper pipe}
    tcpP:LongInt;               {tcp pipe}
    addr:OneTCPaddressRecord;   {remote address}
    port:LongInt;               {remote port}
    grup:LongInt;               {group number}
    size:LongInt;               {size of data packet}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  servAddr:OneTCPaddressRecord;
  servPort:LongInt;



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

Function FindConnectByStat(s:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do if (ConnectionDat^[i].stat=s) then goto f1;
i:=0;
f1:
FindConnectByStat:=i;
End;
