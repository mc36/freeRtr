Type
  OneConnectionRecord=record
    peer:OneTCPaddressRecord;   {peer tunneled address}
    perV:LongInt;               {peer ip version}
    stat:LongInt;               {status: 0-free, 1-ack, 2-upFree, 3-upOpen, 4-conn}
    timR:LongInt;               {time of last receive}
    timT:LongInt;               {time of last transmit}
    pipe:LongInt;               {pipeline id}
    addr:OneTCPaddressRecord;   {remote address}
    port:LongInt;               {remote port}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  servAddr:OneTCPaddressRecord;
  servPort:LongInt;
  servPipe:LongInt;
  ip4addr:OneTCPaddressRecord;
  ip6addr:OneTCPaddressRecord;
  ip4conf:String;
  ip6conf:String;
  upperCon:LongInt;
  upperTim:LongInt;


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
