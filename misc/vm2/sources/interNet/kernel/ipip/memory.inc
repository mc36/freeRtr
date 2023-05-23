Type
  OneConnectionRecord=record
    pipe:LongInt;               {pipeline id of control connection}
    addr:OneTCPaddressRecord;   {remote address}
    mode:LongInt;               {1=ip4, 2=ip6, 3=both}
    end;
Var
  ip4addr,ip6addr:OneTCPaddressRecord;
  ip4prot,ip6prot:LongInt;
  ip4pipe,ip6pipe:LongInt;
  ConnectionDat:array[1..64] of OneConnectionRecord;
  ConnectionNum:LongInt;



Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;


Function findOnePeerAddr(var adr):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do
 if TCPcompareAddress(adr,ConnectionDat[i].addr) then goto f1;
i:=0;
f1:
findOnePeerAddr:=i;
End;
