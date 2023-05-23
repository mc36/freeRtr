Type
  OneConnectionRecord=record
    pipe:LongInt;               {pipeline id of control connection}
    addr:OneTCPaddressRecord;   {remote address}
    mode:LongInt;               {$80=chksum, $20=key, $10=seq}
    key:LongInt;                {remote's key}
    seq:LongInt;                {sent packets}
    end;
Var
  addr:OneTCPaddressRecord;
  ipProto:LongInt;
  pipe:LongInt;
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
