Type
  OneConnectionRecord=record
    adr:OneTCPaddressRecord;    {hardware address}
    lan:String[63];             {vlan name, #13=deny, #10=shutdown}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  ConnectionDef:OneConnectionRecord;
  pipeCmd:LongInt;
  portCmd:LongInt;
  addrCmd:OneTCPaddressRecord;

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

Function FindOneHardwareAddr(var addr):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do if TCPcompareAddress(addr,ConnectionDat^[i].adr) then goto f1;
i:=0;
f1:
FindOneHardwareAddr:=i;
End;

Function convertAddr2str(var buffer):String;
Var
  ha:array[1..1] of byte absolute buffer;
  a:String;
  i:LongInt;
Begin;
a:='';
for i:=1 to 6 do a:=a+'-'+byte2hextype(ha[i]);
convertAddr2str:=copy(a,2,666);
End;
