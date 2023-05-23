Const
  relayListMax=128;
Type
  OneConnectionRecord=record
    pipe1:LongInt;              {pipeline id of accepted connection}
    pipe2:LongInt;              {pipeline id of initiated connection}
    time:LongInt;               {time of last action}
    cadr:OneTCPaddressRecord;   {client address}
    cprt:LongInt;               {client port}
    tadr:OneTCPaddressRecord;   {target address}
    tprt:LongInt;               {target port}
    stat:LongInt;               {status: 0-disconn, 1=init, 2=addr, 3=user}
                                {4=conning, 5=conned}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  relayListDat:array[1..relayListMax] of record
    adr:OneTCPaddressRecord;
    msk:OneTCPaddressRecord;
    end;
  relayListNum:LongInt;
  serverPort:LongInt;
  serverAddr:OneTCPaddressRecord;
  lastSent:LongInt;


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

Function TestAddrMask(a1,a2,a3:OneTCPaddressRecord):Boolean;
Var i:LongInt;
Begin;
TestAddrMask:=False;
for i:=1 to sizeof(a1) do if (a1[i] and a3[i]<>a2[i] and a3[i]) then exit;
TestAddrMask:=True;
End;

Function getWord(var a:String):String;
Var i:LongInt;
Begin;
i:=pos(' ',a);
if (i<1) then i:=666;
getWord:=copy(a,1,i-1);
a:=copy(a,i+1,255);
End;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;
