Type
  OneConnectionRecord=record
    protB:LongInt;              {protocol first}
    protE:LongInt;              {protocol last}
    pipe:LongInt;               {pipeline id}
    name:String;                {name of process}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  serviceName:String;
  etherPipe:LongInt;
  addrSize:LongInt;
  packSize:LongInt;
  localAddr:String;
  broadAddr:String;
  deviceName:String;


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
