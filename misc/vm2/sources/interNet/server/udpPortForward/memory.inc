Const authBufferMax=512;
Type
  OneConnectionRecord=record
    pipe:LongInt;               {pipeline id of initiated connection}
    time:LongInt;               {time of last action}
    addr:OneTCPaddressRecord;   {remote address}
    port:LongInt;               {remote port}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  sourcePort:LongInt;
  sourceAddr:OneTCPaddressRecord;
  sourcePipe:LongInt;
  targetPort:LongInt;
  targetAddr:OneTCPaddressRecord;



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
