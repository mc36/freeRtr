Type
  OneConnectionRecord=record
    pipe1:LongInt;              {pipeline id of accepted connection}
    pipe2:LongInt;              {pipeline id of initiated connection}
    time:LongInt;               {time of last action}
    which:LongInt;              {which server the client connected}
    adr:OneTCPaddressRecord;    {remote address}
    prt:LongInt;                {remote port}
    stat:LongInt;               {status: 0=disconn, 1=init, 2=reconn, 3=conning, 4=conned}
    line:String;                {received line}
    end;
Const targetMax=32;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  sourcePort:LongInt;
  sourceAddr:OneTCPaddressRecord;
  targetPort:array[1..targetMax] of LongInt;
  targetAddr:array[1..targetMax] of OneTCPaddressRecord;
  targetStat:array[0..targetMax] of Byte; {0=ok, 1=try, 2=block}
  targetTime:array[0..targetMax] of LongInt;
  targetNum:LongInt;
  targetNxt:LongInt;
  lastTest:LongInt;



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
