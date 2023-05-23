Type
  OneConnectionRecord=record
    pipe1:LongInt;              {pipeline id of accepted connection}
    pipe2:LongInt;              {pipeline id of initiated connection}
    time:LongInt;               {time of last action}
    adr:OneTCPaddressRecord;    {remote address}
    prt:LongInt;                {remote port}
    stat:LongInt;               {status: 0-disconn, 1-req, 2-conn, 3-next}
                                {4-rxchr, 5-w84chr, 6-tstchr, 7-passtr}
                                {200-data}
    pos:LongInt;                {position in buffer}
    tmp:LongInt;                {temporary storage}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  EventsDat:array[1..1024] of byte;
  EventsNum:LongInt;
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
