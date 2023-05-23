Const
  maxHandles=16;
  basePath='c:\';
Type
  OneConnectionRecord=record
    pipe:LongInt;               {pipeline id of tcp connection}
    stat:LongInt;               {status: 0-disconn, 1-init, 2-cmdHdr, 3-cmdDat}
                                {4-tx, 5-rx, 6-skip}
    time:LongInt;               {time of last action}
    addr:OneTCPaddressRecord;   {remote address}
    port:LongInt;               {remote port}
    user:LongInt;               {user id}
    vers:LongInt;               {version number}
    mask:LongInt;               {attribute mask}
    handles:array[1..maxHandles] of xFile;      {handles}
    txtMode:array[1..maxHandles] of Boolean;    {text mode flag}
    cmdSiz:LongInt;             {command size}
    cmdTyp:LongInt;             {command type}
    cmdHnd:LongInt;             {command handler}
    cmdSeq:LongInt;             {request id}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
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

function repairPath(b:string):string;
begin;
if (copy(b,length(b),255)<>'\') then b:=b+'\';
repairPath:=b;
end;
