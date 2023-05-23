Type
  OneConnectionRecord=record
    stat:LongInt;               {status: 0-free, 1-conn, 2-open, 3-upper, 4-data}
    time:LongInt;               {time of last receive}
    retry:LongInt;              {number of timeouts left}
    cmnd:String;                {command to start}
    pipe:LongInt;               {pipeline id}
    addr:OneTCPaddressRecord;   {remote address}
    port:LongInt;               {remote port}
    tuneR:LongInt;              {remote tunnel id}
    tuneL:LongInt;              {local tunnel id}
    sessn:LongInt;              {session id}
    cmdSeq:LongInt;             {command sequence number}
    datSeq:LongInt;             {data sequence number}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  servAddr:OneTCPaddressRecord;
  servPort:LongInt;
  servPipe:LongInt;
  servName:String;



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

Function FindConnectByID(id:LongInt):LongInt;
Label f1;
Var
  d:OneConnectionRecord;
  i:LongInt;
Begin;
for i:=1 to ConnectionNum do begin;
  d:=ConnectionDat^[i];
  if (d.stat=0) then continue;
  if (d.tuneL<>id) then continue;
  goto f1;
  end;
i:=0;
f1:
FindConnectByID:=i;
End;

Function FindConnectByStat(s:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do if (ConnectionDat^[i].stat=s) then goto f1;
i:=0;
f1:
FindConnectByStat:=i;
End;
