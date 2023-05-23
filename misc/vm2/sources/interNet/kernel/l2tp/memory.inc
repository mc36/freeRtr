Type
  OneConnectionRecord=record
    stat:LongInt;               {status: 0-free, 1-open, 2-conn, 3-bye, 4-start, 5-ocrp}
    time:LongInt;               {time of last receive}
    cmnd:String;                {command to start}
    pipe:LongInt;               {pipeline id}
    addr:OneTCPaddressRecord;   {remote address}
    port:LongInt;               {remote port}
    tuneR:LongInt;              {remote tunnel id}
    sessR:LongInt;              {remote session id}
    tuneL:LongInt;              {local tunnel id}
    sessL:LongInt;              {local session id}
    bufD:array[1..256] of byte; {transfer buffer}
    bufS:LongInt;               {transfer size}
    bufT:LongInt;               {time of last try}
    bufC:LongInt;               {number of times tried}
    bufP:LongInt;               {target session id}
    doAck:LongInt;              {should do ack}
    txSeq:LongInt;              {tx sequence number}
    rxSeq:LongInt;              {rx sequence number}
    datSeq:LongInt;             {data tx sequence number}
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
