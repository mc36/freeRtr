Type
  OneConnectionRecord=record
    stat:LongInt;               {status: 1-wait4conn, 2-src, 3-trg, 4-usr, 5-lcn, 6-calling, 7-conn}
    time:LongInt;               {time of last action}
    lcn:LongInt;                {channel number}
    rxSeq:LongInt;              {next rx sequence number}
    txSeq:LongInt;              {next tx sequence number}
    pipe:LongInt;               {pipeline number}
    src:String;                 {source address}
    trg:String;                 {target address}
    usr:String;                 {user data}
    buf:array[1..1024] of byte; {bytes to send}
    siz:LongInt;                {bytes in buffer}
    retry:LongInt;              {retries left}
    end;
Var
  mod128:Boolean;               {modulo 128 mode}
  modMsk:LongInt;               {modulo mask}
  dataPipe:LongInt;             {data pipeline}
  addrSize:LongInt;             {size of address}
  channelMin:LongInt;           {first lcn number}
  channelMax:LongInt;           {last lcn number}
  channelSiz:LongInt;           {max packet size}
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;        {number of connections}
  listenerPipe:LongInt;         {listener pipeline id}
  listenerProc:LongInt;         {listener process id}
  listenerSize:LongInt;         {listener pipeline size}


Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;




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


Function findLCNnumber(d:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do if (ConnectionDat^[i].lcn=d) then goto f1;
i:=0;
f1:
findLCNnumber:=i;
End;


Function findEmptyLCNnum:LongInt;
Label f1;
Var i:LongInt;
Begin;
i:=Random(channelMax-channelMin+1)+channelMin;
if (findLCNnumber(i)>0) then i:=0;
findEmptyLCNnum:=i;
End;
