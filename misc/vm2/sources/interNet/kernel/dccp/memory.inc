Type
  OneConnectionRecord=record
    pipe:LongInt;               {pipeline id of connection}
    proc:LongInt;               {process of remote}
    stat:LongInt;               {status: 1=command, 2=listen, 3=open}
                                {4=conning, 5=closing, 6-service}
    addr:OneTCPaddressRecord;   {remote address}
    portL:LongInt;              {local port}
    portR:LongInt;              {remote port}
    srvc:LongInt;               {service code}
    timeR:LongInt;              {last time received}
    timeT:LongInt;              {last time sent}
    seqTa:LongInt;              {tx sequence number low}
    seqTb:LongInt;              {tx sequence number high}
    seqRa:LongInt;              {rx sequence number low}
    seqRb:LongInt;              {rx sequence number high}
    nxtOp:String;               {next options to send}
    end;
Var
  localAddr:OneTCPaddressRecord;
  localPipe:LongInt;
  ipProto:LongInt;
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;


Function ResizeMem(n:LongInt):Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
ResizeMem:=True;
i:=n*sizeof(OneConnectionRecord);
ConnectionNum:=n;
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



Function findOnePeerAddr(var adr;prtL,prtR:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do begin;
  if (prtL<>ConnectionDat^[i].portL) then continue;
  if (prtR<>ConnectionDat^[i].portR) then continue;
  if TCPcompareAddress(adr,ConnectionDat^[i].addr) then goto f1;
  end;
i:=0;
f1:
findOnePeerAddr:=i;
End;

Function findOneListening(port:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do begin;
  if (ConnectionDat^[i].stat<>2) then continue;
  if (ConnectionDat^[i].portL=port) then goto f1;
  end;
i:=0;
f1:
findOneListening:=i;
End;
