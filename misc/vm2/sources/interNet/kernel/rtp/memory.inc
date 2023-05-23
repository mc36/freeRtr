Type
  OneConnectionRecord=record
    stat:LongInt;               {status: 0-waiting, 1-connected}
    timT:LongInt;               {time of last tx}
    timR:LongInt;               {time of last rx}
    pipe:LongInt;               {pipeline id}
    locP:LongInt;               {local port number}
    addr:OneTCPaddressRecord;   {remote address}
    port:LongInt;               {remote port}
    seqn:LongInt;               {last sequence number}
    recP:LongInt;               {received packets}
    recB:LongInt;               {received bytes}
    sndP:LongInt;               {sent packets}
    sndB:LongInt;               {sent bytes}
    end;
Const maxServPortNum=32;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  servAddr:OneTCPaddressRecord;
  portNum:LongInt;
  servPort:array[1..maxServPortNum] of LongInt;
  servPipD:array[1..maxServPortNum] of LongInt;
  servPipC:array[1..maxServPortNum] of LongInt;



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


Procedure openOnePort(prt:LongInt);
Label f0,f1;
Var p,p1,p2:LongInt;
Begin;
prt:=prt and $7ffffffe;
f0:
p1:=0;
p2:=0;
p:=prt;
if UDPlistenOnPort(p1,65536,servAddr,p) then goto f1;
if (prt=0) then begin;
  if (p and 1<>0) then begin; pipeLineClose(p1);goto f0; end;
  prt:=p;
  end;
if (p<>prt) then goto f1;
p:=prt+1;
if UDPlistenOnPort(p2,65536,servAddr,p) then goto f1;
if (p<>prt+1) then goto f1;
inc(portNum);
servPort[portNum]:=prt;
servPipD[portNum]:=p1;
servPipC[portNum]:=p2;
WriteLn('listening on '+ipAddr2string(servAddr)+' '+BStr(prt)+'...');
exit;
f1:
pipeLineClose(p1);
pipeLineClose(p2);
End;


Function FindOneConnection(var addr;port,loc:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do begin;
  if (loc<>ConnectionDat^[i].locP) then continue;
  if (port<>ConnectionDat^[i].port) then continue;
  if TCPcompareAddress(addr,ConnectionDat^[i].addr) then goto f1;
  end;
i:=0;
f1:
FindOneConnection:=i;
End;
