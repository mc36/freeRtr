Type
  OneConnectionRecord=record
    pipeT:LongInt;              {pipeline id of tcp connection}
    pipeL:LongInt;              {pipeline id of local connection}
    proc:LongInt;               {process id of owner process}
    addr:OneTCPaddressRecord;   {remote address}
    portL:LongInt;              {local port}
    portR:LongInt;              {remote port}
    time:LongInt;               {last action}
    stat:LongInt;               {status: 0-disconn, 1-cmd, 2-lstn, 3-conn}
    end;
Const listenMax=64;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  serverAddr:OneTCPaddressRecord;
  listenPipe:array[1..listenMax] of LongInt;
  listenPort:array[1..listenMax] of LongInt;
  listenNum:LongInt;
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


Function FindOneSession(var adr;rem,loc:LongInt):LongInt;
Var
  d:OneConnectionRecord;
  i:LongInt;
Begin;
for i:=ConnectionNum downto 1 do begin;
  d:=ConnectionDat^[i];
  if (d.portR<>rem) then continue;
  if (d.portL<>loc) then continue;
  if not TCPcompareAddress(adr,d.addr) then continue;
  FindOneSession:=i;
  exit;
  end;
FindOneSession:=0;
End;

Function FindOneLocalPort(prt:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
if (prt=0) then begin; i:=0;goto f1; end;
for i:=1 to ConnectionNum do if (ConnectionDat^[i].portL=prt) then goto f1;
i:=0;
f1:
FindOneLocalPort:=i;
End;

Function FindOneLocalListen(prt:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
if (prt=0) then begin; i:=0;goto f1; end;
for i:=1 to ConnectionNum do begin;
  if (ConnectionDat^[i].stat<>2) then continue;
  if (ConnectionDat^[i].portL=prt) then goto f1;
  end;
i:=0;
f1:
FindOneLocalListen:=i;
End;

Function FindOneTcpPipe(pip:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
if (pip=0) then begin; i:=0;goto f1; end;
for i:=1 to listenNum do if (listenPipe[i]=pip) then begin; i:=-i;goto f1; end;
for i:=1 to ConnectionNum do if (ConnectionDat^[i].pipeT=pip) then goto f1;
i:=0;
f1:
FindOneTcpPipe:=i;
End;
