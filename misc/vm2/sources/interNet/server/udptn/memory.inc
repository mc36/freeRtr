Type
  OneConnectionRecord=record
    term:LongInt;               {pipeline id of process terminal}
    proc:LongInt;               {process id of new process}
    emu1:LongInt;               {pipeline id of emulation remote}
    emu2:LongInt;               {pipeline id of emulation local}
    time:LongInt;               {time of last action}
    adr:OneTCPaddressRecord;    {remote address}
    prt:LongInt;                {remote port}
    stat:LongInt;               {status: 0-disconn, 1-enter, 2-connected}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  serverPort:LongInt;
  serverAddr:OneTCPaddressRecord;
  serverPipe:LongInt;
  lastSent:LongInt;
  terminalFilter:LongInt;
  startProcNam:String;
  startProcPar:String;
  userIDtoGive:LongInt;

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

Function findOneConnection(var adr:OneTCPaddressRecord;prt:LongInt):LongInt;
Var
  i:LongInt;
Begin;
for i:=1 to ConnectionNum do begin;
  if (ConnectionDat^[i].prt<>prt) then continue;
  if not TCPcompareAddress(ConnectionDat^[i].adr,adr) then continue;
  findOneConnection:=i;
  exit;
  end;
findOneConnection:=0;
End;
