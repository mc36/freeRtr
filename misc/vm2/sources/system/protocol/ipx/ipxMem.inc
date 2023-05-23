Type OneHrdwrAddrRecord=array[1..6] of byte;
Const IPXprotocolNum=$8137;
Type
  OnePacketRecord=record
    d:array[1..4*1024] of byte;
    a:OneHrdwrAddrRecord;
    s:LongInt;
    end;
Type
  OneConnectionRecord=record
    pipe:LongInt;               {pipeline id of connection}
    proc:LongInt;               {process id of connection}
    adr:OneHrdwrAddrRecord;     {remote address}
    prt:LongInt;                {remote socket}
    stat:LongInt;               {status: 1-command, 2-connected}
    time:Longint;               {time of last action}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  LowerProcess:LongInt;
  LowerPipeline:LongInt;
  localAddress:OneHrdwrAddrRecord;
  bcastAddress:OneHrdwrAddrRecord;
  addressSize:LongInt;
  addressBegin:LongInt;
  packetSize:LongInt;
  broadcastAddr:OneTCPaddressRecord;


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

Function FindOneLocalSocket(n:LongInt):LongInt;
Var
  d:OneConnectionRecord;
  i:LongInt;
Begin;
for i:=1 to ConnectionNum do begin;
  d:=ConnectionDat^[i];
  if (d.stat<>2) then continue;
  if (d.prt<>n) then continue;
  FindOneLocalSocket:=i;
  exit;
  end;
FindOneLocalSocket:=0;
End;


Procedure SendPacket(var d:OnePacketRecord);
Var i,o:LongInt;
Begin;
move(d,d.d[addressSize+3],d.s);
move(d.a,d,addressSize);
WriteWordMSB(d.d[addressSize+1],IPXprotocolNum);
pipeLineSend(LowerPipeline,d,d.s+addressSize+2);
End;

Function ReceivePacket(Var d:OnePacketRecord):Boolean;
Var i,o:LongInt;
Begin;
ReceivePacket:=False;
i:=sizeof(d.d);
if (pipeLineRecv(LowerPipeline,d,i)<>0) then exit;
if (i<1) then exit;
if (i<addressSize+2) then begin;
  WriteLn('lower got too short packet!');
  exit;
  end;
if (ReadWordMSB(d.d[addressSize+1])<>IPXprotocolNum) then begin;
  WriteLn('lower got invalid packet type!');
  exit;
  end;
fillchar(d.a,sizeof(d.a),0);
d.s:=i-addressSize+2;
move(d,d.a[addressBegin],addressSize);
move(d.d[addressSize+3],d,d.s);
ReceivePacket:=True;
End;
