Const
  TransmitBufMin=512;
  TransmitBufMax=16*1024;
Type
  OneConnectionStateRecord=record
    typ:Byte;                   {type: 0=init, 1=tcpConn, $81=tcpAcc, $82=udpAcc}
    stat:Byte;                  {tcp.status: 1-SYN sent, 2-SYN+ACK got, 3-data mode, 4-FIN+ACK sent, 5-SYN got}
    remAddr:OneAddressRecord;   {ip address of remote}
    pipeline:LongInt;           {pipeline id}
    process:LongInt;            {process id}
    portRem:LongInt;            {remote port}
    portLoc:LongInt;            {local port}
    sequRem:LongInt;            {remote sequence}
    sequLoc:LongInt;            {local sequence}
    maxFrame:LongInt;           {maximum frame size}
    bufSize:LongInt;            {bytes in buffer}
    bufSent:LongInt;            {bytes outstanding}
    txBegan:LongInt;            {time when tx began}
    MaxOutgo:LongInt;           {maximum outgoing size}
    lastTx:LongInt;             {last time when transmitted}
    lastRx:LongInt;             {last time when received}
    retries:Word;               {retries left}
    timeOut:Byte;               {timeout in ticks}
    buf:array[1..TransmitBufMax] of byte;       {transmit buffer}
    end;
  OneCommandRecord=record
    cmd:LongInt;
    adr:OneAddressRecord;
    prt:LongInt;
    dat:LongInt;
    end;
Var
  OneConnectionStateHead:LongInt;
  OneConnectionStateSize:LongInt;
  TransmitBufSize:LongInt;
  PushTresholdBytes:LongInt;
  ConnectionDat:^array[0..3] of Byte;
  ConnectionNum:LongInt;
  ProtocolPipes:array[0..255] of LongInt;
Var
  retry_TCP_SYN:Byte;  {connecting}
  timot_TCP_SYN:Byte;
  retry_TCP_DAT:Byte;  {estabilished}
  timot_TCP_DAT:Byte;
  retry_TCP_FIN:Byte;  {closing}
  timot_TCP_FIN:Byte;




Function ResizeConnectionMemory(n:LongInt):Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
ResizeConnectionMemory:=True;
i:=n*OneConnectionStateSize;
if (ExtendedMemoryResize(p,i)<i) then exit;
ConnectionNum:=n;
ConnectionDat:=p^;
ResizeConnectionMemory:=False;
End;

Procedure ConnectReadH(n:LongInt;var d:OneConnectionStateRecord);
Begin;
move(ConnectionDat^[(n-1)*OneConnectionStateSize],d,OneConnectionStateHead);
End;

Procedure ConnectReadD(n:LongInt;var d:OneConnectionStateRecord);
Begin;
move(ConnectionDat^[(n-1)*OneConnectionStateSize+OneConnectionStateHead],d.buf,TransmitBufSize);
End;

Procedure ConnectWriteH(n:LongInt;var d:OneConnectionStateRecord);
Begin;
move(d,ConnectionDat^[(n-1)*OneConnectionStateSize],OneConnectionStateHead);
End;

Procedure ConnectWriteD(n:LongInt;var d:OneConnectionStateRecord);
Begin;
move(d.buf,ConnectionDat^[(n-1)*OneConnectionStateSize+OneConnectionStateHead],TransmitBufSize);
End;

Procedure ConnectDelete(n:LongInt);
Var d:OneConnectionStateRecord;
Begin;
ConnectReadH(n,d);
pipeLineClose(d.pipeline);
move(ConnectionDat^[(ConnectionNum-1)*OneConnectionStateSize],ConnectionDat^[(n-1)*OneConnectionStateSize],OneConnectionStateSize);
ResizeConnectionMemory(ConnectionNum-1);
End;

Function FindOneConnection(typ:byte;adr:OneAddressRecord;loc,rem:LongInt):LongInt;
Var
  d:OneConnectionStateRecord;
  i:LongInt;
Begin;
for i:=1 to ConnectionNum do begin;
  ConnectReadH(i,d);
  if (d.typ<>typ) then continue;
  if (d.portLoc<>loc) then continue;
  if (d.portRem<>rem) then continue;
  if (d.remAddr.a4<>adr.a4) then continue;
  if (d.remAddr.a3<>adr.a3) then continue;
  if (d.remAddr.a2<>adr.a2) then continue;
  if (d.remAddr.a1<>adr.a1) then continue;
  FindOneConnection:=i;
  exit;
  end;
FindOneConnection:=0;
End;

Function FindOneListening(typ:byte;loc:LongInt):LongInt;
Var
  d:OneConnectionStateRecord;
  i:LongInt;
Begin;
for i:=1 to ConnectionNum do begin;
  ConnectReadH(i,d);
  if (d.typ<>typ) then continue;
  if (d.portLoc<>loc) then continue;
  FindOneListening:=i;
  exit;
  end;
FindOneListening:=0;
End;

Procedure makeNewConnectRec(typ:byte;adr:OneAddressRecord;loc,rem:LongInt;var d:OneConnectionStateRecord);
Label f1;
Var
  i:LongInt;
Begin;
loc:=loc and $ffff;
rem:=rem and $ffff;
f1:
if (typ>$80) then i:=FindOneListening(typ,loc) else i:=FindOneConnection(typ,adr,loc,rem);
if (loc=0) or (loc=$ffff) then i:=1;
if (i<>0) then begin;
  loc:=$a000+random($3000);
  if (typ>$80) then inc(loc,$3000);
  goto f1;
  end;
d.typ:=typ;
d.remAddr:=adr;
d.portRem:=rem;
d.portLoc:=loc;
d.sequRem:=random($ffffffff);
d.sequLoc:=random($ffffffff);
d.maxFrame:=$200;
d.MaxOutgo:=$200;
d.bufSize:=0;
d.bufSent:=0;
d.txBegan:=CurrentTime;
d.lastTx:=CurrentTime;
d.lastRx:=CurrentTime;
d.retries:=0;
d.timeOut:=0;
End;





Procedure displayInformation;
Begin;
WriteLn('       syn retry: '+BStr(retry_TCP_SYN));
WriteLn('       dat retry: '+BStr(retry_TCP_DAT));
WriteLn('       fin retry: '+BStr(retry_TCP_FIN));
WriteLn('     syn timeout: '+BStr(timot_TCP_SYN));
WriteLn('     dat timeout: '+BStr(timot_TCP_DAT));
WriteLn('     fin timeout: '+BStr(timot_TCP_FIN));
WriteLn('       tx buffer: '+BStr(TransmitBufSize));
WriteLn('     connections: '+BStr(ConnectionNum));
WriteLn('   local address: '+ipAddr2string(LocalAddress));
WriteLn(' link process id: '+BStr(LowerProcess));
WriteLn('link pipeline id: '+BStr(LowerPipeline));
End;

Procedure displayConnectinos;
Var
  d:OneConnectionStateRecord;
  a:String;
  i:LongInt;
Begin;
WriteLn('protocol    address                        local remote process');
for i:=1 to ConnectionNum do begin;
  ConnectReadH(i,d);
  case d.typ of
    $00:a:='ini';
    $01,$81:a:='tcp';
    $02,$82:a:='udp';
    else a:='???';
    end;
  Write(a);
  if (d.typ>$80) then a:='listening' else a:=ipAddr2string(d.remAddr);
  while (length(a)<40) do a:=' '+a;
  Write(a);
  a:=BStr(d.portLoc);
  while (length(a)<6) do a:=' '+a;
  Write(a);
  a:=BStr(d.portRem);
  while (length(a)<6) do a:=' '+a;
  Write(a);
  WriteLn(' '+BStr(d.process));
  end;
End;
