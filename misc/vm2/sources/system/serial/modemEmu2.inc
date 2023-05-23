Procedure serialSelectPort(var d:modemStateRecord);
Begin;
serialData:=d.dataPipe;
serialCtrl:=d.ctrlPipe;
End;

Procedure serialSetSignals(i:LongInt);
Begin;
serialBuff[1]:=3;
serialBuff[2]:=i;
serialCmd(2);
End;

Function serialGetSignals:Boolean;
Begin;
serialBuff[1]:=1;
serialCmd(1);
serialGetSignals:=(serialBuff[6] and 2<>0) and (serialBuff[3]=0);
End;

Procedure serialSendData(var d:modemStateRecord;force:Boolean);
Label f1;
Begin;
if (d.txSiz<1) then goto f1;
if (pipeLineSend(serialData,d.txBuf,d.txSiz)<>0) then begin;
  if force then goto f1;
  exit;
  end;
f1:
d.txSiz:=0;
End;



Procedure modemCommandSign(var d:modemStateRecord);
Var i:LongInt;
Begin;
if d.curr.carrierAlways then i:=3 else i:=2;
if (d.lastSign=i) then exit;
SerialSetSignals(i);
d.lastSign:=i;
End;

Procedure modemCommandData(var d:modemStateRecord);
Var
  buf:array[1..128] of char;
  i,o:LongInt;
Begin;
o:=sizeof(buf);
if (pipeLineRecv(serialData,buf,o)<>0) then o:=0;
if (o<1) then exit;
for i:=1 to o do modemProcessChar(d,buf[i]);
End;

Function modemCommandConn(var loc,rem:modemStateRecord):Boolean;
Begin;
modemCommandConn:=False;
if (loc.state=0) then exit;
if (loc.state=1) and (rem.state=2) then begin;
  WriteLn('connection estabilished!');
  loc.connected:=true;
  rem.connected:=true;
  loc.state:=3;
  rem.state:=3;
  loc.lastSign:=3;
  rem.lastSign:=3;
  serialSelectPort(loc);
  serialSetSignals(3);
  modemAddRingConn(loc,1);
  serialGetSignals;
  serialSelectPort(rem);
  serialSetSignals(3);
  modemAddRingConn(rem,1);
  serialGetSignals;
  modemCommandConn:=True;
  exit;
  end;
if (loc.state=1) and (rem.state=0) then begin;
  serialSelectPort(rem);
  modemAddRingConn(rem,2);
  if (rem.curr.autoAnswer=0) then exit;
  inc(rem.curr.ringCounter);
  if (rem.curr.ringCounter<rem.curr.autoAnswer) then exit;
  rem.curr.ringCounter:=0;
  rem.state:=2;
  exit;
  end;
End;



Procedure modemConnectData(var loc,rem:modemStateRecord);
Label f1;
Var
  buf:array[1..128] of char;
  i,o:LongInt;
Begin;
if not (loc.connected and rem.connected) then exit;
if (loc.state=3) and (rem.state=3) then begin;
  i:=sizeof(rem.txBuf)-rem.txSiz;
  if (i<16) then exit;
  if (pipeLineRecv(serialData,rem.txBuf[rem.txSiz+1],i)<>0) then i:=0;
  inc(rem.txSiz,i);
  exit;
  end;
o:=sizeof(buf);
if (pipeLineRecv(serialData,buf,o)<>0) then o:=0;
if (o<1) then exit;
for i:=1 to o do begin;
  modemProcessChar(loc,buf[i]);
  if not loc.connected then exit;
  end;
End;

Procedure modemConnectSign(var d:modemStateRecord);
Var i:LongInt;
Begin;
if serialGetSignals then exit;
if (d.state<>3) then exit;
if (d.curr.dtrLowAction<=0) then exit;
if (d.curr.dtrLowAction>=1) then d.state:=0;
if (d.curr.dtrLowAction>=2) then d.connected:=false;
serialSendData(d,true);
modemAddRingConn(d,0);
End;

Function modemConnectConn(var loc,rem:modemStateRecord):Boolean;
Begin;
modemConnectConn:=False;
if (loc.connected and rem.connected) then exit;
loc.disconnReason:=2;
rem.disconnReason:=2;
if loc.connected then begin;
  loc.disconnReason:=1;
  WriteLn('connection terminated by '+rem.linkName+'!');
  serialSelectPort(loc);
  serialSendData(loc,true);
  modemAddRingConn(loc,3);
  end;
if rem.connected then begin;
  rem.disconnReason:=1;
  WriteLn('connection terminated by '+loc.linkName+'!');
  serialSelectPort(rem);
  serialSendData(rem,true);
  modemAddRingConn(rem,3);
  end;
loc.state:=0;
rem.state:=0;
loc.connected:=false;
rem.connected:=false;
modemConnectConn:=True;
End;
