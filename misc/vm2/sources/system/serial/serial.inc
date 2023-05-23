Var
  serialProc:LongInt;
  serialPort:LongInt;
  serialCtrl:LongInt;
  serialData:LongInt;
  serialBuff:array[1..16] of longint;

Procedure serialCmd(s:longint);
Var i:longint;
Begin;
if (pipeLineSend(serialCtrl,serialBuff,s*4)<>0) then immErr('error communicating serial driver!');
s:=128;
while (s>0) do begin;
  relequish;
  dec(s);
  i:=sizeof(serialBuff);
  if (pipeLineRecv(serialCtrl,serialBuff,i)<>0) then i:=0;
  if (i>0) then break;
  end;
if (i<1) then immErr('error communicating serial driver!');
End;

Procedure SerialOpen;
Var i,o:LongInt;
Begin;
if (pipeLineCreate(serialCtrl,serialProc,4096,true)<>0) then immErr('error opening control pipeline!');
if (pipeLineCreate(serialData,serialProc,65536,false)<>0) then immErr('error opening data pipeline!');
o:=16;
while (o>0) do begin;
  relequish;
  dec(o);
  i:=sizeof(serialBuff);
  if (pipeLineRecv(serialCtrl,serialBuff,i)<>0) then i:=0;
  if (i>0) then break;
  end;
if (i<1) then immErr('error getting number of ports!');
i:=serialPort-1;
pipeLineSend(serialCtrl,i,sizeof(i));
pipeLineSend(serialCtrl,serialData,sizeof(serialData));
End;

Procedure SerialClose;
Begin;
pipeLineClose(serialCtrl);
pipeLineClose(serialData);
End;
