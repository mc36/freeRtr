{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc memory.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

{$include \sources\internet\kernel\utils\timer2.inc}
{$include serial.inc}

Const
  PacketMax=1500;
  FramEndChar=$c0;
  EscapeChar=$db;
  escaped_end=$dc;
  escaped_esc=$dd;
Var
  upper:LongInt;
  sendBuf:array[1..1024] of byte;
  sendSiz:LongInt;
  recvBuf:array[1..1024] of byte;
  recvSiz:LongInt;
  recvEsc:Boolean;
  packBuf:array[1..PacketMax+256] of byte;
  trnsBuf:array[1..PacketMax+256] of byte;
  packSiz:LongInt;
  packDon:Boolean;
  lastCDchk:LongInt;


Procedure flush;
Begin;
if (sendSiz<1) then exit;
pipeLineSend(serialData,sendBuf,sendSiz);
sendSiz:=0;
relequish;
End;

Procedure send(var dat;siz:LongInt);
Label f1;
Var
  buf:array[1..1] of byte absolute dat;
  i,o:LongInt;
Begin;
sendSiz:=1;
sendBuf[1]:=FramEndChar;
for o:=1 to siz do begin;
  i:=buf[o];
  if (sendSiz>sizeof(sendBuf)-16) then flush;
  case i of
    EscapeChar:begin;
      inc(sendSiz);
      sendBuf[sendSiz]:=EscapeChar;
      i:=escaped_esc;
      end;
    FramEndChar:begin;
      inc(sendSiz);
      sendBuf[sendSiz]:=EscapeChar;
      i:=escaped_end;
      end;
    end;
  inc(sendSiz);
  sendBuf[sendSiz]:=i;
  end;
inc(sendSiz);
sendBuf[sendSiz]:=FramEndChar;
flush;
End;

Procedure receive;
Label f1,f2,f3;
Var i,o:LongInt;
Begin;
f1:
if packDon then exit;
if (recvSiz<1) then begin;
  recvSiz:=sizeof(recvBuf);
  if (pipeLineRecv(serialData,recvBuf,recvSiz)<>0) then recvSiz:=0;
  end;
if (recvSiz<1) then begin;
  pipeLineStats(serialData,o,i,i);
  if (o=0) then immErr('serial driver closed connection!');
  timer2start;
  if (GetTimePast(lastCDchk)<60) then exit;
  serialBuff[1]:=1;serialCmd(1);
  lastCDchk:=CurrentTime;
  if (serialBuff[5]<>0) then immErr('carrier detect changed!');
  sendSiz:=1;
  sendBuf[1]:=FramEndChar;
  flush;
  exit;
  end;
o:=0;
f2:
inc(o);
if (o>recvSiz) then begin;
  recvSiz:=0;
  goto f1;
  end;
i:=recvBuf[o];
if recvEsc then begin;
  case i of
    escaped_end:i:=FramEndChar;
    escaped_esc:i:=EscapeChar;
    else goto f2;
    end;
  goto f3;
  end;
if (i=EscapeChar) then begin;
  recvEsc:=True;
  goto f2;
  end;
if (i<>FramEndChar) then goto f3;
if (packSiz<1) then begin;
  packSiz:=0;
  goto f2;
  end;
packDon:=True;
dec(recvSiz,o);
move(recvBuf[o+1],recvBuf,recvSiz);
exit;
f3:
recvEsc:=False;
inc(packSiz);
packBuf[packSiz]:=i;
if (packSiz<PacketMax) then goto f2;
packSiz:=0;
goto f2;
End;



Label f1;
Var
  name:String;
  a:String;
  i,o,p:LongInt;
BEGIN;
WriteLn('slip v1.0, done by Mc at '#%date' '#%time'.');
name:=paramStr(1);
serialProc:=BVal(name);
if (serialProc=0) then serialProc:=BugOS_findProcNam(name);
serialPort:=BVal(paramStr(2));
if (serialPort<1) then immErr('using: slip.code <process> <port>');
WriteLn('going to open '+name+' '+BStr(serialPort)+'...');
SerialOpen;
name:='slip on '+name+' port '+BStr(serialPort);

Write('waiting for upper level...');
BugOS_SignDaemoning;
pipeLineBegListen;
while (pipeLineGetIncoming(upper)<>0) do relequish;
pipeLineEndListen;
a:='12341234'#0#0#0#0#0#0#0#0+name+#0;
i:=0;move(i,a[1],sizeof(i));
i:=PacketMax;move(i,a[5],sizeof(i));
pipeLineSend(upper,a[1],length(a));
WriteLn(' done!');

recvEsc:=False;
recvSiz:=0;
packSiz:=0;
packDon:=False;
lastCDchk:=-99999;
sendSiz:=0;

f1:
receive;
if packDon then begin;
  pipeLineSend(upper,packBuf,packSiz);
  packSiz:=0;
  packDon:=False;
  end;
i:=sizeof(trnsBuf);
if (pipeLineRecv(upper,trnsBuf,i)<>0) then i:=0;
if (i<1) then begin;
  pipeLineStats(upper,o,i,i);
  if (o=0) then immErr('upper level closed connection!');
  relequish;
  goto f1;
  end;
send(trnsBuf,i);

relequish;
goto f1;
END.