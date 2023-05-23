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
{$include fcs16.inc}
{$include serial.inc}

Const
  PacketMax=1500;
  EscapeChar=$7d;
  FramEndChar=$7e;
  EscpModify=$20;
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
  charMap:LongWord;


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
  i:=i xor EscpModify;
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
if (packSiz<4) then begin;
  WriteLn('got a too short packet!');
  packSiz:=0;
  goto f2;
  end;
if (CalcFCS16(packBuf,packSiz-2)<>ReadWordLSB(packBuf[packSiz-1])) then begin;
  WriteLn('got a packet with wrong fcs!');
  packSiz:=0;
  goto f2;
  end;
dec(packSiz,2);
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
i:=CalcFCS16(buf,siz);
WriteWordLSB(buf[siz+1],i);
inc(siz,2);
sendSiz:=1;
sendBuf[1]:=FramEndChar;
for o:=1 to siz do begin;
  i:=buf[o];
  if (sendSiz>sizeof(sendBuf)-16) then flush;
  if (i=EscapeChar) then goto f1;
  if (i=FramEndChar) then goto f1;
  if (i<32) then if ((1 shl i) and charMap<>0) then goto f1;
  inc(sendSiz);
  sendBuf[sendSiz]:=i;
  continue;
  f1:
  inc(sendSiz);
  sendBuf[sendSiz]:=EscapeChar;
  inc(sendSiz);
  sendBuf[sendSiz]:=i xor EscpModify;
  end;
inc(sendSiz);
sendBuf[sendSiz]:=FramEndChar;
flush;
End;



Label f1;
Var
  name:String;
  a:String;
  i,o,p:LongInt;
BEGIN;
WriteLn('hdlc v1.0, done by Mc at '#%date' '#%time'.');
CreateFCS16Table;
name:=paramStr(1);
serialProc:=BVal(name);
if (serialProc=0) then serialProc:=BugOS_findProcNam(name);
serialPort:=BVal(paramStr(2));
charMap:=BVal(paramStr(3));
if (serialPort<1) then immErr('using: hdlc.code <process> <port> [charmap]');
WriteLn('going to open '+name+' '+BStr(serialPort)+'...');
SerialOpen;
name:='hdlc on '+name+' port '+BStr(serialPort);

Write('masked chars:');
for i:=0 to 31 do if ((1 shl i) and charMap<>0) then write(' '+BStr(i));
WriteLn('');
BugOS_MyProcessInfo(i,o,p);
i:=i xor p xor o;
p:=((i shr 24) xor (i shr 16) xor (i shr 8) xor i) and $ff;
WriteLn('local address: '+BStr(p));

Write('waiting for upper level...');
BugOS_SignDaemoning;
pipeLineBegListen;
while (pipeLineGetIncoming(upper)<>0) do relequish;
pipeLineEndListen;
a:='12341234'#0#0#0#0#0#0#0#0+chr(p)+#255+name+#0;
i:=1;move(i,a[1],sizeof(i));
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