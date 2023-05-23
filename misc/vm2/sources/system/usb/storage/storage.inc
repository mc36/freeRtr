Procedure doMain;
Label f1,f2;
Var
  data:array[1..16] of record
    pipeLine:LongInt;
    blockSiz:LongInt;
    blockMax:LongInt;
    lun:LongInt;
    end;
  max:LongInt;
  packetBuf:record
    cmd:LongInt;
    dat:array[1..1024*4] of char;
    end;
  i,o,p,q:LongInt;
  a:String;

procedure doPipe;
begin;
if (data[q].pipeLine=0) then exit;
p:=sizeof(packetBuf);
pipeLineRecv(data[q].pipeLine,packetBuf,p);
if (p=0) then begin;
  if (pipeLineStats(data[q].pipeLine,i,o,o)<>0) then i:=0;
  if (i<>0) then exit;
  pipeLineClose(data[q].pipeLine);
  data[q].pipeLine:=0;
  exit;
  end;
if (p<4) then exit;
case packetBuf.cmd of
  0:begin;{identify drive}
    i:=0;
    move(i,packetBuf.dat[1],sizeof(i));
    i:=data[q].blockMax;
    move(i,packetBuf.dat[5],sizeof(i));
    i:=1;
    move(i,packetBuf.dat[9],sizeof(i));
    i:=1;
    move(i,packetBuf.dat[13],sizeof(i));
    i:=0;
    move(i,packetBuf.dat[17],sizeof(i));
    i:=data[q].blockMax;
    move(i,packetBuf.dat[21],sizeof(i));
    a:='usb mass storage';
    move(a,packetBuf.dat[25],sizeof(a));
    move(a,packetBuf.dat[281],sizeof(a));
    move(a,packetBuf.dat[537],sizeof(a));
    packetBuf.cmd:=0;
    if (pipeLineSend(data[q].pipeLine,packetBuf,$31c)<>0) then exit;
    end;
  1:begin;{read one sector}
    move(packetBuf.dat,i,sizeof(i));
    if (i>=0) and (i<data[q].blockMax) then begin;
      blockSize:=data[q].blockSiz;
      blocksMax:=data[q].blockMax;
      if cmdsetRead(data[q].lun,i,packetBuf.dat[5]) then packetBuf.cmd:=1 else packetBuf.cmd:=0;
      end else packetBuf.cmd:=1;
    if (pipeLineSend(data[q].pipeLine,packetBuf,520)<>0) then exit;
    end;
  2:begin;{write one sector}
    move(packetBuf.dat,i,sizeof(i));
    if (i>=0) and (i<data[q].blockMax) then begin;
      blockSize:=data[q].blockSiz;
      blocksMax:=data[q].blockMax;
      if cmdsetWrite(data[q].lun,i,packetBuf.dat[5]) then packetBuf.cmd:=1 else packetBuf.cmd:=0;
      packetBuf.cmd:=0;
      end else packetBuf.cmd:=1;
    if (pipeLineSend(data[q].pipeLine,packetBuf,8)<>0) then exit;
    end;
  else writeln('got unknown command: '+BStr(packetBuf.cmd));
  end;
end;

Begin;
if (pipeLineCreate(pipeLine,BVal(ParamStr(1)),65536,true)<>0) then immErr('error creating pipeline!');
device:=BVal(ParamStr(2));
speed:=BVal(ParamStr(3));

protocolGetParams;
WriteLn('number of drives: '+BStr(maxDrives));
max:=0;
fillchar(data,sizeof(data),0);
for i:=0 to maxDrives-1 do begin;
  Write('drive #'+BStr(i)+': ');
  if cmdsetCapacity(i) then exit;
  WriteLn(BStr(blocksMax)+'*'+BStr(blockSize));
  if (blockSize<>512) then continue;
  if (blocksMax<1) then continue;
  inc(max);
  data[max].pipeLine:=0;
  data[max].blockSiz:=blockSize;
  data[max].blockMax:=blocksMax;
  data[max].lun:=i;
  end;
if (max<1) then immErr('no drives available!');

if (pipeLineBegListen<>0) then immErr('error start listening...');
BugOS_SignDaemoning;
WriteLn('serving others...');

f1:
for q:=1 to max do doPipe;
if (pipeLineGetIncoming(q)<>0) then begin;
  Relequish;
  goto f1;
  end;
if (pipeLineStats(q,p,i,o)<>0) then begin;
  f2:
  pipeLineClose(q);
  goto f1;
  end;
BugOS_ProcessName(p,packetBuf,i,i,o);
if (o and $40=0) then goto f2;
if (pipeLineSend(q,max,sizeof(max))<>0) then goto f2;
p:=16;
repeat
  dec(p);
  if (p<0) then goto f2;
  i:=sizeof(packetBuf);
  pipeLineRecv(q,packetBuf,i);
  if (i=0) then continue;
  if (i<>4) then goto f2;
  until (i=0);
if (packetBuf.cmd>=max) then goto f2;
p:=packetBuf.cmd+1;
if (data[p].pipeLine<>0) then goto f2;
data[p].pipeLine:=q;
goto f1;
End;
