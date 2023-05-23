Const
  command_req=1;
  command_str=2;
  command_fin=4;
  command_dat=8;
  initiateStr='mc'#13#29'start'#29;
Type
  oneRequestRecord=record
    cmd:byte;
    num:longint;
    end;
  onePacketRecord=record
    cmd:byte;
    num:longint;
    buf:array[1..1024] of byte;
    crc:word;
    sum:word;
    end;




Procedure flushLine(tim:LongInt);
Label f1;
Var
  buf:array[1..512] of byte;
  i,o:LongInt;
Begin;
timer2start;
o:=CurrentTime;
f1:
relequish;
timer2start;
i:=sizeof(buf);
if (pipeLineRecv(pipe,buf,i)<>0) then i:=0;
if (i>0) then begin;
  o:=CurrentTime;
  goto f1;
  end;
if (GetTimePast(o)>=tim) then exit;
goto f1;
End;


Function lineDataReady:Boolean;
Var i,o:LongInt;
Begin;
pipeLineStats(pipe,i,o,i);
lineDataReady:=(o<>0);
End;


Function receiveBytes(var buffer;size,time:LongInt):Boolean;
Label f1;
Var i,o,t:LongInt;
Begin;
receiveBytes:=True;
timer2start;
t:=CurrentTime;
f1:
relequish;
timer2start;
pipeLineStats(pipe,i,o,i);
if (o<size) then begin;
  if (GetTimePast(t)>=time) then exit;
  goto f1;
  end;
i:=size;
if (pipeLineRecv(pipe,buffer,i)<>0) then i:=0;
if (i<>size) then exit;
receiveBytes:=False;
End;






Procedure sendReqest(cmd,pos:LongInt);
Var buf:oneRequestRecord;
Begin;
buf.cmd:=cmd;
WriteLongLSB(buf.num,pos);
pipeLineSend(pipe,buf,sizeof(buf));
End;


Function recvRequest(var cmd,pos:LongInt):String;
Var buf:oneRequestRecord;
Begin;
cmd:=-1;
if receiveBytes(buf,sizeof(buf),5) then begin;
  flushLine(0);
  recvRequest:='timeout';
  exit;
  end;
cmd:=-2;
if lineDataReady then begin;
  recvRequest:='garbage data arrived';
  exit;
  end;
cmd:=buf.cmd;
pos:=ReadLongLSB(buf.num);
End;






Procedure sendPacket(cmd,pos:LongInt;var buffer);
Var
  buf:onePacketRecord;
  w:Word;
Begin;
buf.cmd:=cmd;
WriteLongLSB(buf.num,pos);
move(buffer,buf.buf,sizeof(buf.buf));
w:=0;
chksum16update(w,buf.buf,sizeof(buf.buf));
WriteWordLSB(buf.sum,w);
w:=0;
crc16update(w,buf.buf,sizeof(buf.buf));
WriteWordMSB(buf.crc,w);
pipeLineSend(pipe,buf,sizeof(buf));
End;


Function recvPacket(var cmd,pos:LongInt;var buffer):String;
Var
  buf:onePacketRecord;
  w:Word;
Begin;
cmd:=-1;
if receiveBytes(buf,sizeof(buf),10) then begin;
  flushLine(0);
  recvPacket:='timeout';
  exit;
  end;
cmd:=-2;
if lineDataReady then begin;
  recvPacket:='garbage data arrived';
  exit;
  end;
w:=0;
chksum16update(w,buf.buf,sizeof(buf.buf));
if (ReadWordLSB(buf.sum)<>w) then begin;
  recvPacket:='chksum error';
  exit;
  end;
w:=0;
crc16update(w,buf.buf,sizeof(buf.buf));
if (ReadWordMSB(buf.crc)<>w) then begin;
  recvPacket:='crc16 error';
  exit;
  end;
move(buf.buf,buffer,sizeof(buf.buf));
pos:=ReadLongLSB(buf.num);
cmd:=buf.cmd;
End;
