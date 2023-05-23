{$sysinc pipeline.inc}
Var
  DriveInternalHandler:LongInt;
  DriveInternalProcess:LongInt;
  DriveInternalBuffer:record
    cmd:LongInt;
    num:LongInt;
    buf:array[1..512] of char;
    end;
  DriveInternalIdentifier:record
    r1:longint;
    cyl:longint;
    hed:longint;
    sec:longint;
    beg:longint;
    siz:longint;
    model:string;
    serial:string;
    firm:string;
    end;


Function DriveOpen(nam:String;num:longint):Word;
Label f1,f2,f3;
Var
  buf:array[1..2*1024] of byte;
  i,o,p:LongInt;
Begin;
DriveOpen:=15;
i:=BugOS_findProcNam(nam);
if (i=0) then exit;
DriveInternalProcess:=i;
if (pipeLineCreate(DriveInternalHandler,i,4096,true)<>0) then exit;
if (pipeLineSend(DriveInternalHandler,num,sizeof(num))<>0) then exit;
for o:=1 to 16 do begin;
  Relequish;
  i:=sizeof(DriveInternalBuffer);
  if (pipeLineRecv(DriveInternalHandler,DriveInternalBuffer,i)<>0) then continue;
  if (i>0) then goto f1;
  end;
exit;
f1:
num:=0;
if (pipeLineSend(DriveInternalHandler,num,sizeof(num))<>0) then exit;
f3:
i:=sizeof(buf);
if (pipeLineRecv(DriveInternalHandler,buf,i)<>0) then begin;
  if (pipeLineStats(DriveInternalHandler,i,o,p)<>0) then exit;
  if (i<>DriveInternalProcess) then exit;
  Relequish;
  goto f3;
  end;
move(buf[5],DriveInternalIdentifier,sizeof(DriveInternalIdentifier));
DriveSize:=DriveInternalIdentifier.siz;
DriveBegin:=DriveInternalIdentifier.beg;
DriveOpen:=0;
End;

Function DriveClose:Word;
Begin;
DriveClose:=pipeLineClose(DriveInternalHandler);
End;

Function DriveRead(sec:longint;var buf):Word;
Label f1;
Var i,o,p,q:LongInt;
Begin;
DriveRead:=15;
DriveInternalBuffer.cmd:=1;
DriveInternalBuffer.num:=sec;
if (pipeLineSend(DriveInternalHandler,DriveInternalBuffer,8)<>0) then exit;
relequish;
f1:
i:=sizeof(DriveInternalBuffer);
if (pipeLineRecv(DriveInternalHandler,DriveInternalBuffer,i)<>0) then begin;
  i:=pipeLineStats(DriveInternalHandler,o,p,q);
  if (i<>0) then exit;
  if (o<>DriveInternalProcess) then exit;
  Relequish;
  goto f1;
  end;
move(DriveInternalBuffer.buf,buf,512);
DriveRead:=DriveInternalBuffer.cmd;
End;

Function DriveWrite(sec:longint;var buf):Word;
Label f1,f2;
Var i,o,p,q:LongInt;
Begin;
if DriveReadOnly then begin; DriveWrite:=4;exit; end;
DriveWrite:=15;
DriveInternalBuffer.cmd:=2;
DriveInternalBuffer.num:=sec;
move(buf,DriveInternalBuffer.buf,512);
if (pipeLineSend(DriveInternalHandler,DriveInternalBuffer,520)<>0) then exit;
relequish;
f1:
i:=sizeof(DriveInternalBuffer);
if (pipeLineRecv(DriveInternalHandler,DriveInternalBuffer,i)<>0) then begin;
  i:=pipeLineStats(DriveInternalHandler,o,p,q);
  if (i<>0) then exit;
  if (o<>DriveInternalProcess) then exit;
  Relequish;
  goto f1;
  end;
DriveWrite:=DriveInternalBuffer.cmd;
End;
