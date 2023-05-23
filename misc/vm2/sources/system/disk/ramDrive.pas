{$heap 15k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc param.inc}
Const
  MaxHed=2;
  MaxSec=18;
Type
  OneSectorRec=array[1..512] of char;
Var
  bufferMax:LongInt;
  bufferBuf:^array[0..1] of OneSectorRec;
  CurrPipe:LongInt;
  CurrProc:LongInt;
  packetBuf:record
    cmd:LongInt;
    dat:array[1..1024*4] of char;
    end;
  packetSiz:LongInt;

Function resize(n:LongInt):Boolean;
Var
  i:LongInt;
  p:Pointer;
Begin;
resize:=true;
i:=ExtendedMemoryResize(p,n);
if (i<n) then exit;
bufferBuf:=p^;
resize:=false;
End;



Function doPipe:Boolean;
Var
  i,o,p:LongInt;
  a:String;
Begin;
doPipe:=True;
packetSiz:=sizeof(packetBuf);
pipeLineRecv(CurrPipe,packetBuf,packetSiz);
if (packetSiz=0) then begin;
  if (pipeLineStats(CurrPipe,i,o,p)<>0) then exit;
  if (i=0) then exit;
  doPipe:=False;
  exit;
  end;
if (packetSiz<4) then exit;
dec(packetSiz,4);
case packetBuf.cmd of
  1:begin;{read one sector}
    move(packetBuf.dat,i,sizeof(i));
    if (i>=0) and (i<bufferMax) then begin;
      move(bufferBuf^[i],packetBuf.dat[5],sizeof(OneSectorRec));
      packetBuf.cmd:=0;
      end else packetBuf.cmd:=1;
    if (pipeLineSend(CurrPipe,packetBuf,520)<>0) then exit;
    doPipe:=False;
    end;
  2:begin;{write one sector}
    move(packetBuf.dat,i,sizeof(i));
    if (i>=0) and (i<bufferMax) then begin;
      move(packetBuf.dat[5],bufferBuf^[i],sizeof(OneSectorRec));
      packetBuf.cmd:=0;
      end else packetBuf.cmd:=1;
    if (pipeLineSend(CurrPipe,packetBuf,8)<>0) then exit;
    doPipe:=False;
    end;
  0:begin;{identify drive}
    i:=0;
    move(i,packetBuf.dat[1],sizeof(i));
    i:=(bufferMax div (MaxHed*MaxSec))+1;
    move(i,packetBuf.dat[5],sizeof(i));
    i:=MaxHed;
    move(i,packetBuf.dat[9],sizeof(i));
    i:=MaxSec;
    move(i,packetBuf.dat[13],sizeof(i));
    i:=0;
    move(i,packetBuf.dat[17],sizeof(i));
    move(bufferMax,packetBuf.dat[21],sizeof(bufferMax));
    a:='ramdrive';
    move(a,packetBuf.dat[25],sizeof(a));
    move(a,packetBuf.dat[281],sizeof(a));
    move(a,packetBuf.dat[537],sizeof(a));
    packetBuf.cmd:=0;
    if (pipeLineSend(CurrPipe,packetBuf,$31c)<>0) then exit;
    doPipe:=False;
    end;
  else writeln('got unknown command: '+BStr(packetBuf.cmd));
  end;
End;


Label f1,f2,f3;
Var i,o,p:LongInt;
BEGIN;
WriteLn('ramDrive v1.0, done by Mc at '#%date' '#%time'.');
bufferMax:=BVal(ParamStr(1))*1024 div sizeof(OneSectorRec);
if (bufferMax<1) then begin;
  WriteLn('using: ramdrive.code <size in KB>');
  Halt(1);
  end;
i:=bufferMax*sizeof(OneSectorRec);
WriteLn('ramdrive size will be '+BStr(i)+' bytes.');
if resize(i) then begin;
  WriteLn('error allocating memory...');
  Halt(1);
  end;
fillchar(bufferBuf^,i,0);
if (pipeLineBegListen<>0) then begin;
  WriteLn('error start listening...');
  Halt(1);
  end;
BugOS_SignDaemoning;

f1:
if (pipeLineGetIncoming(CurrPipe)<>0) then begin;
  Relequish;
  goto f1;
  end;
WriteLn('incoming connection...');
if (pipeLineStats(CurrPipe,CurrProc,i,o)<>0) then goto f2;
WriteLn('pipe='+BStr(CurrPipe)+'  pid='+BStr(CurrProc)+'.');
i:=1;if (pipeLineSend(CurrPipe,i,sizeof(i))<>0) then goto f2;
BugOS_ProcessName(CurrProc,packetBuf,i,o,p);
if (p and $40=0) then goto f2;
WriteLn('asking for drive...');
p:=16;
repeat
  dec(p);
  if (p<0) then goto f2;
  packetSiz:=sizeof(packetBuf);
  pipeLineRecv(CurrPipe,packetBuf,packetSiz);
  if (packetSiz=0) then begin; relequish;continue; end;
  if (packetSiz<>4) then goto f2;
  if (packetBuf.cmd<>0) then goto f2;
  until (packetSiz=4);
WriteLn('serving drive...');

f3:
if doPipe then goto f2;
Relequish;
goto f3;

f2:
pipeLineClose(CurrPipe);
goto f1;
END.