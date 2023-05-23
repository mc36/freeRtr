{$heap 15k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc param.inc}
Const
  MaxHed=2;
  MaxSec=18;
Type
  OneSectorRec=array[1..2048] of char;
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

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

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
  0:begin;{identify drive}
    i:=0;
    move(i,packetBuf.dat[1],sizeof(i));
    move(i,packetBuf.dat[5],sizeof(i));
    move(i,packetBuf.dat[9],sizeof(i));
    move(i,packetBuf.dat[13],sizeof(i));
    move(i,packetBuf.dat[17],sizeof(i));
    move(i,packetBuf.dat[21],sizeof(i));
    a:='cdemu';
    move(a,packetBuf.dat[25],sizeof(a));
    move(a,packetBuf.dat[281],sizeof(a));
    move(a,packetBuf.dat[537],sizeof(a));
    packetBuf.cmd:=0;
    if (pipeLineSend(CurrPipe,packetBuf,$31c)<>0) then exit;
    doPipe:=False;
    end;
  3:begin;{(un)lock door}
    packetBuf.cmd:=0;
    if (pipeLineSend(CurrPipe,packetBuf,4)<>0) then exit;
    doPipe:=False;
    end;
  4:begin;{start playback}
    packetBuf.cmd:=0;
    if (pipeLineSend(CurrPipe,packetBuf,4)<>0) then exit;
    doPipe:=False;
    end;
  5:begin;{pause playback}
    packetBuf.cmd:=0;
    if (pipeLineSend(CurrPipe,packetBuf,4)<>0) then exit;
    doPipe:=False;
    end;
  6:begin;{stop playback}
    packetBuf.cmd:=0;
    if (pipeLineSend(CurrPipe,packetBuf,4)<>0) then exit;
    doPipe:=False;
    end;
  7:begin;{open/close door}
    packetBuf.cmd:=0;
    if (pipeLineSend(CurrPipe,packetBuf,4)<>0) then exit;
    doPipe:=False;
    end;
  8:begin;{read subchannel data}
    packetBuf.cmd:=0;
    i:=-1;
    move(i,packetBuf.dat[1],sizeof(i));
    i:=0;
    move(i,packetBuf.dat[5],sizeof(i));
    if (pipeLineSend(CurrPipe,packetBuf,12)<>0) then exit;
    doPipe:=False;
    end;
  9:begin;{disk readyness test}
    packetBuf.cmd:=0;
    i:=1;
    move(i,packetBuf.dat[1],sizeof(i));
    if (pipeLineSend(CurrPipe,packetBuf,8)<>0) then exit;
    doPipe:=False;
    end;
  10:begin;{read one toc entry}
    packetBuf.cmd:=0;
    i:=1;
    move(i,packetBuf.dat[1],sizeof(i));
    move(i,packetBuf.dat[5],sizeof(i));
    move(i,packetBuf.dat[9],sizeof(i));
    i:=0;
    move(i,packetBuf.dat[13],sizeof(i));
    i:=bufferMax;
    move(i,packetBuf.dat[17],sizeof(i));
    i:=$14;
    move(i,packetBuf.dat[21],sizeof(i));
    if (pipeLineSend(CurrPipe,packetBuf,28)<>0) then exit;
    doPipe:=False;
    end;
  11:begin;{read one sector}
    move(packetBuf.dat,i,sizeof(i));
    if (i>=0) and (i<bufferMax) then begin;
      move(bufferBuf^[i],packetBuf.dat[5],sizeof(OneSectorRec));
      packetBuf.cmd:=0;
      end else packetBuf.cmd:=1;
    if (pipeLineSend(CurrPipe,packetBuf,2058)<>0) then exit;
    doPipe:=False;
    end;
  else writeln('got unknown command: '+BStr(packetBuf.cmd));
  end;
End;


Label f1,f2,f3;
Var
  i,o,p:LongInt;
  a:String;
  f:xFile;
BEGIN;
WriteLn('cdrom emulator v1.0, done by Mc at '#%date' '#%time'.');
a:=ParamStr(1);
if (a='') then immErr('using: cdemu.code <filename>');
if (xOpen(f,a,xGenFilMod_r)<>0) then immErr('error opening file!');
bufferMax:=xFileSize(f) div sizeof(OneSectorRec);
i:=bufferMax*sizeof(OneSectorRec);
WriteLn('cd size will be '+BStr(i)+' bytes.');
if resize(i) then immErr('error allocating memory!');
for i:=0 to bufferMax-1 do
 if (xBlockRead(f,bufferBuf^[i],sizeof(OneSectorRec))<>0) then immErr('error reading file!');
xClose(f);

WriteLn('serving...');
if (pipeLineBegListen<>0) then immErr('error start listening!');
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