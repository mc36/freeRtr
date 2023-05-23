{$heap 15k}
{$stack 7k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}

Const driveMax=16;
Type
  OneDriveRecord=record
    proc:LongInt;
    pipe:LongInt;
    end;
Var
  driveDat:array[1..driveMax] of OneDriveRecord;
  driveNum:LongInt;
  DriveSize:LongInt;
  DriveBegin:LongInt;
  DriveReadOnly:Boolean;
  geomCyl:LongInt;
  geomHed:LongInt;
  geomSec:LongInt;
  CurrPipe:LongInt;
  CurrProc:LongInt;
  packetBuf:record
    cmd:LongInt;
    dat:array[1..1024*4] of char;
    end;
  packetSiz:LongInt;

{$include \sources\filesystem\disk2.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;


Function doPipe:Boolean;
Var
  i,o,p,q:LongInt;
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
    move(geomCyl,packetBuf.dat[5],sizeof(geomCyl));
    move(geomHed,packetBuf.dat[9],sizeof(geomHed));
    move(geomSec,packetBuf.dat[13],sizeof(geomSec));
    i:=0;
    move(driveBegin,packetBuf.dat[17],sizeof(i));
    move(driveSize,packetBuf.dat[21],sizeof(driveSize));
    a:='raid';
    move(a,packetBuf.dat[25],sizeof(a));
    move(a,packetBuf.dat[281],sizeof(a));
    move(a,packetBuf.dat[537],sizeof(a));
    packetBuf.cmd:=0;
    if (pipeLineSend(CurrPipe,packetBuf,$31c)<>0) then exit;
    doPipe:=False;
    end;
  1:begin;{read one sector}
    move(packetBuf.dat,i,sizeof(i));
    if (i>=0) and (i<driveSize) then begin;
      p:=1;
      while (p<driveNum) do begin;
        DriveInternalProcess:=driveDat[p].proc;
        DriveInternalHandler:=driveDat[p].pipe;
        o:=DriveRead(i,packetBuf.dat[5]);
        if (o=0) then break;
        WriteLn('error reading drive #'+BStr(p)+' at '+BStr(i));
        inc(p);
        end;
      end else o:=1;
    packetBuf.cmd:=o;
    if (pipeLineSend(CurrPipe,packetBuf,520)<>0) then exit;
    doPipe:=False;
    end;
  2:begin;{write one sector}
    move(packetBuf.dat,i,sizeof(i));
    if (i>=0) and (i<driveSize) then begin;
      q:=0;
      for p:=1 to driveNum do begin;
        DriveInternalProcess:=driveDat[p].proc;
        DriveInternalHandler:=driveDat[p].pipe;
        o:=DriveWrite(i,packetBuf.dat[5]);
        if (o=0) then continue;
        WriteLn('error writing drive #'+BStr(p)+' at '+BStr(i));
        q:=o;
        end;
      end else q:=1;
    packetBuf.cmd:=q;
    if (pipeLineSend(CurrPipe,packetBuf,8)<>0) then exit;
    doPipe:=False;
    end;
  else writeln('got unknown command: '+BStr(packetBuf.cmd));
  end;
End;



Label f1,f2,f3;
Var
  a:String;
  i,o,p,q:LongInt;
BEGIN;
WriteLn('raid driver v1.0, done by Mc at '#%date' '#%time'.');

DriveReadOnly:=false;
geomCyl:=4095;
geomHed:=15;
geomSec:=63;
p:=$7fffffff;
q:=0;

driveNum:=0;
o:=0;
while (o<paramCount) do begin;
  inc(o);
  a:=paramStr(o);
  inc(o);
  i:=BVal(paramStr(o));
  Write('drive #'+BStr(driveNum+1)+': '+a+' '+BStr(i)+'...');
  i:=DriveOpen(a,i);
  if (i<>0) then begin;
    WriteLn(' error!');
    continue;
    end;
  inc(driveNum);
  driveDat[driveNum].proc:=DriveInternalProcess;
  driveDat[driveNum].pipe:=DriveInternalHandler;
  WriteLn(' '+BStr(DriveSize shr 1)+' kbytes');
  if (DriveSize>p) then continue;
  p:=driveSize;
  q:=driveBegin;
  geomCyl:=DriveInternalIdentifier.cyl;
  geomHed:=DriveInternalIdentifier.hed;
  geomSec:=DriveInternalIdentifier.sec;
  end;
driveSize:=p;
driveBegin:=q;

if (driveNum<2) then immErr('using; raid.code <process> <device> [<process> <device>]...');

WriteLn('raid has '+BStr(driveSize shr 1)+' kbytes capacity...');
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