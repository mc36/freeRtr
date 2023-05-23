Var
  CurrPipe:LongInt;
  CurrProc:LongInt;
  CurrDiskFileName:String;
  CurrDiskProcName:String;
  CurrDiskProcNum:LongInt;
  packetBuf:record
    cmd:LongInt;
    dat:array[1..1024*4] of char;
    end;
  packetSiz:LongInt;


Function doPipe:Boolean;
Const
  MaxHed=2;
  MaxSec=18;
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
    if (i>=0) and (i<CurrentSectorChainSize) then begin;
      packetBuf.cmd:=DriveRead(CurrentSectorChainList^[i+1],packetBuf.dat[5]);
      end else packetBuf.cmd:=1;
    if (pipeLineSend(CurrPipe,packetBuf,520)<>0) then exit;
    doPipe:=False;
    end;
  2:begin;{write one sector}
    move(packetBuf.dat,i,sizeof(i));
    if (i>=0) and (i<CurrentSectorChainSize) then begin;
      packetBuf.cmd:=DriveWrite(CurrentSectorChainList^[i+1],packetBuf.dat[5]);
      end else packetBuf.cmd:=1;
    if (pipeLineSend(CurrPipe,packetBuf,8)<>0) then exit;
    doPipe:=False;
    end;
  0:begin;{identify drive}
    i:=0;
    move(i,packetBuf.dat[1],sizeof(i));
    i:=(CurrentSectorChainSize div (MaxHed*MaxSec))+1;
    move(i,packetBuf.dat[5],sizeof(i));
    i:=MaxHed;
    move(i,packetBuf.dat[9],sizeof(i));
    i:=MaxSec;
    move(i,packetBuf.dat[13],sizeof(i));
    i:=0;
    move(i,packetBuf.dat[17],sizeof(i));
    move(CurrentSectorChainSize,packetBuf.dat[21],sizeof(CurrentSectorChainSize));
    a:='filedisk in '+CurrDiskFileName;
    move(a,packetBuf.dat[25],sizeof(a));
    a:=CurrDiskProcName+' '+BStr(CurrDiskProcNum);
    move(a,packetBuf.dat[281],sizeof(a));
    a:=proggyName;
    move(a,packetBuf.dat[537],sizeof(a));
    packetBuf.cmd:=0;
    if (pipeLineSend(CurrPipe,packetBuf,$31c)<>0) then exit;
    doPipe:=False;
    end;
  else writeln('got unknown command: '+BStr(packetBuf.cmd));
  end;
End;


Label f1,f2,f3;
Var
  a:String;
  i,o,p:LongInt;
BEGIN;
WriteLn(ProggyName+' disk in file v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<3) then begin;
  WriteLn('using: fd-'+proggyName+'.code <process> <drive> <letter>');
  Halt(1);
  end;
a:=ParamStr(2);
i:=BVal(a);
a:=ParamStr(1);
CurrDiskProcName:=a;
CurrDiskProcNum:=i;
WriteLn('opening '+a+' '+BStr(i)+'...');
if (DriveOpen(a,i)<>0) then begin;
  WriteLn('error opening drive!');
  Halt(1);
  end;
DriveReadOnly:=False;
WriteLn('this disk has '+BStr(DriveSize*512)+' bytes total capacity.');

a:=ParamStr(3);
if (copy(a,2,1)=':') then a:=copy(a,3,666);
if (copy(a,1,1)<>'\') then a:='\'+a;
CurrDiskFileName:=a;
WriteLn('scanning sectors of file '+a+'...');
driveLetter:='@';
CurrentSectorChainSize:=0;
CurrentSectorChainAlloc:=0;
if ListUpSectorsOfGivenFile(a) then immErr('error!');
if (CurrentSectorChainSize<1) then immErr('image empty!');
PrintOutTheSectorList;
WriteLn('image has '+BStr(CurrentSectorChainSize*512)+' bytes total capacity.');

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