{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc param.inc}
{$sysinc datetime.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}

Const
  lastMsgsMax=16;
  bufferMax=500;
  systemPrefix='\\system\\';
Type
  OneConnectionRecord=record
    pipe:LongInt;                       {pipeline id}
    proc:LongInt;                       {process id}
    name:String;                        {name of process}
    msgs:LongInt;                       {number of messages}
    buf:array[1..bufferMax] of byte;    {receive buffer}
    size:LongInt;                       {received bytes}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  lastMsgsDat:array[1..lastMsgsMax] of array[1..768] of byte;
  lastMsgsSiz:array[1..lastMsgsMax] of LongInt;
  lastMsgsCur:LongInt;
  mesgNumb:LongInt;
  servAddr:OneTCPaddressRecord;
  servPort:LongInt;
  servPipe:LongInt;
  userPipe:LongInt;
  fileName:String;
  fileHand:xtText;
  fileLock:Boolean;
  fileDate:LongInt;

Function ResizeMem(n:LongInt):Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
ResizeMem:=True;
i:=n*sizeof(OneConnectionRecord);
if (ExtendedMemoryResize(p,i)<i) then exit;
ConnectionNum:=n;
ConnectionDat:=p^;
ResizeMem:=False;
End;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function getCurrentDateBin:LongInt;
Var a,b,c:Word;
Begin;
xGetDate(a,b,c);
getCurrentDateBin:=(c shl 16) or (b shl 8) or c;
End;

Function padUpWithZeros(i:LongInt):String;
Var a:String;
Begin;
a:=BStr(i);
while (length(a)<2) do a:='0'+a;
padUpWithZeros:=a;
End;

Function getCurrentDateStr:String;
Var a,b,c:Word;
Begin;
xGetDate(a,b,c);
getCurrentDateStr:=padUpWithZeros(a)+'-'+padUpWithZeros(b)+'-'+padUpWithZeros(c);
End;

Function getCurrentTimeStr:String;
Var a,b,c:Word;
Begin;
xGetTime(a,b,c);
getCurrentTimeStr:=padUpWithZeros(a)+':'+padUpWithZeros(b)+':'+padUpWithZeros(c);
End;



Procedure append2log(who,what:String);
Var
  buf:array[1..1024] of byte;
  i,o:LongInt;
  a:String;
Begin;
WriteLn(who+' '+what);
a:=getCurrentTimeStr;
move(a[1],buf,sizeof(a));
o:=length(a);
inc(o);buf[o]:=32;
inc(mesgNumb);
a:='#'+BStr(mesgNumb);
move(a[1],buf[o+1],sizeof(a));
inc(o,length(a));
inc(o);buf[o]:=32;
move(who[1],buf[o+1],sizeof(who));
inc(o,length(who));
inc(o);buf[o]:=32;
move(what[1],buf[o+1],sizeof(what));
inc(o,length(what));
UDPsendPacket(servPipe,servAddr,servPort,buf[10],o-9);
inc(o);buf[o]:=13;
inc(o);buf[o]:=10;
pipeLineSend(userPipe,buf,o);
lastMsgsCur:=(lastMsgsCur mod lastMsgsMax)+1;
lastMsgsSiz[lastMsgsCur]:=o;
move(buf,lastMsgsDat[lastMsgsCur],o);
if (fileName='') then exit;
i:=getCurrentDateBin;
if (i<>fileDate) then begin;
  fileDate:=i;
  xtClose(fileHand);
  a:=fileName;
  i:=pos('%',a);
  if (i>0) then a:=copy(a,1,i-1)+getCurrentDateStr+copy(a,i+1,666);
  xCreate(a);
  xtOpen(fileHand,a,false);
  end;
for i:=1 to o do xtPutOneChar(fileHand,buf[i]);
if fileLock then exit;
xtClose(fileHand);
dec(fileDate);
End;



Procedure startOneProcess(b:String);
Label f1,f2;
Var
  dat:record
    nam:string;
    par:string;
    end;
  d:OneConnectionRecord;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
Begin;
WriteLn('');
append2log(systemPrefix+'start:',b);
fillchar(d,sizeof(d),0);
i:=pos(' ',b);
if (i<1) then i:=666;
a:=copy(b,1,i-1);
b:=copy(b,i+1,666);
WriteLn('"'+a+'" "'+b+'"');
i:=xExecInside(a,b,d.proc,d.pipe);
p:=0;
f1:
o:=250;
if (pipeLineRecv(d.pipe,a[1],o)<>0) then o:=0;
if (o<1) then begin;
  pipeLineStats(d.pipe,o,i,i);
  if (o=d.proc) then begin; relequish;goto f1; end;
  pipeLineClose(d.pipe);
  append2log(systemPrefix+'done','');
  exit;
  end;
ab0:=o;
for i:=1 to o do begin;
  if (ab[i]=0) then inc(p) else p:=0;
  if (p>=2) then goto f2;
  end;
Write(a);
goto f1;
f2:
dec(i,2);
if (i<0) then i:=0;
ab0:=i;
Write(a);
BugOS_ProcessName(d.proc,dat,i,o,o);
a:=dat.nam;
o:=0;
for i:=1 to ab0 do if (ab[i]=$5c) then o:=i;
d.name:=copy(a,o+1,666);
if ResizeMem(ConnectionNum+1) then exit;
ConnectionDat^[ConnectionNum]:=d;
End;



Function readUpConfig(a:String):Boolean;
Label f1;
Var
  d:OneConnectionRecord;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
  t:xtText;
Begin;
readUpConfig:=True;
append2log(systemPrefix+'script:',a);
if (xtOpen(t,a,true)<>0) then exit;
f1:
if xtEOF(t) then begin;
  xtClose(t);
  WriteLn('');
  WriteLn('script started successfully!');
  readUpConfig:=False;
  exit;
  end;
a:=xtReadLn(t,666);
i:=pos(';',a);
if (i>0) then ab0:=i;
while (ab[ab0]=32) and (ab0>0) do dec(ab0);
while (ab[1]=32) and (ab0>0) do a:=copy(a,2,666);
if (ab0<1) then goto f1;
startOneProcess(a);
goto f1;
End;



Procedure releq2proc(var d:OneConnectionRecord;n:LongInt);
Const max=250;
Label f1,f2;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
o:=bufferMax-d.size;
if (pipeLineRecv(d.pipe,d.buf[d.size+1],o)<>0) then o:=0;
if (o<1) then begin;
  pipeLineStats(d.pipe,o,i,i);
  if (o=d.proc) then goto f1;
  pipeLineClose(d.pipe);
  append2log(systemPrefix+'stop:',d.name);
  ConnectionDat^[n]:=ConnectionDat^[ConnectionNum];
  ResizeMem(ConnectionNum-1);
  exit;
  end;
inc(d.size,o);
f1:
for o:=1 to d.size do if (d.buf[o] in [13,10]) then goto f2;
if (d.size<max) then exit;
o:=max+1;
f2:
dec(o);
if (o<1) then begin;
  move(d.buf[2],d.buf,d.size);
  dec(d.size);
  goto f1;
  end;
if (o>max) then o:=max;
move(d.buf,ab[1],o);
ab0:=o;
move(d.buf[o+1],d.buf,d.size);
dec(d.size,o);
append2log(d.name,a);
inc(d.msgs);
goto f1;
End;



Procedure releq2new(var pip:LongInt);
Label f1,f2,f3;
Var
  i,o,p:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  buf:array[1..1024] of byte;

Procedure snd(a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
pipeLineSend(pip,ab[1],ab0);
End;

Procedure doLast;
Begin;
snd('here comes the last '+BStr(lastMsgsMax)+' messages:'#13#10);
p:=lastMsgsCur;
for o:=1 to lastMsgsMax do begin;
  relequish;
  p:=(p mod lastMsgsMax)+1;
  pipeLineSend(pip,lastMsgsDat[p],lastMsgsSiz[p]);
  end;
End;

procedure doCurr;
Begin;
if (userPipe<>0) then begin; a:='only one viewer of current messages allowed!';exit end;
userPipe:=pip;
snd('the following processes exist:'#13#10);
for i:=1 to ConnectionNum do snd(ConnectionDat^[i].name+': '+BStr(ConnectionDat^[i].msgs)+#13#10);
snd(BStr(mesgNumb)+' messages saw, here come the new ones...'#13#10);
pip:=0;
End;

Begin;
pipeLineStats(pip,p,i,o);
BugOS_ProcessName(p,buf,o,i,i);
if (o<>0) then begin; a:='not authorized!';goto f2; end;
p:=64;
f1:
ab0:=0;
dec(p);
if (p<0) then goto f2;
o:=sizeof(a)-16;
if (pipeLineRecv(pip,ab[1],o)<>0) then o:=0;
if (o<1) then begin;
  pipeLineStats(pip,o,i,i);
  if (o=0) then goto f2;
  relequish;
  goto f1;
  end;
ab0:=o;
i:=ab[1];
a:=copy(a,2,666);
while (ab0>0) and (ab[1]=32) do a:=copy(a,2,666);
snd('executing command '+chr(i));
snd(a);
snd('...'#13#10);
case i of
  $3f:begin; {? - help}
    snd('the following commands available:'#13#10);
    snd('? - this help message'#13#10);
    snd('e - every messages (last+current)'#13#10);
    snd('l - last messages'#13#10);
    snd('c - current messages'#13#10);
    snd('b - log file buffer (0=disable, 1=enable'#13#10);
    snd('f - log file name (% means current date)'#13#10);
    snd('n - logging host (blank means disabled)'#13#10);
    snd('p - start process'#13#10);
    snd('s - run script'#13#10);
    ab0:=0;
    end;
  $63:begin; {c - current messages}
    doCurr;
    end;
  $6c:begin; {l - last messages}
    doLast;
    a:='end';
    end;
  $65:begin; {e - every messages}
    doLast;
    doCurr;
    end;
  $62:begin; {b - file buffer}
    fileLock:=(BVal(a)<>0);
    fileDate:=getCurrentDateBin-1;
    xtClose(fileHand);
    if fileLock then a:='file buffer enabled!' else a:='file buffer disabled!';
    goto f3;
    end;
  $66:begin; {f - file logging}
    fileName:=a;
    fileDate:=getCurrentDateBin-1;
    xtClose(fileHand);
    if (fileName='') then a:='logging to file disabled!' else a:='logging to '+fileName+'...';
    goto f3;
    end;
  $6e:begin; {n - network logging}
    pipeLineClose(servPipe);
    servPipe:=0;
    if (a='') then begin;
      a:='network logging disabled!';
      f3:
      append2log(systemPrefix,a);
      goto f2;
      end;
    i:=pos(' ',a);
    if (i<1) then i:=666;
    if string2ipAddr(copy(a,1,i-1),servAddr) then begin; a:='invalid adderss, network logging disabled!';goto f3; end;
    servPort:=BVal(copy(a,i+1,666));
    if (servPort=0) then servPort:=514;
    if TCPfindProcess then begin; a:='tcp process not found!';goto f2; end;
    if UDPlistenOnPort(servPipe,65536,a,i) then begin; a:='failed to listen on port, network logging disabled!';goto f2; end;
    a:='logging to '+ipAddr2string(servAddr)+' '+BStr(servPort)+'...';
    goto f3;
    end;
  $70:begin; {p - start process}
    startOneProcess(a);
    a:='process started...';
    end;
  $73:begin; {s - run script}
    if readUpConfig(a) then a:='failed to run script!' else a:='script ran...';
    end;
  else a:='unknown command, type ? for help!';
  end;
f2:
pipeLineSend(pip,ab[1],ab0);
pipeLineClose(pip);
End;



Label f1;
Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('system logger v1.0, done by Mc at '#%date' '#%time'.');
a:=GetAllParameters;
if (a='') then immErr('using: logger.code <filename>');
ConnectionNum:=0;
servPipe:=0;
fileName:='';
fileLock:=true;
userPipe:=0;
mesgNumb:=0;
lastMsgsCur:=0;
fillchar(lastMsgsSiz,sizeof(lastMsgsSiz),0);
if readUpConfig(a) then immErr('error opening file!');
pipeLineBegListen;

BugOS_SignDaemoning;
f1:
relequish;
for i:=ConnectionNum downto 1 do releq2proc(ConnectionDat^[i],i);
if (userPipe<>0) then begin;
  pipeLineStats(userPipe,o,i,i);
  if (o=0) then begin; pipeLineClose(userPipe);userPipe:=0; end;
  end;
if (pipeLineGetIncoming(o)=0) then begin;
  releq2new(o);
  pipeLineClose(o);
  end;
goto f1;
END.