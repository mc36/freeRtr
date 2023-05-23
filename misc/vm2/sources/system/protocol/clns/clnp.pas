{$heap 127k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc crt.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc random.inc}
{$sysinc inet_tcp.inc}
{$sysinc memory.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

{$include clnpMem.inc}
{$include clnpCore.inc}

Procedure ProcessNewPipe(pip:LongInt);
Var
  d:OneConnectionRecord;
  i,o:LongInt;
Begin;
if ResizeMem(ConnectionNum+1) then begin;
  pipeLineClose(pip);
  exit;
  end;
fillchar(d,sizeof(d),0);
d.pipe:=pip;
d.stat:=1;
d.time:=currentTime;
pipeLineStats(pip,d.proc,i,i);
ConnectionDat^[ConnectionNum]:=d;
End;

Label f1;
Var
  con:OneConnectionRecord;
  a:String;
  i,o,p:LongInt;
BEGIN;
WriteLn('clnp v1.0, done by Mc at '#%date' '#%time'.');

Randomize;
timer2start;

a:=paramStr(2);
if (a='') then begin;
  WriteLn('using: clnp.code <process> <net>');
  Halt(1);
  end;
kicserel('.','',a);
networkAddrSiz:=0;
while (a<>'') do begin;
  inc(networkAddrSiz);
  networkAddrDat[networkAddrSiz]:=BVal('$'+copy(a,1,2));
  a:=copy(a,3,666);
  end;
networkAddrDat[networkAddrSiz]:=0;
a:=paramStr(1);
WriteLn('process: '+a);
p:=BVal(a);
if (p=0) then p:=BugOS_findProcNam(a);
if (p=0) then begin;
  WriteLn('failed to find process!');
  Halt(1);
  end;
LowerProcess:=p;
if (pipeLineCreate(LowerPipeline,LowerProcess,65536,true)<>0) then begin;
  WriteLn('error creating connection!');
  halt(1);
  end;
for i:=1 to 16 do relequish;
i:=255;
if (pipeLineRecv(LowerPipeline,a[1],i)<>0) then begin;
  WriteLn('failed to receive initial data!');
  halt(1);
  end;
a[0]:=chr(i);
move(a[1],addressSize,sizeof(addressSize));
move(a[5],packetSize,sizeof(packetSize));
move(a[9],i,sizeof(i));
move(a[13],i,sizeof(i));
if (addressSize>sizeof(localAddress)) then begin;
  WriteLn('link has too big address size!');
  halt(1);
  end;
o:=17;
fillchar(localAddress,sizeof(localAddress),0);
fillchar(bcastAddress,sizeof(bcastAddress),0);
move(a[o],localAddress,addressSize);inc(o,addressSize);
move(a[o],bcastAddress,addressSize);inc(o,addressSize);
a:=copy(a,o,255);
WriteLn('name: '+copy(a,1,pos(#0,a)-1));
WriteLn('broadcast: '+hwaddr2str(bcastAddress));
WriteLn('unicast: '+hwaddr2str(localAddress));
WriteLn('net: '+netaddr2str(networkAddrDat,networkAddrSiz));

if (pipeLineBegListen<>0) then begin;
  WriteLn('failed to start listening!');
  Halt(2);
  end;
BugOS_SignDaemoning;
ConnectionNum:=0;
nextPacketNumber:=0;
lastHelloTime:=-99999;
uplinkAddr:=bcastAddress;
f1:
timer2start;
relequish2lower;
for i:=ConnectionNum downto 1 do if relequish2connection(ConnectionDat^[i]) then begin;
  con:=ConnectionDat^[i];
  pipeLineClose(con.pipe);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  end;
while (pipeLineGetIncoming(o)=0) do ProcessNewPipe(o);
relequish;
goto f1;
END.