{$undef debug}
{{$define debug}
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

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$include \sources\internet\kernel\utils\checksum.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

{$include lower.inc}
{$include memory.inc}
{$include pseudo.inc}
{$include udp.inc}
{$include tcp.inc}
{$include relequ.inc}

Procedure ProcessNewPipe(pip:LongInt);
Var
  d:OneConnectionStateRecord;
  i,o:LongInt;
Begin;
if ResizeConnectionMemory(ConnectionNum+1) then begin;
  pipeLineClose(pip);
  exit;
  end;
fillchar(d,sizeof(d),0);
d.pipeline:=pip;
d.typ:=0;
pipeLineStats(pip,d.process,i,i);
d.stat:=128;
{$ifdef debug}WriteLn('incoming pipeline ('+BStr(pip)+') from '+BStr(o)+'.');{$endif}
ConnectWriteH(ConnectionNum,d);
End;

Label f1,f2;
Var
  pck:OnePacketRecord;
  a:String;
  lastTest:LongInt;
  i,o,p:LongInt;
BEGIN;
WriteLn('tcp/udp v1.0, done by Mc at '#%date' '#%time'.');

Randomize;
TransmitBufSize:=BVal(ParamStr(2));
if (TransmitBufSize<TransmitBufMin) then TransmitBufSize:=TransmitBufMin;
if (TransmitBufSize>TransmitBufMax) then TransmitBufSize:=TransmitBufMax;
fillchar(ProtocolPipes,sizeof(ProtocolPipes),0);
ConnectionNum:=0;
OneConnectionStateHead:=sizeof(OneConnectionStateRecord)-TransmitBufMax;
OneConnectionStateSize:=OneConnectionStateHead+TransmitBufSize;
PushTresholdBytes:=TransmitBufSize div 2;
timer2start;
retry_TCP_SYN:=8+1;
timot_TCP_SYN:=8;
retry_TCP_DAT:=32+1;
timot_TCP_DAT:=24;
retry_TCP_FIN:=4+1;
timot_TCP_FIN:=8;

a:=paramStr(1);
if (a='') then begin;
  WriteLn('using: tcp.code <ip-process> [tx-buffer]');
  Halt(1);
  end;
p:=BVal(a);
if (p=0) then p:=BugOS_findProcNam(a);
if (p=0) then begin;
  WriteLn('failed to find ip process!');
  Halt(1);
  end;
LowerProcess:=p;
a:=doOneIPrequest('param---',true);
if (copy(a,1,5)<>'param') then begin;
  WriteLn('invalid parameter reply received!');
  Halt(1);
  end;
move(a[6],LocalAddress,sizeof(LocalAddress));
a:=doOneIPrequest('data----',false);
if (a<>'data') then begin;
  WriteLn('failed to open data connection!');
  Halt(1);
  end;



if (pipeLineBegListen<>0) then begin;
  WriteLn('failed to start listening!');
  Halt(2);
  end;
lastTest:=-1;
lastSent:=0;
BugOS_SignDaemoning;
f1:
timer2start;
f2:
if RecvOneLowerPacket(pck) then begin;
  if (pck.p=TCPprotocolNum) then TCPgotOnePacket(pck) else
   if (pck.p=UDPprotocolNum) then UDPgotOnePacket(pck) else protoGotOnePacket(pck);
  goto f2;
  end;
for i:=ConnectionNum downto 1 do relequish2connection(i);
for i:=0 to 255 do relequish2proto(ProtocolPipes[i],i);
while (pipeLineGetIncoming(o)=0) do ProcessNewPipe(o);
relequish;
if (getTimePast(lastTest)<5) then goto f1;
while keypressed do case readkey of
  $0478:exit; {alt+x}
  $0469:displayInformation; {alt+i}
  $0463:displayConnectinos; {alt+c}
  end;
lastTest:=CurrentTime;
goto f1;
END.