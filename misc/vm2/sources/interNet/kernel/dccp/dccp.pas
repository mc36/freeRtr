{$undef debug}
{{$define debug}
{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}
{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
Type OneInternetAddress=OneTCPaddressRecord;
{$include \sources\internet\kernel\utils\checksum.inc}
{$include \sources\internet\kernel\utils\timer2.inc}
{$include \sources\internet\kernel\ip6\pseudo.inc}

{$include memory.inc}
{$include dccp1.inc}
{$include dccp2.inc}

Procedure gotNewIncomingConnection(pipe:LongInt);
Label f1;
Var
  con:OneConnectionRecord;
  i:LongInt;
Begin;
if ResizeMem(ConnectionNum+1) then begin;
  f1:
  pipeLineClose(pipe);
  exit;
  end;
fillchar(con,sizeof(con),0);
con.pipe:=pipe;
pipeLineStats(pipe,con.proc,i,i);
con.stat:=1;
ConnectionDat^[ConnectionNum]:=con;
End;

Label f1,f2;
Var
  a:String;
  i,o:LongInt;
  d:array[1..1024*4] of byte;
BEGIN;
WriteLn('dccp v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');
if pipeLineBegListen then immErr('failed to start listening!');
Randomize;

ipProto:=BVal(ParamStr(1));
if (ipProto=0) then ipProto:=$21;
if ProtoListenOnPort(localPipe,65536,localAddr,ipProto) then exit;
WriteLn('listening on '+ipAddr2string(localAddr)+' '+BStr(ipProto)+'...');
ConnectionNum:=0;

BugOS_SignDaemoning;
f1:
relequish;
timer2start;
while (pipeLineGetIncoming(i)=0) do gotNewIncomingConnection(i);
for i:=ConnectionNum downto 1 do if relequish2connection(ConnectionDat^[i]) then begin;
  pipeLineClose(ConnectionDat^[i].pipe);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(ConnectionDat^[i]));
  ResizeMem(ConnectionNum-1);
  end;
f2:
i:=sizeof(d);
if UDPreceivePacket(localPipe,a,o,d,i) then goto f1;
gotOnePacket(i,a,d);
goto f2;
END.