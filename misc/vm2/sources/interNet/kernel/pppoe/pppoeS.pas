{$undef debug}
{{$define debug}
{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc memory.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Const proggyName='pppoe server v1.0';

{$include memory.inc}
{$include config.inc}
{$include pppoeS.inc}

Procedure gotIncoming(p:LongInt);
Var
  i,o:LongInt;
  a:String;
Begin;
o:=FindConnectByStat(1);
if (o<1) then begin;
  pipeLineClose(p);
  exit;
  end;
ConnectionDat^[o].pipe:=p;
ConnectionDat^[o].stat:=2;
a:=convEtherAddr(ConnectionDat^[o].addr);
a:='12341234'#0#0#0#0#0#0#0#0+chr(ConnectionDat^[o].cnid)+#255'pppoe with '+a+#0;
i:=1;move(i,a[1],sizeof(i));
i:=1490;move(i,a[5],sizeof(i));
pipeLineSend(p,a[1],length(a));
End;


Label f1;
Var
  a:String;
  i,o:LongInt;
  pck:OnePacketRecord;
BEGIN;
WriteLn(proggyName+', done by Mc at '#%date' '#%time'.');

a:=paramStr(1);
if (a='') then immErr('using: pppoe.code <config>');
ReadUpConfig(a);
EthernetOpen;

pipeLineBegListen;
BugOS_SignDaemoning;
timer2start;

f1:
o:=sizeof(pck);
if (pipeLineRecv(etherPipe,pck,o)=0) then begin;
  gotOnePacket(pck,o-14);
  goto f1;
  end;
for i:=1 to ConnectionNum do relequishConn(ConnectionDat^[i]);
while (pipeLineGetIncoming(i)=0) do gotIncoming(i);
relequish;
timer2start;
goto f1;
END.