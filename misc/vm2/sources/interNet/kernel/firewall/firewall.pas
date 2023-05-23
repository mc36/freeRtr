{$undef debug}
{{$define debug}
{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$include \sources\internet\kernel\utils\checksum.inc}

{$include \sources\internet\kernel\tcp\lower.inc}
{$include \sources\internet\kernel\tcp\pseudo.inc}

{$include memory.inc}
{$include config.inc}
{$include firewall.inc}




Procedure ProcessNewPipe(pip:LongInt);
Label f1;
Var
  buf:array[1..1024] of byte;
  a,b:string;
  i,o,p,q,r,s:LongInt;
Begin;
pipeLineStats(pip,p,i,o);
BugOS_ProcessName(p,buf,i,i,o);
if (o and $40=0) then goto f1;
i:=128;
if (pipeLineRecv(pip,b[1],i)<>0) then i:=0;
b[0]:=chr(i);
a:=kicsi(copy(b,1,8));
b:=copy(b,9,255);
if (a='param---') then begin;
  a:='param1234567890123456';
  move(localAddr,a[6],sizeof(localAddr));
  goto f1;
  end;
if (a='data----') and (PrtclPipe=0) then begin;
  a:='data';
  pipeLineSend(pip,a[1],length(a));
  PrtclPipe:=pip;
  pipeLineStats(PrtclPipe,PrtclProc,i,o);
  exit;
  end;

a:='error';
f1:
pipeLineSend(pip,a[1],length(a));
pipeLineClose(pip);
End;


Label f1;
Var
  pck:OnePacketRecord;
  lastTest:LongInt;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('firewall v1.0, done by Mc at '#%date' '#%time'.');

if (paramCount<>2) then immErr('using: firewall.code <ip-process> <config>');
a:=paramStr(1);
p:=BVal(a);
if (p=0) then p:=BugOS_findProcNam(a);
if (p=0) then immErr('failed to find ip process!');
LowerProcess:=p;
readUpConfiguration(paramStr(2));

a:=doOneIPrequest('param---',true);
if (copy(a,1,5)<>'param') then immErr('invalid parameter reply received!');
move(a[6],localAddr,sizeof(localAddr));
a:=doOneIPrequest('data----',false);
if (a<>'data') then immErr('failed to open data connection!');

WriteLn('local address: '+ipAddr2string(localAddr));
if (pipeLineBegListen<>0) then immErr('failed to start listening!');
PrtclPipe:=0;
PrtclProc:=0;
lastSent:=0;
BugOS_SignDaemoning;
f1:
if RecvOneLowerPacket(pck) then begin;
  if (pck.p=TCPprotocolNum) then gotOnePacket(pck,1) else
   if (pck.p=UDPprotocolNum) then gotOnePacket(pck,2) else
   WriteLn('got invalid protocol from '+ipAddr2string(pck.a));
  goto f1;
  end;
ProcessUpperStream(pck);
while (pipeLineGetIncoming(o)=0) do ProcessNewPipe(o);
relequish;
goto f1;
END.