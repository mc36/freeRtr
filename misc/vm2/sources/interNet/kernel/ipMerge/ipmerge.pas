{$undef debug}
{{$define debug}
{$heap 15k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc alap.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$include \sources\internet\kernel\utils\checksum.inc}

{$include ipmerge.inc}


Procedure ProcessNewPipe(pip:LongInt);
Label f1;

function cip(buf:OneAddressRecord):String; var a:String;begin;a[0]:=#16;move(buf,a[1],16);cip:=a; end;

Var
  buf:array[1..1024] of byte;
  a,b:string;
  i,o,p:LongInt;
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
  if (defStck=1) then a:=cip(ip4addr) else a:=cip(ip6addr);
  a:='param'+a;
  goto f1;
  end;
if (a='data----') and (uppPipe=0) then begin;
  a:='data';
  pipeLineSend(pip,a[1],length(a));
  uppPipe:=pip;
  pipeLineStats(uppPipe,uppProc,i,o);
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
  a:String;
  lastTest:LongInt;
  i,o,p:LongInt;
BEGIN;
WriteLn('ipmerge v1.0, done by Mc at '#%date' '#%time'.');

WriteLn('accessing ip4 stack...');
ip4proc:=BugOS_findProcNam('ip4.code');
if (ip4proc=0) then immErr('failed to find process!');
ip6proc:=ip4proc;
a:=doOneIPrequest('param---',true);
if (copy(a,1,5)<>'param') then immErr('invalid parameter reply received!');
move(a[6],ip4addr,sizeof(ip4addr));
a:=doOneIPrequest('data----',false);
if (a<>'data') then immErr('failed to open data connection!');
ip4pipe:=ip6pipe;

WriteLn('accessing ip6 stack...');
ip6proc:=BugOS_findProcNam('ip6.code');
if (ip6proc=0) then immErr('failed to find process!');
a:=doOneIPrequest('param---',true);
if (copy(a,1,5)<>'param') then immErr('invalid parameter reply received!');
move(a[6],ip6addr,sizeof(ip6addr));
a:=doOneIPrequest('data----',false);
if (a<>'data') then immErr('failed to open data connection!');

o:=CalculateSum(ip4addr,sizeof(ip4addr));
p:=CalculateSum(ip6addr,sizeof(ip6addr));
if (ip4proc<ip6proc) then begin;
  a:='ip4';
  defStck:=1;
  chkAdjO.src:=o;
  chkAdjO.trg:=not p;
  addrPri:=ip4addr;
  addrSec:=ip6addr;
  end else begin;
  a:='ip6';
  defStck:=2;
  chkAdjO.src:=p;
  chkAdjO.trg:=not o;
  addrPri:=ip6addr;
  addrSec:=ip4addr;
  end;
chkAdjI.trg:=not chkAdjO.src;
chkAdjI.src:=not chkAdjO.trg;
WriteLn('the '+a+' address will be advertised!');
WriteLn('ip4 address: '+ipAddr2string(ip4addr));
WriteLn('ip6 address: '+ipAddr2string(ip6addr));

if (pipeLineBegListen<>0) then immErr('failed to start listening!');
lastSent:=0;
uppProc:=0;
uppPipe:=0;
BugOS_SignDaemoning;
f1:
while (pipeLineGetIncoming(o)=0) do ProcessNewPipe(o);
relequish;
while (1=1) do begin;
  p:=sizeof(pck);
  if (pipeLineRecv(ip4pipe,pck,p)<>0) then break;
  if (p<1) then break;
  gotLowerPacker(pck,p,1);
  end;
while (1=1) do begin;
  p:=sizeof(pck);
  if (pipeLineRecv(ip6pipe,pck,p)<>0) then break;
  if (p<1) then break;
  gotLowerPacker(pck,p,2);
  end;
if (uppPipe=0) then goto f1;
o:=0;
while (1=1) do begin;
  p:=sizeof(pck);
  if (pipeLineRecv(uppPipe,pck,p)<>0) then break;
  if (p<1) then break;
  gotUpperPacker(pck,p);
  end;
if (o>0) then goto f1;
pipeLineStats(uppPipe,p,i,o);
if (p=uppProc) then goto f1;
pipeLineClose(uppPipe);
uppProc:=0;
uppPipe:=0;
goto f1;
END.