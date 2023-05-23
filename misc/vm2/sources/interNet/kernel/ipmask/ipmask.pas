{$undef debug}
{{$define debug}
{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$include \sources\internet\kernel\utils\timer2.inc}
{$include \sources\internet\kernel\utils\checksum.inc}

{$include \sources\internet\kernel\tcp\lower.inc}
{$include \sources\internet\kernel\tcp\pseudo.inc}
{$include memory.inc}
{$include ipmask.inc}


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
  move(xtrnlAddr,a[6],sizeof(xtrnlAddr));
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


Label f1,f2;
Var
  pck:OnePacketRecord;
  lastTest:LongInt;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('ipmask v1.0, done by Mc at '#%date' '#%time'.');
Randomize;

if (paramCount<>4) then immErr('using: ipmask.code <ip-process> <locip> <locmask> <extip>');
a:=paramStr(1);
p:=BVal(a);
if (p=0) then p:=BugOS_findProcNam(a);
if (p=0) then immErr('failed to find ip process!');
LowerProcess:=p;
if string2ipAddr(paramStr(2),LocalAddr) then immErr('error in local address!');
if string2ipAddr(paramStr(3),LocalMask) then immErr('error in local netmask!');
if string2ipAddr(paramStr(4),XtrnlAddr) then immErr('error in external address!');
fillchar(WholeMask,sizeof(WholeMask),255);

a:=doOneIPrequest('param---',true);
if (copy(a,1,5)<>'param') then immErr('invalid parameter reply received!');
a:=doOneIPrequest('data----',false);
if (a<>'data') then immErr('failed to open data connection!');

if (pipeLineBegListen<>0) then immErr('failed to start listening!');
lastTest:=-1;
lastSent:=0;
ConnectionNum:=0;
PrtclPipe:=0;
PrtclProc:=0;
BugOS_SignDaemoning;
f1:
relequish;
timer2start;
f2:
if RecvOneLowerPacket(pck) then begin;
  if (pck.p=TCPprotocolNum) then gotOnePacket(pck,1) else
   if (pck.p=UDPprotocolNum) then gotOnePacket(pck,2) else gotOnePacket(pck,0);
  goto f2;
  end;
ProcessUpperStream(pck);
while (pipeLineGetIncoming(o)=0) do ProcessNewPipe(o);
if (getTimePast(lastTest)<5) then goto f1;
if (PrtclPipe<>0) then begin;
  if (pipeLineStats(PrtclPipe,lastTest,o,p)<>0) then lastTest:=0;
  if (lastTest=0) then begin;
    pipeLineClose(PrtclPipe);
    PrtclPipe:=0;
    end;
  end;
while keypressed do case readkey of
  $0478:exit; {alt+x}
  $0469:displayInformation; {alt+i}
  $0463:displayConnectinos; {alt+c}
  end;
for i:=ConnectionNum downto 1 do begin;
  if (getTimePast(ConnectionDat^[i].time)<60*5) then continue;
  {$ifdef debug}WriteLn('timeout on connection '+ipAddr2string(ConnectionDat^[i].locA)+' --> '+ipAddr2string(ConnectionDat^[i].extA));{$endif}
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(OneConnectionRecord));
  ResizeMem(ConnectionNum-1);
  end;
lastTest:=CurrentTime;
goto f1;
END.