{$undef debug}
{{$define debug}
{$heap 127k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}
{$sysinc crypto.inc}
{$sysinc bignum.inc}
{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

{$include memory.inc}
{$include config.inc}
{$include ipsec1.inc}
{$include ipsec2.inc}
{$include ipsec3.inc}
{$include ipsec4.inc}
{$include ipsec5.inc}


Label f1,f2;
Var
  a:String;
  i,o:LongInt;
  d:array[1..1024*4] of byte;
BEGIN;
WriteLn('ipsec server v1.0, done by Mc at '#%date' '#%time'.');
Randomize;
if TCPfindProcess then immErr('failed to find tcp process!');
if pipeLineBegListen then immErr('failed to start listening!');
if CryptoStartActions then immErr('failed to find crypto process!');
if BigNumStartActions then immErr('failed to find bignum process!');
CryptoGetHasherList(d,i);
for o:=1 to HasherAlgoMax do doFindCrytoAlgo(HasherAlgoList[o].a,d,i,HasherAlgoList[o].n);
CryptoGetCipherList(d,i);
for o:=1 to CipherAlgoMax do doFindCrytoAlgo(CipherAlgoList[o].a,d,i,CipherAlgoList[o].n);

a:=ParamStr(1);
if (a='') then immErr('using: ipsec.code <config>');
ResizeMem(0);
ReadUpConfig(a);
if (ConnectionNum<1) then immErr('no peers configured!');

if (isakmpPort1<>-1) then begin;
  if (isakmpPort1=0) then isakmpPort1:=500;
  if UDPlistenOnPort(isakmpPipe1,65536,isakmpAddr1,isakmpPort1) then immErr('failed to listen on udp port!');
  WriteLn('listening on '+ipAddr2string(isakmpAddr1)+' '+BStr(isakmpPort1)+' udp port...');
  end;
if (isakmpPort2<>-1) then begin;
  if (isakmpPort2=0) then isakmpPort2:=4500;
  if UDPlistenOnPort(isakmpPipe2,65536,isakmpAddr2,isakmpPort2) then immErr('failed to listen on udp port!');
  WriteLn('listening on '+ipAddr2string(isakmpAddr2)+' '+BStr(isakmpPort2)+' udp port...');
  end;

BugOS_SignDaemoning;
f1:
relequish;
timer2start;
while (pipeLineGetIncoming(i)=0) do gotNewIncomingConnection(i);
for i:=1 to ConnectionNum do relequish2connection(ConnectionDat^[i]);
i:=sizeof(d);
if not UDPreceivePacket(isakmpPipe1,a,o,d,i) then gotOnePacket(d,i,1,a,o);
f2:
i:=sizeof(d);
if UDPreceivePacket(isakmpPipe2,a,o,d,i) then goto f1;
gotOnePacket(d,i,2,a,o);
goto f2;
END.