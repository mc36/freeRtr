{$undef debug}
{{$define debug}
{$heap 63k}
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
{$sysinc crypto.inc}
{$sysinc datetime.inc}
{$sysinc random.inc}
{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}

{$include \sources\system\login\login.inc}
{$include \sources\system\login\authenticator.inc}
{$include memory.inc}
{$include radius1.inc}
{$include radius2.inc}
{$include config.inc}


Label f1;
Var
  a:String;
  i,o:LongInt;
  d:OneRadiusPacketRecord;
BEGIN;
WriteLn('radius server v1.0, done by Mc at '#%date' '#%time'.');
Randomize;
if TCPfindProcess then immErr('failed to find tcp process!');
if pipeLineBegListen then immErr('failed to start listening!');
if CryptoStartActions then immErr('failed to find crypto process!');
CryptoGetHasherList(d,i);
md5algoNum:=CryptoFindOneAlgo(d,i,'md5');
if (md5algoNum<1) then immErr('failed to find md5 algorithm!');


a:=ParamStr(1);
if (a='') then immErr('using: radius.code <config>');
authentingPipe:=0;
accountingPipe:=0;
ReadUpConfig(a);
logFileDate:=0;

a:=chr(Random($100));
for i:=0 to 255 do RandomDat[i]:=a;
RandomNum:=0;

if (authentingPort<>-1) then begin;
  if (authentingPort=0) then authentingPort:=1812;
  if UDPlistenOnPort(authentingPipe,65536,authentingAddr,authentingPort) then immErr('failed to listen on udp port!');
  WriteLn('authentication is listening on '+ipAddr2string(authentingAddr)+' '+BStr(authentingPort)+' udp port...');
  end;
if (accountingPort<>-1) then begin;
  if (accountingPort=0) then accountingPort:=1813;
  if UDPlistenOnPort(accountingPipe,65536,accountingAddr,accountingPort) then immErr('failed to listen on udp port!');
  WriteLn('accounting is listening on '+ipAddr2string(accountingAddr)+' '+BStr(accountingPort)+' udp port...');
  end;

BugOS_SignDaemoning;
f1:
relequish;
i:=sizeof(d);
if not UDPreceivePacket(authentingPipe,d.adr,d.prt,d,i) then begin;
  d.siz:=i;
  if gotOnePacket(d) then UDPsendPacket(authentingPipe,d.adr,d.prt,d,d.siz);
  end;
i:=sizeof(d);
if not UDPreceivePacket(accountingPipe,d.adr,d.prt,d,i) then begin;
  d.siz:=i;
  if gotOnePacket(d) then UDPsendPacket(accountingPipe,d.adr,d.prt,d,d.siz);
  end;
goto f1;
END.