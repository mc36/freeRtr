{$heap 31k}
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

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}

{$include memory.inc}
{$include config.inc}
{$include vmps.inc}



Label f1;
Var
  buf:array[1..4096] of byte;
  i,o,p:LongInt;
  a:String;
BEGIN;
WriteLn('vmps server v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');

a:=ParamStr(1);
if (a='') then immErr('using: vmps.code <config>');
ReadUpConfig(a);

if (portCmd=0) then portCmd:=1589;
i:=portCmd;
if UDPlistenOnPort(pipeCmd,65536,addrCmd,i) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(addrCmd)+' '+BStr(i)+' port...');
portCmd:=i;

BugOS_SignDaemoning;
f1:
o:=sizeof(buf);
if UDPreceivePacket(pipeCmd,a,i,buf,o) then o:=0;
if (o<1) then begin;
  relequish;
  goto f1;
  end;
WriteLn(ipAddr2string(a)+' '+BStr(i)+': '+processOnePacket(buf,o));
UDPsendPacket(pipeCmd,a,i,buf,o);
goto f1;
END.