{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
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
{$include \sources\internet\kernel\utils\timer2.inc}

{$include memory.inc}
{$include config.inc}
{$include dhcp.inc}



Label f1;
Var
  con:OneConnectionRecord;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('dhcp4 server v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');

ConnectionNum:=0;
lastSent:=0;
if pipeLineBegListen then immErr('failed to start listening!');

a:=ParamStr(1);
if (a='') then immErr('using: dhcp.code <config>');
ReadUpConfig(a);

if (portCmd=0) then portCmd:=67;
i:=portCmd;
if UDPlistenOnPort(pipeCmd,65536,addrCmd,i) then immErr('failed to listen on port!');
WriteLn('server listening on '+ipAddr2string(addrCmd)+' '+BStr(i)+' port...');
portCmd:=i;

lastPerid:=-99999;
lastCheck:=0;

BugOS_SignDaemoning;
f1:
relequish;
timer2start;
while ReceivePacket do;
if (GetTimePast(lastPerid)<5) then goto f1;
lastPerid:=currentTime;
while keyPressed do case ReadKey of
  $0466:listAddressCache(0,'free addresses:'); {alt+f}
  $0475:listAddressCache(1,'used addresses:'); {alt+u}
  $0463:listAddressCache(2,'constant addresses:'); {alt+c}
  $0469:begin; {alt+i}
    WriteLn('information:');
    WriteLn('local address: '+ipAddr2string(addrCmd)+' '+BStr(portCmd));
    WriteLn('address pool: '+BStr(ConnectionNum));
    WriteLn('gateway ip: '+ipAddr2string(gateAddr));
    WriteLn('netmask: '+ipAddr2string(maskAddr));
    WriteLn('dnses: '+BStr(length(dnsLists) shr 2));
    WriteLn('domain: '+domainNam);
    WriteLn('boot server: '+bootServ);
    WriteLn('boot file: '+bootFile);
    WriteLn('lease seconds: '+BStr(leaseTime));
    end;
  end;
DoOnePurgingRound;
goto f1;
END.