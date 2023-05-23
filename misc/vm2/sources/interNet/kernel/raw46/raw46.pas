{$heap 15k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$sysinc inet_tcp.inc}
{$sysinc inet_addr.inc}

{$include device.inc}
{$include param.inc}
{$include upper.inc}
{$include lower.inc}



Label f1;
BEGIN;
WriteLn('raw media access v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<1) then immErr('using: raw46.code <process> [notype] <my> <rem> <mask> [<my> <rem> <mask>]');

parseParams;
up4pipe:=0;
up6pipe:=0;
openDevice(ParamStr(1));
WriteLn('  local ipv4: '+ipAddr2string(loc4ip));
WriteLn(' remote ipv4: '+ipAddr2string(rem4ip));
WriteLn('ipv4 netmask: '+ipAddr2string(msk4ip));
WriteLn('  local ipv6: '+ipAddr2string(loc6ip));
WriteLn(' remote ipv6: '+ipAddr2string(rem6ip));
WriteLn('ipv6 netmask: '+ipAddr2string(msk6ip));
WriteLn('size of type: '+BStr(typeSiz-1));
pipeLineBegListen;
BugOS_SignDaemoning;

f1:
relequish;
while doLower do;
while doUpper do;
goto f1;
END.