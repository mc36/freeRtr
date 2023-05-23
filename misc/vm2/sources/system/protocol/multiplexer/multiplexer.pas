{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc hex.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc memory.inc}

{$include memory.inc}
{$include config.inc}
{$include multiplexer.inc}

Label f1;
BEGIN;
WriteLn('multiplexer v1.0, done by Mc at '#%date' '#%time'.');

if (paramCount<>2) then immErr('using: multiplexer.code <config> <process>');
readUpConfig(paramStr(1));
openInterface(paramStr(2));

pipeLineBegListen;
BugOS_SignDaemoning;
f1:
relequish;
servLower;
servUpper;
servLogin;
goto f1;
END.