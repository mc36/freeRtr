{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}

{$include \sources\internet\kernel\utils\timer2.inc}

{{$define debug1}
{{$define debug2}

{$include memory.inc}
{$include config.inc}
{$include isdn1.inc}
{$include isdn2.inc}
{$include isdn3.inc}


Label f1;
Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('isdn v1.0, done by Mc at '#%date' '#%time'.');

a:=paramStr(2);
if (a='') then immErr('using: isdn.code <process> <config>');
ReadUpConfig(a);
layer1openDevice(paramStr(1));
timer2start;
currWaitProc:=0;
layer1tryDone:=0;
layer1sendSABME(true);

pipeLineBegListen;
BugOS_SignDaemoning;

f1:
relequish;
timer2start;
layer1releq;
releq2new;
for i:=1 to ConnectionNum do releq2conn(ConnectionDat^[i]);
goto f1;
END.