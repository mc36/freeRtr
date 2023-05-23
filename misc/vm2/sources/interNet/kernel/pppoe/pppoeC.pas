{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

{$include pppoeC.inc}

Label f1;
Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('pppoe client v1.0, done by Mc at '#%date' '#%time'.');

a:=GetAllParameters;
if (a='') then immErr('using: pppoe.code <interface> [service]');
i:=pos(' ',a);
if (i<1) then i:=666;
serviceName:=copy(a,i+1,255);
a:=copy(a,1,i-1);
etherPipe:=BVal(a);
if (etherPipe=0) then etherPipe:=BugOS_findProcNam(a);
if (etherPipe=0) then immErr('interface not found!');
EthernetOpen;
tryConnecting;
WaitForUpperLayer;
doConnection;
END.