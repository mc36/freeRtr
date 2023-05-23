{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

{$include \sources\internet\kernel\utils\timer2.inc}
{$include serial.inc}
{$include modem.inc}


Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('modem hangUp v1.0, done by Mc at '#%date' '#%time'.');
a:=paramStr(1);
serialProc:=BVal(a);
if (serialProc=0) then serialProc:=BugOS_findProcNam(a);
serialPort:=BVal(paramStr(2));
if (serialPort<1) then immErr('using: modemHangup.code <process> <port>');
SerialOpen;

ModemShow:=True;
if ModemDisconnectNow('ath',5) then immErr('failed to hang up!');
WriteLn('good!');
END.