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
  cmd:String;
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('modem connect v1.0, done by Mc at '#%date' '#%time'.');
a:=paramStr(1);
serialProc:=BVal(a);
if (serialProc=0) then serialProc:=BugOS_findProcNam(a);
serialPort:=BVal(paramStr(2));
cmd:='';
for i:=3 to paramCount do cmd:=cmd+' '+paramStr(i);
cmd:=copy(cmd,2,255);
if (cmd='') then begin;
  WriteLn('using: modemCommand.code <process> <port> <phonenumber>');
  WriteLn('phonenumber cold be:');
  WriteLn('  real phone number');
  WriteLn('  \ to leave phone number empty');
  WriteLn('  / to answer against dial');
  halt(1);
  end;
SerialOpen;
if (cmd='\') then cmd:='';
if (cmd='/') then cmd:='ata' else cmd:='atd'+cmd;

ModemShow:=True;
ModemFlushReceiver;
ModemSendCommand(cmd);
if ModemWaitConnected(a,60) then immErr('failed to connect!');
WriteLn('connected, string: "'+a+'"...');
END.