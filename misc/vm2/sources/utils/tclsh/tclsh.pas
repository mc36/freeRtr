{$stack 128k}
{$heap 768k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc alap.inc}
{$sysinc bin.inc}
{$sysinc hex.inc}
{$sysinc crt.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}

{$include string.inc}
{$include calc.inc}
{$include memory.inc}
{$include exec.inc}
{$include shell.inc}


Var a:String;
BEGIN;
WriteLn('tclsh v1.0, done by Mc at '#%date' '#%time'.');
stringClear(userInput);
currProcOfs:=0;
currMemTopSize:=0;
memResize(0);
a:=paramStr(1);
if (a<>'') then begin;
  addOneUserLine('source '+a);
  halt(0);
  end;
while (1=1) do readOneUserLine;
END.