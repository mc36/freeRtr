{$stack 7k}
{$heap 127k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
Const PrgTxt='Shell v1.0, done by Mc at '#%date' '#%time'.';

{$include shell.inc}
Label f1;
Var
  a:string;
  i:Word;
BEGIN;
WriteLn(PrgTxt);

WasError:=false;
overWriter:=false;

a:='';
for i:=1 to ParamCount do a:=a+' '+ParamStr(i);
delete(a,1,1);
if (a<>'') then begin;
  WriteLn(xGetDir+'>'+a);
  a:=ProcessCommand(a);
  if (copy(a,1,1)=#2) then RunOneScript(copy(a,2,255));
  Halt(0);
  end;

f1:
WriteLn('');
a:=ReadLine;
a:=ProcessCommand(a);
if (a='') then goto f1;
case a[1] of
  #1:WriteLn('label not found!');
  #2:RunOneScript(copy(a,2,255));
  end;
goto f1;
END.