{$heap 63k}
{$stack 15k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc param.inc}

Var
  a,b:String;
  i,o:LongInt;
BEGIN;
WriteLn('background run v1.0, done by Mc at '#%date' '#%time'.');
b:=GetAllParameters;
i:=pos(' ',b);
if (i<1) then i:=666;
a:=copy(b,1,i-1);
b:=copy(b,i+1,255);
if (a='') then begin;
  writeln('using: backgrundrun.code <filename> [parameters]');
  halt(1);
  end;
WriteLn('executable: "'+a+'"');
WriteLn('parameters: "'+b+'"');
i:=xExecBgnd(a,b,o);
if (i<>0) then begin;
  writeLn('error: '+xGetErrorName(i));
  halt(1);
  end;
WriteLn('started, new pid: '+BStr(o));
END.