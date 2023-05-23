{$stack 63k}
{$heap 96m}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc alap.inc}
{$sysinc filesys.inc}

{$define name}

{$include bin.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Var
  sx,sy:LongInt;
  f:xFile;
  a,b:String;
  i,o,p:LongInt;
BEGIN;
writeLn('deadend v1.0, done by Mc at '#%date' '#%time'.');
a:=paramStr(1);
if (a='') then immErr('using: prog.code <bin>');
a:=paramStr(1);
writeln('reading '+a+'...');

xOpen(f,a,xGenFilMod_r);
readUpTables(f);

writeLn('nodes='+BStr(headR.nodes)+' conns='+BStr(headR.conns));

for i:=1 to headR.nodes do begin;
  if (nodeD[i].e-nodeD[i].b+1>1) then continue;
  writeLn('node#'+BStr(i)+' - '+readOneName(f,nodeD[i].nam));
  end;

END.