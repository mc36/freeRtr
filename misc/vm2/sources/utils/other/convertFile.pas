{$heap 63k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc memory.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}

Var
  currentPalD:array[1..256*3] of byte;
  currentResX:LongInt;
  currentResY:LongInt;
  currentData:^array[1..1] of byte;

{$include convertFile.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Procedure getExt(var a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  x,y:LongInt;
Begin;
x:=0;
for y:=1 to ab0 do if (ab[y]=$2e) then x:=y;
a:=copy(a,x+1,666);
End;


Var
  a:String;
  i,o:LongInt;
  f:xFile;
BEGIN;
WriteLn('graphic converter v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<>2) then immErr('using: conv.code <src> <trg>');
a:=paramStr(1);
WriteLn('reading '+a+'...');
if (xOpen(f,a,xGenFilMod_r)<>0) then immErr('error opening file!');
getExt(a);
a:=doConvertOneFile(f,a,1,0,xFileSize(f));
if (a<>'') then immErr(a);
a:=paramStr(2);
WriteLn('Writing '+a+'...');
if (xCreate(a)<>0) then immErr('error creating file!');
if (xOpen(f,a,xGenFilMod_rw)<>0) then immErr('error opening file!');
getExt(a);
a:=doConvertOneFile(f,a,2,0,0);
if (a<>'') then immErr(a);
WriteLn('successful!');
END.