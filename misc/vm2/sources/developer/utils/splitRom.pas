{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}

Const bufSiz=1024;
Var
  b1:array[1..2*bufSiz] of byte;
  b2,b3:array[1..bufSiz] of byte;
  p,s:LongInt;
  f1,f2,f3:xFile;
  i,o,q:LongInt;
  a:String;
BEGIN;
WriteLn('rom splitter v1.0, done by Mc at '#%date' '#%time'.');
if (xOpen(f1,paramStr(1),xGenFilMod_r)<>0) then exit;
a:=paramStr(2);
if (xCreate(a)<>0) then exit;
if (xOpen(f2,a,xGenFilMod_rw)<>0) then exit;
a:=paramStr(3);
if (xCreate(a)<>0) then exit;
if (xOpen(f3,a,xGenFilMod_rw)<>0) then exit;
s:=xFileSize(f1);
p:=0;
while (p<s) do begin;
  q:=s-p;
  if (q>sizeof(b1)) then q:=sizeof(b1);
  xBlockRead(f1,b1,q);
  o:=0;i:=0;
  while (i<q) do begin;
    inc(o);
    b2[o]:=b1[i+1];
    b3[o]:=b1[i+2];
    inc(i,2);
    end;
  xBlockWrite(f2,b2,o);
  xBlockWrite(f3,b3,o);
  inc(p,q);
  end;
xClose(f3);
xClose(f2);
xClose(f1);
WriteLn('successful!');
END.