{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}

Const bufSiz=1024;
Var
  b1,b2:array[1..bufSiz] of byte;
  b3:array[1..2*bufSiz] of byte;
  p1,p2,s1,s2:LongInt;
  f1,f2,f3:xFile;
  i,o:LongInt;
  a:String;
BEGIN;
WriteLn('rom merger v1.0, done by Mc at '#%date' '#%time'.');
if (xOpen(f1,paramStr(1),xGenFilMod_r)<>0) then exit;
if (xOpen(f2,paramStr(2),xGenFilMod_r)<>0) then exit;
a:=paramStr(3);
if (xCreate(a)<>0) then exit;
if (xOpen(f3,a,xGenFilMod_rw)<>0) then exit;
s1:=xFileSize(f1);
s2:=xFileSize(f2);
p1:=0;
p2:=0;
while (p1<s1) and (p2<s2) do begin;
  fillchar(b1,sizeof(b1),0);
  fillchar(b2,sizeof(b2),0);
  i:=s1-p1;
  if (i>sizeof(b1)) then i:=sizeof(b1);
  xBlockRead(f1,b1,i);
  inc(p1,i);
  i:=s2-p2;
  if (i>sizeof(b2)) then i:=sizeof(b2);
  xBlockRead(f2,b2,i);
  inc(p2,i);
  o:=0;
  for i:=1 to bufSiz do begin;
    b3[o+1]:=b1[i];
    b3[o+2]:=b2[i];
    inc(o,2);
    end;
  xBlockWrite(f3,b3,o);
  end;
xClose(f3);
xClose(f2);
xClose(f1);
WriteLn('successful!');
END.