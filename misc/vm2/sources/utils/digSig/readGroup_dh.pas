{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc random.inc}
{$sysinc bignum.inc}
{$include \sources\internet\kernel\utils\keys.inc}

{$include random.inc}
{$include makekey.inc}

Procedure immErr(a:string);
Begin;
writeLn(a);
halt(1);
End;

var key:OneDiffieHellmanGroup;

Procedure ReadUpNumber(var t:xtText;var num:BigNumContextRecord);
Label f1;
Var
  n1,n2,n3:BigNumContextRecord;
  a:String;
Begin;
BigNumStoreInt(0,num);
BigNumStoreInt(10,n1);
f1:
a:=xtReadLn(t,255);
if (a='') then exit;
while (a<>'') do begin;
  BigNumStoreInt(ord(a[1])-$30,n3);
  a:=copy(a,2,255);
  BigNumMul(n1,num,n2);
  BigNumAdd(n2,n3,num);
  end;
goto f1;
End;


Var
  a:String;
  i,o:LongInt;
  t:xtText;
  f:xFile;
BEGIN;
WriteLn('diffie-hellman group adder v1.0, done by Mc at '#%date' '#%time'.');
Randomize;
if BigNumStartActions then immErr('failed to find bignum process!');
fillchar(key,sizeof(key),0);

a:=ParamStr(2);
if (a='') then immErr('using: addGroup.exe <groupCollection> <groupToAdd>');

if (xtOpen(t,a,true)<>0) then immErr('error opening source file!');

Write('reading prime...');
ReadUpNumber(t,key.p);
showNumberStats(key.p);

Write('reading generator...');
ReadUpNumber(t,key.g);
showNumberStats(key.g);

xtClose(t);

WriteLn('writing group...');
BigNumClearPadding(key.p);
BigNumClearPadding(key.g);
a:=ParamStr(1);
xCreate(a);
if (xOpen(f,a,xGenFilMod_rw)<>0) then immErr('error opening target file!');
i:=xFileSize(f);
if (i mod sizeof(key)<>0) then begin;
  WriteLn('invalid file size, truncating...');
  i:=0;
  end;
xSeek(f,i);
xBlockWrite(f,key,sizeof(key));
xTruncate(f);
xClose(f);

WriteLn('successfully finished!');
END.