{$heap 127k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
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

var key:OneDSSkeyRecord;

Procedure GenerateX;
Begin;
GetRndBigNum(key.x,160);
End;

Procedure CalculateY;
Begin;
BigNumPowerMod(key.g,key.x,key.p,key.y);
End;

Procedure CalculateG;
Label f1;
Var
  n1,n2,n3,n4:BigNumContextRecord;
  h:LongInt;
Begin;
f1:
h:=Random($7fffffff);
BigNumStoreInt(h,n1);
BigNumAbs(n1,n1);
BigNumDiv(key.p,key.q,n2,n3);
BigNumPowerMod(n1,n2,key.p,n4);
BigNumStoreInt(1,n1);
if (BigNumComp(n1,n4)<>2) then goto f1;
key.g:=n4;
End;

Procedure GenerateQ;
Var
  n1,n2,n3,n4:BigNumContextRecord;
Begin;
GetRndBigNum(n1,160);
BigNumStoreInt(1,n2);
BigNumOr(n1,n2,n3);
BigNumShl(159,n2,n4);
BigNumOr(n3,n4,n1);
repeat
  BigNumStoreInt(2,n2);
  repeat
    PutNextPoint;
    BigNumAdd(n1,n2,n1);
    until PrimalityTest2(n1);
  PutNextPoint;
  until PrimalityTest3(n1,100);
key.q:=n1;
End;

Procedure GenerateP(i:LongInt);
Var
  n1,n2,n3,n4,n5,n6,n7:BigNumContextRecord;
Begin;
BigNumAdd(key.q,key.q,n7);
dec(i,BigNumSizeInBits(n7));
if (i<16) then i:=16;
GetRndBigNum(n6,i);
BigNumStoreInt(1,n2);
BigNumOr(n6,n2,n3);
BigNumShl(i-1,n2,n4);
BigNumOr(n3,n4,n6);
BigNumMul(n7,n6,n5);
BigNumStoreInt(1,n2);
BigNumAdd(n5,n2,n1);
repeat
  repeat
    PutNextPoint;
    BigNumAdd(n1,n7,n1);
    until PrimalityTest2(n1);
  PutNextPoint;
  until PrimalityTest3(n1,100);
key.p:=n1;
End;




Var
  i:LongInt;
  a:String;
  f:xFile;
BEGIN;
WriteLn('dss key generator v1.0, done by Mc at '#%date' '#%time'.');
Randomize;
if BigNumStartActions then immErr('failed to find bignum process!');
fillchar(key,sizeof(key),0);

if (ParamStr(1)='') then immErr('using: mkDSSkey.exe <keyfile> [bits]');
a:=ParamStr(2);
i:=BVal(a);
if (i<160) or (i>8192) then i:=512;

WriteLn('going to generate '+BStr(i)+' bit DSS key... please wait!');

Write('generating x... ');
GenerateX;
showNumberStats(key.x);

Write('generating q... ');
GenerateQ;
showNumberStats(key.q);

Write('generating p... ');
GenerateP(i);
showNumberStats(key.p);

Write('calculating g... ');
CalculateG;
showNumberStats(key.g);

Write('calculating y... ');
CalculateY;
showNumberStats(key.y);

WriteLn('writing key...');
BigNumClearPadding(key.p);
BigNumClearPadding(key.q);
BigNumClearPadding(key.g);
BigNumClearPadding(key.x);
BigNumClearPadding(key.y);
a:=ParamStr(1);
xErase(a);
xCreate(a);
if (xOpen(f,a,xGenFilMod_rw)<>0) then immErr('error opening target file!');
xSeek(f,0);
xBlockWrite(f,key,sizeof(key));
xTruncate(f);
xClose(f);

WriteLn('successfully finished!');
END.