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

var key:OneRSAkeyRecord;


Procedure EnsuringPQ;
Var n:BigNumContextRecord;
Begin;
if (BigNumComp(key.p,key.q)=1) then Exit;
n:=key.p;
key.p:=key.q;
key.q:=n;
End;

Procedure CalculateN;
Begin;
BigNumMul(key.p,key.q,key.n);
End;

Procedure CalculateD;
Var
  n1,n2,n3:BigNumContextRecord;
Begin;
BigNumStoreInt(1,n1);
BigNumSub(key.p,n1,n2);
BigNumSub(key.q,n1,n3);
BigNumMul(n2,n3,n2);
BigNumInverseMod(key.e,n2,n3);
key.d:=n3;
End;

Procedure GenerateE;
Var
  n1,n2,n3:BigNumContextRecord;
Begin;
GetRndBigNum(n3,16);
BigNumStoreInt(1,n2);
BigNumOr(n3,n2,n1);
repeat
  BigNumStoreInt(2,n2);
  repeat
    PutNextPoint;
    BigNumAdd(n1,n2,n1);
    until PrimalityTest2(n1);
  PutNextPoint;
  until PrimalityTest3(n1,100);
key.e:=n1;
End;

Procedure GenerateP(i:LongInt);
Var
  n1,n2,n3,n4:BigNumContextRecord;
Begin;
i:=i shr 1;
dec(i,Random($10));
GetRndBigNum(n1,i);
BigNumStoreInt(1,n2);
BigNumOr(n1,n2,n3);
BigNumShl(i-1,n2,n4);
BigNumOr(n3,n4,n1);
repeat
  BigNumStoreInt(2,n2);
  repeat
    PutNextPoint;
    BigNumAdd(n1,n2,n1);
    until PrimalityTest2(n1);
  PutNextPoint;
  until PrimalityTest3(n1,100);
key.p:=n1;
End;

Procedure GenerateQ(i:LongInt);
Var
  n1,n2,n3,n4:BigNumContextRecord;
Begin;
dec(i,BigNumSizeInBits(key.p));
GetRndBigNum(n1,i);
BigNumStoreInt(1,n2);
BigNumOr(n1,n2,n3);
BigNumShl(i-1,n2,n4);
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



Var
  i:LongInt;
  a:String;
  f:xFile;
BEGIN;
WriteLn('rsa key generator v1.0, done by Mc at '#%date' '#%time'.');
Randomize;
if BigNumStartActions then immErr('failed to find bignum process!');
fillchar(key,sizeof(key),0);

if (ParamStr(1)='') then immErr('using: mkRSAkey.exe <keyfile> [bits]');
a:=ParamStr(2);
i:=BVal(a);
if (i<160) or (i>8192) then i:=512;

WriteLn('going to generate '+BStr(i)+' bit RSA key... please wait!');


Write('generating p... ');
GenerateP(i);
showNumberStats(key.p);

Write('generating q... ');
GenerateQ(i);
showNumberStats(key.q);

Write('ensuring p>q... ');
EnsuringPQ;
WriteLn('done.');

Write('generating e... ');
GenerateE;
showNumberStats(key.e);

Write('calculating n... ');
CalculateN;
showNumberStats(key.n);

Write('calculating d... ');
CalculateD;
showNumberStats(key.d);

WriteLn('writing key...');
BigNumClearPadding(key.p);
BigNumClearPadding(key.q);
BigNumClearPadding(key.e);
BigNumClearPadding(key.n);
BigNumClearPadding(key.d);
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