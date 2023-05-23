Function UnpadRSAwithPKCS1(var data;var size:LongInt;type:LongInt;var num:BigNumContextRecord):Boolean;
Var
  dat:array[1..1] of byte absolute data;
  i,o:LongInt;
Begin;
UnpadRSAwithPKCS1:=True;
BigNumConvBN2MSB(num,size,data);
if (dat[1]<>type) then exit;
o:=1;
repeat
  if (o>size) then exit;
  inc(o);
  until (dat[o]=0);
dec(size,o);
if (size<1) then exit;
move(dat[o+1],data,size);
UnpadRSAwithPKCS1:=False;
End;

Procedure PadRSAwithPKCS1t1(var data;size,need:LongInt;var num:BigNumContextRecord);
Var
  dat:array[1..1] of byte absolute data;
  i,o:LongInt;
procedure addByte(n:byte);begin;  inc(o);dat[o]:=n;  end;
Begin;
move(dat,dat[2],size);
dat[1]:=0;
BigNumConvMSB2BN(size+1,data,num);
BigNumConvBN2LSB(num,o,data);
while (o<size) do addByte(0);
o:=size;
addByte($00);
size:=(need+7) shr 3;
for i:=o to size-3 do addByte($ff);
addByte($01);
addByte($00);
BigNumConvLSB2BN(size,data,num);
End;

Procedure PadRSAwithPKCS1t2(var data;size,need:LongInt;var num:BigNumContextRecord);
Label f1;
Var
  dat:array[1..1] of byte absolute data;
  i,o,p:LongInt;
procedure addByte(n:byte);begin;  inc(o);dat[o]:=n;  end;
Begin;
move(dat,dat[2],size);
dat[1]:=0;
BigNumConvMSB2BN(size+1,data,num);
BigNumConvBN2LSB(num,o,data);
while (o<size) do addByte(0);
o:=size;
addByte($00);
size:=(need+7) shr 3;
for i:=o to size-3 do begin;
  f1:
  p:=random($100);
  if (p=0) then goto f1;
  addByte(p);
  end;
addByte($02);
addByte($00);
BigNumConvLSB2BN(size,data,num);
End;




Function VerifySignWithDSS(var p,q,g,y,r,s:BigNumContextRecord;var data;size:LongInt):Boolean;
Var
  dat:array[1..1] of byte absolute data;
  n1,n2,n3:BigNumContextRecord;
Begin;
VerifySignWithDSS:=True;
move(dat,dat[2],size);
dat[1]:=0;
BigNumConvMSB2BN(size+1,data,n1);
BigNumInverseMod(s,q,n3);
BigNumMul(n1,n3,n1);
BigNumMul(r,n3,n2);
BigNumDiv(n1,q,n3,n1);
BigNumDiv(n2,q,n3,n2);
BigNumPowerMod(g,n1,p,n1);
BigNumPowerMod(y,n2,p,n2);
BigNumMul(n1,n2,n1);
BigNumDiv(n1,p,n3,n1);
BigNumDiv(n1,q,n3,n1);
if (BigNumComp(n1,r)<>0) then exit;
VerifySignWithDSS:=False;
End;



Function VerifySignWithRSA(var n,e,s:BigNumContextRecord;var data;size:LongInt):Boolean;
Var
  n1,n2:BigNumContextRecord;
Begin;
VerifySignWithRSA:=True;
BigNumPowerMod(s,e,n,n1);
PadRSAwithPKCS1t1(data,size,BigNumSizeInBits(n),n2);
if (BigNumComp(n2,n1)<>0) then exit;
VerifySignWithRSA:=False;
End;



Procedure SignDataWithDSSkey(var key:OneDSSkeyRecord;var r,s:BigNumContextRecord;var data;size:LongInt);
Var
  dat:array[1..1] of byte absolute data;
  n1,n2:BigNumContextRecord;
Begin;
move(dat[1],dat[2],size);
dat[1]:=0;
GetRndBigNum(n2,160);
BigNumDiv(n2,key.q,n2,n1);
BigNumInverseMod(n1,key.q,n2);
BigNumPowerMod(key.g,n1,key.p,r);
BigNumDiv(r,key.q,s,r);
BigNumConvMSB2BN(size+1,data,n1);
BigNumMul(r,key.x,s);
BigNumAdd(s,n1,n1);
BigNumMul(n1,n2,s);
BigNumDiv(s,key.q,n1,s);
End;

Procedure SignDataWithRSAkey(var key:OneRSAkeyRecord;var s:BigNumContextRecord;var data;size:LongInt);
Begin;
PadRSAwithPKCS1t1(data,size,BigNumSizeInBits(key.n),s);
BigNumPowerMod(s,key.d,key.n,s);
End;
