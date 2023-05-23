Const
  DigitalSignatureNameDSS='ssh-dss';
  DigitalSignatureNameRSA='ssh-rsa';


Procedure GetCurrentDSSkey(var pck:OnePacketRecord);
Begin;
add2packetString(pck,DigitalSignatureNameDSS);
add2packetBignum(pck,hostKeyDSS.p);
add2packetBignum(pck,hostKeyDSS.q);
add2packetBignum(pck,hostKeyDSS.g);
add2packetBignum(pck,hostKeyDSS.y);
End;

Procedure GetCurrentRSAkey(var pck:OnePacketRecord);
Begin;
add2packetString(pck,DigitalSignatureNameRSA);
add2packetBignum(pck,hostKeyRSA.e);
add2packetBignum(pck,hostKeyRSA.n);
End;



Procedure SignDataWithDSSkey(var pck:OnePacketRecord;var data;size:LongInt);

function getRes(num:BigNumContextRecord):string;
var a:string;w:word;i:longint;
begin;
move(num,w,sizeof(w));
if (w>5) then w:=5;
move(w,num,sizeof(w));
BigNumConvBN2MSB(num,i,a[1]);
a[0]:=chr(i);
while (length(a)<20) do a:=#0+a;
while (length(a)>20) do a:=copy(a,2,255);
getRes:=a;
end;

Var
  n1,n2,n3,n4:BigNumContextRecord;
  a:String;
Begin;
CryptoImmHasher(sha1algoNum,'','',data,size);
move(data,a[1],size);
a[0]:=chr(size);
a:=#0#0+a;
GetRndBigNum(n2,160);
BigNumDiv(n2,hostKeyDSS.q,n2,n1);
BigNumInverseMod(n1,hostKeyDSS.q,n2);
BigNumPowerMod(hostKeyDSS.g,n1,hostKeyDSS.p,n3);
BigNumDiv(n3,hostKeyDSS.q,n4,n3);
BigNumConvMSB2BN(length(a),a[1],n1);
BigNumMul(n3,hostKeyDSS.x,n4);
BigNumAdd(n4,n1,n1);
BigNumMul(n1,n2,n4);
BigNumDiv(n4,hostKeyDSS.q,n1,n4);
a:=getRes(n3)+getRes(n4);
add2packetString(pck,DigitalSignatureNameDSS);
add2packetString(pck,a);
End;

Procedure SignDataWithRSAkey(var pck:OnePacketRecord;var data;size:LongInt);
Var
  dat:array[1..1] of byte absolute data;
  n1,n2:BigNumContextRecord;
  i,o:LongInt;

Procedure addByte(n:byte);
Begin;
inc(o);
dat[o]:=n;
End;

Begin;
CryptoImmHasher(sha1algoNum,'','',data,size);
BigNumConvMSB2BN(size,data,n1);
BigNumConvBN2LSB(n1,o,data);
while (o<20) do addByte(0);
o:=20;
size:=(BigNumSizeInBits(hostKeyRSA.n)+7) shr 3;
addByte($14);addByte($04);addByte($00);addByte($05);
addByte($1a);addByte($02);addByte($03);addByte($0e);
addByte($2b);addByte($05);addByte($06);addByte($09);
addByte($30);addByte($21);addByte($30);addByte($00);
for i:=o to size-3 do addByte($ff);
addByte($01);
addByte($00);
BigNumConvLSB2BN(o,data,n1);
BigNumPowerMod(n1,hostKeyRSA.d,hostKeyRSA.n,n2);
add2packetString(pck,DigitalSignatureNameRSA);
add2packetBignum(pck,n2);
End;



Function VerifySignWithDSS(key,blob:OnePacketRecord;var data;size:LongInt):String;
Var
  dat:array[1..1] of byte absolute data;
  p,q,g,y:BigNumContextRecord;
  rs:String;
  n1,n2,n3:BigNumContextRecord;
  i,o:LongInt;

Procedure getNum(p:Byte;var n:BigNumContextRecord);
Var a:String;
Begin;
a:=#0+copy(rs,p,20);
BigNumConvMSB2BN(21,a[1],n);
End;

Begin;
VerifySignWithDSS:='';
CryptoImmHasher(sha1algoNum,'','',data,size);
move(dat,dat[2],size);
dat[1]:=0;
BigNumConvMSB2BN(size+1,data,n1);
i:=0;if (get4packetString(i,key)<>DigitalSignatureNameDSS) then exit;
get4packetBignum(i,key,p);
get4packetBignum(i,key,q);
get4packetBignum(i,key,g);
get4packetBignum(i,key,y);
if (key.s<>i) then exit;
i:=0;if (get4packetString(i,blob)<>DigitalSignatureNameDSS) then exit;
rs:=get4packetString(i,blob);
if (length(rs)<>40) then exit;
if (blob.s<>i) then exit;
getNum(21,n3);
BigNumInverseMod(n3,q,n3);
getNum(1,n2);
BigNumMul(n1,n3,n1);
BigNumMul(n2,n3,n2);
BigNumDiv(n1,q,n3,n1);
BigNumDiv(n2,q,n3,n2);
BigNumPowerMod(g,n1,p,n1);
BigNumPowerMod(y,n2,p,n2);
BigNumMul(n1,n2,n1);
BigNumDiv(n1,p,n3,n1);
BigNumDiv(n1,q,n3,n1);
getNum(1,n2);
if (BigNumComp(n1,n2)<>0) then exit;
VerifySignWithDSS:=DigitalSignatureNameDSS+' '+BStr(BigNumSizeInBits(p));
End;

Function VerifySignWithRSA(key,blob:OnePacketRecord;var data;size:LongInt):String;
Var
  dat:array[1..1] of byte absolute data;
  e,n:BigNumContextRecord;
  n1,n2:BigNumContextRecord;
  i,o:LongInt;

Procedure addByte(n:byte);
Begin;
inc(o);
dat[o]:=n;
End;

Begin;
VerifySignWithRSA:='';
CryptoImmHasher(sha1algoNum,'','',data,size);
move(dat,dat[2],size);
dat[1]:=0;
BigNumConvMSB2BN(size+1,data,n1);
i:=0;if (get4packetString(i,key)<>DigitalSignatureNameRSA) then exit;
get4packetBignum(i,key,e);
get4packetBignum(i,key,n);
if (key.s<>i) then exit;
i:=0;if (get4packetString(i,blob)<>DigitalSignatureNameRSA) then exit;
get4packetBinHdr(i,blob,data,size);
if (blob.s<>i) then exit;
move(dat,dat[2],size);
dat[1]:=0;
BigNumConvMSB2BN(size+1,data,n2);
BigNumPowerMod(n2,e,n,n2);
BigNumConvBN2LSB(n1,o,data);
while (o<20) do addByte(0);
o:=20;
size:=(BigNumSizeInBits(n)+7) shr 3;
addByte($14);addByte($04);addByte($00);addByte($05);
addByte($1a);addByte($02);addByte($03);addByte($0e);
addByte($2b);addByte($05);addByte($06);addByte($09);
addByte($30);addByte($21);addByte($30);addByte($00);
for i:=o to size-3 do addByte($ff);
addByte($01);
addByte($00);
BigNumConvLSB2BN(o,data,n1);
if (BigNumComp(n1,n2)<>0) then exit;
VerifySignWithRSA:=DigitalSignatureNameRSA+' '+BStr(BigNumSizeInBits(n));
End;
