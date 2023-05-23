Procedure ReadUpConfig(a:String);
Var
  macList:array[1..HasherAlgoMax] of Byte;
  encList:array[1..CipherAlgoMax] of Byte;
  kexList:array[1..2] of byte;
  crtList:array[1..2] of byte;
  algList:array[1..CipherSuitesMax] of LongInt;
  t:xtText;

Function gnl:string;
Var
  i:longint;
  a:String;
Begin;
a:=xtReadLn(t,255);
i:=pos(';',a);
if (i>0) then a:=copy(a,1,i-1);
a:=' '+a+' ';
kicserel('  ',' ',a);
a:=copy(a,2,length(a)-2);
gnl:=a;
End;

Procedure xchg(var a,b:LongInt);
Var i:LongInt;
Begin;
i:=a;
a:=b;
b:=i;
End;

Function QuickReadOne(n:LongInt):String;
Var
  a:String;
  ab:array[0..4] of byte absolute a;
Begin;
n:=algList[n];
ab[0]:=3;
ab[1]:=n shr 16;
ab[2]:=n shr 8;
ab[3]:=n;
QuickReadOne:=a;
End;

Procedure QuickSwapOne(n1,n2:LongInt);
Begin;
xchg(algList[n1],algList[n2]);
xchg(CipherSuitesDat[n1],CipherSuitesDat[n2]);
End;

{$sysinc quicksrt.inc}

Procedure convertCert(var p1:OnePacketRecord);
Var
  p2:OnePacketRecord;
  s1,s2,i,o,p,q:LongInt;
Begin;
s1:=0;
s2:=0;
while (s1<p1.s) do begin;
  q:=s1;
  readASN1header(p1.d,s1,i,o,p);
  i:=s1-q;
  inc(p,i);
  s1:=q;
  WriteLongMSB(p2.d[s2+1],p shl 8);
  inc(s2,3);
  move(p1.d[s1+1],p2.d[s2+1],p);
  inc(s2,p);
  inc(s1,p);
  end;
WriteLongMSB(p1.d,s2 shl 8);
move(p2.d,p1.d[4],s2);
p1.s:=s2+3;
End;


Var
  f:xFile;
  b:String;
  i,o,p,q:LongInt;
Begin;
CryptoGetCipherList(hostKeyDSS,i);
CipherAlgoList[1].a:=CryptoFindOneAlgo(hostKeyDSS,i,'aes-cbc');
CipherAlgoList[2].a:=CryptoFindOneAlgo(hostKeyDSS,i,'des-cbc');
CipherAlgoList[3].a:=CryptoFindOneAlgo(hostKeyDSS,i,'3des-cbc');
CipherAlgoList[4].a:=CryptoFindOneAlgo(hostKeyDSS,i,'rc2-cbc');
CryptoGetHasherList(hostKeyDSS,i);
sha1algoNum:=CryptoFindOneAlgo(hostKeyDSS,i,'sha1');
md5algoNum:=CryptoFindOneAlgo(hostKeyDSS,i,'md5');
HasherAlgoList[1].a:=CryptoFindOneAlgo(hostKeyDSS,i,'md5');
HasherAlgoList[2].a:=CryptoFindOneAlgo(hostKeyDSS,i,'sha1');

if (xtOpen(t,a,true)<>0) then immErr('error opening config!');
b:=gnl;
for i:=1 to HasherAlgoMax do macList[i]:=Pos(BStr(i),b);
b:=gnl;
for i:=1 to CipherAlgoMax do encList[i]:=Pos(BStr(i),b);
a:=gnl;
fillchar(hostKeyDH,sizeof(hostKeyDH),0);
if (xOpen(f,a,xGenFilMod_r)=0) then begin;
  xBlockRead(f,hostKeyDH,sizeof(hostKeyDH));
  xClose(f);
  end;
a:=gnl;
fillchar(hostKeyRSAe,sizeof(hostKeyRSAe),0);
if (xOpen(f,a,xGenFilMod_r)=0) then begin;
  xBlockRead(f,hostKeyRSAe,sizeof(hostKeyRSAe));
  xClose(f);
  end;
hostKeyDHdiv:=BVal(gnl);
hostKeyDHmin:=BVal(gnl);
hostKeyDHmax:=BVal(gnl);
a:=gnl;
fillchar(hostKeyDSS,sizeof(hostKeyDSS),0);
if (xOpen(f,a,xGenFilMod_r)=0) then begin;
  xBlockRead(f,hostKeyDSS,sizeof(hostKeyDSS));
  xClose(f);
  end;
a:=gnl;
fillchar(hostKeyRSA,sizeof(hostKeyRSA),0);
if (xOpen(f,a,xGenFilMod_r)=0) then begin;
  xBlockRead(f,hostKeyRSA,sizeof(hostKeyRSA));
  xClose(f);
  end;
a:=gnl;
fillchar(hostCrtDSS,sizeof(hostCrtDSS),0);
if (xOpen(f,a,xGenFilMod_r)=0) then begin;
  hostCrtDSS.s:=xFileSize(f);
  xBlockRead(f,hostCrtDSS.d,hostCrtDSS.s);
  xClose(f);
  end;
a:=gnl;
fillchar(hostCrtRSA,sizeof(hostCrtRSA),0);
if (xOpen(f,a,xGenFilMod_r)=0) then begin;
  hostCrtRSA.s:=xFileSize(f);
  xBlockRead(f,hostCrtRSA.d,hostCrtRSA.s);
  xClose(f);
  end;
xtClose(t);

move(hostKeyDSS,i,sizeof(i));move(hostCrtDSS,o,sizeof(o));
if (i<>0) and (o<>0) then i:=1 else i:=0;
crtList[1]:=i;
move(hostKeyRSA,i,sizeof(i));move(hostCrtRSA,o,sizeof(o));
if (i<>0) and (o<>0) then i:=2 else i:=0;
crtList[2]:=i;
move(hostKeyRSAe,o,sizeof(o));
if (o=0) then i:=0;
kexList[2]:=i;
move(hostKeyDH,i,sizeof(i));
if (i<>0) then i:=1 else i:=0;
kexList[1]:=i;
CipherSuitesNum:=0;
for p:=1 to 512 do begin;
  o:=decodeOneCipherSuite(p);
  if (o<1) then continue;
  q:=(o and 1);                         {export}
  i:=encList[(o shr 8) and $f];         {cipher}
  if (i<1) then continue;
  q:=(q shl 4) or i;
  i:=macList[(o shr 4) and $f];         {hash}
  if (i<1) then continue;
  q:=(q shl 4) or i;
  i:=kexList[(o shr 16) and $f];        {kex}
  if (i<1) then continue;
  q:=(q shl 4) or i;
  i:=crtList[(o shr 12) and $f];        {cert}
  if (i<1) then continue;
  q:=(q shl 4) or i;
  inc(CipherSuitesNum);
  CipherSuitesDat[CipherSuitesNum]:=p;
  algList[CipherSuitesNum]:=q;
  end;
if (CipherSuitesNum>1) then QuickSort(1,CipherSuitesNum);

convertCert(hostCrtDSS);
convertCert(hostCrtRSA);

currentRandomData:='';
for i:=1 to 32 do currentRandomData:=currentRandomData+chr(random($100));
End;
