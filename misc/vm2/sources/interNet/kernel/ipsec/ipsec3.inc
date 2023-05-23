Procedure generateSecKeys(var con:OneConnectionRecord);
Var
  c1,c2:CryptoContextRecord;
  buf:array[1..1024] of byte;
  alg:LongInt;

Function finHM:String;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
i:=0;
hmacGenerateSum(c1,c2,ab[1],ab[1],i);
ab0:=i;
finHM:=a;
End;

Function finHS:String;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
CryptoFinHasher(c1,ab[1],i);
ab0:=i;
finHS:=a;
End;

Function doer(b:String;p:Byte):String;
Begin;
hmacInitialize(c1,c2,alg,con.skeysG);
CryptoAddHasher(c1,b[1],length(b));
CryptoAddHasher(c1,buf,con.encCS);
CryptoAddHasher(c1,con.initCok,sizeof(con.initCok));
CryptoAddHasher(c1,con.respCok,sizeof(con.respCok));
CryptoAddHasher(c1,p,sizeof(p));
doer:=finHM;
End;

Function populate(b:String):String;
Var
  a,c:String;
  i,o:LongInt;
Begin;
c:='';
a:=#0;
while (length(c)<200) do begin;
  hmacInitialize(c1,c2,alg,b);
  CryptoAddHasher(c1,a[1],length(a));
  a:=finHM;
  c:=c+a;
  end;
populate:=c;
End;

Var
  i,o:LongInt;
Begin;
alg:=HasherAlgoList[con.isaH].a;
hmacInitialize(c1,c2,alg,con.shky);
CryptoAddHasher(c1,con.initNon[1],length(con.initNon));
CryptoAddHasher(c1,con.respNon[1],length(con.respNon));
con.skeysG:=finHM;
savePaddedBigNumber(con.encLRc,buf,con.encCS);
con.skeysD:=doer('',0);
con.skeysA:=doer(con.skeysD,1);
con.skeysE:=doer(con.skeysA,2);
CryptoBegHasher(c1,alg,'','');
if (con.mode=0) then begin;
  savePaddedBigNumber(con.macLRa,buf,con.encCS);
  CryptoAddHasher(c1,buf,con.encCS);
  savePaddedBigNumber(con.macLRb,buf,con.encCS);
  CryptoAddHasher(c1,buf,con.encCS);
  end else begin;
  savePaddedBigNumber(con.macLRb,buf,con.encCS);
  CryptoAddHasher(c1,buf,con.encCS);
  savePaddedBigNumber(con.macLRa,buf,con.encCS);
  CryptoAddHasher(c1,buf,con.encCS);
  end;
i:=CipherAlgoList[con.isaE].s;
con.encS:=i;
con.encA:=CipherAlgoList[con.isaE].a;
con.encI:=copy(finHS,1,i);
i:=CipherAlgoList[con.isaE].ks;
con.encK:=con.skeysE;
if (length(con.encK)<i) then con.encK:=populate(con.encK);
con.encK:=copy(con.encK,1,i);
if (con.stat<100) then inc(con.stat);
End;



Function generateMMhashVal(var con:OneConnectionRecord;mode:LongInt):String;
Var
  c1,c2:CryptoContextRecord;
  buf:array[1..1024] of byte;
  hdr:OneGenericMextHeaderRecord absolute buf;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
generateMMhashVal:='';
hmacInitialize(c1,c2,HasherAlgoList[con.isaH].a,con.skeysG);
if (mode=1) then begin;
  savePaddedBigNumber(con.macLRa,buf,con.encCS);
  end else begin;
  savePaddedBigNumber(con.macLRb,buf,con.encCS);
  end;
CryptoAddHasher(c1,buf,con.encCS);
if (mode=1) then begin;
  savePaddedBigNumber(con.macLRb,buf,con.encCS);
  end else begin;
  savePaddedBigNumber(con.macLRa,buf,con.encCS);
  end;
CryptoAddHasher(c1,buf,con.encCS);
mode:=con.mode xor mode;
if (mode=1) then begin;
  CryptoAddHasher(c1,con.initCok,sizeof(con.initCok));
  CryptoAddHasher(c1,con.respCok,sizeof(con.respCok));
  end else begin;
  CryptoAddHasher(c1,con.respCok,sizeof(con.respCok));
  CryptoAddHasher(c1,con.initCok,sizeof(con.initCok));
  end;
if (con.mode=1) then begin;
  o:=oldPositionInHeader;
  i:=0;
  addMMsecAssHeader(con,buf,i,true);
  oldPositionInHeader:=o;
  end else begin;
  move(con.encRLc,buf,sizeof(buf));
  end;
CryptoAddHasher(c1,buf[sizeof(hdr)+1],ReadWordMSB(hdr.len)-sizeof(hdr));
if (con.mode xor mode=1) then begin;
  a:=con.remID;
  end else begin;
  o:=oldPositionInHeader;
  i:=0;
  addIdentityHeader(con,buf,i);
  oldPositionInHeader:=o;
  ab0:=ReadWordMSB(hdr.len)-sizeof(hdr);
  move(buf[sizeof(hdr)+1],ab[1],ab0);
  end;
CryptoAddHasher(c1,ab[1],ab0);
i:=0;
hmacGenerateSum(c1,c2,ab[1],ab[1],i);
ab0:=i;
generateMMhashVal:=a;
End;



Function generateQMhashVal(var con:OneConnectionRecord;var buf;siz,mode:LongInt):String;
Label vege;
Var
  c1,c2:CryptoContextRecord;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
generateQMhashVal:='';
hmacInitialize(c1,c2,HasherAlgoList[con.isaH].a,con.skeysA);
if (siz<1) then begin;
  i:=0;
  CryptoAddHasher(c1,i,1);
  CryptoAddHasher(c1,con.msgID,sizeof(con.msgID));
  a:=con.initNon;
  CryptoAddHasher(c1,ab[1],ab0);
  a:=con.respNon;
  CryptoAddHasher(c1,ab[1],ab0);
  goto vege;
  end;
CryptoAddHasher(c1,con.msgID,sizeof(con.msgID));
if (mode<>0) then begin;
  a:=con.initNon;
  CryptoAddHasher(c1,ab[1],ab0);
  end;
CryptoAddHasher(c1,buf,siz);
vege:
i:=0;
hmacGenerateSum(c1,c2,ab[1],ab[1],i);
ab0:=i;
generateQMhashVal:=a;
End;


Procedure generateLocalCookie(var con:OneConnectionRecord);
Var i:LongInt;
Begin;
if (con.mode=0) then begin;
  for i:=1 to sizeof(con.respCok) do con.respCok[i]:=random($100);
  end else begin;
  for i:=1 to sizeof(con.initCok) do con.initCok[i]:=random($100);
  end;
End;



Procedure generateLocalNonce(var con:OneConnectionRecord);
Var
  i:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
ab0:=16;
for i:=1 to ab0 do ab[i]:=random($100);
if (con.mode=0) then con.respNon:=a else con.initNon:=a;
End;



Procedure generateLocalSPI(var con:OneConnectionRecord);
Begin;
con.locSPI:=random($7fffffff);
End;



Procedure doDiffieHellmanRound1(var con:OneConnectionRecord);
Var
  n1,n2,n3,n4,n5:BigNumContextRecord;
  i:LongInt;
Begin;
GetDiffieHellmanGroup(con.isaG,n2);
i:=BigNumSizeInBits(n2);
con.encCS:=(i+7) shr 3;
i:=i div diffieHellmanDiv;
if (i<diffieHellmanMin) then i:=diffieHellmanMin;
if (i>diffieHellmanMax) then i:=diffieHellmanMax;
GetRndBigNum(n3,i);
BigNumStoreInt(2,n4);
BigNumPowerMod(n4,n3,n2,n5);
move(n5,con.macLRb,sizeof(con.macLRb));
move(n3,con.encLRc,sizeof(con.encLRc));
End;



Procedure doDiffieHellmanRound2(var con:OneConnectionRecord);
Var
  n1,n2,n3,n4,n5:BigNumContextRecord;
  i:LongInt;
Begin;
GetDiffieHellmanGroup(con.isaG,n2);
move(con.macLRa,n1,sizeof(n1));
move(con.encLRc,n3,sizeof(n3));
BigNumStoreInt(2,n4);
BigNumPowerMod(n1,n3,n2,n5);
move(n5,con.encLRc,sizeof(con.encLRc));
End;



Procedure generateQuickModeIV(var con:OneConnectionRecord);
Var
  c:CryptoContextRecord;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
{$ifdef debug}dumpBinaryData('old iv',con.encI,23);{$endif}
CryptoBegHasher(c,HasherAlgoList[con.isaH].a,'','');
CryptoAddHasher(c,con.encI[1],con.encS);
CryptoAddHasher(c,con.msgID,sizeof(con.msgID));
CryptoFinHasher(c,ab[1],i);
ab0:=i;
con.encI:=copy(a,1,con.encS);
{$ifdef debug}dumpBinaryData('new iv',con.encI,23);{$endif}
End;



Procedure generateESPkeyMaterial(var con:OneConnectionRecord);
Var
  buf:array[1..1024] of byte;
  c1,c2:CryptoContextRecord;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;

Function calc(spi:LongInt):String;
Var b:String;
Begin;
b:='';
a:='';
savePaddedBigNumber(con.encLRc,buf,con.encCS);
while (length(b)<200) do begin;
  hmacInitialize(c1,c2,HasherAlgoList[con.isaH].a,con.skeysD);
  CryptoAddHasher(c1,ab[1],ab0);
  if (con.isaG<>0) then CryptoAddHasher(c1,buf,con.encCS);
  a:=#3;
  CryptoAddHasher(c1,ab[1],ab0);
  CryptoAddHasher(c1,spi,sizeof(spi));
  a:=con.initNon;
  CryptoAddHasher(c1,ab[1],ab0);
  a:=con.respNon;
  CryptoAddHasher(c1,ab[1],ab0);
  i:=0;
  hmacGenerateSum(c1,c2,ab[1],ab[1],i);
  ab0:=i;
  b:=b+a;
  end;
calc:=b;
End;

Procedure init(a:String;var e,c1,c2:CryptoContextRecord);
Var
  alg:LongInt;
  key:String;
Begin;
alg:=CipherAlgoList[con.espE].a;
key:=copy(a,1,CipherAlgoList[con.espE].ks);
CryptoBegCipher(e,alg,key,copy(a,1,con.encCS));
alg:=HasherAlgoList[con.espH].a;
key:=copy(a,con.encCS+1,con.encHS);
hmacInitialize(c1,c2,alg,key);
End;

Var b,c:String;
Begin;
b:=calc(con.locSPI);
c:=calc(con.remSPI);
{$ifdef debug}
dumpBinaryData('loc spi',con.locSPI,4);
dumpBinaryData('rem spi',con.remSPI,4);
dumpBinaryData('loc key',b[1],64);
dumpBinaryData('rem key',c[2],64);
{$endif}
con.encCS:=CipherAlgoList[con.espE].s;
con.encHS:=HasherAlgoList[con.espH].s;
init(b,con.encRLc,con.macRLa,con.macRLb);
init(c,con.encLRc,con.macLRa,con.macLRb);
con.encHS:=12;
con.stat:=1000;
WriteLn('tunnel up with '+ipAddr2string(con.addr)+' '+BStr(con.portR));
End;
