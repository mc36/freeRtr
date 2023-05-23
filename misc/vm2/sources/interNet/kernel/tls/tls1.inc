Function convertSSL2DTLSversion(i:LongInt):LongInt;
Begin;
convertSSL2DTLSversion:=(not (i-$202)) and $ffff;
End;

Function convertDTLS2SSLversion(i:LongInt):LongInt;
Begin;
convertDTLS2SSLversion:=((not i)+$202) and $ffff;
End;

Function decodeOneCipherSuite(i:LongInt):LongInt;
{nibbles: keyExchange, certificate, encryption, hash, exportable}
Begin;
case i of
{         kcehx          name}
  $03:i:=$22511;        {TLS_RSA_EXPORT_WITH_RC4_40_MD5}
  $04:i:=$22510;        {TLS_RSA_WITH_RC4_128_MD5}
  $05:i:=$22520;        {TLS_RSA_WITH_RC4_128_SHA}
  $06:i:=$22411;        {TLS_RSA_EXPORT_WITH_RC2_CBC_40_MD5}
{ $07:i:=$22x20;        {TLS_RSA_WITH_IDEA_CBC_SHA}
  $08:i:=$22221;        {TLS_RSA_EXPORT_WITH_DES40_CBC_SHA}
  $09:i:=$22220;        {TLS_RSA_WITH_DES_CBC_SHA}
  $0a:i:=$22320;        {TLS_RSA_WITH_3DES_EDE_CBC_SHA}
{ $0b:i:=$11221;        {TLS_DH_DSS_EXPORT_WITH_DES40_CBC_SHA}
{ $0c:i:=$11220;        {TLS_DH_DSS_WITH_DES_CBC_SHA}
{ $0d:i:=$11320;        {TLS_DH_DSS_WITH_3DES_EDE_CBC_SHA}
{ $0e:i:=$12221;        {TLS_DH_RSA_EXPORT_WITH_DES40_CBC_SHA}
{ $0f:i:=$12220;        {TLS_DH_RSA_WITH_DES_CBC_SHA}
{ $10:i:=$12320;        {TLS_DH_RSA_WITH_3DES_EDE_CBC_SHA}
  $11:i:=$11221;        {TLS_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA}
  $12:i:=$11220;        {TLS_DHE_DSS_WITH_DES_CBC_SHA}
  $13:i:=$11320;        {TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA}
  $14:i:=$12221;        {TLS_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA}
  $15:i:=$12220;        {TLS_DHE_RSA_WITH_DES_CBC_SHA}
  $16:i:=$12320;        {TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA}
{ $2f:i:=$22120;        {TLS_RSA_WITH_AES_128_CBC_SHA}
{ $30:i:=$11120;        {TLS_DH_DSS_WITH_AES_128_CBC_SHA}
{ $31:i:=$12120;        {TLS_DH_RSA_WITH_AES_128_CBC_SHA}
{ $32:i:=$11120;        {TLS_DHE_DSS_WITH_AES_128_CBC_SHA}
{ $33:i:=$12120;        {TLS_DHE_RSA_WITH_AES_128_CBC_SHA}
  $35:i:=$22120;        {TLS_RSA_WITH_AES_256_CBC_SHA}
{ $36:i:=$11120;        {TLS_DH_DSS_WITH_AES_256_CBC_SHA}
{ $37:i:=$12120;        {TLS_DH_RSA_WITH_AES_256_CBC_SHA}
  $38:i:=$11120;        {TLS_DHE_DSS_WITH_AES_256_CBC_SHA}
  $39:i:=$12120;        {TLS_DHE_RSA_WITH_AES_256_CBC_SHA}
  else i:=0;
  end;
decodeOneCipherSuite:=i;
End;



Function dtls_calcSum(c1,c2:CryptoContextRecord;lo,hi:LongInt;var packet;var data;size:LongInt):LongInt;
Const max=1024;
Var
  seq:record
    hi:LongInt;
    lo:LongInt;
    end;
  pck:array[1..1] of byte absolute packet;
  i,o:LongInt;
Begin;
WriteLongMSB(seq.hi,(hi and $ffff) or $10000);
WriteLongMSB(seq.lo,lo);
CryptoAddHasher(c1,seq,sizeof(seq));
CryptoAddHasher(c1,pck[1],3);
o:=12;
dec(size,11);
while (size>0) do begin;
  if (size>max) then i:=max else i:=size;
  CryptoAddHasher(c1,pck[o],i);
  inc(o,i);
  dec(size,i);
  end;
CryptoFinHasher(c1,c1,size);
CryptoAddHasher(c2,c1,size);
CryptoFinHasher(c2,data,size);
dtls_calcSum:=size;
End;



Function tls_calcSum(c1,c2:CryptoContextRecord;lo,hi:LongInt;var packet;var data;size:LongInt):LongInt;
Const max=1024;
Var
  seq:record
    hi:LongInt;
    lo:LongInt;
    end;
  pck:array[1..1] of byte absolute packet;
  i,o:LongInt;
Begin;
WriteLongMSB(seq.hi,hi);
WriteLongMSB(seq.lo,lo);
CryptoAddHasher(c1,seq,sizeof(seq));
o:=1;
while (size>0) do begin;
  if (size>max) then i:=max else i:=size;
  CryptoAddHasher(c1,pck[o],i);
  inc(o,i);
  dec(size,i);
  end;
CryptoFinHasher(c1,c1,size);
CryptoAddHasher(c2,c1,size);
CryptoFinHasher(c2,data,size);
tls_calcSum:=size;
End;



Function ssl_calcSum(c1,c2:CryptoContextRecord;lo,hi:LongInt;var packet;var data;size:LongInt):LongInt;
Const max=1024;
Var
  seq:record
    hi:LongInt;
    lo:LongInt;
    end;
  pck:array[1..1] of byte absolute packet;
  i,o:LongInt;
Begin;
WriteLongMSB(seq.hi,hi);
WriteLongMSB(seq.lo,lo);
CryptoAddHasher(c1,seq,sizeof(seq));
CryptoAddHasher(c1,pck[1],1);
o:=4;
dec(size,3);
while (size>0) do begin;
  if (size>max) then i:=max else i:=size;
  CryptoAddHasher(c1,pck[o],i);
  inc(o,i);
  dec(size,i);
  end;
CryptoFinHasher(c1,c1,size);
CryptoAddHasher(c2,c1,size);
CryptoFinHasher(c2,data,size);
ssl_calcSum:=size;
End;




Function GetBinaryNumber(var pck:OnePacketRecord;var ps:LongInt;var num:BigNumContextRecord):Boolean;
Var
  buf:array[1..1] of byte absolute num;
  i:LongInt;
Begin;
GetBinaryNumber:=True;
i:=ReadWordMSB(pck.d[ps+1]);
inc(ps,2);
if (ps+i>pck.s) then exit;
if (i>sizeof(num)) then exit;
buf[1]:=0;
move(pck.d[ps+1],buf[2],i);
inc(ps,i);
BigNumConvMSB2BN(i+1,num,num);
GetBinaryNumber:=False;
End;



Procedure PutBinaryNumber(var pck:OnePacketRecord;var ps:LongInt;num:BigNumContextRecord);
Var
  buf:array[1..1] of byte absolute num;
  i:LongInt;
Begin;
BigNumConvBN2MSB(num,i,num);
if (buf[1]=0) and (i>1) then begin;
  dec(i);
  move(buf[2],buf[1],i);
  end;
WriteWordMSB(pck.d[ps+1],i);
move(buf,pck.d[ps+3],i);
inc(ps,i+2);
End;






Procedure hmacInitialize(var c1,c2:CryptoContextRecord;alg:Byte;keyB:OnePacketRecord);

Function padup(key:String;num:byte):string;
Const max=64;
Var
  dat:array[0..1] of byte absolute key;
  i:LongInt;
Begin;
for i:=dat[0]+1 to max do dat[i]:=0;
dat[0]:=max;
for i:=1 to max do dat[i]:=dat[i] xor num;
padup:=key;
End;

Var
  a,b:String;
Begin;
if (keyb.s>64) then begin;
  CryptoBegHasher(c1,alg,'','');
  CryptoAddHasher(c1,keyb.d,keyb.s);
  CryptoFinHasher(c1,keyb.d,keyb.s);
  end;
b[0]:=chr(keyb.s);
move(keyb.d,b[1],keyb.s);
CryptoBegHasher(c1,alg,'','');
CryptoBegHasher(c2,alg,'','');
a:=padup(b,$36);
CryptoAddHasher(c1,a[1],length(a));
a:=padup(b,$5c);
CryptoAddHasher(c2,a[1],length(a));
End;

Procedure hmacGenerateSum(c1,c2:CryptoContextRecord;var packet;var data;var size:LongInt);
Begin;
CryptoAddHasher(c1,packet,size);
CryptoFinHasher(c1,c1,size);
CryptoAddHasher(c2,c1,size);
CryptoFinHasher(c2,data,size);
End;





Procedure tls_pseudoRandomFnc(var sec,sed:OnePacketRecord;size:LongInt);
Var
  c1:CryptoContextRecord;
  c2:CryptoContextRecord;
  seed:String;

Procedure doOne(alg:Byte;var out:OnePacketRecord);
Var
  a,b:String;
  i:LongInt;
Begin;
out.s:=0;
a:=seed;
out.s:=0;
while (out.s<size) do begin;
  hmacInitialize(c1,c2,alg,sec);
  i:=length(a);
  hmacGenerateSum(c1,c2,a[1],a[1],i);
  a[0]:=chr(i);
  hmacInitialize(c1,c2,alg,sec);
  b:=a+seed;
  i:=length(b);
  hmacGenerateSum(c1,c2,b[1],out.d[out.s+1],i);
  inc(out.s,i);
  end;
out.s:=size;
End;

Var
  b1,b2:OnePacketRecord;
  i:LongInt;
Begin;
i:=sed.s;
if (i>255) then i:=255;
seed[0]:=chr(i);
move(sed.d,seed[1],i);
i:=sec.s;
sec.s:=(i+1) div 2;
doOne(md5algoNum,b1);
move(sec.d[i-sec.s+1],sec.d,sec.s);
doOne(sha1algoNum,b2);
sec.s:=size;
for i:=1 to size do sec.d[i]:=b1.d[i] xor b2.d[i];
sec.t:=0;
End;





Procedure ssl_pseudoRandomFnc(var sec,rnd:OnePacketRecord;size:LongInt);
Var
  out:OnePacketRecord;
  ctx:CryptoContextRecord;
  a:String;
  i,o:LongInt;
Begin;
out.s:=0;
o:=0;
while (out.s<size) do begin;
  inc(o);
  fillchar(a,sizeof(a),o+$40);
  a[0]:=chr(o);
  CryptoBegHasher(ctx,sha1algoNum,'','');
  CryptoAddHasher(ctx,a[1],length(a));
  CryptoAddHasher(ctx,sec.d,sec.s);
  CryptoAddHasher(ctx,rnd.d,rnd.s);
  CryptoFinHasher(ctx,a,i);
  CryptoBegHasher(ctx,md5algoNum,'','');
  CryptoAddHasher(ctx,sec.d,sec.s);
  CryptoAddHasher(ctx,a,i);
  CryptoFinHasher(ctx,out.d[out.s+1],i);
  inc(out.s,i);
  end;
sec:=out;
sec.t:=0;
End;




Procedure tls_generateKeyBlock(var con:OneConnectionRecord;var macC1,macC2,encC,macS1,macS2,encS:CryptoContextRecord);
Var pck1,pck2:OnePacketRecord;

Procedure putStrng(var pck:OnePacketRecord;a:String);
Begin;
pck.s:=length(a);
move(a[1],pck.d,pck.s);
End;

Procedure addRands(var pck:OnePacketRecord;mode:Boolean);
Begin;
if mode then begin;
  move(con.srvRnd,pck.d[pck.s+1],sizeof(con.srvRnd));
  inc(pck.s,sizeof(con.srvRnd));
  move(con.clnRnd,pck.d[pck.s+1],sizeof(con.clnRnd));
  inc(pck.s,sizeof(con.clnRnd));
  end else begin;
  move(con.clnRnd,pck.d[pck.s+1],sizeof(con.clnRnd));
  inc(pck.s,sizeof(con.clnRnd));
  move(con.srvRnd,pck.d[pck.s+1],sizeof(con.srvRnd));
  inc(pck.s,sizeof(con.srvRnd));
  end;
End;

Procedure finished(a:String;ps:LongInt);
Var
  ctx:CryptoContextRecord;
  i:LongInt;
Begin;
pck1.s:=sizeof(con.secret);
move(con.secret,pck1.d,sizeof(con.secret));
putStrng(pck2,a);
ctx:=con.macLRb;
CryptoFinHasher(ctx,pck2.d[pck2.s+1],i);
inc(pck2.s,i);
ctx:=con.macLRa;
CryptoFinHasher(ctx,pck2.d[pck2.s+1],i);
inc(pck2.s,i);
tls_pseudoRandomFnc(pck1,pck2,12);
a[0]:=chr(pck1.s);
move(pck1.d,a[1],length(a));
move(a,con.recDat[sizeof(con.recDat)-ps],92);
End;

Function partition(s:LongInt):String;
Var a:String;
Begin;
a[0]:=chr(s);
move(pck1.d[pck1.t+1],a[1],s);
inc(pck1.t,s);
partition:=a;
End;

Var
  clnt_mac,srvr_mac:String;
  clnt_key,srvr_key:String;
  clnt_iv,srvr_iv:String;
  i:LongInt;
Begin;
con.macSiz:=HasherAlgoList[con.planMac].s;
con.encSiz:=CipherAlgoList[con.planEnc].s;
finished('client finished',100);
finished('server finished',200);
pck1.s:=sizeof(con.secret);
move(con.secret,pck1.d,sizeof(con.secret));
putStrng(pck2,'key expansion');
addRands(pck2,true);
tls_pseudoRandomFnc(pck1,pck2,256);
i:=HasherAlgoList[con.planMac].s;
clnt_mac:=partition(i);
srvr_mac:=partition(i);
i:=CipherAlgoList[con.planEnc].ks;
if con.planExp then i:=CipherAlgoList[con.planEnc].es;
clnt_key:=partition(i);
srvr_key:=partition(i);
i:=CipherAlgoList[con.planEnc].s;
clnt_iv:=partition(i);
srvr_iv:=partition(i);
if con.planExp then begin;
  i:=CipherAlgoList[con.planEnc].ks;
  putStrng(pck1,clnt_key);
  putStrng(pck2,'client write key');
  addRands(pck2,false);
  tls_pseudoRandomFnc(pck1,pck2,i);
  clnt_key:=partition(i);
  putStrng(pck1,srvr_key);
  putStrng(pck2,'server write key');
  addRands(pck2,false);
  tls_pseudoRandomFnc(pck1,pck2,i);
  srvr_key:=partition(i);
  i:=CipherAlgoList[con.planEnc].s;
  putStrng(pck1,'');
  putStrng(pck2,'IV block');
  addRands(pck2,false);
  tls_pseudoRandomFnc(pck1,pck2,i*2);
  clnt_iv:=partition(i);
  srvr_iv:=partition(i);
  end;
i:=CipherAlgoList[con.planEnc].a;
CryptoBegCipher(encC,i,clnt_key,clnt_iv);
CryptoBegCipher(encS,i,srvr_key,srvr_iv);
i:=HasherAlgoList[con.planMac].a;
putStrng(pck1,clnt_mac);
putStrng(pck2,srvr_mac);
hmacInitialize(macC1,macC2,i,pck1);
hmacInitialize(macS1,macS2,i,pck2);
End;




Procedure ssl_generateKeyBlock(var con:OneConnectionRecord;var macC1,macC2,encC,macS1,macS2,encS:CryptoContextRecord);
Var
  pck1,pck2:OnePacketRecord;
  ctx:CryptoContextRecord;

Procedure addPadding(var ctx:CryptoContextRecord;s,v:Byte);
Var buf:array[1..256] of byte;
Begin;
fillchar(buf,s,v);
CryptoAddHasher(ctx,buf,s);
End;

Procedure putStrng(var pck:OnePacketRecord;a:String);
Begin;
pck.s:=length(a);
move(a[1],pck.d,pck.s);
End;

Procedure addRands(var pck:OnePacketRecord;mode:Boolean);
Begin;
if mode then begin;
  move(con.srvRnd,pck.d[pck.s+1],sizeof(con.srvRnd));
  inc(pck.s,sizeof(con.srvRnd));
  move(con.clnRnd,pck.d[pck.s+1],sizeof(con.clnRnd));
  inc(pck.s,sizeof(con.clnRnd));
  end else begin;
  move(con.clnRnd,pck.d[pck.s+1],sizeof(con.clnRnd));
  inc(pck.s,sizeof(con.clnRnd));
  move(con.srvRnd,pck.d[pck.s+1],sizeof(con.srvRnd));
  inc(pck.s,sizeof(con.srvRnd));
  end;
End;

Procedure finished(a:String;ps:LongInt);
Var i:LongInt;
Begin;
pck1.s:=0;
ctx:=con.macLRb;
CryptoAddHasher(ctx,a[1],length(a));
CryptoAddHasher(ctx,con.secret,sizeof(con.secret));
addPadding(ctx,48,$36);
CryptoFinHasher(ctx,pck2.d,pck2.s);
CryptoBegHasher(ctx,md5algoNum,'','');
CryptoAddHasher(ctx,con.secret,sizeof(con.secret));
addPadding(ctx,48,$5c);
CryptoAddHasher(ctx,pck2.d,pck2.s);
CryptoFinHasher(ctx,pck1.d[pck1.s+1],i);
inc(pck1.s,i);
ctx:=con.macLRa;
CryptoAddHasher(ctx,a[1],length(a));
CryptoAddHasher(ctx,con.secret,sizeof(con.secret));
addPadding(ctx,40,$36);
CryptoFinHasher(ctx,pck2.d,pck2.s);
CryptoBegHasher(ctx,sha1algoNum,'','');
CryptoAddHasher(ctx,con.secret,sizeof(con.secret));
addPadding(ctx,40,$5c);
CryptoAddHasher(ctx,pck2.d,pck2.s);
CryptoFinHasher(ctx,pck1.d[pck1.s+1],i);
inc(pck1.s,i);
a[0]:=chr(pck1.s);
move(pck1.d,a[1],length(a));
move(a,con.recDat[sizeof(con.recDat)-ps],92);
End;

Function partition(s:LongInt):String;
Var a:String;
Begin;
a[0]:=chr(s);
move(pck1.d[pck1.t+1],a[1],s);
inc(pck1.t,s);
partition:=a;
End;

Var
  clnt_mac,srvr_mac:String;
  clnt_key,srvr_key:String;
  clnt_iv,srvr_iv:String;
  i:LongInt;
Begin;
con.macSiz:=HasherAlgoList[con.planMac].s;
con.encSiz:=CipherAlgoList[con.planEnc].s;
finished('CLNT',100);
finished('SRVR',200);
pck1.s:=sizeof(con.secret);
move(con.secret,pck1.d,sizeof(con.secret));
pck2.s:=0;
addRands(pck2,true);
ssl_pseudoRandomFnc(pck1,pck2,256);
i:=HasherAlgoList[con.planMac].s;
clnt_mac:=partition(i);
srvr_mac:=partition(i);
i:=CipherAlgoList[con.planEnc].ks;
if con.planExp then i:=CipherAlgoList[con.planEnc].es;
clnt_key:=partition(i);
srvr_key:=partition(i);
i:=CipherAlgoList[con.planEnc].s;
clnt_iv:=partition(i);
srvr_iv:=partition(i);
if con.planExp then begin;
  putStrng(pck1,clnt_key);
  addRands(pck1,false);
  CryptoBegHasher(ctx,md5algoNum,'','');
  CryptoAddHasher(ctx,pck1.d,pck1.s);
  CryptoFinHasher(ctx,clnt_key[1],i);
  clnt_key[0]:=chr(i);
  putStrng(pck1,srvr_key);
  addRands(pck1,true);
  CryptoBegHasher(ctx,md5algoNum,'','');
  CryptoAddHasher(ctx,pck1.d,pck1.s);
  CryptoFinHasher(ctx,srvr_key[1],i);
  srvr_key[0]:=chr(i);
  pck1.s:=0;
  addRands(pck1,false);
  CryptoBegHasher(ctx,md5algoNum,'','');
  CryptoAddHasher(ctx,pck1.d,pck1.s);
  CryptoFinHasher(ctx,clnt_iv[1],i);
  clnt_iv[0]:=chr(i);
  pck1.s:=0;
  addRands(pck1,true);
  CryptoBegHasher(ctx,md5algoNum,'','');
  CryptoAddHasher(ctx,pck1.d,pck1.s);
  CryptoFinHasher(ctx,srvr_iv[1],i);
  srvr_iv[0]:=chr(i);
  i:=CipherAlgoList[con.planEnc].ks;
  clnt_key:=copy(clnt_key,1,i);
  srvr_key:=copy(srvr_key,1,i);
  i:=CipherAlgoList[con.planEnc].s;
  clnt_iv:=copy(clnt_iv,1,i);
  srvr_iv:=copy(srvr_iv,1,i);
  end;
i:=CipherAlgoList[con.planEnc].a;
CryptoBegCipher(encC,i,clnt_key,clnt_iv);
CryptoBegCipher(encS,i,srvr_key,srvr_iv);
i:=HasherAlgoList[con.planMac].a;
CryptoBegHasher(macC1,i,'','');
CryptoBegHasher(macS1,i,'','');
CryptoAddHasher(macC1,clnt_mac[1],length(clnt_mac));
CryptoAddHasher(macS1,srvr_mac[1],length(srvr_mac));
macC2:=macC1;
macS2:=macS1;
case con.planMac of
  1:i:=48; {md5}
  2:i:=40; {sha1}
  else i:=0;
  end;
addPadding(macC1,i,$36);
addPadding(macS1,i,$36);
addPadding(macC2,i,$5c);
addPadding(macS2,i,$5c);
End;





Function generateMasterSecret(var con:OneConnectionRecord;var prem:OnePacketRecord):Boolean;
Var
  mast:OnePacketRecord;
  a:String;
Begin;
generateMasterSecret:=True;
case con.vers of
  $300:begin; {ssl}
    mast.s:=0;
    move(con.clnRnd,mast.d[mast.s+1],sizeof(con.clnRnd));
    inc(mast.s,sizeof(con.clnRnd));
    move(con.srvRnd,mast.d[mast.s+1],sizeof(con.srvRnd));
    inc(mast.s,sizeof(con.srvRnd));
    ssl_pseudoRandomFnc(prem,mast,sizeof(con.secret));
    end;
  $301,$302:begin; {tls}
    a:='master secret';
    mast.s:=length(a);
    move(a[1],mast.d,mast.s);
    move(con.clnRnd,mast.d[mast.s+1],sizeof(con.clnRnd));
    inc(mast.s,sizeof(con.clnRnd));
    move(con.srvRnd,mast.d[mast.s+1],sizeof(con.srvRnd));
    inc(mast.s,sizeof(con.srvRnd));
    tls_pseudoRandomFnc(prem,mast,sizeof(con.secret));
    end;
  else exit;
  end;
move(prem.d,con.secret,sizeof(con.secret));
generateMasterSecret:=False;
End;

Function generateKeyBlk(var con:OneConnectionRecord):Boolean;
Begin;
generateKeyBlk:=True;
case con.peerTp of
  1:case con.vers of {client}
    $300:ssl_generateKeyBlock(con,con.macLRa,con.macLRb,con.encLRc,con.macRLa,con.macRLb,con.encRLc); {ssl}
    $301,$302:tls_generateKeyBlock(con,con.macLRa,con.macLRb,con.encLRc,con.macRLa,con.macRLb,con.encRLc); {tls}
    else exit;
    end;
  2:case con.vers of {server}
    $300:ssl_generateKeyBlock(con,con.macRLa,con.macRLb,con.encRLc,con.macLRa,con.macLRb,con.encLRc); {ssl}
    $301,$302:tls_generateKeyBlock(con,con.macRLa,con.macRLb,con.encRLc,con.macLRa,con.macLRb,con.encLRc); {tls}
    else exit;
    end;
  else exit;
  end;
generateKeyBlk:=False;
End;



Procedure dumpOnePacket(a:String;var pck:OnePacketRecord);
Var i:LongInt;
Begin;
Write(a);
for i:=1 to pck.s do write(' '+byte2hextype(pck.d[i]));
WriteLn('');
End;
