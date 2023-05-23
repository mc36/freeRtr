Type
  OnePacketRecord=record
    s:LongInt;
    d:array[1..4*1024] of byte;
    end;
  OnePersonRecord=record
    commonName:String;
    email:String;
    country:String;
    state:String;
    locality:String;
    orgName:String;
    orgUnit:String;
    end;
Var
  sha1algoNum:LongInt;
  md5algoNum:LongInt;


Procedure ExportPublicKeyDSS(var key:OneDSSkeyRecord;var out:OnePacketRecord);
Var t1,t2:OnePacketRecord;
Begin;
t1.s:=0;
writeASN1integer(t1.d,t1.s,key.p);
writeASN1integer(t1.d,t1.s,key.q);
writeASN1integer(t1.d,t1.s,key.g);
t2.s:=0;
writeASN1object(t2.d,t2.s,ASN1objectId_DSSencrypt);
writeASN1sequence(t2.d,t2.s,t1.d,t1.s);
t1.s:=0;
writeASN1sequence(t1.d,t1.s,t2.d,t2.s);
t2.s:=1;
t2.d[1]:=0;
writeASN1integer(t2.d,t2.s,key.y);
out.s:=0;
writeASN1bitstring(out.d,out.s,t2.d,t2.s);
addASN1buffer(t1.d,t1.s,out.d,out.s);
out.s:=0;
writeASN1sequence(out.d,out.s,t1.d,t1.s);
End;



Procedure ExportPublicKeyRSA(var key:OneRSAkeyRecord;var out:OnePacketRecord);
Var t1,t2:OnePacketRecord;
Begin;
t1.s:=0;
writeASN1integer(t1.d,t1.s,key.n);
writeASN1integer(t1.d,t1.s,key.e);
t2.s:=1;
t2.d[1]:=0;
writeASN1sequence(t2.d,t2.s,t1.d,t1.s);
t1.s:=0;
writeASN1bitstring(t1.d,t1.s,t2.d,t2.s);
out.s:=0;
writeASN1object(out.d,out.s,ASN1objectId_RSAencrypt);
writeASN1null(out.d,out.s);
t2.s:=0;
writeASN1sequence(t2.d,t2.s,out.d,out.s);
addASN1buffer(t2.d,t2.s,t1.d,t1.s);
out.s:=0;
writeASN1sequence(out.d,out.s,t2.d,t2.s);
End;



Procedure ExportPersonalityData(var per:OnePersonRecord;var out:OnePacketRecord);
Var t1,t2:OnePacketRecord;

Procedure add(o,v:String;t:LongInt);
Begin;
if (v='') then exit;
out.s:=0;
writeASN1object(out.d,out.s,o);
writeASN1header(out.d,out.s,0,t,length(v));
addASN1buffer(out.d,out.s,v[1],length(v));
t2.s:=0;
writeASN1sequence(t2.d,t2.s,out.d,out.s);
writeASN1setof(t1.d,t1.s,t2.d,t2.s);
End;

Begin;
t1.s:=0;
add(ASN1objectId_commonName,per.commonName,19);
add(ASN1objectId_emailAddress,per.email,22);
add(ASN1objectId_countryName,per.country,19);
add(ASN1objectId_stateOrProvince,per.state,19);
add(ASN1objectId_localityName,per.locality,19);
add(ASN1objectId_organization,per.orgName,19);
add(ASN1objectId_organizationalUnit,per.orgUnit,19);
out.s:=0;
writeASN1sequence(out.d,out.s,t1.d,t1.s);
End;



Procedure ExportRequestData(var per,pub,out:OnePacketRecord);
Var
  tmp:OnePacketRecord;
  bn:BigNumContextRecord;
Begin;
BigNumStoreInt(0,bn);
tmp.s:=0;
writeASN1integer(tmp.d,tmp.s,bn);
addASN1buffer(tmp.d,tmp.s,per.d,per.s);
addASN1buffer(tmp.d,tmp.s,pub.d,pub.s);
writeASN1header(tmp.d,tmp.s,$a0,0,0);
out.s:=0;
writeASN1sequence(out.d,out.s,tmp.d,tmp.s);
End;




Procedure ExportCertificateData(alg,valb,vale:String;self:Boolean;ser:LongInt;var ca,usr,pub,out:OnePacketRecord);
Var
  bn:BigNumContextRecord;
  tmp:OnePacketRecord;
Begin;
tmp.s:=0;
if self then begin;
  out.s:=0;
  BigNumStoreInt(2,bn);
  writeASN1integer(out.d,out.s,bn);
  writeASN1header(tmp.d,tmp.s,$a0,0,out.s);
  move(out.d,tmp.d[tmp.s+1],out.s);
  inc(tmp.s,out.s);
  end;
BigNumStoreInt(ser,bn);
writeASN1integer(tmp.d,tmp.s,bn);
out.s:=0;
writeASN1object(out.d,out.s,alg);
if (alg<>ASN1objectId_DSSwithSHA1) then writeASN1null(out.d,out.s);
writeASN1sequence(tmp.d,tmp.s,out.d,out.s);
addASN1buffer(tmp.d,tmp.s,ca.d,ca.s);
out.s:=0;
writeASN1header(out.d,out.s,0,23,length(valb));
move(valb[1],out.d[out.s+1],length(valb));
inc(out.s,length(valb));
writeASN1header(out.d,out.s,0,23,length(vale));
move(vale[1],out.d[out.s+1],length(vale));
inc(out.s,length(vale));
writeASN1sequence(tmp.d,tmp.s,out.d,out.s);
addASN1buffer(tmp.d,tmp.s,usr.d,usr.s);
addASN1buffer(tmp.d,tmp.s,pub.d,pub.s);
out.s:=0;
writeASN1sequence(out.d,out.s,tmp.d,tmp.s);
End;




Procedure signDataWithRSA(var key:OneRSAkeyRecord;var buf,out:OnePacketRecord);
Var
  s:BigNumContextRecord;
  tmp:OnePacketRecord;
Begin;
CryptoImmHasher(md5algoNum,'','',buf.d,buf.s);
tmp.s:=0;
writeASN1object(tmp.d,tmp.s,ASN1objectId_hasherMD5);
writeASN1null(tmp.d,tmp.s);
out.s:=0;
writeASN1sequence(out.d,out.s,tmp.d,tmp.s);
writeASN1octetstr(out.d,out.s,buf.d,buf.s);
buf.s:=0;
writeASN1sequence(buf.d,buf.s,out.d,out.s);
SignDataWithRSAkey(key,s,buf.d,buf.s);
buf.s:=0;
BigNumConvBN2MSB(s,buf.s,buf.d);
if (buf.d[1]<>0) then begin;
  move(buf.d,buf.d[2],buf.s);
  inc(buf.s);
  buf.d[1]:=0;
  end;
tmp.s:=0;
writeASN1object(tmp.d,tmp.s,ASN1objectId_RSAwithMD5);
writeASN1null(tmp.d,tmp.s);
out.s:=0;
writeASN1sequence(out.d,out.s,tmp.d,tmp.s);
writeASN1bitstring(out.d,out.s,buf.d,buf.s);
End;



Procedure signDataWithDSS(var key:OneDSSkeyRecord;var buf,out:OnePacketRecord);
Var
  r,s:BigNumContextRecord;
  tmp:OnePacketRecord;
Begin;
CryptoImmHasher(sha1algoNum,'','',buf.d,buf.s);
SignDataWithDSSkey(key,r,s,buf.d,buf.s);
tmp.s:=0;
writeASN1object(tmp.d,tmp.s,ASN1objectId_DSSwithSHA1);
out.s:=0;
writeASN1sequence(out.d,out.s,tmp.d,tmp.s);
tmp.s:=0;
writeASN1integer(tmp.d,tmp.s,r);
writeASN1integer(tmp.d,tmp.s,s);
buf.s:=1;
buf.d[1]:=0;
writeASN1sequence(buf.d,buf.s,tmp.d,tmp.s);
writeASN1bitstring(out.d,out.s,buf.d,buf.s);
End;



Function ImportRequestData(var in,per,pub:OnePacketRecord):Boolean;
Var i,o,p,q:LongInt;

Function get(var d:OnePacketRecord):Boolean;
Begin;
get:=true;
readASN1header(in.d,q,i,o,p);
if (o<>16) then exit;
if (q+p>in.s) then exit;
d.s:=0;
writeASN1sequence(d.d,d.s,in.d[q+1],p);
inc(q,p);
get:=false;
End;

Begin;
ImportRequestData:=True;
q:=0;
readASN1header(in.d,q,i,o,p);
if (o<>16) then exit;
if (q+p<>in.s) then exit;
readASN1header(in.d,q,i,o,p);
if (o<>16) then exit;
if (q+p>in.s) then exit;
readASN1header(in.d,q,i,o,p);
if (o<>2) then exit;
inc(q,p);
if (q>in.s) then exit;
if get(per) then exit;
if get(pub) then exit;
ImportRequestData:=False;
End;



Function verifyDataWithRSA(var key:OneRSAkeyRecord;var buf,sig:OnePacketRecord):Boolean;
Var
  s:BigNumContextRecord;
  tmp1,tmp2:OnePacketRecord;
  i,o,p,q:LongInt;
Begin;
verifyDataWithRSA:=True;
CryptoImmHasher(md5algoNum,'','',buf.d,buf.s);
tmp1.s:=0;
writeASN1object(tmp1.d,tmp1.s,ASN1objectId_hasherMD5);
writeASN1null(tmp1.d,tmp1.s);
tmp2.s:=0;
writeASN1sequence(tmp2.d,tmp2.s,tmp1.d,tmp1.s);
writeASN1octetstr(tmp2.d,tmp2.s,buf.d,buf.s);
buf.s:=0;
writeASN1sequence(buf.d,buf.s,tmp2.d,tmp2.s);
q:=0;
readASN1header(sig.d,q,i,o,p);
if (o<>16) then exit;
inc(q,p);
readASN1header(sig.d,q,i,o,p);
if (o<>3) then exit;
BigNumConvMSB2BN(p,sig.d[q+1],s);
if VerifySignWithRSA(key.n,key.e,s,buf.d,buf.s) then exit;
verifyDataWithRSA:=False;
End;



Function verifyDataWithDSS(var key:OneDSSkeyRecord;var buf,sig:OnePacketRecord):Boolean;
Var
  r,s:BigNumContextRecord;
  i,o,p,q:LongInt;
Begin;
verifyDataWithDSS:=True;
CryptoImmHasher(sha1algoNum,'','',buf.d,buf.s);
q:=0;
readASN1header(sig.d,q,i,o,p);
if (o<>16) then exit;
inc(q,p);
readASN1header(sig.d,q,i,o,p);
if (o<>3) then exit;
inc(q);
readASN1header(sig.d,q,i,o,p);
if (o<>16) then exit;
if readASN1integer(sig.d,q,r) then exit;
if readASN1integer(sig.d,q,s) then exit;
if VerifySignWithDSS(key.p,key.q,key.g,key.y,r,s,buf.d,buf.s) then exit;
verifyDataWithDSS:=False;
End;
