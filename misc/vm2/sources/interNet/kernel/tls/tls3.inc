Procedure duplicateConnect;
Begin;
connEmpty:=con;
connEmpty.pipeT:=0;
connEmpty.pipeL:=0;
End;

Procedure unpackCiphersuite(p:LongInt);
Begin;
con.planKex:=(p shr 16) and $f;
con.planCrt:=(p shr 12) and $f;
con.planEnc:=(p shr 8) and $f;
con.planMac:=(p shr 4) and $f;
con.planExp:=(p and $f<>0);
End;



Procedure clnt_init;
Begin;
for i:=1 to sizeof(con.clnRnd) do con.clnRnd[i]:=random($100);
con.peerTp:=1;
End;



Procedure clnt_hllT(c:String;dtg:Boolean);
Begin;
con.vers:=protocolVersionMin;
CryptoBegHasher(con.macLRa,sha1algoNum,'','');
CryptoBegHasher(con.macLRb,md5algoNum,'','');
i:=FindSessionIDclnt(con.addr,con.port);
if (i>0) then begin;
  connEmpty:=ConnectionDat^[i];
  con.sessID:=connEmpty.sessID;
  con.vers:=connEmpty.vers;
  move(connEmpty.secret,con.secret,sizeof(con.secret));
  informClient('plan.kex:reuse');
  end;
i:=protocolVersionMax;
if dtg then i:=convertSSL2DTLSversion(i);
writeWordMSB(pck1.d[1],i);
move(con.clnRnd,pck1.d[3],sizeof(con.clnRnd));
move(con.sessID,pck1.d[35],sizeof(con.sessID));
i:=length(con.sessID)+35;
if dtg then begin;
  move(c,pck1.d[i+1],sizeof(c));
  inc(i,length(c)+1);
  end;
writeWordMSB(pck1.d[i+1],CipherSuitesNum*2);
inc(i,2);
for o:=1 to CipherSuitesNum do begin;
  writeWordMSB(pck1.d[i+1],CipherSuitesDat[o]);
  inc(i,2);
  end;
pck1.d[i+1]:=1;
pck1.d[i+2]:=0;
pck1.s:=i+2;
pck1.t:=1;
clearHndshkBuf(con);
sendPackHndshk(con,pck1);
if dtg then con.vers:=0;
con.stat:=6;
End;



Procedure clnt_hllR(dtg:Boolean);
Label err;
Begin;
if (pck1.t=3) then begin; {hello verify}
  move(pck1.d[3],c,sizeof(c));
  clnt_hllT(c,true);
  exit;
  end;
if (pck1.t<>2) then begin; err:con.stat:=0;exit; end;
con.vers:=readWordMSB(pck1.d[1]);
if dtg then begin;
  con.vers:=convertDTLS2SSLversion(con.vers);
  case con.vers of
    $0302:a:='dtls 1.0';
    else goto err;
    end;
  end else begin;
  case con.vers of
    $0300:a:='ssl 3.0';
    $0301:a:='tls 1.0';
    $0302:a:='tls 1.1';
    else goto err;
    end;
  end;
informClient('version:'+a);
move(pck1.d[3],con.srvRnd,sizeof(con.srvRnd));
move(pck1.d[35],b,sizeof(b));
i:=length(b)+35;
p:=readWordMSB(pck1.d[i+1]);
if (pck1.d[i+3]<>0) then goto err;
p:=decodeOneCipherSuite(p);
if (p=0) then goto err;
unpackCiphersuite(p);
case con.planKex of
  1:a:='dh';
  2:a:='rsa';
  else goto err;
  end;
informClient('plan.kex:'+a);
case con.planCrt of
  1:a:='dss';
  2:a:='rsa';
  else goto err;
  end;
informClient('plan.cert:'+a);
case con.planEnc of
  1:a:='aes';
  2:a:='des';
  3:a:='3des';
  4:a:='rc2';
  5:begin; a:='rc4';if dtg then goto err; end;
  else goto err;
  end;
informClient('plan.enc:'+a);
case con.planMac of
  1:a:='md5';
  2:a:='sha1';
  else goto err;
  end;
informClient('plan.mac:'+a);
if con.planExp then a:='yes' else a:='no';
informClient('plan.exp:'+a);
if (b<>'') and (con.sessID=b) then begin;
  informClient('hostkey:reuse');
  con.stat:=12;
  exit;
  end;
con.sessID:=b;
con.stat:=7;
End;



Procedure clnt_cert;
Label err;
Begin;
if (pck1.t<>11) then begin; err:con.stat:=0;exit; end;
if (readLongMSB(pck1.d[1]) shr 8<>pck1.s-3) then goto err;
if (pck1.s<6) then goto err;
p:=3;
while (p<pck1.s) do begin;
  q:=readLongMSB(pck1.d[p+1]) shr 8;
  inc(p,3);
  if (p+q>pck1.s) then goto err;
  move(pck1.d[p+1],pck2,q);
  informClientBin('cert:',q,pck2);
  inc(p,q);
  end;
i:=readLongMSB(pck1.d[4]) shr 8;
fillchar(pck2,sizeof(pck2),0);
move(pck1.d[7],pck2.d,i);
pck2.s:=i;
pck1.s:=0;
case con.planCrt of
  1:begin; {dss}
    if GetDSSkeyFromCertificate(pck2,num1,num2,num3,num4) then goto err;
    PutBinaryNumber(pck1,pck1.s,num1);
    PutBinaryNumber(pck1,pck1.s,num2);
    PutBinaryNumber(pck1,pck1.s,num3);
    PutBinaryNumber(pck1,pck1.s,num4);
    a:='dss '+BStr(BigNumSizeInBits(num1));
    end;
  2:begin; {rsa}
    if GetRSAkeyFromCertificate(pck2,num1,num2) then goto err;
    PutBinaryNumber(pck1,pck1.s,num1);
    PutBinaryNumber(pck1,pck1.s,num2);
    a:='rsa '+BStr(BigNumSizeInBits(num1));
    end;
  else goto err;
  end;
informClient('hostcert:'+a);
move(pck1,con.encRLc,sizeof(con.encRLc));
move(pck1,con.encLRc,sizeof(con.encLRc));
con.stat:=8;
if (con.planKex<>2) then exit;
if (con.planCrt<>2) then exit;
con.stat:=9;
if not con.planExp then exit;
if (BigNumSizeInBits(num1)>512) then con.stat:=8;
End;



Procedure clnt_kex;
Label err;
Begin;
if (pck1.t<>12) then begin; err:con.stat:=0;exit; end;
pck2.s:=0;
p:=0;
case con.planKex of
  1:begin; {dh}
    if GetBinaryNumber(pck1,p,num1) then goto err;
    if GetBinaryNumber(pck1,p,num2) then goto err;
    if GetBinaryNumber(pck1,p,num3) then goto err;
    PutBinaryNumber(pck2,pck2.s,num1);
    PutBinaryNumber(pck2,pck2.s,num2);
    PutBinaryNumber(pck2,pck2.s,num3);
    a:='dh '+BStr(BigNumSizeInBits(num1));
    end;
  2:begin; {rsa}
    if GetBinaryNumber(pck1,p,num1) then goto err;
    if GetBinaryNumber(pck1,p,num2) then goto err;
    PutBinaryNumber(pck2,pck2.s,num1);
    PutBinaryNumber(pck2,pck2.s,num2);
    a:='rsa '+BStr(BigNumSizeInBits(num1));
    end;
  else goto err;
  end;
informClient('hostkey:'+a);
move(pck2,con.encLRc,sizeof(con.encLRc));
i:=0;
move(con.clnRnd,pck2.d[i+1],sizeof(con.clnRnd));
inc(i,sizeof(con.clnRnd));
move(con.srvRnd,pck2.d[i+1],sizeof(con.srvRnd));
inc(i,sizeof(con.srvRnd));
move(pck1.d,pck2.d[i+1],p);
pck2.s:=i+p;
if GetBinaryNumber(pck1,p,num1) then goto err;
if (p<>pck1.s) then goto err;
pck3.s:=0;
PutBinaryNumber(pck3,pck3.s,num1);
dec(pck3.s,2);
move(pck3.d[3],pck3.d,pck3.s);
pck1:=pck2;
CryptoImmHasher(sha1algoNum,'','',pck1.d,pck1.s);
CryptoImmHasher(md5algoNum,'','',pck2.d,pck2.s);
case con.planCrt of
  1:begin; {dss}
    move(con.encRLc,pck2,sizeof(con.encRLc));
    p:=0;
    if GetBinaryNumber(pck2,p,num1) then goto err;
    if GetBinaryNumber(pck2,p,num2) then goto err;
    if GetBinaryNumber(pck2,p,num3) then goto err;
    if GetBinaryNumber(pck2,p,num4) then goto err;
    if GetDSSparamFromSignature(pck3,num5,num6) then goto err;
    if VerifySignWithDSS(num1,num2,num3,num4,num5,num6,pck1.d,pck1.s) then goto err;
    end;
  2:begin; {rsa}
    move(pck1.d,pck2.d[pck2.s+1],pck1.s);
    inc(pck2.s,pck1.s);
    move(con.encRLc,pck1,sizeof(con.encRLc));
    p:=0;
    if GetBinaryNumber(pck1,p,num1) then goto err;
    if GetBinaryNumber(pck1,p,num2) then goto err;
    if GetRSAparamFromSignature(pck3,num3) then goto err;
    if VerifySignWithRSA(num1,num2,num3,pck2.d,pck2.s) then goto err;
    end;
  else goto err;
  end;
con.stat:=9;
End;



Procedure clnt_done;
Label err;
Begin;
if (pck1.t=13) then exit; {certificate request}
if (pck1.t<>14) then begin; err:con.stat:=0;exit; end;
if (pck1.s<>0) then goto err;
move(con.encLRc,pck2,sizeof(con.encLRc));
case con.planKex of
  1:begin; {dh}
    p:=0;
    if GetBinaryNumber(pck2,p,num1) then goto err;
    if GetBinaryNumber(pck2,p,num2) then goto err;
    if GetBinaryNumber(pck2,p,num3) then goto err;
    i:=BigNumSizeInBits(num1);
    o:=i-2;
    i:=i div hostKeyDHdiv;
    if (i<hostKeyDHmin) then i:=hostKeyDHmin;
    if (i>hostKeyDHmax) then i:=hostKeyDHmax;
    if (i>o) then i:=o;
    GetRndBigNum(num4,i);
    BigNumPowerMod(num2,num4,num1,num5);
    BigNumPowerMod(num3,num4,num1,num6);
    pck1.s:=0;
    PutBinaryNumber(pck1,pck1.s,num6);
    move(pck1.d[3],pck1.d,pck1.s);
    dec(pck1.s,2);
    end;
  2:begin; {rsa}
    p:=0;
    if GetBinaryNumber(pck2,p,num1) then goto err;
    if GetBinaryNumber(pck2,p,num2) then goto err;
    pck1.s:=48;
    for i:=1 to pck1.s do pck1.d[i]:=random($100);
    WriteWordMSB(pck1.d,protocolVersionMax);
    pck2:=pck1;
    PadRSAwithPKCS1t2(pck2.d,pck2.s,BigNumSizeInBits(num1),num5);
    BigNumPowerMod(num5,num2,num1,num5);
    end;
  else goto err;
  end;
pck2.s:=0;
PutBinaryNumber(pck2,pck2.s,num5);
if (con.planKex=2) and (con.vers=$300) then begin;
  move(pck2.d[3],pck2.d,pck2.s);
  dec(pck2.s,2);
  end;
clearHndshkBuf(con);
pck2.t:=16;
sendPackHndshk(con,pck2);
if generateMasterSecret(con,pck1) then goto err;
duplicateConnect;
if generateKeyBlk(con) then goto err;
a:=#1;
pck1.t:=$1000+20;
pck1.s:=length(a);
move(a[1],pck1.d,pck1.s);
sendPackHndshk(con,pck1);
con.pckLRa:=0;
con.pckLRb:=0;
con.pckRLa:=0;
con.pckRLb:=0;
pck1.t:=20;
move(con.recDat[sizeof(con.recDat)-100],a,sizeof(a));
pck1.s:=length(a);
move(a[1],pck1.d,pck1.s);
pck2:=pck1;
con.secure:=1;
sendPackHndshk(con,pck1);
con.secure:=0;
sendPackHndshk(connEmpty,pck2);
if generateKeyBlk(connEmpty) then goto err;
con.recDat:=connEmpty.recDat;
con.stat:=10;
End;



Procedure clnt_chg;
Label err;
Begin;
if (pck1.t=21) then exit; {alert}
if (pck1.t<>20) then begin; err:con.stat:=0;exit; end;
if (pck1.s<>1) then goto err;
if (pck1.d[1]<>1) then goto err;
con.secure:=1;
con.stat:=11;
End;



Procedure clnt_fnsh;
Label err;
Begin;
if (pck1.t<>20) then begin; err:con.stat:=0;exit; end;
move(con.recDat[sizeof(con.recDat)-200],a,sizeof(a));
b[0]:=chr(pck1.s);
move(pck1.d,b[1],length(b));
if (a<>b) then goto err;
con.secure:=2;
informClient('secure');
con.stat:=77;
End;



Procedure clnt_chgRe;
Label err;
Begin;
if (pck1.t=21) then exit; {alert}
if (pck1.t<>20) then begin; err:con.stat:=0;exit; end;
if (pck1.s<>1) then goto err;
if (pck1.d[1]<>1) then goto err;
duplicateConnect;
if generateKeyBlk(con) then goto err;
con.macLRa:=connEmpty.macLRa;
con.macLRb:=connEmpty.macLRb;
con.pckRLa:=0;
con.pckRLb:=0;
con.secure:=1;
con.stat:=13;
End;



Procedure clnt_fnshRe;
Label err;
Begin;
if (pck1.t<>20) then begin; err:con.stat:=0;exit; end;
move(con.recDat[sizeof(con.recDat)-200],a,sizeof(a));
b[0]:=chr(pck1.s);
move(pck1.d,b[1],length(b));
if (a<>b) then goto err;
con.secure:=0;
duplicateConnect;
clearHndshkBuf(con);
changeSeqOrder(connEmpty,-1);
sendPackHndshk(connEmpty,pck1);
if generateKeyBlk(connEmpty) then goto err;
con.macLRa:=connEmpty.macLRa;
con.macLRb:=connEmpty.macLRb;
con.recDat:=connEmpty.recDat;
a:=#1;
pck1.t:=$1000+20;
pck1.s:=length(a);
move(a[1],pck1.d,pck1.s);
sendPackHndshk(con,pck1);
pck1.t:=20;
move(con.recDat[sizeof(con.recDat)-100],a,sizeof(a));
pck1.s:=length(a);
move(a[1],pck1.d,pck1.s);
con.secure:=2;
con.pckLRa:=0;
con.pckLRb:=0;
sendPackHndshk(con,pck1);
informClient('secure');
con.stat:=77;
End;
