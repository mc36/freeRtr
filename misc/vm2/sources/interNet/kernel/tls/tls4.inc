Procedure got_closing;
Begin;
if (pck1.t<>21) then exit;
con.stat:=0;
End;



Procedure send_closing;
Begin;
clearHndshkBuf(con);
a:=#1#0;
pck1.t:=$1000+21;
pck1.s:=length(a);
move(a[1],pck1.d,pck1.s);
sendPackHndshk(con,pck1);
con.stat:=78;
End;



Procedure serv_init;
Begin;
for i:=1 to sizeof(con.srvRnd) do con.srvRnd[i]:=random($100);
con.peerTp:=2;
con.vers:=0;
CryptoBegHasher(con.macLRa,sha1algoNum,'','');
CryptoBegHasher(con.macLRb,md5algoNum,'','');
End;


Procedure serv_hello(c:String;dtg:Boolean);
Label err;

Function doVersionChange:Boolean;
Begin;
doVersionChange:=True;
i:=con.vers;
if dtg then i:=convertDTLS2SSLversion(i);
if (i<protocolVersionMin) then begin;
  con.err:='protocol version too small';
  exit;
  end;
if (i>protocolVersionMax) then i:=protocolVersionMax;
con.vers:=i;
doVersionChange:=False;
End;

Begin;
clearHndshkBuf(con);
{client hello}
if (con.vers shr 8<>2) then begin;
{ssl3, tls1}
  if (pck1.t<>1) then begin; err:con.stat:=0;exit; end;
  con.vers:=ReadWordMSB(pck1.d);
  move(pck1.d[3],con.clnRnd,sizeof(con.clnRnd));
  move(pck1.d[35],a,sizeof(a));
  i:=length(a)+35;
  con.sessID:=a;
  if dtg then begin;
    inc(i);
    move(pck1.d[i],a,sizeof(a));
    inc(i,length(a));
    end else a:='';
  if (a<>c) then begin;
    if doVersionChange then goto err;
    {hello verify}
    if dtg then i:=convertSSL2DTLSversion(con.vers) else i:=con.vers;
    WriteWordMSB(pck1.d,i);
    move(c,pck1.d[3],sizeof(c));
    pck1.s:=length(c)+3;
    pck1.t:=3;
    sendPackHndshk(con,pck1);
    con.vers:=0;
    CryptoBegHasher(con.macLRa,sha1algoNum,'','');
    CryptoBegHasher(con.macLRb,md5algoNum,'','');
    exit;
    end;
  o:=ReadWordMSB(pck1.d[i+1]);
  inc(i,2);
  if (i+o>pck1.s) then goto err;
  if (o mod 2<>0) then goto err;
  pck2.s:=o;
  move(pck1.d[i+1],pck2.d,o);
  inc(i,o);
  move(pck1.d[i+1],a,sizeof(a));
  inc(i,length(a)+1);
  if (i>pck1.s) then goto err;
  if (pos(#0,a)<1) then goto err;
  end else begin;
{ssl2}
  CryptoAddHasher(con.macLRa,pck1.d,pck1.s);
  CryptoAddHasher(con.macLRb,pck1.d,pck1.s);
  if (pck1.d[1]<>1) then goto err;
  con.vers:=ReadWordMSB(pck1.d[2]);
  i:=9;
  p:=ReadWordMSB(pck1.d[4]); {cipher}
  if (i+p>pck1.s) then goto err;
  pck2.s:=0;
  if (p mod 3<>0) then goto err;
  while (p>0) do begin;
    o:=ReadLongMSB(pck1.d[i+1]) shr 8;
    if (o and $ff0000=0) then begin;
      WriteWordMSB(pck2.d[pck2.s+1],o);
      inc(pck2.s,2);
      end;
    inc(i,3);
    dec(p,3);
    end;
  p:=ReadWordMSB(pck1.d[6]); {session}
  if (i+p>pck1.s) then goto err;
  a[0]:=chr(p);
  move(pck1.d[i+1],a[1],length(a));
  con.sessID:=a;
  inc(i,p);
  p:=ReadWordMSB(pck1.d[8]); {random}
  if (i+p>pck1.s) then goto err;
  if (p<8) then goto err;
  a[0]:=chr(p);
  move(pck1.d[i+1],a[1],length(a));
  while (length(a)<sizeof(con.clnRnd)) do a:=#0+a;
  while (length(a)>sizeof(con.clnRnd)) do a:=copy(a,2,255);
  move(a[1],con.clnRnd,sizeof(con.clnRnd));
  inc(i,p);
  end;
if doVersionChange then goto err;
p:=CipherSuitesNum+1;
i:=1;
while (i<pck2.s) do begin;
  q:=ReadWordMSB(pck2.d[i]);
  inc(i,2);
  for o:=1 to CipherSuitesNum do if (CipherSuitesDat[o]=q) then begin;
    if (o<p) then p:=o;
    break;
    end;
  end;
if (p>CipherSuitesNum) then begin;
  con.err:='no compatible ciphersuite';
  goto err;
  end;
p:=CipherSuitesDat[p];
i:=decodeOneCipherSuite(p);
if (i=0) then goto err;
unpackCiphersuite(i);
q:=FindSessionIDsrvr(con.addr,con.port,con.sessID);
if (q=0) then begin;
  repeat
    a[0]:=#32;
    for i:=1 to length(a) do a[i]:=chr(random($100));
    until (FindSessionIDsrvr(con.addr,con.port,a)=0);
  con.sessID:=a;
  end;
{server hello}
if dtg then i:=convertSSL2DTLSversion(con.vers) else i:=con.vers;
WriteWordMSB(pck1.d[1],i);
move(con.srvRnd,pck1.d[3],sizeof(con.srvRnd));
move(con.sessID,pck1.d[35],sizeof(con.sessID));
i:=length(con.sessID)+35;
WriteWordMSB(pck1.d[i+1],p);
pck1.d[i+3]:=0;
pck1.s:=i+3;
pck1.t:=2;
sendPackHndshk(con,pck1);
if (q<>0) then begin;
  move(ConnectionDat^[q].secret,con.secret,sizeof(con.secret));
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
  move(con.recDat[sizeof(con.recDat)-200],a,sizeof(a));
  pck1.s:=length(a);
  move(a[1],pck1.d,pck1.s);
  pck2:=pck1;
  con.secure:=1;
  sendPackHndshk(con,pck1);
  con.secure:=0;
  sendPackHndshk(connEmpty,pck2);
  if generateKeyBlk(connEmpty) then goto err;
  con.recDat:=connEmpty.recDat;
  con.stat:=104;
  exit;
  end;
{certificate}
case con.planCrt of
  1:pck1:=hostCrtDSS;
  2:pck1:=hostCrtRSA;
  else goto err;
  end;
pck1.t:=11;
sendPackHndshk(con,pck1);
{server key exchange}
pck3.s:=0;
case con.planKex of
  1:begin; {dh}
    PutBinaryNumber(pck3,pck3.s,hostKeyDH.p);
    PutBinaryNumber(pck3,pck3.s,hostKeyDH.g);
    i:=BigNumSizeInBits(hostKeyDH.p);
    o:=i-2;
    i:=i div hostKeyDHdiv;
    if (i<hostKeyDHmin) then i:=hostKeyDHmin;
    if (i>hostKeyDHmax) then i:=hostKeyDHmax;
    if (i>o) then i:=o;
    GetRndBigNum(num1,i);
    move(num1,con.encLRc,sizeof(con.encLRc));
    BigNumPowerMod(hostKeyDH.g,num1,hostKeyDH.p,num1);
    PutBinaryNumber(pck3,pck3.s,num1);
    p:=1;
    end;
  2:begin; {rsa}
    if con.planExp then begin;
      PutBinaryNumber(pck3,pck3.s,hostKeyRSAe.n);
      PutBinaryNumber(pck3,pck3.s,hostKeyRSAe.e);
      end else begin;
      PutBinaryNumber(pck3,pck3.s,hostKeyRSA.n);
      PutBinaryNumber(pck3,pck3.s,hostKeyRSA.e);
      end;
    p:=0;
    if (con.planCrt<>2) then p:=1;
    if con.planExp and (BigNumSizeInBits(hostKeyRSA.n)>512) then p:=1;
    end;
  else goto err;
  end;
if (p<>0) then begin;
  move(con.clnRnd,pck1.d,sizeof(con.clnRnd));
  pck1.s:=sizeof(con.clnRnd);
  move(con.srvRnd,pck1.d[pck1.s+1],sizeof(con.srvRnd));
  inc(pck1.s,sizeof(con.srvRnd));
  move(pck3.d,pck1.d[pck1.s+1],pck3.s);
  inc(pck1.s,pck3.s);
  pck2:=pck1;
  CryptoImmHasher(sha1algoNum,'','',pck1.d,pck1.s);
  CryptoImmHasher(md5algoNum,'','',pck2.d,pck2.s);
  case con.planCrt of
    1:begin; {dss}
      SignDataWithDSSkey(hostKeyDSS,num1,num2,pck1.d,pck1.s);
      PutDSSparamToSignature(pck1,num1,num2);
      end;
    2:begin; {rsa}
      move(pck1.d,pck2.d[pck2.s+1],pck1.s);
      inc(pck2.s,pck1.s);
      SignDataWithRSAkey(hostKeyRSA,num1,pck2.d,pck2.s);
      PutRSAparamToSignature(pck1,num1);
      end;
    else goto err;
    end;
  WriteWordMSB(pck3.d[pck3.s+1],pck1.s);
  inc(pck3.s,2);
  move(pck1.d,pck3.d[pck3.s+1],pck1.s);
  inc(pck3.s,pck1.s);
  pck3.t:=12;
  sendPackHndshk(con,pck3);
  end;
{server hello done}
pck1.t:=14;
pck1.s:=0;
sendPackHndshk(con,pck1);
con.stat:=101;
End;



Procedure serv_kex;
Label err;
Begin;
if (pck1.t<>16) then begin; err:con.stat:=0;exit; end;
if (con.planKex=2) and (con.vers=$300) then begin;
  move(pck1.d,pck1.d[3],pck1.s);
  WriteWordMSB(pck1.d,pck1.s);
  inc(pck1.s,2);
  end;
p:=0;
if GetBinaryNumber(pck1,p,num1) then goto err;
if (p<>pck1.s) then goto err;
case con.planKex of
  1:begin; {dh}
    move(con.encLRc,num2,sizeof(num2));
    BigNumPowerMod(num1,num2,hostKeyDH.p,num3);
    pck1.s:=0;
    PutBinaryNumber(pck1,pck1.s,num3);
    move(pck1.d[3],pck1.d,pck1.s);
    dec(pck1.s,2);
    end;
  2:begin; {rsa}
    if con.planExp then begin;
      BigNumPowerMod(num1,hostKeyRSAe.d,hostKeyRSAe.n,num2);
      end else begin;
      BigNumPowerMod(num1,hostKeyRSA.d,hostKeyRSA.n,num2);
      end;
    if UnpadRSAwithPKCS1(pck1.d,pck1.s,2,num2) then goto err;
    if (pck1.s<>48) then goto err;
    end;
  else goto err;
  end;
if generateMasterSecret(con,pck1) then goto err;
con.stat:=102;
End;



Procedure serv_chg;
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
con.secure:=1;
con.stat:=103;
End;



Procedure serv_fnsh;
Label err;
Begin;
if (pck1.t<>20) then begin; err:con.stat:=0;exit; end;
move(con.recDat[sizeof(con.recDat)-100],a,sizeof(a));
b[0]:=chr(pck1.s);
move(pck1.d,b[1],length(b));
if (a<>b) then goto err;
con.secure:=0;
duplicateConnect;
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
clearHndshkBuf(con);
sendPackHndshk(con,pck1);
con.pckLRa:=0;
con.pckLRb:=0;
pck1.t:=20;
move(con.recDat[sizeof(con.recDat)-200],a,sizeof(a));
pck1.s:=length(a);
move(a[1],pck1.d,pck1.s);
con.secure:=2;
sendPackHndshk(con,pck1);
con.stat:=77;
End;



Procedure serv_chgRe;
Label err;
Begin;
if (pck1.t=21) then exit; {alert}
if (pck1.t<>20) then begin; err:con.stat:=0;exit; end;
if (pck1.s<>1) then goto err;
if (pck1.d[1]<>1) then goto err;
con.secure:=1;
con.stat:=105;
End;



Procedure serv_fnshRe;
Label err;
Begin;
if (pck1.t<>20) then begin; err:con.stat:=0;exit; end;
move(con.recDat[sizeof(con.recDat)-100],a,sizeof(a));
b[0]:=chr(pck1.s);
move(pck1.d,b[1],length(b));
if (a<>b) then goto err;
con.secure:=2;
con.stat:=77;
End;
