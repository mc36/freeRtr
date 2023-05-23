Procedure gotOnePacket(var buffer;siz,loc:LongInt;var addr;port:LongInt);
Label f1,f2,esp,vege;
Var
  buf:array[1..1] of byte absolute buffer;
  isakmp:OneISAKMPheaderRecord absolute buf;
  ipsec:OneIPSECheaderRecord absolute buf;
  isahdr:LongInt;      {1-sa, 2-ke, 4-nonce, 8-id, 16-hash}
  genhdr:OneGenericMextHeaderRecord;
  secass:OneSecurityAssociationRecord;
  propos:OneProposalHeaderRecord;
  trans:OneTransformHeaderRecord;
  nonce:OneNonceHeaderRecord;
  keyex:OneKeyExchangeHeaderRecord;
  ident:OneIdentificateHeaderRecord;
  hash:OneHashingStateHeaderRecord;
  vendor:OneVendorIdHeaderRecord;
  natdt:OneNatDetectHeaderRecord;
  num1,num2,num3,num4,num5:BigNumContextRecord;
  con:OneConnectionRecord;
  ps,po,tp,num:LongInt;
  encC,hshC,grpC,timC,bytC,typ:LongInt;
  encB,hshB,grpB,timB,bytB:LongInt;
  i,o,p:LongInt;
  a,b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;

Function pointAlgo(e,h,g:LongInt):LongInt;
Var i,o:LongInt;
Begin;
if (isakmp.exTyp=32) then g:=2;
if (e<1) or (e>CipherAlgoMax) then i:=0 else i:=CipherAlgoList[e].p;
if (i<1) then i:=99999;
o:=i*100;
if (h<1) or (h>HasherAlgoMax) then i:=0 else i:=HasherAlgoList[h].p;
if (i<1) then i:=99999;
inc(o,i*10);
if (g<1) or (g>2) then i:=999999 else i:=10-g;
pointAlgo:=o+i;
End;

Procedure parseMMattr;
Begin;
case i of
  11:typ:=readWordMSB(a[1]);
  12:begin;
    while (ab0<4) do a:=#0+a;
    case typ of
      1:timC:=readLongMSB(a[1]);
      2:bytC:=readLongMSB(a[1]);
      end;
    typ:=0;
    end;
  1:case readWordMSB(a[1]) of
    7:encC:=1;    {aes}
    3:encC:=2;    {blowfish}
    5:encC:=3;    {3des}
    4:encC:=4;    {rc5}
    1:encC:=5;    {des}
    6:encC:=6;    {cast}
    end;
  2:case readWordMSB(a[1]) of
    1:hshC:=1;    {md5}
    2:hshC:=2;    {sha1}
    end;
  4:case readWordMSB(a[1]) of
    1:grpC:=1;
    2:grpC:=2;
    end;
  3:o:=readWordMSB(a[1]);
  end;
End;

Procedure parseQMattr;
Begin;
case i of
  4:begin;
    o:=1;
    case readWordMSB(a[1]) of
      1:con.isaN:=0;
      3:con.isaN:=3;
      61443:con.isaN:=2;
      else o:=0;
      end;
    end;
  1:typ:=readWordMSB(a[1]);
  2:begin;
    while (ab0<4) do a:=#0+a;
    case typ of
      1:timC:=readLongMSB(a[1]);
      2:bytC:=readLongMSB(a[1]);
      end;
    typ:=0;
    end;
  3:case readWordMSB(a[1]) of
    1:grpC:=1;
    2:grpC:=2;
    else grpC:=0;
    end;
  5:case readWordMSB(a[1]) of
    1:hshC:=1;    {md5}
    2:hshC:=2;    {sha1}
    end;
  end;
End;

Begin;
{$ifdef debug}dumpBinaryData('got from '+ipAddr2string(addr)+' '+BStr(port),buf,siz);{$endif}
num:=findOneConnection(addr);
if (num<1) then begin;
  WriteLn('got packet from unknown peer '+ipAddr2string(addr)+' '+BStr(port));
  fillchar(con,sizeof(con),0);
  move(addr,con.addr,sizeof(con.addr));
  con.portR:=port;
  con.portL:=loc;
  sendNotifyMessage(con,26);
  exit;
  end;
con:=ConnectionDat^[num];
if (loc=2) then begin;
  move(isakmp,i,sizeof(i));
  if (i<>0) then goto esp;
  dec(siz,4);
  move(buf[5],buf,siz);
  end;
if (con.stat=1000) then begin;
  WriteLn('got non-esp from '+ipAddr2string(addr)+' '+BStr(port));
  inc(con.retry);
  goto vege;
  end;
i:=ReadLongMSB(isakmp.len);
if (i>siz) then begin;
  WriteLn('got truncated packet from '+ipAddr2string(addr)+' '+BStr(port));
  sendNotifyMessage(con,16);
  exit;
  end;
siz:=i;
if (isakmp.vers and $f0<>$10) then begin;
  WriteLn('got unknown version packet from '+ipAddr2string(addr)+' '+BStr(port));
  sendNotifyMessage(con,6);
  exit;
  end;
ps:=sizeof(isakmp);
tp:=isakmp.nxtPay;
isahdr:=0;
if (isakmp.exTyp=32) and (con.msgID=0) then begin;
  con.msgID:=isakmp.msgID;
  generateQuickModeIV(con);
  end;
if (isakmp.flags and 1<>0) then begin;
  if (con.stat<100) then begin;
    WriteLn('got encrypted packet from '+ipAddr2string(addr)+' '+BStr(port));
    sendNotifyMessage(con,19);
    exit;
    end;
  {$ifdef debug}
  WriteLn('decrypting');
  dumpBinaryData('  key',con.encK,33);
  dumpBinaryData('  iv',con.encI,23);
  {$endif}
  move(buf[siz-con.encS+1],a,con.encS);
  CryptoImmDecrypt(con.encA,con.encK,con.encI,buf[ps+1],siz-ps);
  move(a,con.encI[1],con.encS);
  o:=ps;i:=tp;
  while (o<siz) and (i<>0) do begin;
    move(buf[o+1],genhdr,sizeof(genhdr));
    inc(o,readWordMSB(genhdr.len));
    i:=genhdr.nxtPay;
    end;
  if (o<=siz) then siz:=o;
  {$ifdef debug}
  dumpBinaryData(' next iv',con.encI,23);
  dumpBinaryData('result',buf,siz);
  {$endif}
  end else if (con.stat>100) then begin;
  WriteLn('got unencrypted packet from '+ipAddr2string(addr)+' '+BStr(port));
  sendNotifyMessage(con,19);
  exit;
  end;

f1:
if (tp=0) then goto f2;
move(buf[ps+1],genhdr,sizeof(genhdr));
po:=ps;
if (readWordMSB(genhdr.len)+ps>siz) then begin;
  WriteLn('got too short packet from '+ipAddr2string(addr)+' '+BStr(port));
  sendNotifyMessage(con,16);
  exit;
  end;
case tp of
  1:begin; {security association}
    isahdr:=isahdr or 1;
    if (con.mode=0) then begin;
      move(isakmp.initCok,con.initCok,sizeof(con.initCok));
      end else begin;
      move(isakmp.respCok,con.respCok,sizeof(con.respCok));
      end;
    move(buf[ps+1],con.encRLc,sizeof(con.encRLc));
    move(buf[ps+1],secass,sizeof(secass));
    inc(ps,sizeof(secass));
    propos.nxtPay:=2;
    encB:=0;
    hshB:=0;
    grpB:=0;
    timB:=0;
    bytB:=0;
    while (propos.nxtPay=2) do begin;
      move(buf[ps+1],propos,sizeof(propos));
      inc(ps,sizeof(propos));
      {$ifdef debug}WriteLn('proposal #'+BStr(propos.propN)+'; proto='+BStr(propos.prot));{$endif}
      move(buf[ps+1],con.remSPI,sizeof(con.remSPI));
      inc(ps,propos.spiLen);
      trans.nxtPay:=3;
      while (trans.nxtPay=3) do begin;
        move(buf[ps+1],trans,sizeof(trans));
        p:=ReadWordMSB(trans.len)+ps;
        inc(ps,sizeof(trans));
        {$ifdef debug}WriteLn('  transform #'+BStr(trans.trnsN)+'; proto='+BStr(trans.trnsI));{$endif}
        encC:=0;hshC:=0;grpC:=0;timC:=0;bytC:=0;typ:=0;o:=0;
        if (isakmp.exTyp=32) then case trans.trnsI of
          2:encC:=5;     {des}
          3:encC:=3;     {3des}
          4:encC:=4;     {rc5}
          6:encC:=6;     {cast}
          7:encC:=2;     {blowfish}
          12:encC:=1;    {aes}
          end;
        while (ps<p) do begin;
          i:=getOneAttribute(buf,ps,a);
          if (isakmp.exTyp<>32) then parseMMattr else parseQMattr;
          end;
        ps:=p;
        {$ifdef debug}WriteLn('  enc='+BStr(encC)+' hash='+BStr(hshC)+' grp='+BStr(grpC)+' tim='+BStr(timC)+' kb='+BStr(bytC)+' mode='+BStr(o));{$endif}
        if (o<>1) then continue;
        if (pointAlgo(encC,hshC,grpC)>pointAlgo(encB,hshB,grpB)) then continue;
        encB:=encC;
        hshB:=hshC;
        grpB:=grpC;
        timB:=timC;
        bytB:=bytC;
        end;
      end;
    if (pointAlgo(encB,hshB,grpB)>1000) then begin;
      WriteLn('no matching proposal with '+ipAddr2string(addr)+' '+BStr(port));
      sendNotifyMessage(con,14);
      exit;
      end;
    if (isakmp.exTyp<>32) then begin;
      con.isaE:=encB;
      con.isaH:=hshB;
      con.isaG:=grpB;
      con.isaT:=timB;
      con.isaB:=bytB;
      end else begin;
      con.espE:=encB;
      con.espH:=hshB;
      con.isaG:=grpB;
      con.espT:=timB;
      con.espB:=bytB;
      end;
    end;
  4:begin; {key exchange}
    isahdr:=isahdr or 2;
    {$ifdef debug}WriteLn('keyex');{$endif}
    move(buf[ps+1],keyex,sizeof(keyex));
    inc(ps,sizeof(keyex));
    i:=ReadWordMSB(keyex.len)-sizeof(keyex);
    buf[ps]:=0;
    BigNumConvMSB2BN(i+1,buf[ps],num1);
    inc(ps,i);
    move(num1,con.macLRa,sizeof(con.macLRa));
    end;
  5:begin; {identification}
    isahdr:=isahdr or 8;
    {$ifdef debug}WriteLn('ident');{$endif}
    move(buf[ps+1],ident,sizeof(ident));
    i:=ReadWordMSB(ident.len)-sizeof(genhdr);
    ab0:=i;
    move(buf[ps+sizeof(genhdr)+1],ab[1],ab0);
    con.remID:=a;
    end;
  8:begin; {hash}
    isahdr:=isahdr or 16;
    {$ifdef debug}WriteLn('hash');{$endif}
    move(buf[ps+1],hash,sizeof(hash));
    inc(ps,sizeof(hash));
    i:=ReadWordMSB(hash.len)-sizeof(hash);
    ab0:=i;
    move(buf[ps+1],ab[1],ab0);
    inc(ps,ab0);
    if (isakmp.exTyp=32) then begin;
      b:=generateQMhashVal(con,buf[ps+1],siz-ps,con.mode);
      end else begin;
      b:=generateMMhashVal(con,1);
      end;
    if (a<>b) then begin;
      WriteLn('got bad hash value from '+ipAddr2string(addr)+' '+BStr(port));
      sendNotifyMessage(con,23);
      exit;
      end;
    end;
  10:begin; {nonce}
    isahdr:=isahdr or 4;
    {$ifdef debug}WriteLn('nonce');{$endif}
    move(buf[ps+1],nonce,sizeof(nonce));
    inc(ps,sizeof(nonce));
    o:=ReadWordMSB(nonce.len)-sizeof(nonce);
    if (o>255) then ab0:=255 else ab0:=o;
    move(buf[ps+1],ab[1],ab0);
    inc(ps,o);
    if (con.mode=0) then con.initNon:=a else con.respNon:=a;
    end;
  11:begin; {notify}
    WriteLn('got notify from '+ipAddr2string(addr)+' '+BStr(port));
    end;
  13:begin; {vendor}
    {$ifdef debug}WriteLn('vendor');{$endif}
    move(buf[ps+1],vendor,sizeof(vendor));
    inc(ps,sizeof(vendor));
    o:=ReadWordMSB(vendor.len)-sizeof(vendor);
    if (o>255) then ab0:=255 else ab0:=o;
    move(buf[ps+1],ab[1],ab0);
    inc(ps,o);
    i:=0;
    if (a=vendorIDnat1) then i:=1;
    if (a=vendorIDnat2) then i:=2;
    if (a=vendorIDnat3) then i:=3;
    if (con.isaN<i) then con.isaN:=i;
    end;
  130,20:begin; {natd}
    {$ifdef debug}WriteLn('natd');{$endif}
    move(buf[ps+1],natdt,sizeof(natdt));
    inc(ps,sizeof(natdt));
    o:=ReadWordMSB(natdt.len)-sizeof(natdt);
    if (o>255) then ab0:=255 else ab0:=o;
    move(buf[ps+1],ab[1],ab0);
    inc(ps,o);
    end;
  else begin;
    WriteLn('got unknown next header from '+ipAddr2string(addr)+' '+BStr(port));
    sendNotifyMessage(con,1);
    exit;
    end;
  end;
ps:=readWordMSB(genhdr.len)+po;
tp:=genhdr.nxtPay;
goto f1;

f2:
{$ifdef debug}WriteLn('got headers='+BStr(isahdr));{$endif}
con.portR:=port;
con.portL:=loc;
if (con.mode=1) then begin;
  con.time:=-99999;
  con.retry:=0;
  con.lstGot:=isahdr;
  goto vege;
  end;
con.time:=currentTime;
con.retry:=0;

case isakmp.exTyp of
  1:begin; {base exchange}
    WriteLn('got base exchange from '+ipAddr2string(addr)+' '+BStr(port));
    if (isahdr=5) then begin;
      generateLocalCookie(con);
      generateLocalNonce(con);
      putIsaKmpHeader(con,buf,ps);
      isakmp.exTyp:=1;
      addMMsecAssHeader(con,buf,ps,false);
      addNonceHeader(con,buf,ps);
      addNatVendorHeader(con,buf,ps,false);
      sendIsaKmpMessage(con,buf,ps);
      con.stat:=11;
      goto vege;
      end;
    if (isahdr=26) then begin;
      doDiffieHellmanRound1(con);
      putIsaKmpHeader(con,buf,ps);
      isakmp.exTyp:=1;
      addKeyExHeader(con,buf,ps);
      addIdentityHeader(con,buf,ps);
      addHashingHeader(con,buf,ps,generateMMhashVal(con,0));
      addNatDetectHeader(con,buf,ps);
      addNatDetectHeader(con,buf,ps);
      sendIsaKmpMessage(con,buf,ps);
      doDiffieHellmanRound2(con);
      generateSecKeys(con);
      con.stat:=201;
      goto vege;
      end;
    WriteLn('got unknown headers from '+ipAddr2string(addr)+' '+BStr(port));
    sendNotifyMessage(con,1);
    exit;
    end;
  2:begin; {identity protection}
    WriteLn('got identity protection exchange from '+ipAddr2string(addr)+' '+BStr(port));
    if (isahdr=1) then begin;
      generateLocalCookie(con);
      putIsaKmpHeader(con,buf,ps);
      addMMsecAssHeader(con,buf,ps,false);
      addNatVendorHeader(con,buf,ps,false);
      sendIsaKmpMessage(con,buf,ps);
      con.stat:=1;
      goto vege;
      end;
    if (isahdr=6) then begin;
      generateLocalNonce(con);
      doDiffieHellmanRound1(con);
      putIsaKmpHeader(con,buf,ps);
      addKeyExHeader(con,buf,ps);
      addNonceHeader(con,buf,ps);
      addNatDetectHeader(con,buf,ps);
      addNatDetectHeader(con,buf,ps);
      sendIsaKmpMessage(con,buf,ps);
      doDiffieHellmanRound2(con);
      generateSecKeys(con);
      con.stat:=102;
      goto vege;
      end;
    if (isahdr=24) then begin;
      putIsaKmpHeader(con,buf,ps);
      addIdentityHeader(con,buf,ps);
      addHashingHeader(con,buf,ps,generateMMhashVal(con,0));
      sendIsaKmpMessage(con,buf,ps);
      con.stat:=201;
      goto vege;
      end;
    WriteLn('got unknown headers from '+ipAddr2string(addr)+' '+BStr(port));
    sendNotifyMessage(con,1);
    exit;
    end;
  4:begin; {aggressive}
    WriteLn('got aggressive exchange from '+ipAddr2string(addr)+' '+BStr(port));
    if (isahdr=15) then begin;
      generateLocalCookie(con);
      generateLocalNonce(con);
      doDiffieHellmanRound1(con);
      putIsaKmpHeader(con,buf,ps);
      isakmp.exTyp:=4;
      addMMsecAssHeader(con,buf,ps,false);
      addKeyExHeader(con,buf,ps);
      addNonceHeader(con,buf,ps);
      addIdentityHeader(con,buf,ps);
      addHashingHeader(con,buf,ps,generateMMhashVal(con,0));
      addNatVendorHeader(con,buf,ps,false);
      addNatDetectHeader(con,buf,ps);
      addNatDetectHeader(con,buf,ps);
      sendIsaKmpMessage(con,buf,ps);
      doDiffieHellmanRound2(con);
      generateSecKeys(con);
      con.stat:=121;
      goto vege;
      end;
    if (isahdr=16) then begin;
      con.stat:=201;
      goto vege;
      end;
    WriteLn('got unknown headers from '+ipAddr2string(addr)+' '+BStr(port));
    sendNotifyMessage(con,1);
    exit;
    end;
  5:begin; {informational}
    WriteLn('got informational exchange from '+ipAddr2string(addr)+' '+BStr(port));
    end;
  32:begin; {quick mode}
    WriteLn('got quick mode exchange from '+ipAddr2string(addr)+' '+BStr(port));
    if (isahdr=16) then begin;
      generateESPkeyMaterial(con);
      goto vege;
      end;
    if (isahdr and 1=0) then begin;
      WriteLn('got unknown headers from '+ipAddr2string(addr)+' '+BStr(port));
      sendNotifyMessage(con,1);
      exit;
      end;
    generateLocalNonce(con);
    generateLocalSPI(con);
    putIsaKmpHeader(con,buf,ps);
    addHashingHeader(con,buf,ps,generateQMhashVal(con,a,0,0));
    p:=ps;
    addQMsecAssHeader(con,buf,ps,false);
    addNonceHeader(con,buf,ps);
    if (isahdr and 2<>0) then begin;
      doDiffieHellmanRound1(con);
      doDiffieHellmanRound2(con);
      addKeyExHeader(con,buf,ps);
      end;
    a:=generateQMhashVal(con,buf[p+1],ps-p,1);
    move(ab[1],buf[p-ab0+1],ab0);
    sendIsaKmpMessage(con,buf,ps);
    con.stat:=202;
    goto vege;
    end;
  else begin;
    WriteLn('got unknown exchange from '+ipAddr2string(addr)+' '+BStr(port));
    sendNotifyMessage(con,7);
    exit;
    end;
  end;
goto vege;

esp:
{$ifdef debug}WriteLn('got esp packet from '+ipAddr2string(addr)+' '+BStr(port));{$endif}
con.time:=currentTime;
con.retry:=0;
if (siz=1) then goto vege;
if (ipsec.spi<>con.locSPI) then begin;
  WriteLn('got unknown spi from '+ipAddr2string(addr)+' '+BStr(port));
  exit;
  end;
dec(siz,con.encHS);
if (siz<con.encCS+sizeof(ipsec)) then begin;
  WriteLn('got too short from '+ipAddr2string(addr)+' '+BStr(port));
  exit;
  end;
o:=siz;
hmacGenerateSum(con.macRLa,con.macRLb,buf,ab[1],o);
for i:=1 to con.encHS do if (buf[siz+i]<>ab[i]) then begin;
  WriteLn('got invalid hash from '+ipAddr2string(addr)+' '+BStr(port));
  exit;
  end;
ab0:=con.encCS;
move(buf[sizeof(ipsec)+1],ab[1],ab0);
{$ifdef debug}dumpBinaryData('iv',ab[1],ab0);{$endif}
CryptoUpdtCipher(con.encRLc,a);
ps:=sizeof(ipsec)+con.encCS;
CryptoAddDecrypt(con.encRLc,buf[ps+1],siz-ps);
if (buf[siz]<>4) then begin;
  WriteLn('got invalid padding from '+ipAddr2string(addr)+' '+BStr(port));
  exit;
  end;
dec(siz,buf[siz-1]);
dec(siz,2);
dec(siz,ps);
if (siz<1) then begin;
  WriteLn('got full padding from '+ipAddr2string(addr)+' '+BStr(port));
  exit;
  end;
con.locPcks:=ipsec.seq;
{$ifdef debug}dumpBinaryData('result',buf[ps+1],siz);{$endif}
pipeLineSend(con.pipe,buf[ps+1],siz);

vege:
ConnectionDat^[num]:=con;
End;
