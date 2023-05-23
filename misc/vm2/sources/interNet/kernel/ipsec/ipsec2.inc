Var oldPositionInHeader:LongInt;

Function getOneAttribute(var buffer;var ps:LongInt;var a:String):LongInt;
Const max=255;
Var
  buf:array[1..1] of byte absolute buffer;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  at:OneAttributeHeaderRecord;
  i,o:LongInt;
Begin;
fillchar(a,sizeof(a),0);
move(buf[ps+1],at,sizeof(at));
inc(ps,sizeof(at));
i:=ReadWordMSB(at.typ);
getOneAttribute:=i and $7fff;
if (i and $8000<>0) then begin;
  ab0:=sizeof(at.len);
  move(at.len,ab[1],ab0);
  exit;
  end;
o:=ReadWordMSB(at.len);
if (o>max) then ab0:=max else ab0:=o;
move(buf[ps+1],ab[1],ab0);
inc(ps,o);
End;


Procedure putOneAttribute(var buffer;var ps:LongInt;t:LongInt;a:String);
Var
  buf:array[1..1] of byte absolute buffer;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  at:OneAttributeHeaderRecord;
Begin;
if (ab0=2) then begin;
  WriteWordMSB(at.typ,t or $8000);
  move(ab[1],at.len,sizeof(at.len));
  move(at,buf[ps+1],sizeof(at));
  inc(ps,sizeof(at));
  exit;
  end;
WriteWordMSB(at.typ,t);
WriteWordMSB(at.len,ab0);
move(at,buf[ps+1],sizeof(at));
inc(ps,sizeof(at));
move(ab[1],buf[ps+1],ab0);
inc(ps,ab0);
End;




Procedure putIsaKmpHeader(var con:OneConnectionRecord;var buffer;var ps:LongInt);
Var isakmp:OneISAKMPheaderRecord absolute buffer;
Begin;
fillchar(isakmp,sizeof(isakmp),0);
move(con.initCok,isakmp.initCok,sizeof(isakmp.initCok));
move(con.respCok,isakmp.respCok,sizeof(isakmp.respCok));
isakmp.vers:=$10;  {version}
isakmp.msgID:=con.msgID;
if (isakmp.msgID=0) then isakmp.exTyp:=2 else isakmp.exTyp:=32;
ps:=sizeof(isakmp);
oldPositionInHeader:=sizeof(isakmp.initCok)+sizeof(isakmp.respCok);
End;



Procedure sendIsaKmpMessage(var con:OneConnectionRecord;var buffer;ps:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  isakmp:OneISAKMPheaderRecord absolute buffer;
  i:LongInt;
Begin;
if (con.stat>100) then begin;
  {$ifdef debug}
  WriteLn('encrypting');
  dumpBinaryData('before',buf,ps);
  dumpBinaryData('  key',con.encK,33);
  dumpBinaryData('  iv',con.encI,23);
  {$endif}
  i:=(ps-sizeof(isakmp)) mod con.encS;
  if (i>0) then begin;
    i:=con.encS-i;
    fillchar(buf[ps+1],i,0);
    inc(ps,i);
    end;
  CryptoImmEncrypt(con.encA,con.encK,con.encI,buf[sizeof(isakmp)+1],ps-sizeof(isakmp));
  move(buf[ps-con.encS+1],con.encI[1],con.encS);
  inc(isakmp.flags,1);
  {$ifdef debug}dumpBinaryData(' next iv',con.encI,23);{$endif}
  end;
WriteLongMSB(isakmp.len,ps);
if (con.portL=2) then begin;
  move(buf,buf[5],ps);
  inc(ps,4);
  fillchar(buf,4,0);
  i:=isakmpPipe2;
  end else i:=isakmpPipe1;
UDPsendPacket(i,con.addr,con.portR,buffer,ps);
{$ifdef debug}dumpBinaryData('sent to '+ipAddr2string(con.addr)+' '+BStr(con.portR),buf,ps);{$endif}
End;



Procedure addMMsecAssHeader(var con:OneConnectionRecord;var buffer;var ps:LongInt;full:Boolean);
Label f1;
Var
  buf:array[1..1] of byte absolute buffer;
  secass:OneSecurityAssociationRecord;
  propos:OneProposalHeaderRecord;
  trans:OneTransformHeaderRecord;
  tps,tpn:LongInt;

Function prefChp(n:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to CipherAlgoMax do if (CipherAlgoList[i].p=n) then goto f1;
i:=0;
f1:
prefChp:=i;
End;

Function prefHsh(n:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to HasherAlgoMax do if (HasherAlgoList[i].p=n) then goto f1;
i:=0;
f1:
prefHsh:=i;
End;

Procedure addTrans(e,h,g:LongInt);
Var
  a:String;
  i:LongInt;
Begin;
case e of
  1:e:=7;
  2:e:=3;
  3:e:=5;
  4:e:=4;
  5:e:=1;
  6:e:=6;
  else exit;
  end;
case h of
  1:h:=1;
  2:h:=2;
  else exit;
  end;
case g of
  1:g:=1;
  2:g:=2;
  else exit;
  end;
{$ifdef debug}WriteLn('enc='+BStr(e)+' hash='+BStr(h)+' grp='+BStr(g));{$endif}
buf[tps+1]:=3;
inc(tpn);
tps:=ps;
fillchar(trans,sizeof(trans),0);
trans.trnsN:=tpn;
trans.trnsI:=1;
move(trans,buf[tps+1],sizeof(trans));
inc(ps,sizeof(trans));
putOneAttribute(buf,ps,1,#0+chr(e));
if (e=7) then putOneAttribute(buf,ps,14,#0#128);
putOneAttribute(buf,ps,2,#0+chr(h));
putOneAttribute(buf,ps,3,#0#1);
putOneAttribute(buf,ps,4,#0+chr(g));
a:='1234';
i:=con.isaT;
if (i=0) then i:=86400;
WriteLongMSB(a[1],i);
putOneAttribute(buf,ps,11,#0#1);
putOneAttribute(buf,ps,12,a);
i:=con.isaB;
if (i>0) then begin;
  WriteLongMSB(a[1],i);
  putOneAttribute(buf,ps,11,#0#2);
  putOneAttribute(buf,ps,12,a);
  end;
WriteWordMSB(trans.len,ps-tps);
move(trans,buf[tps+1],sizeof(trans));
End;

Var i,o,p,q:LongInt;
Begin;
buf[oldPositionInHeader+1]:=1;
oldPositionInHeader:=ps;
fillchar(secass,sizeof(secass),0);
WriteLongMSB(secass.doi,1);
WriteLongMSB(secass.situ,1);
move(secass,buf[ps+1],sizeof(secass));
inc(ps,sizeof(secass));
fillchar(propos,sizeof(propos),0);
move(propos,buf[ps+1],sizeof(propos));
propos.propN:=1;
propos.prot:=1;
inc(ps,sizeof(propos));
tpn:=0;tps:=ps+64;
if not full then begin;
  addTrans(con.isaE,con.isaH,con.isaG);
  goto f1;
  end;
for p:=2 downto 1 do begin;
  for o:=1 to 10 do begin;
    q:=prefChp(o);
    if (q=0) then continue;
    for i:=1 to 10 do addTrans(q,prefHsh(i),p);
    end;
  end;
f1:
WriteWordMSB(secass.len,ps-oldPositionInHeader);
move(secass,buf[oldPositionInHeader+1],sizeof(secass));
tps:=oldPositionInHeader+sizeof(secass);
propos.trans:=tpn;
WriteWordMSB(propos.len,ps-tps);
move(propos,buf[tps+1],sizeof(propos));
End;



Procedure addQMsecAssHeader(var con:OneConnectionRecord;var buffer;var ps:LongInt;full:Boolean);
Label f1;
Var
  buf:array[1..1] of byte absolute buffer;
  secass:OneSecurityAssociationRecord;
  propos:OneProposalHeaderRecord;
  trans:OneTransformHeaderRecord;
  tps,tpn:LongInt;

Function prefChp(n:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to CipherAlgoMax do if (CipherAlgoList[i].p=n) then goto f1;
i:=0;
f1:
prefChp:=i;
End;

Function prefHsh(n:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to HasherAlgoMax do if (HasherAlgoList[i].p=n) then goto f1;
i:=0;
f1:
prefHsh:=i;
End;

Procedure addTrans(e,h,g:LongInt);
Var
  a:String;
  i:LongInt;
Begin;
case e of
  1:e:=12;
  2:e:=7;
  3:e:=3;
  4:e:=4;
  5:e:=2;
  6:e:=6;
  else exit;
  end;
case h of
  1:h:=1;
  2:h:=2;
  else exit;
  end;
case g of
  1:g:=1;
  2:g:=2;
  0:g:=0;
  else exit;
  end;
{$ifdef debug}WriteLn('enc='+BStr(e)+' hash='+BStr(h)+' grp='+BStr(g));{$endif}
buf[tps+1]:=3;
inc(tpn);
tps:=ps;
fillchar(trans,sizeof(trans),0);
trans.trnsN:=tpn;
trans.trnsI:=e;
move(trans,buf[tps+1],sizeof(trans));
inc(ps,sizeof(trans));
if (e=12) then putOneAttribute(buf,ps,6,#0#128);
putOneAttribute(buf,ps,5,#0+chr(h));
case con.isaN of
  1,2:a:=#$f0#3;
  3:a:=#0#3;
  else a:=#0#1;
  end;
putOneAttribute(buf,ps,4,a);
if (g<>0) then putOneAttribute(buf,ps,3,#0+chr(g));
a:='1234';
i:=con.espT;
if (i=0) then i:=28800;
WriteLongMSB(a[1],i);
putOneAttribute(buf,ps,1,#0#1);
putOneAttribute(buf,ps,2,a);
i:=con.espB;
if (i>0) then begin;
  WriteLongMSB(a[1],i);
  putOneAttribute(buf,ps,1,#0#2);
  putOneAttribute(buf,ps,2,a);
  end;
WriteWordMSB(trans.len,ps-tps);
move(trans,buf[tps+1],sizeof(trans));
End;

Var i,o,p,q:LongInt;
Begin;
buf[oldPositionInHeader+1]:=1;
oldPositionInHeader:=ps;
fillchar(secass,sizeof(secass),0);
WriteLongMSB(secass.doi,1);
WriteLongMSB(secass.situ,1);
move(secass,buf[ps+1],sizeof(secass));
inc(ps,sizeof(secass));
fillchar(propos,sizeof(propos),0);
move(propos,buf[ps+1],sizeof(propos));
propos.propN:=1;
propos.prot:=3;
propos.spiLen:=sizeof(con.locSPI);
inc(ps,sizeof(propos));
move(con.locSPI,buf[ps+1],sizeof(con.locSPI));
inc(ps,sizeof(con.locSPI));
tpn:=0;tps:=ps+64;
if not full then begin;
  addTrans(con.espE,con.espH,con.isaG);
  goto f1;
  end;
for o:=1 to 10 do begin;
  q:=prefChp(o);
  if (q=0) then continue;
  for i:=1 to 10 do addTrans(q,prefHsh(i),con.isaG);
  end;
f1:
WriteWordMSB(secass.len,ps-oldPositionInHeader);
move(secass,buf[oldPositionInHeader+1],sizeof(secass));
tps:=oldPositionInHeader+sizeof(secass);
propos.trans:=tpn;
WriteWordMSB(propos.len,ps-tps);
move(propos,buf[tps+1],sizeof(propos));
End;


Procedure addKeyExHeader(var con:OneConnectionRecord;var buffer;var ps:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  keyex:OneKeyExchangeHeaderRecord;
  i,o:LongInt;
Begin;
buf[oldPositionInHeader+1]:=4;
oldPositionInHeader:=ps;
fillchar(keyex,sizeof(keyex),0);
WriteWordMSB(keyex.len,sizeof(keyex)+con.encCS);
move(keyex,buf[ps+1],sizeof(keyex));
inc(ps,sizeof(keyex));
savePaddedBigNumber(con.macLRb,buf[ps+1],con.encCS);
inc(ps,con.encCS);
End;



Procedure addIdentityHeader(var con:OneConnectionRecord;var buffer;var ps:LongInt);
Var
  addr:array[1..1] of byte absolute isakmpAddr1;
  buf:array[1..1] of byte absolute buffer;
  ident:OneIdentificateHeaderRecord;
  i,o:LongInt;
Begin;
buf[oldPositionInHeader+1]:=5;
oldPositionInHeader:=ps;
fillchar(ident,sizeof(ident),0);
move(ident,buf[ps+1],sizeof(ident));
inc(ps,sizeof(ident));
if isAddressIPv4mask(addr) then begin;
  ident.typ:=1;
  move(addr[sizeof(OneTCPaddressRecord)-3],buf[ps+1],4);
  inc(ps,4);
  end else begin;
  ident.typ:=5;
  move(addr,buf[ps+1],sizeof(OneTCPaddressRecord));
  inc(ps,sizeof(OneTCPaddressRecord));
  end;
WriteWordMSB(ident.len,ps-oldPositionInHeader);
move(ident,buf[oldPositionInHeader+1],sizeof(ident));
End;



Procedure addHashingHeader(var con:OneConnectionRecord;var buffer;var ps:LongInt;a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  buf:array[1..1] of byte absolute buffer;
  hash:OneHashingStateHeaderRecord;
  i,o:LongInt;
Begin;
buf[oldPositionInHeader+1]:=8;
oldPositionInHeader:=ps;
fillchar(hash,sizeof(hash),0);
WriteWordMSB(hash.len,ab0+sizeof(hash));
move(hash,buf[ps+1],sizeof(hash));
inc(ps,sizeof(hash));
move(ab[1],buf[ps+1],ab0);
inc(ps,ab0);
End;



Procedure addNonceHeader(var con:OneConnectionRecord;var buffer;var ps:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  nonce:OneNonceHeaderRecord;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
buf[oldPositionInHeader+1]:=10;
oldPositionInHeader:=ps;
if (con.mode=0) then a:=con.respNon else a:=con.initNon;
fillchar(nonce,sizeof(nonce),0);
WriteWordMSB(nonce.len,sizeof(nonce)+ab0);
move(nonce,buf[ps+1],sizeof(nonce));
inc(ps,sizeof(nonce));
move(ab[1],buf[ps+1],ab0);
inc(ps,ab0);
End;



Procedure addVendorIdHeader(var con:OneConnectionRecord;var buffer;var ps:LongInt;a:String);
Var
  buf:array[1..1] of byte absolute buffer;
  vendor:OneVendorIdHeaderRecord;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
buf[oldPositionInHeader+1]:=13;
oldPositionInHeader:=ps;
fillchar(vendor,sizeof(vendor),0);
WriteWordMSB(vendor.len,sizeof(vendor)+ab0);
move(vendor,buf[ps+1],sizeof(vendor));
inc(ps,sizeof(vendor));
move(ab[1],buf[ps+1],ab0);
inc(ps,ab0);
End;



Procedure addNatDetectHeader(var con:OneConnectionRecord;var buffer;var ps:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  natdt:OneNatDetectHeaderRecord;
  a:String;
  i,o:LongInt;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  c:CryptoContextRecord;
Begin;
case con.isaN of
  1,2:o:=130;
  3:o:=20;
  else exit;
  end;
CryptoBegHasher(c,HasherAlgoList[con.isaH].a,'','');
CryptoAddHasher(c,con.initCok,sizeof(con.initCok));
CryptoAddHasher(c,con.respCok,sizeof(con.respCok));
CryptoFinHasher(c,ab[1],i);
ab0:=i;
inc(ab[1],random($100));
buf[oldPositionInHeader+1]:=o;
oldPositionInHeader:=ps;
fillchar(natdt,sizeof(natdt),0);
WriteWordMSB(natdt.len,sizeof(natdt)+ab0);
move(natdt,buf[ps+1],sizeof(natdt));
inc(ps,sizeof(natdt));
move(ab[1],buf[ps+1],ab0);
inc(ps,ab0);
End;



Procedure addNotifyHeader(var con:OneConnectionRecord;var buffer;var ps:LongInt;typ:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  notify:OneNotifyHeaderRecord;
Begin;
buf[oldPositionInHeader+1]:=11;
oldPositionInHeader:=ps;
fillchar(notify,sizeof(notify),0);
WriteLongMSB(notify.doi,1);
notify.prtId:=1;
notify.spiLen:=sizeof(con.respCok)+sizeof(con.initCok);
WriteWordMSB(notify.len,sizeof(notify)+notify.spiLen);
WriteWordMSB(notify.notId,typ);
move(notify,buf[ps+1],sizeof(notify));
inc(ps,sizeof(notify));
move(con.initCok,buf[ps+1],sizeof(con.initCok));
inc(ps,sizeof(con.initCok));
move(con.respCok,buf[ps+1],sizeof(con.respCok));
inc(ps,sizeof(con.respCok));
End;



Procedure sendNotifyMessage(var con:OneConnectionRecord;typ:LongInt);
Label vege;
Var
  buf:array[1..1024] of byte;
  isakmp:OneISAKMPheaderRecord absolute buf;
  notify:OneNotifyHeaderRecord;
  o:LongInt;
Begin;
putIsaKmpHeader(con,buf,o);
isakmp.exTyp:=5;
addNotifyHeader(con,buf,o,typ);
sendIsaKmpMessage(con,buf,o);
End;



Procedure addNatVendorHeader(var con:OneConnectionRecord;var buffer;var ps:LongInt;full:Boolean);
Var buf:array[1..1] of byte absolute buffer;
Begin;
if full then begin;
  addVendorIdHeader(con,buf,ps,vendorIDnat3);
  addVendorIdHeader(con,buf,ps,vendorIDnat2);
  addVendorIdHeader(con,buf,ps,vendorIDnat1);
  exit;
  end;
case con.isaN of
  1:addVendorIdHeader(con,buf,ps,vendorIDnat1);
  2:addVendorIdHeader(con,buf,ps,vendorIDnat2);
  3:addVendorIdHeader(con,buf,ps,vendorIDnat3);
  end;
End;
