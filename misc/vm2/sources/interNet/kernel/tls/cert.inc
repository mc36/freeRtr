Const
  ASN1objectId_DSSencrypt=#42#134#72#206#56#4#1;
  ASN1objectId_RSAencrypt=#42#134#72#134#247#13#1#1#1;

Function ASN1parserEnterTag(var pck:OnePacketRecord;var ps:LongInt;tag:LongInt):Boolean;
Var i,o,p:LongInt;
Begin;
ASN1parserEnterTag:=True;
readASN1header(pck.d,ps,i,o,p);
if (o<>tag) then exit;
if (ps+p>pck.s) then exit;
ASN1parserEnterTag:=False;
End;

Function ASN1parserSkipTag(var pck:OnePacketRecord;var ps:LongInt;tag:LongInt):Boolean;
Var i,o,p:LongInt;
Begin;
ASN1parserSkipTag:=True;
readASN1header(pck.d,ps,i,o,p);
if (o<>tag) then exit;
if (ps+p>pck.s) then exit;
inc(ps,p);
ASN1parserSkipTag:=False;
End;

Function ASN1parserCopyTag(var pck,out:OnePacketRecord;var ps:LongInt;tag:LongInt):Boolean;
Var i,o,p:LongInt;
Begin;
ASN1parserCopyTag:=True;
readASN1header(pck.d,ps,i,o,p);
if (o<>tag) then exit;
if (ps+p>pck.s) then exit;
out.s:=p;
move(pck.d[ps+1],out.d,p);
inc(ps,p);
ASN1parserCopyTag:=False;
End;

Function ASN1parserReadTag(var pck:OnePacketRecord;var ps:LongInt;tag:LongInt):String;
Var
  i,o,p:LongInt;
  a:String;
Begin;
ASN1parserReadTag:='';
readASN1header(pck.d,ps,i,o,p);
if (o<>tag) then exit;
if (ps+p>pck.s) then exit;
o:=p;
if (o>255) then o:=255;
move(pck.d[ps+1],a[1],o);
a[0]:=chr(o);
inc(ps,p);
ASN1parserReadTag:=a;
End;



Function FindPubKeyInCertificate(var pck:OnePacketRecord;var ps:LongInt):Boolean;
Var i,o,p,q:LongInt;
Begin;
FindPubKeyInCertificate:=True;
ps:=0;
readASN1header(pck.d,ps,i,o,p);
if (o<>16) then exit;
if (ps+p<>pck.s) then exit;
ps:=0;
if ASN1parserEnterTag(pck,ps,16) then exit;
if ASN1parserEnterTag(pck,ps,16) then exit;
q:=ps;
readASN1header(pck.d,ps,i,o,p);
ps:=q;
if (o=0) then if ASN1parserSkipTag(pck,ps,0) then exit;
if ASN1parserSkipTag(pck,ps,2) then exit;
if ASN1parserSkipTag(pck,ps,16) then exit;
if ASN1parserSkipTag(pck,ps,16) then exit;
if ASN1parserSkipTag(pck,ps,16) then exit;
if ASN1parserSkipTag(pck,ps,16) then exit;
if ASN1parserEnterTag(pck,ps,16) then exit;
FindPubKeyInCertificate:=False;
End;





Function GetDSSkeyFromCertificate(var pck:OnePacketRecord;var p,q,g,y:BigNumContextRecord):Boolean;
Var i,o:LongInt;
Begin;
GetDSSkeyFromCertificate:=True;
if FindPubKeyInCertificate(pck,o) then exit;
i:=o;
if ASN1parserEnterTag(pck,o,16) then exit;
if (ASN1parserReadTag(pck,o,6)<>ASN1objectId_DSSencrypt) then exit;
if ASN1parserEnterTag(pck,o,16) then exit;
if readASN1integer(pck.d,o,p) then exit;
if readASN1integer(pck.d,o,q) then exit;
if readASN1integer(pck.d,o,g) then exit;
o:=i;
if ASN1parserSkipTag(pck,o,16) then exit;
if ASN1parserEnterTag(pck,o,3) then exit;
inc(o);
if readASN1integer(pck.d,o,y) then exit;
GetDSSkeyFromCertificate:=False;
End;



Function GetRSAkeyFromCertificate(var pck:OnePacketRecord;var n,e:BigNumContextRecord):Boolean;
Var i,o:LongInt;
Begin;
GetRSAkeyFromCertificate:=True;
if FindPubKeyInCertificate(pck,o) then exit;
i:=o;
if ASN1parserEnterTag(pck,o,16) then exit;
if (ASN1parserReadTag(pck,o,6)<>ASN1objectId_RSAencrypt) then exit;
o:=i;
if ASN1parserSkipTag(pck,o,16) then exit;
if ASN1parserEnterTag(pck,o,3) then exit;
inc(o);
if ASN1parserEnterTag(pck,o,16) then exit;
if readASN1integer(pck.d,o,n) then exit;
if readASN1integer(pck.d,o,e) then exit;
GetRSAkeyFromCertificate:=False;
End;



Function GetDSSparamFromSignature(var pck:OnePacketRecord;var r,s:BigNumContextRecord):Boolean;
Var i,o,p,q:LongInt;
Begin;
GetDSSparamFromSignature:=True;
q:=0;
readASN1header(pck.d,q,i,o,p);
if (o<>16) then exit;
if (q+p<>pck.s) then exit;
o:=0;
if ASN1parserEnterTag(pck,o,16) then exit;
if readASN1integer(pck.d,o,r) then exit;
if readASN1integer(pck.d,o,s) then exit;
GetDSSparamFromSignature:=False;
End;

Function GetRSAparamFromSignature(var pck:OnePacketRecord;var s:BigNumContextRecord):Boolean;
Var buf:array[1..1] of byte absolute s;
Begin;
GetRSAparamFromSignature:=True;
move(pck.d,buf[2],pck.s);
buf[1]:=0;
BigNumConvMSB2BN(pck.s+1,s,s);
GetRSAparamFromSignature:=False;
End;




Procedure PutDSSparamToSignature(var pck:OnePacketRecord;var r,s:BigNumContextRecord);
Var
  buf:OnePacketRecord;
  i,o:LongInt;
Begin;
buf.s:=0;
writeASN1integer(buf.d,buf.s,r);
writeASN1integer(buf.d,buf.s,s);
pck.s:=0;
writeASN1header(pck.d,pck.s,$20,16,buf.s);
move(buf.d,pck.d[pck.s+1],buf.s);
inc(pck.s,buf.s);
End;

Procedure PutRSAparamToSignature(var pck:OnePacketRecord;var s:BigNumContextRecord);
Begin;
BigNumConvBN2MSB(s,pck.s,pck.d);
if (pck.s>0) and (pck.d[1]=0) then begin;
  dec(pck.s);
  move(pck.d[2],pck.d,pck.s);
  end;
End;
