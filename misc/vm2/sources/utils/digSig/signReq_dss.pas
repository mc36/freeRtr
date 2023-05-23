{$heap 127k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc bignum.inc}
{$sysinc crypto.inc}
{$sysinc random.inc}
{$sysinc datetime.inc}
{$include \sources\internet\kernel\utils\keys.inc}

{$include random.inc}
{$include \sources\internet\kernel\tls\asn1hdr.inc}
{$include \sources\internet\kernel\tls\asn1num.inc}
{$include \sources\internet\kernel\tls\digsig.inc}
{$include asn1obj.inc}
{$include certify.inc}
{$include personal.inc}
{$include dater.inc}


Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Label err;
Var
  buf1,buf2,buf3,buf4:OnePacketRecord;
  key:OneDSSkeyRecord;
  per:OnePersonRecord;
  i,o:LongInt;
  a,b:String;
  f:xFile;
  t:xtText;
BEGIN;
WriteLn('dss req signer v1.0, done by Mc at '#%date' '#%time'.');
Randomize;
if CryptoStartActions then immErr('failed to find crypto process!');
if BigNumStartActions then immErr('failed to find bignum process!');

CryptoGetHasherList(key,i);
sha1algoNum:=CryptoFindOneAlgo(key,i,'sha1');
md5algoNum:=CryptoFindOneAlgo(key,i,'md5');
fillchar(key,sizeof(key),0);

if (paramStr(3)='') then immErr('using: signDSSreq.exe <key> <person> <req> <output> [days]');

WriteLn('going to sign request with DSS key...');


WriteLn('reading key...');
if (xOpen(f,ParamStr(1),xGenFilMod_r)<>0) then immErr('error opening file!');
xBlockRead(f,key,sizeof(key));
xClose(f);

WriteLn('reading person...');
if (xtOpen(t,ParamStr(2),true)<>0) then immErr('error opening file!');
ReadUpPersonalityData(t,per);
xtClose(t);

WriteLn('reading request...');
if (xOpen(f,ParamStr(3),xGenFilMod_r)<>0) then immErr('error opening file!');
buf4.s:=xFileSize(f);
xBlockRead(f,buf4.d,buf4.s);
xClose(f);

i:=BVal(ParamStr(5));
GetValidityPeriod(a,b,i);
WriteLn('coding tags...');
if ImportRequestData(buf4,buf1,buf2) then immErr('error in request!');
ExportPersonalityData(per,buf4);
ExportCertificateData(ASN1objectId_DSSwithSHA1,a,b,false,0,buf4,buf1,buf2,buf3);
buf1:=buf3;
signDataWithDSS(key,buf3,buf2);
addASN1buffer(buf1.d,buf1.s,buf2.d,buf2.s);
buf3.s:=0;
writeASN1sequence(buf3.d,buf3.s,buf1.d,buf1.s);

WriteLn('writing certificate file...');
a:=ParamStr(4);
xErase(a);
xCreate(a);
if (xOpen(f,a,xGenFilMod_rw)<>0) then immErr('error opening target file!');
xSeek(f,0);
xBlockWrite(f,buf3.d,buf3.s);
xTruncate(f);
xClose(f);

WriteLn('successfully finished!');
END.