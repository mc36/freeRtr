{$heap 63k}
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
{$include \sources\internet\kernel\utils\keys.inc}

{$include random.inc}
{$include \sources\internet\kernel\tls\asn1hdr.inc}
{$include \sources\internet\kernel\tls\asn1num.inc}
{$include \sources\internet\kernel\tls\digsig.inc}
{$include asn1obj.inc}
{$include certify.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Label err;
Var
  buf:OnePacketRecord;
  key:OneRSAkeyRecord;
  i,o:LongInt;
  a:String;
  f:xFile;
BEGIN;
WriteLn('rsa key exporter v1.0, done by Mc at '#%date' '#%time'.');
if BigNumStartActions then immErr('failed to find bignum process!');
fillchar(key,sizeof(key),0);

if (paramStr(2)='') then immErr('using: putRSAkey.exe <infile> <outfile>');

WriteLn('going to export RSA key...');


WriteLn('reading key...');
if (xOpen(f,ParamStr(1),xGenFilMod_r)<>0) then immErr('error opening file!');
xBlockRead(f,key,sizeof(key));
xClose(f);

WriteLn('coding tags...');
ExportPublicKeyRSA(key,buf);

WriteLn('writing asn1 file...');
a:=ParamStr(2);
xErase(a);
xCreate(a);
if (xOpen(f,a,xGenFilMod_rw)<>0) then immErr('error opening target file!');
xSeek(f,0);
xBlockWrite(f,buf.d,buf.s);
xTruncate(f);
xClose(f);

WriteLn('successfully finished!');
END.