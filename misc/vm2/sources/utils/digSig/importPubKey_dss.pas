{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc bignum.inc}
{$include \sources\internet\kernel\utils\keys.inc}

{$include \sources\internet\kernel\tls\asn1hdr.inc}
{$include \sources\internet\kernel\tls\asn1num.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Label err;
Var
  buf:array[1..32*1024] of byte;
  key:OneDSSkeyRecord;
  num:BigNumContextRecord;
  a:String;
  i,o,p,q,r,w:LongInt;
  f:xFile;
BEGIN;
WriteLn('dss key importer v1.0, done by Mc at '#%date' '#%time'.');
if BigNumStartActions then immErr('failed to find bignum process!');
fillchar(key,sizeof(key),0);

if (paramStr(2)='') then immErr('using: getDSSkey.exe <infile> <outfile>');

WriteLn('going to import DSS key...');


WriteLn('reading asn1 file...');
if (xOpen(f,ParamStr(1),xGenFilMod_r)<>0) then immErr('error opening file!');
w:=xFileSize(f);
if (w>sizeof(buf)) then immErr('file too big!');
xBlockRead(f,buf,w);
xClose(f);

WriteLn('parsing tags...');
p:=0;
readASN1header(buf,p,i,o,q);
if (i<>$20) then begin;
  err:
  immErr('error in key file!');
  end;
if (o<>16) then goto err;
if (p+q<>w) then goto err;
readASN1header(buf,p,i,o,q);
if (i<>$20) then goto err;
if (o<>16) then goto err;
r:=p+q;
readASN1header(buf,p,i,o,q);
if (i<>0) then goto err;
if (o<>6) then goto err;
inc(p,q);
readASN1header(buf,p,i,o,q);
if (i<>$20) then goto err;
if (o<>16) then goto err;
if readASN1integer(buf,p,key.p) then goto err;
if readASN1integer(buf,p,key.q) then goto err;
if readASN1integer(buf,p,key.g) then goto err;
if (p<>r) then goto err;
p:=r;
readASN1header(buf,p,i,o,q);
if (i<>0) then goto err;
if (o<>3) then goto err;
inc(p);
if readASN1integer(buf,p,key.y) then goto err;

WriteLn('writing key...');
BigNumClearPadding(key.p);
BigNumClearPadding(key.q);
BigNumClearPadding(key.g);
BigNumClearPadding(key.x);
BigNumClearPadding(key.y);
a:=ParamStr(2);
xErase(a);
xCreate(a);
if (xOpen(f,a,xGenFilMod_rw)<>0) then immErr('error opening target file!');
xSeek(f,0);
xBlockWrite(f,key,sizeof(key));
xTruncate(f);
xClose(f);

WriteLn('successfully finished!');
END.