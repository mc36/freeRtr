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
  a:String;
  i,o,p:LongInt;
  f:xFile;
BEGIN;
WriteLn('dss key exporter v1.0, done by Mc at '#%date' '#%time'.');
if BigNumStartActions then immErr('failed to find bignum process!');
fillchar(key,sizeof(key),0);

if (paramStr(2)='') then immErr('using: putDSSkey.exe <infile> <outfile>');

WriteLn('going to export DSS key...');


WriteLn('reading key...');
if (xOpen(f,ParamStr(1),xGenFilMod_r)<>0) then immErr('error opening file!');
xBlockRead(f,key,sizeof(key));
xClose(f);

WriteLn('coding tags...');
p:=0;
writeASN1header(buf,p,0,2,1);
i:=0;
move(i,buf[p+1],1);
inc(p);
writeASN1integer(buf,p,key.p);
writeASN1integer(buf,p,key.q);
writeASN1integer(buf,p,key.g);
writeASN1integer(buf,p,key.y);
writeASN1integer(buf,p,key.x);
o:=0;
writeASN1header(a,o,$20,16,p);
move(buf,buf[o+1],p);
move(a,buf,o);
inc(p,o);

WriteLn('writing asn1 file...');
a:=ParamStr(2);
xErase(a);
xCreate(a);
if (xOpen(f,a,xGenFilMod_rw)<>0) then immErr('error opening target file!');
xSeek(f,0);
xBlockWrite(f,buf,p);
xTruncate(f);
xClose(f);

WriteLn('successfully finished!');
END.