{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc param.inc}
{$sysinc hex.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

{$include \sources\internet\kernel\tls\asn1hdr.inc}


Procedure dumpASN1sequence(var data;b,e:LongInt;s:String);
Label f1;
Var
  d:array[1..1] of byte absolute data;
  typ,tag,len:LongInt;
  i:LongInt;
Begin;
f1:
if (b>=e) then exit;
readASN1header(d,b,typ,tag,len);
write(s+convertASN1type(tag)+' ('+byte2hextype(typ)+') ');
if (typ and ASN1type_constructed<>ASN1type_constructed) then begin;
  for i:=1 to len do write(':'+byte2hextype(d[b+i]));
  writeln('');
  end else begin;
  writeln('');
  dumpASN1sequence(d,b,b+len,s+'  ');
  end;
inc(b,len);
goto f1;
End;

Var
  buf:array[1..32*1024] of byte;
  i:LongInt;
  a:String;
  f:xFile;
BEGIN;
WriteLn('asn1 file parser v1.0, done by Mc at '#%date' '#%time'.');
a:=GetAllParameters;
if (a='') then immErr('using: asn1parse.code <filename>');
WriteLn('parsing '+a+'...');
if (xOpen(f,a,xGenFilMod_r)<>0) then immErr('error opening file!');
i:=xFileSize(f);
if (i>sizeof(buf)) then immErr('file too big!');
xBlockRead(f,buf,i);
xClose(f);
dumpASN1sequence(buf,0,i,'');
END.