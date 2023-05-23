{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc crt.inc}
{$sysinc param.inc}
{$sysinc datetime.inc}

Var
  b:array[1..512] of byte;
  f:xFile;
  a:String;
  i:LongInt;
BEGIN;
WriteLn('disk creator');
a:=ParamStr(1);
i:=BVal(ParamStr(2))*2;
if (i<1) then begin;
  writeln('using: makedisk.code <filename> <sizeInKB>');
  exit;
  end;
WriteLn('creating '+a+' to '+bstr(i*sizeof(b))+' size...');
xCreate(a);
if (xOpen(f,a,xGenFilMod_rw)<>0) then begin;
  writeln('failed!');
  exit;
  end;
xSeek(f,0);
fillchar(b,sizeof(b),$5a);
for i:=1 to i do xBlockWrite(f,b,sizeof(b));
xTruncate(f);
xClose(f);
WRiteLn('successful!');
END.