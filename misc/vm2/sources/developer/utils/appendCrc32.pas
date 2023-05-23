{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc param.inc}
{$sysinc memory.inc}

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Var crc32table:Array [0..255] of LongInt;

Procedure crc32build;
Var
  crc:LongInt;
  i,n:word;
Begin;
for i:=0 to 255 do begin;
  crc:=i;
  for n:=1 to 8 do
   if (crc and 1<>0) then crc:=(crc shr 1) xor $EDB88320 else crc:=crc shr 1;
  crc32table[i]:=crc;
  end;
End;

Procedure crc32update(var crc:LongInt;c:LongInt);
Begin
crc:=crc32table[(crc and $ff) xor c] xor (crc shr 8);
End;

Label f1,msb,lsb;
Var
  buf:array[1..4096] of byte;
  i,o,p,s,c:LongInt;
  a:String;
  f:xFile;
BEGIN;
WriteLn('crc32 appender v1.0, done by Mc at '#%date' '#%time'.');
crc32build;
a:=paramStr(1);
if (a='') then immErr('using: appcrc.code <filename> [sizemsb/sizelsb]');
WriteLn('Updating '+a+'...');
if (xOpen(f,a,xGenFilMod_rw)<>0) then immErr('error opening file!');
s:=xFileSize(f);
p:=0;
o:=0;
a:=paramStr(2);
if (a='sizemsb') then goto msb;
if (a='sizelsb') then goto lsb;
if (s<8) then goto f1;
xSeek(f,0);
if (xBlockRead(f,buf,8)<>0) then goto f1;
if (ReadLongLSB(buf)<>$63657865) then goto f1;
if (ReadLongMSB(buf[5])=s) then begin; msb:WriteLongMSB(o,s+4); end;
if (ReadLongLSB(buf[5])=s) then begin; lsb:WriteLongLSB(o,s+4); end;
if (o=0) then goto f1;
xSeek(f,4);
if (xBlockWrite(f,o,sizeof(o))<>0) then immErr('error writing header!');
f1:
xSeek(f,p);
c:=$ffffffff;
while (p<s) do begin;
  Write(BStr(p)+#13);
  o:=s-p;
  if (o>sizeof(buf)) then o:=sizeof(buf);
  if (xBlockRead(f,buf,o)<>0) then immErr('error reading file!');
  for i:=1 to o do crc32update(c,buf[i]);
  inc(p,o);
  end;
c:=c xor $ffffffff;
WriteLongLSB(buf,c);
xSeek(f,s);
if (xBlockWrite(f,buf,4)<>0) then immErr('error writing checksum!');
if (xClose(f)<>0) then immErr('error closing file!');
WriteLn('Successfully finished!');
END.