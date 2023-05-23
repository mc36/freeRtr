{$heap 255k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc param.inc}
{$sysinc memory.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Const signature=$72706d63;
Var
  fileIn,fileOut:xFile;
  fileSiz,filePos:LongInt;
  buffIn,buffOut:array[1..1024] of byte;
  sizeIn,sizeOut,posIn:longint;

Procedure flushOut;
Begin;
if (xBlockWrite(fileOut,buffOut,sizeOut)<>0) then immErr('error writing file!');
sizeOut:=0;
End;

Function getByte:longInt;
Begin;
if (posIn<sizeIn) then begin;
  inc(posIn);
  getByte:=buffIn[posIn];
  exit;
  end;
Write(BStr(filePos)+#13);
sizeIn:=fileSiz-filePos;
if (sizeIn<1) then begin;
  getByte:=-1;
  exit;
  end;
if (sizeIn>sizeof(buffIn)) then sizeIn:=sizeof(buffIn);
if (xBlockRead(fileIn,buffIn,sizeIn)<>0) then immErr('error reading file!');
inc(filePos,sizeIn);
posIn:=1;
getByte:=buffIn[1];
End;

Procedure putByte(b:LongInt);
Begin;
inc(sizeOut);
buffOut[sizeOut]:=b;
if (sizeOut>=sizeof(buffOut)) then flushOut;
End;


{$include compressor2.inc}

Var
  a,c:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
BEGIN;
WriteLn('compressor v1.0, done by Mc at '#%date' '#%time'.');
a:=ParamStr(1);
i:=bval(copy(a,2,255));
if (i=0) then i:=4;
method(i);
a:=copy(a,1,1);
p:=0;
if (a='c') or (a='C') then p:=1;
if (a='d') or (a='d') then p:=2;
a:=ParamStr(2);
c:=ParamStr(3);
if (p<1) or (a='') then begin;
  WriteLn('using: compress.code <command>[mode] <source> [target]');
  WriteLn('commands: c=compress, d=decompress');
  WriteLn('modes: 1..8; 1=fastest, 4=normal, 8=slowest');
  exit;
  end;
sizeIn:=0;
sizeOut:=0;
posIn:=0;
WriteLn('source: '+a);
if (xOpen(fileIn,a,xGenFilMod_r)<>0) then immErr('error opening source!');
fileSiz:=xFileSize(fileIn);
filePos:=0;
o:=0;
for i:=1 to ab0 do if (ab[i]=92) then o:=i;
if (p=1) then begin;
  if (c='') then c:=a+'.cmp';
  WriteLn('target: '+c);
  if (xCreate(c)<>0) then immErr('error creating target!');
  if (xOpen(fileOut,c,xGenFilMod_rw)<>0) then immErr('error opening target!');
  c:=copy(a,o+1,255);
  WriteLongLSB(ab[1],signature);
  for i:=1 to 4 do putByte(ab[i]);
  a:=c;
  for i:=0 to ab0 do putByte(ab[i]);
  WriteLongLSB(ab[1],fileSiz);
  for i:=1 to 4 do putByte(ab[i]);
  WriteLn('compressing...');
  compress;
  end else begin;
  if (c='') then c:=copy(a,1,o);
  if (c='') then c:='.\';
  for i:=1 to 4 do ab[i]:=getByte;
  if (ReadLongLSB(ab[1])<>signature) then immErr('file signature mismatch!');
  ab0:=getByte;
  for i:=1 to ab0 do ab[i]:=getByte;
  WriteLn('original: '+a);
  if (copy(c,length(c),255)='\') then c:=c+a;
  for i:=1 to 4 do ab[i]:=getByte;
  o:=ReadLongLSB(ab[1]);
  WriteLn('original: '+BStr(o));
  WriteLn('target: '+c);
  if (xCreate(c)<>0) then immErr('error creating target!');
  if (xOpen(fileOut,c,xGenFilMod_rw)<>0) then immErr('error opening target!');
  WriteLn('decompressing...');
  a:=decompress;
  if (a<>'') then immErr(a);
  end;
flushOut;
o:=xFileSize(fileOut);
xClose(fileOut);
xClose(fileIn);
i:=fileSiz-o;
if (i<0) then i:=-i;
WriteLn('source: '+BStr(fileSiz));
WriteLn('tagret: '+BStr(o));
WriteLn('change: '+BStr(i));
WriteLn('successful!');
END.