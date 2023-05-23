{$heap 255k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}






Type romBufferRecord=record
  d:array[1..$20000] of Byte;
  s:LongInt;
  r:LongInt;
  end;
Const romPatchSize=4;

Procedure patchROMdata(var d:romBufferRecord);
Var i,o,p:LongInt;

procedure sum;
begin;
o:=0;
for i:=1 to d.s do inc(o,d.d[i]);
o:=o and $ff;
end;

Begin;
move(d.d[1],d.d[4],d.s);
inc(d.s,romPatchSize);
p:=d.s;
i:=512;
while (i<p) or (i<d.r) do i:=i*2;
fillchar(d.d[p+1],i-d.s,$ff);
d.s:=i;
d.d[1]:=$55;
d.d[2]:=$aa;
d.d[3]:=d.s shr 9;
d.d[p]:=0;
repeat
  sum;
  d.d[p]:=$100-o;
  sum;
  until (o=0);
End;






Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Var
  buf:romBufferRecord;
  a:String;
  f:xFile;
BEGIN;
WriteLn('rom creator v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<2) then immErr('using: romcreator.code <source> <target> [size]');

a:=paramStr(1);
WriteLn('reading '+a+'...');
if (xOpen(f,a,xGenFilMod_r)<>0) then immErr('error opening source!');
buf.s:=xfileSize(f);
if (buf.s>sizeof(buf.d)-romPatchSize) then immErr('file too big!');
xBlockRead(f,buf.d,buf.s);
xClose(f);

WriteLn('patching rom...');
buf.r:=BVal(paramStr(3));
patchROMdata(buf);

a:=paramStr(2);
WriteLn('writing '+a+'...');
if (xCreate(a)<>0) then immErr('error creating target!');
if (xOpen(f,a,xGenFilMod_rw)<>0) then immErr('error opening target!');
xBlockWrite(f,buf.d,buf.s);
xClose(f);

WriteLn('successful!');
END.