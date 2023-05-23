{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc param.inc}
{$include \sources\developer\utils\codereader.inc}
{$include packer.inc}


Procedure ImmErr(a:String);
Begin;
WriteLn('');
WriteLn(a);
Halt(2);
End;


Label f1,f2,f3;
Var
  buf:array[1..1024*16] of byte;
  hdr:OneFileHeaderRecord;
  i,o,p:LongInt;
  ps:LongInt;
  f:xFile;
  a:String;
BEGIN;
WriteLn('self extractor v1.0, done by Mc at '#%date' '#%time'.');
WriteLn('source: '+paramStr(0));
a:=paramStr(1);
if (a='') then a:='.\';
if (copy(a,length(a),666)<>'\') then a:=a+'\';
if (xChDir(a)<>0) then immErr('error changing directory!');
WriteLn('target: '+xGetDir);
ps:=4;
f1:
readBytesAfterCode(ps,sizeof(i),i);
if (i<>IdentifyValue) then goto f3;
readBytesAfterCode(ps,sizeof(hdr),hdr);
inc(ps,sizeof(hdr));
Write('0  % '+hdr.nam+#13);
if (hdr.rgt and xRights_Directory<>0) then begin;
  i:=xMkDir(hdr.nam);
  goto f2;
  end;
if (xCreate(hdr.nam)<>0) then immErr('error creating target');
if (xOpen(f,hdr.nam,xGenFilMod_rw)<>0) then immErr('error opening target');
p:=0;
while (p<hdr.siz) do begin;
  o:=hdr.siz-p;
  if (o>sizeof(buf)) then o:=sizeof(buf);
  readBytesAfterCode(ps,o,buf);
  if (xBlockWrite(f,buf,o)<>0) then immErr('error writing data');
  inc(ps,o);
  inc(p,o);
  write(BStr(p*100 div hdr.siz)+#13);
  end;
xTruncate(f);
xClose(f);
f2:
WriteLn('ok!  ');
xSetRight(hdr.nam,hdr.own,hdr.rgt);
xSetDate(hdr.nam,hdr.crt,hdr.mdf);
goto f1;
f3:
WriteLn('successful!');
END.