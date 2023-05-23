{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc memory.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc hex.inc}

Var
  buf:array[1..1024] of byte;
  buf1:byte absolute buf;
  siz:LongInt;
  beg:LongInt;

Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Procedure put(var t:xtText;a:String);
Var
  i,o,p:LongInt;
Begin;
p:=siz+1;
a:=a+byte2hextype(p);
for o:=1 to siz do begin;
  i:=buf[o];
  inc(p,i);
  a:=a+byte2hextype(i);
  end;
a:=a+byte2hextype(not p);
xtWriteLn(t,a);
End;

Var
  t:xtText;
  f:xFile;
  ps,sz:LongInt;
  i,o:LongInt;
  a:String;
BEGIN;
WriteLn('binary to srecord converter v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<2) then immErr('using: bin2srec.code <source> <target> [base]');
a:=ParamStr(1);
if (xOpen(f,a,xGenFilMod_r)<>0) then immErr('error opening source!');
a:=ParamStr(2);
xErase(a);
xCreate(a);
if (xtOpen(t,a,false)<>0) then immErr('error opening target!');
beg:=BVal(ParamStr(3));
sz:=xFileSize(f);
ps:=0;

{name}
WriteWordMSB(buf,0);
siz:=2;
put(t,'S0');

while (ps<sz) do begin;
  i:=sz-ps;
  if (i>$20) then i:=$20;
  xBlockRead(f,buf[5],i);
  WriteLongMSB(buf,beg+ps);
  inc(ps,i);
  siz:=i+4;
  put(t,'S3');
  end;

{termination}
WriteLongMSB(buf,beg);
siz:=4;
put(t,'S7');

xClose(f);
xtClose(t);
WriteLn(#13'successful!');
END.