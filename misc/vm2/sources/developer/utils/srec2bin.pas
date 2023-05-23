{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc memory.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}

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

Procedure str(a:String);
Begin;
while (a<>'') do begin;
  inc(siz);
  if (siz>sizeof(buf)) then immErr('line too long!');
  buf[siz]:=BVal('$'+copy(a,1,2));
  a:=copy(a,3,255);
  end;
End;

Procedure sek(var f:xFile;p:LongInt);
Var
  buf:array[1..1024] of byte;
  i,o:LongInt;
Begin;
dec(p,beg);
if (p<0) then immErr('data before base!');
o:=xFileSize(f);
if (o<p) then begin;
  xSeek(f,o);
  o:=p-o;
  fillchar(buf,sizeof(buf),0);
  while (o>0) do begin;
    i:=o;
    if (i>sizeof(buf)) then i:=sizeof(buf);
    xBlockWrite(f,buf,i);
    dec(o,i);
    end;
  o:=p;
  end;
xSeek(f,p);
End;


Var
  t:xtText;
  f:xFile;
  ln:LongInt;
  i,o:LongInt;
  a:String;
BEGIN;
WriteLn('srecord to binary converter v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<2) then immErr('using: srec2bin.code <source> <target> [base]');
a:=ParamStr(1);
if (xtOpen(t,a,true)<>0) then immErr('error opening source!');
a:=ParamStr(2);
xErase(a);
xCreate(a);
if (xOpen(f,a,xGenFilMod_rw)<>0) then immErr('error opening target!');
beg:=BVal(ParamStr(3));

ln:=0;
while not xtEOF(t) do begin;
  inc(ln);
  Write(#13+BStr(ln));
  a:=xtRead(t,2);
  if (a='') then begin;
    xtReadLn(t,666);
    continue;
    end;
  if (copy(a,1,1)<>'S') then immErr('invalid line beginning!');
  siz:=1;
  buf1:=BVal(copy(a,2,255));
  while not xtEOL(t) do str(xtRead(t,128));
  str(xtReadLn(t,128));
  if (buf[2]+2<>siz) then immErr('invalid record size!');
  o:=0;
  for i:=2 to siz do inc(o,buf[i]);
  if (o and $ff<>$ff) then immErr('invalid record checksum!');
  case buf1 of
    1:begin; {16bit address + data}
      o:=ReadWordMSB(buf[3]);
      sek(f,o);
      xBlockWrite(f,buf[5],siz-5);
      end;
    2:begin; {24bit address + data}
      o:=ReadLongMSB(buf[3]) shr 8;
      sek(f,o);
      xBlockWrite(f,buf[6],siz-6);
      end;
    3:begin; {32bit address + data}
      o:=ReadLongMSB(buf[3]);
      sek(f,o);
      xBlockWrite(f,buf[7],siz-7);
      end;
    0:begin; {name}
      end;
    7:begin; {32bit address + termination}
      end;
    8:begin; {24bit address + termination}
      end;
    9:begin; {16bit address + termination}
      end;
    else immErr('invalid record type!');
    end;
  end;

xClose(f);
xtClose(t);
WriteLn(#13'successful!');
END.