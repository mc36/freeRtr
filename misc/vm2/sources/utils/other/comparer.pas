{$heap 63k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;

Var
  f1,f2:xFile;
  b1,b2:array[1..1024] of byte;
  a,b:String;
  p,s,e,i,o:LongInt;
BEGIN;
WriteLn('comparer v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<>2) then begin;
  WriteLn('using: comparer.code <file1> <file2>');
  Halt(1);
  end;
a:=ParamStr(1);
b:=ParamStr(2);
WriteLn('file1='+a);
WriteLn('file2='+b);
if (xOpen(f1,a,xGenFilMod_r)<>0) then immErr('error opening file1!');
if (xOpen(f2,b,xGenFilMod_r)<>0) then immErr('error opening file1!');
p:=xFileSize(f1);
s:=xFileSize(f2);
a:='';
if (p<s) then begin; s:=p;a:='file1'; end;
if (p>s) then begin; s:=s;a:='file2'; end;
if (a<>'') then WriteLn('WARNiNG: '+a+' is smaller!');
WriteLn('comparing '+Alakit(s)+' bytes...');
p:=0;e:=0;
while (p<s) do begin;
  o:=s-p;
  if (o>sizeof(b1)) then o:=sizeof(b1);
  if (xBlockRead(f1,b1,o)<>0) then immErr('error reading file1!');
  if (xBlockRead(f2,b2,o)<>0) then immErr('error reading file2!');
  for i:=1 to o do if (b1[i]<>b2[i]) then begin;
    WriteLn(BStr(p+i-1)+': $'+byte2hextype(b1[i])+' <> $'+byte2hextype(b2[i]));
    inc(e);
    end;
  inc(p,o);
  Write(BStr(p)+#13);
  end;
if (e=0) then a:='no' else a:='WARNiNG: '+Alakit(e);
WriteLn(a+' differences found!');
END.