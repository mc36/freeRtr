{$stack 512}
{$heap 65500}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}

Var
  a:String;
  t:xtText;
BEGIN;
a:=GetAllParameters;
if (a='') then begin;
  WriteLn('using: '+GetMyFullFileName+' <filename>');
  Exit;
  end;
WriteLn('going to type '+a+'...');
if (xtOpen(t,a,true)<>0) then begin;
  WriteLn('error opening file!');
  exit;
  end;
while not xtEOF(t) do begin;
  while not xtEOL(t) do write(xtRead(t,128));
  WriteLn(xtReadLn(t,255));
  end;
END.