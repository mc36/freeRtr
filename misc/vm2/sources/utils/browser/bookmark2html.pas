{$stack 1k}
{$heap 15k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc alap.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Var
  t1,t2:xtText;
  a,b:String;
BEGIN;
WriteLn('bookmark converter v1.0, done by Mc at '#%date' '#%time'.');
a:=paramStr(1);
b:=paramStr(2);
if (b='') then immErr('using: converter.code <source> <target>');
WriteLn('source: '+a);
if (xtOpen(t1,a,true)<>0) then immErr('error opening file!');
WriteLn('target: '+b);
if (xCreate(b)<>0) then immErr('error creating file!');
if (xtOpen(t2,b,false)<>0) then immErr('error opening file!');
xtWriteLn(t2,'<html><head>');
xtWriteLn(t2,'<title>my bookmarks</title>');
xtWriteLn(t2,'</head><body>');
while not xtEOF(t1) do begin;
  a:=xtReadLn(t1,666);
  if (a='') then continue;
  if (a='---') then begin;
    xtWriteLn(t2,'<hr>');
    continue;
    end;
  a:=xtReadLn(t1,666);
  b:=xtReadLn(t1,666);
  if (b='') then begin;
    xtWrite(t2,'<b><i><u>');
    xtWrite(t2,a);
    xtWriteLn(t2,'</u></i></b><br>');
    continue;
    end;
  xtWrite(t2,'<a href="');
  xtWrite(t2,b);
  xtWrite(t2,'" target="_new">');
  xtWrite(t2,a);
  xtWriteLn(t2,'</a><br>');
  end;
xtWriteLn(t2,'</body></html>');
xtClose(t2);
xtClose(t1);
WriteLn('successful!');
END.