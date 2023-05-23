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
  i,o:LongInt;
BEGIN;
WriteLn('friends converter v1.0, done by Mc at '#%date' '#%time'.');
a:=paramStr(1);
b:=paramStr(2);
if (b='') then immErr('using: converter.code <source> <target>');
WriteLn('source: '+a);
if (xtOpen(t1,a,true)<>0) then immErr('error opening file!');
WriteLn('target: '+b);
if (xCreate(b)<>0) then immErr('error creating file!');
if (xtOpen(t2,b,false)<>0) then immErr('error opening file!');
xtWriteLn(t2,'<html><head>');
xtWriteLn(t2,'<title>my friends</title>');
xtWriteLn(t2,'</head><body>');
while not xtEOF(t1) do begin;
  a:=xtReadLn(t1,666);
  if (a='') then continue;
  i:=pos('<',a);
  if (i<1) then begin;
    xtWrite(t2,'<b><i><u>');
    xtWrite(t2,a);
    xtWriteLn(t2,'</u></i></b><br>');
    continue;
    end;
  b:=copy(a,i+1,666);
  a:=copy(a,1,i-1);
  while (copy(a,length(a),666)=' ') do a:=copy(a,1,length(a)-1);
  i:=pos('>',b);
  a:=a+copy(b,i+1,666);
  b:=copy(b,1,i-1);
  xtWrite(t2,'<a href="mailto:');
  xtWrite(t2,b);
  xtWrite(t2,'">');
  xtWrite(t2,a);
  xtWriteLn(t2,'</a><br>');
  end;
xtWriteLn(t2,'</body></html>');
xtClose(t2);
xtClose(t1);
WriteLn('successful!');
END.