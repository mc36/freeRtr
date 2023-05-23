{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Function getWord(var a:String):String;
Var i:LongInt;
Begin;
i:=pos(' ',a);
if (i<1) then i:=666;
getWord:=copy(a,1,i-1);
a:=copy(a,i+1,666);
End;

Label f1;
Var
  a,b,c,d:String;
  i,n:LongInt;
  t1,t2:xtText;
BEGIN;
WriteLn('dns zone reverser v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<>3) then immErr('using: revZone <source> <domain> <target>');
a:=paramStr(1);
WriteLn('source: '+a);
if (xtOpen(t1,a,true)<>0) then immErr('error opening file!');
a:=paramStr(3);
WriteLn('target: '+a);
xErase(a);
xCreate(a);
if (xtOpen(t2,a,false)<>0) then immErr('error opening file!');
d:=paramStr(2);
WriteLn('domain: '+d);
WriteLn('converting...');
for i:=1 to 8 do begin;
  a:=xtReadLn(t1,666);
  kicserel('@',d,a);
  xtWriteLn(t2,a);
  end;
xtWriteLn(t2,'');
n:=0;
while not xtEOF(t1) do begin;
  a:=' '+xtReadLn(t1,666);
  kicserel('  ',' ',a);
  a:=copy(a,2,666);
  b:=getWord(a);
  c:=kicsi(getWord(a));
  if (c='ptr') then begin;
    c:=b;
    b:=getWord(a);
    goto f1;
    end;
  if (c<>'a') and (c<>'aaaa') then continue;
  c:=getWord(a);
  f1:
  while (length(c)<45) do c:=c+' ';
  kicserel('@',d,b);
  xtWriteLn(t2,c+'ptr '+b);
  inc(n);
  end;
xtClose(t2);
xtClose(t1);
WriteLn(BStr(n)+' entries converted.');
END.