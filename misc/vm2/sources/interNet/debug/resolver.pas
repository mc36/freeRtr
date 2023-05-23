{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc hex.inc}
{$sysinc crt.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_dns.inc}

Label f1,f2;
Var
  i,o,p,q:LongInt;
  a,b,c:String;
BEGIN;
DNSstartResolver;
if (paramCount<>2) then begin;
  f1:
  WriteLn('using: resolver.code <type> <name>');
  WriteLn('types: a, ptr, mx');
  exit;
  end;
a:=kicsi(paramStr(1));
b:=paramStr(2);
q:=-1;
if (a='a') then q:=1;
if (a='ptr') then begin;
  if string2ipAddr(b,b) then goto f1;
  q:=2;
  end;
if (a='mx') then q:=3;
case q of
  1:a:='address of '+b;
  2:a:='name of '+ipAddr2string(b);
  3:a:='mailer of '+b;
  else goto f1;
  end;
Write('querying '+a+'...');
DNSresolvePut(q,b);
f2:
i:=DNSresolveGet(a,b);
if keypressed then exit;
if (i=0) then goto f2;
if (i and $80<>0) then begin;
  writeln(' failed!');
  exit;
  end;
WriteLn(' done.');
case q of
  1:a:='address is '+ipAddr2string(b);
  2:a:='name is '+a;
  3:begin;
    kicserel(#0,', ',b);
    b:=copy(b,1,length(b)-2);
    a:='mailers are '+b;
    end;
  else goto f1;
  end;
WriteLn('got reply: '+a+'...');
END.