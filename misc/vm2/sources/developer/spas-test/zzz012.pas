{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
Var
  a1:longword;
  a2:longword;
  a3:longint  absolute a1;
  a4:longint  absolute a2;
  b1:word     absolute a3;
  b2:word     absolute a4;
  b3:integer  absolute a1;
  b4:integer  absolute a2;
  c1:byte     absolute a3;
  c2:byte     absolute a4;
  c3:shortint absolute a1;
  c4:shortint absolute a2;

BEGIN;
a1:=$60006060;
a2:=$90009090;
writeln(bstr(a1<a2)+bstr(a3<a4));
writeln(bstr(b1<b2)+bstr(b3<b4));
writeln(bstr(c1<c2)+bstr(c3<c4));
END.