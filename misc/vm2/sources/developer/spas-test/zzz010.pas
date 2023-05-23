{$stack 512}
{$heap 65500}
{$sysinc system.inc}
var
  w1,w2:LongInt;
BEGIN;
w1:=$deadbabe;
w2:=$19791025;
writeln(bstr(byte(w1))+' '+bstr(byte(w2)));
writeln(bstr(shortint(w1))+' '+bstr(shortint(w2)));
writeln(bstr(word(w1))+' '+bstr(word(w2)));
writeln(bstr(integer(w1))+' '+bstr(integer(w2)));
writeln(bstr(longint(w1))+' '+bstr(longint(w2)));
END.