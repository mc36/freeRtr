{$stack 512}
{$heap 65500}
{$sysinc system.inc}

var
  c:char;
  o:byte absolute c;
  i:byte;
BEGIN;
c:='a';
for i:=1 to 10 do begin;
  case c of
    'a':writeln('a!');
    'b':writeln('b!');
    'c':writeln('c!');
    else writeln('passz');
    end;
  inc(o);
  end;
END.