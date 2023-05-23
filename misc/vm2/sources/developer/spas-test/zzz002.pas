{$stack 512}
{$heap 65500}
{$sysinc system.inc}

BEGIN;
writeln(bstr(1=1)+bstr(1<>1)+bstr(1<1)+bstr(1>1));
writeln(bstr(1=2)+bstr(1<>2)+bstr(1<2)+bstr(1>2));
writeln(bstr(2=1)+bstr(2<>1)+bstr(2<1)+bstr(2>1));
writeln(bstr('a'='a')+bstr('a'<>'a')+bstr('a'<'a')+bstr('a'>'a'));
writeln(bstr('a'='b')+bstr('a'<>'b')+bstr('a'<'b')+bstr('a'>'b'));
writeln(bstr('b'='a')+bstr('b'<>'a')+bstr('b'<'a')+bstr('b'>'a'));
writeln(bstr('aa'='aa')+bstr('aa'<>'aa')+bstr('aa'<'aa')+bstr('aa'>'aa'));
writeln(bstr('aa'='aaa')+bstr('aa'<>'aaa')+bstr('aa'<'aaa')+bstr('aa'>'aaa'));
writeln(bstr('aaa'='aa')+bstr('aaa'<>'aa')+bstr('aaa'<'aa')+bstr('aaa'>'aa'));
if (1=1) then write('i') else write('h');
if (1<>1) then write('i') else write('h');
if (1<1) then write('i') else write('h');
if (1>1) then write('i') else write('h');
writeln('');
if (1=2) then write('i') else write('h');
if (1<>2) then write('i') else write('h');
if (1<2) then write('i') else write('h');
if (1>2) then write('i') else write('h');
writeln('');
if ('a'='a') then write('i') else write('h');
if ('a'<>'a') then write('i') else write('h');
if ('a'<'a') then write('i') else write('h');
if ('a'>'a') then write('i') else write('h');
writeln('');
if ('a'='b') then write('i') else write('h');
if ('a'<>'b') then write('i') else write('h');
if ('a'<'b') then write('i') else write('h');
if ('a'>'b') then write('i') else write('h');
writeln('');
END.