{$stack 512}
{$heap 65500}
{$sysinc system.inc}

function fn1(a:string):string;
begin;
fn1:=a+a+a+a;
end;

function fn2(a:char):string;
begin;
fn2:=a+a+a+a;
end;

function fn3(a:string):char;
begin;
fn3:=a+a+a+a;
end;

var a:String;
BEGIN;
writeln(chr(160));
writeln(bstr(ord('A')));
a:='hello vilag!';
writeln(fn1(a));
writeln(fn2(a));
writeln(fn3(a));
writeln('"'+copy(a,-10,666)+'"');
writeln('"'+copy(a,-15,5)+'"');
writeln('"'+copy(a,5,5)+'"');
writeln('"'+copy(a,10,5)+'"');
writeln('"'+copy(a,25,5)+'"');
END.