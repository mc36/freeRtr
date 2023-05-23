{$stack 512}
{$heap 65500}
{$sysinc system.inc}

Procedure rekurziv(n:word;s:String);
Begin;
s:=s+chr(n+97);
WriteLn('elotte: '+BStr(n)+' - '+s);
if (n<10) then rekurziv(n+1,s);
WriteLn('utana: '+BStr(n)+' - '+s);
End;

BEGIN;
rekurziv(1,'a');
END.