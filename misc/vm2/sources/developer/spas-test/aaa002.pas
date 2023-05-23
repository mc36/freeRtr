{$stack 512}
{$heap 65500}
{$sysinc system.inc}
{$sysinc datetime.inc}

Function conv(w,s:word):String;
Var a:string;
Begin;
a:=BStr(w);
while (length(a)<s) do a:='0'+a;
conv:=a;
End;

Const DayNames:array[0..6] of string[10]=('vas rnap','h‚tf“','kedd','szerda','cst”rt”k','p‚ntek','szombat');
Var a,b,c,d:Word;
BEGIN;
xGetDate(a,b,c);
Write(conv(a,4)+'-'+conv(b,2)+'-'+conv(c,2));
d:=getDayOfWeek(a,b,c);
Write(' ('+DayNames[d]+') ');
xGetTime(a,b,c);
Write(conv(a,2)+':'+conv(b,2)+':'+conv(c,2));
WriteLn('');
END.