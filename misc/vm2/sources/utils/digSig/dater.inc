Procedure GetValidityPeriod(var bg,ed:String;d:Longint);
Var a,b,c:Word;

function lead(i:word):string;
var a:string;
begin;
a:=bstr(i);
while (length(a)<2) do a:='0'+a;
while (length(a)>2) do a:=copy(a,2,255);
lead:=a;
end;

function part:string;
begin;
part:=lead(a)+lead(b)+lead(c);
end;

Begin;
if (d<1) then d:=30;
xGetTime(a,b,c);
ed:=part+'Z';
xGetDate(a,b,c);
bg:=part+ed;
dec(b);
c:=c+d-1;
inc(b,c div 30);
c:=(c mod 30)+1;
inc(a,b div 12);
b:=(b mod 12)+1;
ed:=part+ed;
End;
