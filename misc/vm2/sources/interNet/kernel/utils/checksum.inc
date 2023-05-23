Function CalculateSum(Var dat;siz:word):word;
Var
  db:array[1..1] of byte absolute dat;
  dw:array[1..1] of word absolute dat;
  i,o:LongInt;
  od:LongInt;
  ob:Byte absolute od;
Begin;
o:=0;
for i:=1 to siz shr 1 do inc(o,dw[i]);
if (siz and 1<>0) then begin; od:=0;ob:=db[siz];inc(o,od); end;
while (o and $ffff0000<>0) do o:=(o shr 16)+(o and $ffff);
CalculateSum:=o;
End;
