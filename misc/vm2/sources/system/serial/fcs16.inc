Var FCS16Table:Array[0..255] of Word;

Procedure CreateFCS16Table;
Var v,b,i,o:Word;
Begin;
for b:=0 to 255 do begin;
  v:=b;
  for i:=1 to 8 do begin;
    o:=v shr 1;
    if (v and 1=1) then v:=o xor $8408 else v:=o;
    end;
  FCS16Table[b]:=v;
  end;
End;

Function CalcFCS16(var buf;siz:LongInt):Word;
Var
  dat:array[1..1] of byte absolute buf;
  i:LongInt;
  fcs:Word;
Begin;
fcs:=$ffff;
for i:=1 to siz do begin;
  fcs:=(fcs shr 8) xor FCS16Table[((fcs xor dat[i]) and 255)];
  end;
CalcFCS16:=fcs xor $ffff;
End;
