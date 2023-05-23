Var crc32table:Array [0..255] of LongInt;

Procedure crc32build;
Var
  crc:LongInt;
  i,n:word;
Begin;
for i:=0 to 255 do begin;
  crc:=i;
  for n:=1 to 8 do
   if (crc and 1<>0) then crc:=(crc shr 1) xor $EDB88320 else crc:=crc shr 1;
  crc32table[i]:=crc;
  end;
End;

Procedure crc32update(var crc:LongInt;var buffer;siz:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  i:LongInt;
Begin
for i:=1 to siz do crc:=crc32table[(crc and $ff) xor buf[i]] xor (crc shr 8);
End;
