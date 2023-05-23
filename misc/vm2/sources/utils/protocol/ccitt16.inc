Var ccitt16table:Array[0..255] of Word;

Procedure ccitt16build;
Var v,b,i,o:Word;
Begin;
for b:=0 to 255 do begin;
  v:=b;
  for i:=1 to 8 do begin;
    o:=v shr 1;
    if (v and 1=1) then v:=o xor $8408 else v:=o;
    end;
  ccitt16table[b]:=v;
  end;
End;

Procedure ccitt16update(var crc:Word;var buffer;siz:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  i:LongInt;
Begin;
for i:=1 to siz do crc:=(crc shr 8) xor ccitt16table[((crc xor buf[i]) and 255)];
End;
