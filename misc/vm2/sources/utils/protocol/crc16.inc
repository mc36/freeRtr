Procedure crc16update(var crc:Word;var buffer;siz:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  i,o:LongInt;
Begin;
for o:=1 to siz do begin;
  crc:=(buf[o] shl 8) xor crc;
  for i:=1 to 8 do if ((crc and $8000)<>0) then crc:=(crc shl 1) xor $1021 else crc:=crc shl 1;
  end;
End;
