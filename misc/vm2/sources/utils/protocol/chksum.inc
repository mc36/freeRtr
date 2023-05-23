Procedure chksum8update(var crc:Byte;var buffer;siz:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  i:LongInt;
Begin;
for i:=1 to siz do inc(crc,buf[i]);
End;

Procedure chksum16update(var crc:Word;var buffer;siz:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  i:LongInt;
Begin;
for i:=1 to siz do inc(crc,buf[i]);
End;
