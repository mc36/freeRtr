Procedure crc32build;
Var crc,i,n:LongInt;
Begin;
for i:=0 to 255 do begin;
  crc:=i;
  for n:=1 to 8 do
   if (crc and 1<>0) then crc:=(crc shr 1) xor $edb88320 else crc:=crc shr 1;
  crc32table[i]:=crc;
  end;
End;



Procedure crc32update(var crc:LongInt;c:LongInt);
Begin
crc:=crc32table[(crc and $ff) xor c] xor (crc shr 8);
End;



Procedure immErr(a:String);
Begin;
WriteLn('');
WriteLn(a);
Halt(1);
End;



Function conv2hex(d,s:LongInt):String;
Var a:String;
Begin;
a:='';
while (s>0) do begin;
  a:=copy('0123456789abcdef',(d and $f)+1,1)+a;
  d:=d shr 4;
  dec(s);
  end;
conv2hex:=a;
End;



Function lowCaseByte(i:LongInt):LongInt;
Begin;
i:=i and $ff;
if (i in [65..90]) then inc(i,32);
lowCaseByte:=i;
End;



Function lowCaseString(a:String):String;
Var
  ab:array[0..1] of byte absolute a;
  i:LongInt;
Begin;
for i:=1 to ab[0] do ab[i]:=lowCaseByte(ab[i]);
lowCaseString:=a;
End;



Function justFileName(a:String):String;
Var
  ab:array[0..1] of byte absolute a;
  i,o:LongInt;
Begin;
o:=0;
for i:=1 to ab[0] do if (ab[i]=92) then o:=i;
justFileName:=copy(a,o+1,666);
End;


{$ifdef MSBmachine}
Function readWordDEF(var buf):Word;
Begin;
asm varAddr src buf;
asm addrLod src [src];
asm varAddr trg @result;
asm movR muwuw a [src];
asm movW duwuw [trg] a;
End;

Function readLongDEF(var buf):LongWord;
Begin;
asm varAddr src buf;
asm addrLod src [src];
asm varAddr trg @result;
asm movR mudud a [src];
asm movW dudud [trg] a;
End;

Procedure writeWordDEF(var buf;dat:Word);
Begin;
asm varAddr trg buf;
asm addrLod trg [trg];
asm varAddr src dat;
asm movR duwuw a [src];
asm movW muwuw [trg] a;
End;

Procedure writeLongDEF(var buf;dat:LongWord);
Begin;
asm varAddr trg buf;
asm addrLod trg [trg];
asm varAddr src dat;
asm movR dudud a [src];
asm movW mudud [trg] a;
End;
{$else}
Function readWordDEF(var buf):Word;
Begin;
asm varAddr src buf;
asm addrLod src [src];
asm varAddr trg @result;
asm movR luwuw a [src];
asm movW duwuw [trg] a;
End;

Function readLongDEF(var buf):LongWord;
Begin;
asm varAddr src buf;
asm addrLod src [src];
asm varAddr trg @result;
asm movR ludud a [src];
asm movW dudud [trg] a;
End;

Procedure writeWordDEF(var buf;dat:Word);
Begin;
asm varAddr trg buf;
asm addrLod trg [trg];
asm varAddr src dat;
asm movR duwuw a [src];
asm movW luwuw [trg] a;
End;

Procedure writeLongDEF(var buf;dat:LongWord);
Begin;
asm varAddr trg buf;
asm addrLod trg [trg];
asm varAddr src dat;
asm movR dudud a [src];
asm movW ludud [trg] a;
End;
{$endif}
