Const
  BinNumbers:Array[0..1] of Char=('0','1');
  BinTable:Array[0..7] of Byte=($80,$40,$20,$10,$8,$4,$2,$1);

Function byte2bintype(b:Byte):String;
Var
  s:String;
  k,h,i,o:defint;
Begin;
s:='12345678';
For k:=0 to 7 do begin;
  o:=((b or BinTable[k])=b);
  s[k+1]:=BinNumbers[o];
  end;
byte2bintype:=s;
End;

Function bintype2byte(h:String):Byte;
Var
  k,i,o:defint;
  b:byte;
Begin;
b:=0;
For k:=0 to 7 do begin;
  if (h[k+1]=BinNumbers[1]) then b:=b or BinTable[k];
  end;
bintype2byte:=b;
End;
