Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;

Function dup(n:Byte;c:Char):String;
Var
  a:string;
  i:word;
Begin;
a:='';
for i:=1 to n do a:=a+c;
dup:=a;
End;

Function conv2hex(i:LongInt):String;
Begin;
conv2hex:=byte2hextype(i shr 24)+byte2hextype(i shr 16)+byte2hextype(i shr 8)+byte2hextype(i);
End;
