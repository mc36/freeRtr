Function RepairOneFileName(a,b:String):String;
Var i,o:Byte;
Begin;
o:=0;
for i:=1 to length(b) do if (b[i]='\') then o:=i;
b:=copy(b,1,o);
if (copy(a,2,1)<>':') and (copy(a,1,1)<>'\') then a:=b+a;
RepairOneFileName:=Kicsi(a);
End;

Procedure clreol;
begin;
write(#13'                                                                '#13);
end;

Function b2h(b:Byte):String;
Begin;
b2h:=byte2hextype(b);
End;

Function h2b(b:String):Byte;
Begin;
h2b:=hextype2byte(b);
End;

Function h2w(b:String):Word;
Begin;
h2w:=hextype2byte(Copy(b,1,2))*256+hextype2byte(Copy(b,3,2));
End;

Function w2h(w:Word):String;
Begin;
w2h:=byte2hextype(w div 256)+byte2hextype(w mod 256);
End;

Procedure WriteErr(a:String;i:LongInt);
Begin;
Write(#13);
clreol;
if (a='') then a:='error in obj file!';
if (i<>0) then a:=a+' (at line #'+BStr(i)+')';
WriteLn('  '+a);
Halt(1);
End;
