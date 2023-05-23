Function Kicsi(a:String):String;
Var i:defword;
Begin;
for i:=1 to Length(a) do a[i]:=LowCase(a[i]);
Kicsi:=a;
End;

Function Nagy(a:String):String;
Var i:defword;
Begin;
for i:=1 to Length(a) do a[i]:=UpCase(a[i]);
Nagy:=a;
End;

Function alakit(sz:LongInt):String;
Var
  a,b:String;
  i:defint;
Begin;
a:=bstr(abs(sz));
i:=Length(a);
While ((i-3)>0) do i:=i-3;
b:=copy(a,1,i);
While (i<Length(a)) do begin;
  b:=b+','+copy(a,i+1,3);
  i:=i+3;
  end;
if (sz<0) then b:='-'+b;
alakit:=b;
End;

Function Left(a:String;i:LongInt):String;
Begin;
Left:=Copy(a,1,i);
End;

Function Right(a:String;i:LongInt):String;
begin;
Right:=Copy(a,Length(a)-i+1,i);
end;

Function Levesz(a:String):String;
Begin;
While (a[Length(a)] in [' ',#0,#255]) and (Length(a)>0) do a:=Copy(a,1,Length(a)-1);
Levesz:=a;
End;

Function Kicserel(f,t:String;Var b:String):LongInt;
Var
  i,o:defint;
Begin;
o:=0;
Repeat
  i:=Pos(f,b);
  inc(o);
  if (i<>0) then begin;
    Delete(b,i,Length(f));
    Insert(t,b,i);
    end;
  Until i=0;
Kicserel:=o-1;
End;
