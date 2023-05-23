Const base64table='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';



Function decodeBase64(s:String):String;
Label f1,f2;
Var
  a,b:String;
  i:LongInt;
  BufDat:LongInt;
  BufLen:LongInt;

Procedure AddBits(d,s:LongInt);
Begin;
inc(BufLen,s);
BufDat:=BufDat or (d shl (16-BufLen));
while (BufLen>=8) do begin;
  b:=b+Chr(BufDat shr 8);
  BufDat:=(BufDat and $ff) shl 8;
  dec(BufLen,8);
  end;
End;

Begin;
b:='';
BufLen:=0;
BufDat:=0;
f1:
a:=Copy(s,1,1);
delete(s,1,1);
if (a='') then goto f2;
i:=Pos(a,base64table);
if (i<1) then goto f1;
AddBits(i-1,6);
goto f1;
f2:
decodeBase64:=b;
End;




Function encodeBase64(s:String):String;
Var
  sb:array[0..1] of byte absolute s;
  sb0:byte absolute s;
  q,w:LongInt;
  a:String;

procedure addbit(d,s:longint);
begin;
inc(q,s);
w:=w or (d shl (16-q));
while (q>=6) do begin;
  a:=a+Copy(Base64table,(w shr 10)+1,1);
  w:=(w shl 6) and $ffff;
  dec(q,6);
  end;
end;

Var i:Longint;
Begin;
a:='';
q:=0;
w:=0;
for i:=1 to sb0 do addbit(sb[i],8);
addbit(0,5);
while (Length(a) and 3<>0) do a:=a+'=';
encodeBase64:=a;
End;
