Function xFileName(s:String;p:Byte):String;
Var
  i,o:Byte;
  d1,d2,d3:String;
Begin;
o:=0;
for i:=1 to length(s) do if (s[i]='\') then o:=i;
d1:=copy(s,1,o);
s:=copy(s,o+1,255);
o:=length(s)+1;
for i:=1 to length(s) do if (s[i]='.') then o:=i;
d2:=copy(s,1,o-1);
d3:=copy(s,o,255);
case p of
  1:s:=d1;
  2:s:=d2;
  3:s:=d3;
  else s:='';
  end;
xFileName:=s;
End;


Function FileMaskingIsThis(a:String):Boolean;
Var i:Word;
Begin;
i:=0;
inc(i,pos('?',a));
inc(i,pos('*',a));
FileMaskingIsThis:=(i<>0);
End;

Function FileMaskingTestOne(fn,msk:String):Boolean;
Label f1,oke;
Var i,o:Word;
Begin;
FileMaskingTestOne:=False;
fn:=kicsi(fn);
msk:=kicsi(msk);
f1:
if (msk='') and (fn='') then goto oke;
if (msk='*') then goto oke;
if (msk[1]='*') then begin;
  delete(msk,1,1);
  i:=pos('?',msk);if (i<1) then i:=$666;
  o:=pos('*',msk);if (o<1) then o:=$666;
  if (o<i) then i:=o;
  if (i>$333) then i:=length(msk)+1;
  dec(i);
  o:=pos(copy(msk,1,i),fn);
  if (o<1) then exit;
  delete(fn,1,o-1);
  delete(fn,1,i);
  delete(msk,1,i);
  goto f1;
  end;
if (msk[1]='?') then msk[1]:=fn[1];
if (fn[1]<>msk[1]) then exit;
delete(fn,1,1);
delete(msk,1,1);
goto f1;
oke:
FileMaskingTestOne:=True;
End;

Function xGetErrorName(n:Word):String;
Var a:String;
Begin;
case n of
  0:a:='no error';
  1:a:='default error';
  2:a:='out of memory';
  3:a:='out of disk space';
  4:a:='no right';
  5:a:='sharing violation';
  6:a:='path not exists';
  7:a:='file not exists';
  8:a:='file already exists';
  9:a:='invalid handle';
  10:a:='directory not empty';
  11:a:='embedded directories';
  12:a:='file/directory mismatch';
  13:a:='file pointer too big';
  14:a:='drive io fault';
  15:a:='drive not ready';
  16:a:='eof encountered';
  17:a:='invalid filename format';
  18:a:='abnormal program termination';
  else a:='unknown error: #'+BStr(n);
  end;
xGetErrorName:=a;
End;
