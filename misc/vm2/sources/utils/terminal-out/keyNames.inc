Const
  keyNamesNum=54;
  keyNamesDat:array[1..keyNamesNum] of record c:byte;n:string[22]; end=(
  (c:0;n:'noop'),(c:1;n:'redraw'),(c:2;n:'tab'),(c:3;n:'backspace'),
  (c:4;n:'enter'),(c:5;n:'escape'),(c:6;n:'insert'),(c:7;n:'delete'),
  (c:8;n:'home'),(c:9;n:'end'),(c:10;n:'pgup'),(c:11;n:'pgdn'),
  (c:12;n:'up'),(c:13;n:'down'),(c:14;n:'left'),(c:15;n:'right'),
  (c:16;n:'printscreen'),(c:17;n:'break'),(c:18;n:'start-left'),(c:19;n:'menu'),
  (c:20;n:'f1'),(c:21;n:'f2'),(c:22;n:'f3'),(c:23;n:'f4'),(c:24;n:'f5'),
  (c:25;n:'f6'),(c:26;n:'f7'),(c:27;n:'f8'),(c:28;n:'f9'),(c:29;n:'f10'),
  (c:30;n:'f11'),(c:31;n:'f12'),(c:32;n:'f13'),(c:33;n:'f14'),
  (c:34;n:'f15'),(c:35;n:'f16'),(c:36;n:'f17'),(c:37;n:'f18'),
  (c:38;n:'f19'),(c:39;n:'f20'),(c:40;n:'f21'),(c:41;n:'f22'),
  (c:42;n:'f23'),(c:43;n:'f24'),(c:44;n:'f25'),(c:45;n:'f26'),
  (c:46;n:'f27'),(c:47;n:'f28'),(c:48;n:'f29'),(c:49;n:'f30'),
  (c:50;n:'start-right'),(c:51;n:'wakeup'),(c:52;n:'sleep'),(c:53;n:'power')
  );

Function keyNamesCnv(a:String):LongInt;
Label f1,f2;
Var
  b:String;
  i,o,p:LongInt;
Begin;
keyNamesCnv:=-1;
p:=0;
kicserel(' ','',a);
f1:
i:=pos('+',a);
if (i<2) then goto f2;
b:=kicsi(copy(a,1,i-1));
a:=copy(a,i+1,255);
if (b='shift') then p:=p or $100;
if (b='ctrl') then p:=p or $200;
if (b='alt') then p:=p or $400;
goto f1;
f2:
if (a='') then exit;
if (length(a)=1) then begin;
  keyNamesCnv:=ord(a[1])+p;
  exit;
  end;
if (a[1]='#') then begin;
  keyNamesCnv:=(BVal(copy(a,2,255)) or p) and $ffff;
  exit;
  end;
a:=kicsi(a);
if (copy(a,1,6)='screen') then begin;
  a:=copy(a,7,255);
  i:=pos('x',a);
  o:=BVal(copy(a,1,i-1)) and $fff;
  p:=BVal(copy(a,i+1,255)) and $fff;
  if (o<1) or (p<1) then exit;
  keyNamesCnv:=(o shl 12) or p or $81000000;
  exit;
  end;
for i:=1 to keyNamesNum do if (a=keyNamesDat[i].n) then begin;
  keyNamesCnv:=keyNamesDat[i].c+p+$8000;
  exit;
  end;
End;
