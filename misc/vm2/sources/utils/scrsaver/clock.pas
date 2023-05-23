{$heap 3k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc random.inc}
{$sysinc crt.inc}
{$sysinc datetime.inc}
{$sysinc param.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Var mono:Boolean;

const charSet:array[0..10] of array[1..5] of String[8]=(
  (' ‹€ﬂ€‹  ','€€   €€ ','€€ € €€ ','€€   €€ ',' ﬂ€‹€ﬂ  '),
  ('  ‹€€   ',' ﬂﬂ€€   ','   €€   ','   €€   ',' ‹‹€€‹‹ '),
  ('‹€ﬂﬂﬂ€‹ ','    ‹€ﬂ ','  ‹€ﬂ   ','‹€ﬂ     ','€€‹‹‹€€ '),
  ('‹€ﬂﬂﬂ€‹ ','     €€ ','  ﬂﬂﬂ€‹ ','     €€ ','ﬂ€‹‹‹€ﬂ '),
  ('   ‹€€  ',' ‹€ﬂ€€  ','€€‹‹€€‹ ','    €€  ','   ‹€€‹ '),
  ('€€ﬂﬂﬂﬂﬂ ','€€      ','ﬂﬂﬂﬂﬂ€‹ ','     €€ ','ﬂ€‹‹‹€ﬂ '),
  (' ‹€ﬂﬂ   ','€€      ','€€ﬂﬂﬂ€‹ ','€€   €€ ','ﬂ€‹‹‹€ﬂ '),
  ('€€ﬂﬂﬂ€€ ','     €€ ','   ‹€ﬂ  ','  €€    ','  €€    '),
  ('‹€ﬂﬂﬂ€‹ ','€€   €€ ','‹€ﬂﬂﬂ€‹ ','€€   €€ ','ﬂ€‹‹‹€ﬂ '),
  ('‹€ﬂﬂﬂ€‹ ','€€   €€ ',' ﬂﬂﬂﬂ€€ ','     €€ ',' ‹‹‹€ﬂ  '),
  ('   ','€€ ','   ','‹‹ ','ﬂﬂ '));

Procedure draw(var a:String;x,y:LongInt);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  b:String;
  bb0:byte absolute b;
  i,o,p:LongInt;
Begin;
o:=1;
for p:=1 to 5 do begin;
  gotoXY(x,y+p-1);
  bb0:=0;
  for i:=1 to ab0 do b:=b+charSet[ab[i]][o];
  inc(o);
  dec(bb0);
  write(b);
  end;
End;

Label f1,f2,f3;
Var
  a:String;
  ab:array[0..9] of byte absolute a;
  ab0:byte absolute a;
  scrX,scrY:Word;
  b,c,d:Word;
  time:LongInt;
BEGIN;
WriteLn('clock v1.0, done by Mc at '#%date' '#%time'.');
Randomize;
mono:=(GetAllParameters<>'');
timer2start;time:=currentTime;
f1:
textColor(0);
clrscr;
ConsoleSize(scrX,scrY);
dec(scrX,35);
dec(scrY,5);
f2:
if mono then textColor(15) else textColor(random(15)+1);
xGetTime(b,c,d);
ab0:=5;
ab[1]:=b div 10;
ab[2]:=b mod 10;
ab[3]:=10;
ab[4]:=c div 10;
ab[5]:=c mod 10;
ab[6]:=10;
ab[7]:=d div 10;
ab[8]:=d mod 10;
b:=random(scrX)+1;
c:=random(scrY)+1;
draw(a,b,c);
while (getTimePast(time)<4) do begin; relequish;timer2start; end;
time:=currentTime;
TextColor(0);
draw(a,b,c);
while keypressed do if (readKey<>$8001) then goto f3 else goto f1;
goto f2;
f3:
textColor(7);
clrscr;
END.