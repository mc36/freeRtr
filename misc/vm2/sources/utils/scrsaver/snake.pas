{$heap 3k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc random.inc}
{$sysinc crt.inc}
{$sysinc param.inc}
{$include \sources\internet\kernel\utils\timer2.inc}
Const
  snakeSiz=25;
  snakeChr:array[1..snakeSiz] of byte=(32,
    176,176,176,176,176,176,177,177,177,177,177,177,
    178,178,178,178,178,178,219,219,219,219,219,219);
  snakeMul=5;
Var
  scrX,scrY:Word;
  posX:array[1..snakeSiz] of LongInt;
  posY:array[1..snakeSiz] of LongInt;
  movX,movY:LongInt;
  mono:Boolean;

Procedure genMove(x,y:LongInt);
Begin;
if (x*movX<0) then movX:=-1 else movX:=1;
if (y*movY<0) then movY:=-1 else movY:=1;
movX:=(random(snakeMul+snakeMul)+1)*movX;
movY:=(random(snakeMul)+1)*movY;
End;

Label f1,f2,f3;
Var
  i,o,t,m,r:LongInt;
  a:String;
  ab:array[0..2] of byte absolute a;
  ab0:byte absolute a;
BEGIN;
WriteLn('snake v1.0, done by Mc at '#%date' '#%time'.');
Randomize;
mono:=(GetAllParameters<>'');
f1:
TextColor(7);clrscr;
ConsoleSize(scrX,scrY);
dec(scrX,2);
for i:=1 to snakeSiz do begin;
  posX[i]:=snakeMul;
  posY[i]:=snakeMul;
  end;
movX:=0;
movY:=0;
genMove(1,1);
timer2start;
m:=-(ticksPerSec div 16);
f2:
i:=(posX[snakeSiz]+movX) div snakeMul;
if (i<1) or (i>scrX) then begin; genMove(-1,1);goto f2; end;
i:=(posY[snakeSiz]+movY) div snakeMul;
if (i<1) or (i>scrY) then begin; genMove(1,-1);goto f2; end;
if (r and $3f=0) then begin;
  if mono then textColor(15) else textColor(random(15)+1);
  end;
move(posX[2],posX,sizeof(posX));
move(posY[2],posY,sizeof(posY));
posX[snakeSiz]:=posX[snakeSiz-1]+movX;
posY[snakeSiz]:=posY[snakeSiz-1]+movY;
ab0:=2;
for i:=1 to snakeSiz do begin;
  ab[1]:=snakeChr[i];
  ab[2]:=snakeChr[i];
  gotoXY(posX[i] div snakeMul,posY[i] div snakeMul);
  Write(a);
  end;
inc(r);
while (currentTime and m=t and m) do begin; relequish;timer2start; end;
t:=currentTime;
while keypressed do if (readKey<>$8001) then goto f3 else goto f1;
goto f2;
f3:
textColor(7);
clrscr;
END.