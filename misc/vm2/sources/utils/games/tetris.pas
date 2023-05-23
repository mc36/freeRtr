{$sysinc system.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}
{$sysinc crt.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Const
  NextPX=10;
  NextPY=5;
  StatPX=9;
  StatPY=12;
  Filled='                                                                               ';


procedure Ki(x,y:LongInt;d:String;c0,c1:LongInt);
begin;
gotoxy(x,y);
TextColor((c0 shl 4)+c1);
write(d);
end;

procedure clearscreen;
var i:LongInt;
begin;
textColor(7);
clrscr;
GotoXY(1,1);
end;


Procedure WinKi(x,y,xs,ys,c0,c1:LongInt;tit:String);
Var
  i,o:LongInt;
  b:String;
Begin;
b:='Ú';
for i:=1 to xs do b:=b+'Ä';
b:=b+'¿';
Ki(x,y,b,c0,c1);
b[1]:='À';
b[xs+2]:='Ù';
Ki(x,y+ys+1,b,c0,c1);
b:='³'+Copy(Filled,1,xs)+'³';
for i:=y+1 to y+ys do Ki(x,i,b,c0,c1);
Ki(x+2,y,tit,c0,c1);
End;



{$include tetris.inc}






Label NewAlakzat,Kiir,Valaszt1,Vege;
Var
  sThings:LongInt;
  sLines:LongInt;
  sScore:LongInt;
  BegTime:LongInt;
  MovTime:LongInt;
  NextAlak:LongInt;
  CurrAlak:AlakzatRec;
  CurrX,CurrY:LongInt;
  d:AlakzatRec;
  i,o,p:LongInt;
  a:String;
BEGIN;
randomize;
timer2start;
WriteLn('tetris v1.0, done by Mc at '#%date' '#%time'.');
InitAlakzatok;
ClearPaja;
clearscreen;
DrawBorder;
WinKi(NextPX-2,NextPY-1,5*2,4,PajaBC,PajaBF,'next');
WinKi(StatPX-1,StatPY-1,20,4,PajaBC,PajaBF,'stat');
Ki(StatPX+1,StatPY+0,'  Time: ',PajaBC,PajaBF);
Ki(StatPX+1,StatPY+1,' Lines: ',PajaBC,PajaBF);
Ki(StatPX+1,StatPY+2,'Things: ',PajaBC,PajaBF);
Ki(StatPX+1,StatPY+3,' Score: ',PajaBC,PajaBF);

BegTime:=CurrentTime;
NextAlak:=Random(AlakN)+1;
sThings:=-1;
sLines:=0;
NewAlakzat:
inc(sThings,1);
sScore:=CalcScore(sLines,sThings);
CurrX:=PajaMX div 2;
CurrY:=1;
CurrAlak:=AlakD[NextAlak];
NextAlak:=Random(AlakN)+1;
if not IsTherePlace(CurrX,CurrY,CurrAlak) then Goto Vege;
ShowAlakzat(NextPX,NextPY,AlakD[NextAlak],PajaBC);
MovTime:=CurrentTime;
Ki(StatPX+9,StatPY+1,BStr(sLines),PajaBC,PajaBF);
Ki(StatPX+9,StatPY+2,BStr(sThings),PajaBC,PajaBF);
Ki(StatPX+9,StatPY+3,BStr(sScore),PajaBC,PajaBF);
Kiir:
i:=GetTimePast(BegTime);
a:=BStr(i mod 60);
while (length(a)<2) do a:='0'+a;
Ki(StatPX+9,StatPY+0,BStr(i div 60)+':'+a,PajaBC,PajaBF);
PutOneAlakzatToTable(CurrX,CurrY,CurrAlak);
ShowTable;
i:=CurrAlak.c;
CurrAlak.c:=0;
PutOneAlakzatToTable(CurrX,CurrY,CurrAlak);
CurrAlak.c:=i;
Valaszt1:
if keypressed then begin;
  case ReadKey of
    $8005:Goto Vege; {escape}
    $0470:begin; {alt+p}
      Ki(1,1,Filled,1,1);
      a:='Paused';
      Ki(40-(Length(a) div 2),1,a,1,15);
      ReadKey;
      Ki(1,1,Filled,0,0);
      end;
    $800c:begin; {up}
      d:=CurrAlak;
      RotateAlakzat90Scale(d);
      if not IsTherePlace(CurrX,CurrY,d) then Goto Valaszt1;
      CurrAlak:=d;
      Goto Kiir;
      end;
    $800d:begin; {down}
      i:=CurrY;
      While IsTherePlace(CurrX,i+1,CurrAlak) do inc(i);
      if (i=CurrY) then Goto Valaszt1;
      CurrY:=i;
      MovTime:=CurrentTime;
      Goto Kiir;
      end;
    $800e:begin; {left}
      if not IsTherePlace(CurrX-1,CurrY,CurrAlak) then Goto Valaszt1;
      dec(CurrX);
      Goto Kiir;
      end;
    $800f:begin; {right}
      if not IsTherePlace(CurrX+1,CurrY,CurrAlak) then Goto Valaszt1;
      inc(CurrX);
      Goto Kiir;
      end;
    else Goto Valaszt1;
    end;
  end;
relequish;
timer2start;
if (GetTimePast(MovTime)<1) then Goto Valaszt1;
if IsTherePlace(CurrX,CurrY+1,CurrAlak) then begin;
  inc(CurrY);
  MovTime:=CurrentTime;
  goto kiir;
  end;
PutOneAlakzatToTable(CurrX,CurrY,CurrAlak);
inc(sLines,DeleteFullLines);
Goto NewAlakzat;
Vege:
clearscreen;
END.