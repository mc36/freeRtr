Const
  PajaPX:LongInt=35;
  PajaPY:LongInt=4;
  PajaBC:LongInt=0;
  PajaBF:LongInt=7;
  PajaMX=12;
  PajaMY=19;
Type
  AlakzatRec=record
    sx:LongInt;
    sy:LongInt;
    d:array[0..5,0..5] of Byte;
    c:LongInt;
    end;
  AllAlakRec=array[1..20] of AlakzatRec;
  OneLineRec=array[0..PajaMX+1] of Byte;
  OnePajaRec=array[0..PajaMY+1] of OneLineRec;
  TetrisPixelsRec=Array[0..1] of String[3];
Var
  PixsD:TetrisPixelsRec;
  AlakD:AllAlakRec;
  AlakN:LongInt;
  PajaD:OnePajaRec;
  PajaO:OnePajaRec;



Procedure AddOneAlakzat(s:String;c:LongInt);
Var
  i,o,p,b:longint;
  d:AlakzatRec;
Begin;
fillchar(d,sizeof(d),0);
p:=0;
d.c:=c;
for o:=1 to 4 do for i:=1 to 4 do begin;
  inc(p);
  if (s[p]='x') then b:=1 else b:=0;
  d.d[o][i]:=b;
  end;
inc(AlakN);
AlakD[AlakN]:=d;
End;



Procedure CalcAlakzatBoundaries(Var d:AlakzatRec);
Var i,o,p:LongInt;
Begin;
p:=0;
for o:=1 to 4 do if (d.d[o][1]+d.d[o][2]+d.d[o][3]+d.d[o][4]<>0) then p:=o;
d.sy:=p;
p:=0;
for o:=1 to 4 do if (d.d[1][o]+d.d[2][o]+d.d[3][o]+d.d[4][o]<>0) then p:=o;
d.sx:=p;
End;



Procedure InitAlakzatok;
Var i:LongInt;
Begin;
AlakN:=0;
PixsD[0]:='  ';
PixsD[1]:='лл';
AddOneAlakzat('x   '+
              'x   '+
              'x   '+
              'x   ',13);
AddOneAlakzat('x   '+
              'xx  '+
              ' x  '+
              '    ',10);
AddOneAlakzat('x   '+
              'xx  '+
              'x   '+
              '    ',11);
AddOneAlakzat(' x  '+
              ' x  '+
              'xx  '+
              '    ',15);
AddOneAlakzat('x   '+
              'x   '+
              'xx  '+
              '    ',14);
AddOneAlakzat('xx  '+
              'xx  '+
              '    '+
              '    ',12);
AddOneAlakzat(' x  '+
              'xx  '+
              'x   '+
              '    ',9);
for i:=1 to AlakN do CalcAlakzatBoundaries(AlakD[i]);
End;



Procedure ClearPaja;
Begin;
fillchar(PajaD,sizeOf(PajaD),0);
fillchar(PajaO,sizeOf(pajaO),255);
End;



Procedure ShowAlakzat(x,y:LongInt;Var d:AlakzatRec;bc:LongInt);
Var
  i,o:LongInt;
  a:String;
Begin;
for o:=1 to 4 do begin;
  a:='';
  for i:=1 to 4 do a:=a+PixsD[d.d[o][i]];
  Ki(x,y,a,bc,d.c);
  inc(y);
  end;
End;



Procedure ShowTable;
Var
  i,o,p,x,y:LongInt;
  a:String;
Begin;
y:=PajaPY;
for o:=1 to PajaMY do begin;
  x:=PajaPX;
  for i:=1 to PajaMX do begin;
    p:=PajaD[o][i];
    if (p=0) then a:=PixsD[0] else a:=PixsD[1];
    if (pajaO[o][i]<>p) then Ki(x,y,a,PajaBC,p);
    inc(x,Length(a));
    end;
  inc(y);
  end;
pajaO:=pajaD;
End;



procedure DrawBorder;
Begin;
WinKi(PajaPX-1,PajaPY-1,PajaMX*2,PajaMY,PajaBC,PajaBF,'');
End;



Procedure PutOneAlakzatToTable(x,y:LongInt;Var d:AlakzatRec);
Var i,o,b:LongInt;
Begin;
dec(x);dec(y);
for o:=1 to 4 do for i:=1 to 4 do begin;
  b:=d.d[o][i];
  if (b<>0) then PajaD[y+o][x+i]:=d.c;
  end;
End;



Procedure RotateAlakzat90Scale(Var d:AlakzatRec);
Var
  od:AlakzatRec;
  x,y,mx,my:LongInt;
Begin;
od:=d;
fillchar(d,sizeof(d),0);
mx:=od.sx+1;my:=od.sy+1;
d.sx:=od.sy;
d.sy:=od.sx;
d.c:=od.c;
for y:=1 to od.sy do for x:=1 to od.sx do d.d[mx-x][y]:=od.d[y][x];
End;



Function IsTherePlace(x,y:LongInt;Var d:AlakzatRec):Boolean;
Var i,o,b1,b2:LongInt;
Begin;
IsTherePlace:=False;
if (x<1) then Exit;
dec(x);
dec(y);
if (x+d.sx>PajaMX) then Exit;
if (y+d.sy>PajaMY) then Exit;

for o:=1 to d.sy do for i:=1 to d.sx do begin;
  b1:=PajaD[y+o][x+i];
  b2:=d.d[o][i];
  if (b1<>0) and (b2<>0) then Exit;
  end;

IsTherePlace:=True;
End;



Function DeleteFullLines:LongInt;
Var
  i,b,x,cy,ny:LongInt;
  nt:OnePajaRec;
Begin;
FillChar(nt,SizeOf(nt),0);
ny:=PajaMY;
for cy:=PajaMY downto 1 do begin;
  i:=0;
  for x:=1 to PajaMX do begin;
    b:=PajaD[cy][x];
    if (b<>0) then inc(i);
    end;
  if (i<>PajaMX) then begin;
    nt[ny]:=PajaD[cy];
    dec(ny);
    end;
  end;
PajaD:=nt;
DeleteFullLines:=ny;
End;



Function CalcScore(li,th:LongInt):LongInt;
Begin;
CalcScore:=th*9+li*36;
End;
