Var
  ScrSizX,ScrSizY:Word;
  RefreshScr:byte; {1-curroid, 2-curroid, 4-alloid, 8-descr, 64-status, 128-screen}




Procedure drawScreen;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;

Function getLin(p:LongInt):String;
Var b:String;
Begin;
if (p<1) or (p>mibEntryNum) then begin;
  fillchar(a,sizeof(a),32);
  ab0:=ScrSizX;
  end;
a:=mibEntryDat^[p].nam;
a:=copy(a,pos(#0,a)+1,666);
while (ab0+ab0<ScrSizX) do begin; inc(ab0);ab[ab0]:=32; end;
b:=a;
a:=mibEntryDat^[p].oid;
while (ab0+ab0<ScrSizX) do begin; inc(ab0);ab[ab0]:=32; end;
getLin:=a+b;
End;


Begin;
if (RefreshScr and $80<>0) then begin;
  ConsoleSize(ScrSizX,ScrSizY);
  textColor($07);clrscr;
  RefreshScr:=$fe;
  end;
if (RefreshScr and $40<>0) then begin;
  textColor($1f);
  a:='oid';
  while (ab0<ScrSizX shr 1) do begin; inc(ab0);ab[ab0]:=32; end;
  a:=a+'name';
  while (ab0<ScrSizX) do begin; inc(ab0);ab[ab0]:=32; end;
  gotoXY(1,1);Write(a);
  a:='description';
  while (ab0<ScrSizX) do begin; inc(ab0);ab[ab0]:=32; end;
  gotoXY(1,ScrSizY shr 1);Write(a);
  a:='keys:  f3=view  f4=get  f5=next  f10=exit';
  while (ab0<ScrSizX-1) do begin; inc(ab0);ab[ab0]:=32; end;
  gotoXY(1,ScrSizY);Write(a);
  end;
if (RefreshScr and $08<>0) then begin;
  p:=0;
  for o:=1 to descrBeg do begin;
    inc(p);
    move(descrBuf[p],a[1],sizeof(a)-1);
    i:=descrSiz-p;
    if (i>255) then i:=255;
    if (i<0) then i:=0;
    ab0:=i;
    i:=pos(#0,a)-1;
    if (i<0) then i:=ab0;
    ab0:=i;
    inc(p,ab0);
    end;
  textColor($07);
  for o:=(ScrSizY shr 1)+1 to ScrSizY-1 do begin;
    inc(p);
    move(descrBuf[p],a[1],sizeof(a)-1);
    i:=descrSiz-p;
    if (i>255) then i:=255;
    if (i<0) then i:=0;
    ab0:=i;
    i:=pos(#0,a)-1;
    if (i<0) then i:=ab0;
    ab0:=i;
    inc(p,ab0);
    while (ab0<ScrSizX) do begin; inc(ab0);ab[ab0]:=32; end;
    gotoXY(1,o);
    Write(a);
    end;
  end;
if (RefreshScr and $04<>0) then begin;
  textColor($07);
  p:=oidBeg+1;
  for o:=2 to (ScrSizY shr 1)-1 do begin;
    gotoXY(1,o);
    Write(getLin(p));
    inc(p);
    end;
  RefreshScr:=2;
  end;
if (RefreshScr and $02<>0) then begin;
  textColor($2f);
  gotoXY(1,oidCur-oidBeg+1);
  Write(getLin(oidCur));
  RefreshScr:=0;
  end;
if (RefreshScr and $01<>0) then begin;
  textColor($07);
  gotoXY(1,oidCur-oidBeg+1);
  Write(getLin(oidCur));
  end;
RefreshScr:=0;
End;
