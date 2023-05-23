Procedure gpsParser(bb:Byte);
Label f1;
Var
  gb:array[0..128] of byte absolute gpsReaded;
  gb0:byte absolute gpsReaded;
  i,o,p:LongInt;
Begin;
case gpsTemp1 of
  3:begin;
    inc(gb0);
    gb[gb0]:=bb;
    if (gb0>=gpsTemp2) then gpsTemp1:=4;
    end;
  4:begin;
    gpsTemp3:=bb and $7f;
    gpsTemp1:=5;
    end;
  5:begin;
    gpsTemp3:=(gpsTemp3 shl 8) or bb;
    gpsTemp1:=6;
    o:=0;
    for i:=1 to gpsTemp2 do inc(o,gb[i]);
    o:=o and $7fff;
    if (o<>gpsTemp3) then gpsTemp1:=0;
    end;
  0:begin;
    if (bb=$a0) then gpsTemp1:=1;
    end;
  1:begin;
    if (bb=$a2) then gpsTemp1:=2 else gpsTemp1:=0;
    gb0:=0;
    end;
  2:begin;
    inc(gb0);
    gb[gb0]:=bb;
    if (gb0<2) then exit;
    gpsTemp2:=ReadWordMSB(gb[1]) and $7fff;
    gpsTemp1:=3;
    if (gpsTemp2<1) or (gpsTemp2>250) then gpsTemp1:=0;
    gb0:=0;
    end;
  6:begin;
    if (bb=$b0) then gpsTemp1:=7 else gpsTemp1:=0;
    end;
  7:begin;
    gpsTemp1:=0;
    if (bb=$b3) then goto f1;
    end;
  else gpsTemp1:=0;
  end;
exit;
f1:
{process data}
End;




Procedure gspPrepare(var buffer;var size:LongInt);
Var
  buf:array[1..128] of byte absolute buffer;
  i,o:LongInt;
Begin;
o:=0;
for i:=1 to size do inc(o,buf[i]);
move(buf,buf[5],size);
buf[1]:=$a0;
buf[2]:=$a2;
WriteWordMSB(buf[3],size);
inc(size,8);
WriteWordMSB(buf[size-3],o);
buf[size-1]:=$b0;
buf[size-0]:=$b3;
End;
