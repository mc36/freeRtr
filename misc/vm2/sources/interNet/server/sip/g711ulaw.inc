Var
  ulaw2pcmTable:array[0..127] of LongInt;
  pcm2ulawTable:array[0..32768] of Byte;

Procedure generateUlawTable;
Var i,o,p,q,r:LongInt;
Begin;
p:=0;
for q:=0 to 7 do begin;
  r:=$10;
  o:=q+3;
  r:=r shl o;
  for i:=0 to 15 do begin;
    ulaw2pcmTable[p xor $7f]:=(i shl o) or r;
    inc(p);
    end;
  end;
for p:=0 to 32768 do begin;
  q:=192;
  r:=999999;
  for o:=0 to 127 do begin;
    i:=ulaw2pcmTable[o]-p;
    if (i<0) then i:=-i;
    if (i>r) then continue;
    r:=i;
    q:=o;
    end;
  pcm2ulawTable[p]:=q;
  end;
End;

Function pcm2ulawConverter(o:LongInt):LongInt;
Begin;
if (o<0) then o:=pcm2ulawTable[-o] else o:=pcm2ulawTable[o]  or $80;
pcm2ulawConverter:=o;
End;

Function ulaw2pcmConverter(o:LongInt):LongInt;
Var i:LongInt;
Begin;
i:=ulaw2pcmTable[o and $7f];
if (o and $80=0) then i:=-i;
ulaw2pcmConverter:=i;
End;
