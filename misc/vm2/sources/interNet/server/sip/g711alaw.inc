Var
  alaw2pcmTable:array[0..127] of LongInt;
  pcm2alawTable:array[0..32768] of Byte;

Procedure generateAlawTable;
Var i,o,p,q,r:LongInt;
Begin;
p:=0;
for q:=0 to 7 do begin;
  r:=$10;
  o:=q+3;
  if (q=0) then begin; r:=0;inc(o); end;
  r:=r shl o;
  for i:=0 to 15 do begin;
    alaw2pcmTable[p xor $55]:=(i shl o) or r;
    inc(p);
    end;
  end;
for p:=0 to 32768 do begin;
  q:=192;
  r:=999999;
  for o:=0 to 127 do begin;
    i:=alaw2pcmTable[o]-p;
    if (i<0) then i:=-i;
    if (i>r) then continue;
    r:=i;
    q:=o;
    end;
  pcm2alawTable[p]:=q;
  end;
End;

Function pcm2alawConverter(o:LongInt):LongInt;
Begin;
if (o<0) then o:=pcm2alawTable[-o] else o:=pcm2alawTable[o]  or $80;
pcm2alawConverter:=o;
End;

Function alaw2pcmConverter(o:LongInt):LongInt;
Var i:LongInt;
Begin;
i:=alaw2pcmTable[o and $7f];
if (o and $80=0) then i:=-i;
alaw2pcmConverter:=i;
End;
