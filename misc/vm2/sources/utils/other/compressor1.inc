Const
  TablesMax=4*1024-1;
  TablesESC=256;
Var
  tableValues:array[0..TablesMax] of Byte;
  tableParent:array[0..TablesMax] of LongInt;
  tableSize:LongInt;
  tableBits:LongInt;
  streamBuf:LongInt;
  streamSiz:LongInt;



Function GetBits:LongInt;
Begin;
while (streamSiz<24) do begin;
  streamBuf:=(getByte shl streamSiz) or streamBuf;
  inc(streamSiz,8);
  end;
GetBits:=((1 shl tableBits)-1) and streamBuf;
streamBuf:=streamBuf shr tableBits;
dec(streamSiz,tableBits);
End;

Procedure PutBits(n:LongInt);
Begin;
streamBuf:=(n shl streamSiz) or streamBuf;
inc(streamSiz,tableBits);
while (streamSiz>=8) do begin;
  putByte(streamBuf);
  streamBuf:=streamBuf shr 8;
  dec(streamSiz,8);
  end;
End;



Procedure initialize_bit;
Begin;
streamBuf:=0;
streamSiz:=0;
End;

Procedure initialize_tab;
Begin;
fillchar(tableParent,sizeof(tableParent),$ff);
fillchar(tableValues,sizeof(tableValues),$ff);
for tableSize:=0 to 255 do tableValues[tableSize]:=tableSize;
tableParent[TablesESC]:=-2;
tableSize:=TablesESC;
tableBits:=9;
End;

Procedure method(i:LongInt);
Begin;
End;



Procedure compress;
Label f1,f2;
Var i,o,p:LongInt;

Procedure updt;
Var i:LongInt;
Begin;
while (1 shl tableBits<=p) do begin;
  PutBits(TablesESC);
  i:=tableBits+1;
  tableBits:=2;
  PutBits(1);
  tableBits:=i;
  end;
End;

Begin;
initialize_bit;
initialize_tab;
p:=-1;
f1:
o:=getByte;
if (o<0) then goto f2;
for i:=p+1 to tableSize do begin;
  if (tableValues[i]<>o) then continue;
  if (tableParent[i]<>p) then continue;
  p:=i;
  goto f1;
  end;
updt;
PutBits(p);
inc(tableSize);
tableValues[tableSize]:=o;
tableParent[tableSize]:=p;
p:=o;
if (tableSize<TablesMax) then goto f1;
PutBits(TablesESC);
tableBits:=2;
PutBits(0);
initialize_tab;
goto f1;
f2:
updt;
PutBits(p);
PutBits(TablesESC);
tableBits:=2;
PutBits(3);
tableBits:=1;
while (streamSiz<>0) do PutBits(0);
End;



Function decompress:String;
Label f1,f2;
Var buf:array[1..TablesMax] of byte;

Function output(p:LongInt):LongInt;
Var s:LongInt;
Begin;
s:=sizeof(buf)+1;
while (p>=0) do begin;
  output:=p;
  dec(s);
  buf[s]:=tableValues[p];
  p:=tableParent[p];
  end;
for p:=s to sizeof(buf) do putByte(buf[p]);
End;

Var i,o,p:LongInt;
Begin;
decompress:='error';
initialize_bit;
f1:
initialize_tab;
o:=GetBits;
output(o);
f2:
inc(tableSize);
p:=GetBits;
if (p=TablesESC) then begin;
  dec(tableSize);
  i:=tableBits;
  tableBits:=2;
  p:=GetBits;
  tableBits:=i;
  case p of
    0:goto f1;
    1:begin; inc(tableBits);goto f2; end;
    3:begin; decompress:='';exit; end;
    end;
  decompress:='unknown command';
  exit;
  goto f1;
  end;
if (tableSize>sizeof(tableValues)) then begin; decompress:='table too long';exit; end;
if (p=tableSize) then begin;
  i:=output(o);
  putByte(i);
  tableValues[tableSize]:=i;
  tableParent[tableSize]:=o;
  o:=p;
  goto f2;
  end;
if (p>tableSize) then begin; decompress:='unknown code';exit; end;
i:=output(p);
tableValues[tableSize]:=i;
tableParent[tableSize]:=o;
o:=p;
goto f2;
End;
