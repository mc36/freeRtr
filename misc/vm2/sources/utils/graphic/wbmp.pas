{$include filter1.pas}
Const
  proggyName='wireless bitmap v1.0';
  proggyCapa=3;




Function doReadup:Boolean;
Var
  bfi:array[1..1024] of byte;
  bfs,bfp,bfr,bfq:LongInt;
  buf:array[1..maxScreenWidth] of byte;
  sx,sy:LongInt;
  i,o,p,q,r:LongInt;

Function getByte:LongInt;
Var i:LongInt;
Begin;
getByte:=0;
if (bfp>=bfs) then begin;
  i:=bfr;
  if (i>sizeof(bfi)) then i:=sizeof(bfi);
  uppReadFile(bfq,i,bfi);
  inc(bfq,i);
  dec(bfr,i);
  bfs:=i;
  bfp:=0;
  end;
inc(bfp);
getByte:=bfi[bfp];
End;

Function getInt:LongInt;
Var i,o:LongInt;
Begin;
o:=0;
repeat
  i:=getByte;
  o:=(o shl 7) or (i and $7f);
  until (i and $80=0);
getInt:=o;
End;

Begin;
doReadup:=true;
bfr:=uppFileSize;
bfq:=0;
bfp:=0;
bfs:=0;
if (getByte<>0) then exit;
if (getByte<>0) then exit;
sx:=getInt;
sy:=getInt;
fillchar(buf,768,0);
fillchar(buf[4],3,255);
uppPutPalette(sx,sy,buf);
for p:=1 to sy do begin;
  r:=0;
  for o:=1 to (sx+7) shr 3 do begin;
    q:=getByte;
    for i:=1 to 8 do begin;
      inc(r);
      if (q and $80<>0) then buf[r]:=1 else buf[r]:=0;
      q:=q shl 1;
      end;
    end;
  uppPutLine(0,p,sx,buf);
  end;
doReadup:=false;
End;


Procedure doWriter;
Var
  bfi:array[1..2048] of byte;
  bfp,bfq:LongInt;
  buf:array[1..maxScreenWidth] of byte;
  sx,sy:LongInt;
  i,o,p,q,r:LongInt;

Procedure putByte(i:LongInt);
Begin;
inc(bfp);
bfi[bfp]:=i;
if (bfp<1024) then exit;
uppWriteFile(bfq,bfp,bfi);
inc(bfq,bfp);
bfp:=0;
End;

Procedure putInt(p:LongInt);
Var
  b:array[0..16] of byte;
  i,o:LongInt;
Begin;
fillchar(b,sizeof(b),0);
o:=0;
while (p>0) do begin;
  inc(o);
  b[o]:=p and $7f;
  p:=p shr 7;
  end;
if (o<1) then o:=1;
for i:=2 to o do inc(b[i],$80);
for i:=o downto 1 do putByte(b[i]);
End;


Begin;
bfq:=0;
bfp:=0;
putByte(0);
putByte(0);
uppGetPalette(sx,sy,buf);
putInt(sx);
putInt(sy);
for p:=1 to sy do begin;
  uppGetLine(0,p,sx,buf);
  r:=0;
  for o:=1 to (sx+7) shr 3 do begin;
    q:=0;
    for i:=0 to 7 do begin;
      q:=q shl 1;
      inc(r);
      if (buf[r]<>0) then inc(q);
      end;
    putByte(q);
    end;
  end;
uppWriteFile(bfq,bfp,bfi);
End;

{$include filter2.pas}BEGIN;doMain;END.