{$include filter1.pas}
{$sysinc alap.inc}
Const
  proggyName='cisco ip phone image v1.0';
  proggyCapa=3;




Function doReadup:Boolean;
Label f1;
Var
  bfi:array[1..1024] of byte;
  bfs,bfp,bfr,bfq,bbv,bbs:LongInt;
  buf:array[1..maxScreenWidth] of byte;
  sx,sy:LongInt;
  i,o,p:LongInt;
  a:String;
  ab:array[0..15] of byte absolute a;
  ab0:byte absolute a;

Function getByte:LongInt;
Label f1;
Var i:LongInt;
Begin;
getByte:=-1;
f1:
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
i:=bfi[bfp];
if (bfp>bfs) then exit;
if (i in [0,8,9,10,13,32,255]) then goto f1;
getByte:=i;
End;

Function getTag:String;
Label f1,f2;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
getTag:='';
f1:
i:=getByte;
if (i<1) then exit;
if (i<>$3c) then goto f1;
ab0:=0;
f2:
i:=getByte;
if (i<1) then exit;
if (i=$3e) then begin;
  getTag:=a;
  exit;
  end;
inc(ab0);
ab[ab0]:=i;
goto f2;
End;

Function getData:String;
Label f1;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
getData:='';
ab0:=0;
f1:
i:=getByte;
if (i<1) then exit;
if (i=$3c) then begin;
  dec(bfp);
  getData:=a;
  exit;
  end;
inc(ab0);
ab[ab0]:=i;
goto f1;
End;

Function getBits:LongInt;
Begin;
if (bbs<1) then begin;
  ab0:=3;
  ab[1]:=$24;
  ab[2]:=getByte;
  ab[3]:=getByte;
  bbv:=BVal(a);
  bbs:=8;
  end;
getBits:=bbv and 3;
bbv:=bbv shr 2;
dec(bbs,2);
End;

Begin;
doReadup:=true;
bfr:=uppFileSize;
bfq:=0;
bfp:=0;
bfs:=0;
bbs:=0;
bbv:=0;
sx:=-1;
sy:=-1;
f1:
a:=kicsi(getTag);
if (a='') then exit;
if (a='width') then sx:=BVal(getData);
if (a='height') then sy:=BVal(getData);
if (a<>'data') then goto f1;
if (sx<1) then exit;
if (sy<1) then exit;
fillchar(buf,768,0);
fillchar(buf[4],3,255);
uppPutPalette(sx,sy,buf);
for p:=1 to sy do begin;
  for o:=1 to sx do if (getBits=0) then buf[o]:=1 else buf[o]:=0;
  uppPutLine(0,p,sx,buf);
  end;
doReadup:=false;
End;


Procedure doWriter;
Const bitVals:array[0..15] of byte=($30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$41,$42,$43,$44,$45,$46);
Var
  bfi:array[1..2048] of byte;
  bfp,bfq,bbv,bbs:LongInt;
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

Procedure putStr(a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
for i:=1 to ab0 do putByte(ab[i]);
End;

Procedure putLn(a:String);
Begin;
putStr(a);
putByte(13);
putByte(10);
End;

Procedure putBits(i:LongInt);
Begin;
inc(bbv,i shl bbs);
inc(bbs,2);
if (bbs<8) then exit;
putByte(bitVals[bbv shr 4]);
putByte(bitVals[bbv and $f]);
bbv:=0;
bbs:=0;
End;

Begin;
bfq:=0;
bfp:=0;
bbs:=0;
bbv:=0;
uppGetPalette(sx,sy,buf);
putLn('<CiscoIPPhoneImage>');
putLn('  <LocationX>-1</LocationX>');
putLn('  <LocationY>-1</LocationY>');
putLn('  <Width>'+BStr(sx)+'</Width>');
putLn('  <Height>'+BStr(sy)+'</Height>');
putLn('  <Depth>2</Depth>');
putStr('  <Data>');
for p:=1 to sy do begin;
  uppGetLine(0,p,sx,buf);
  for o:=1 to sx do if (buf[o]=0) then putBits(3) else putBits(0);
  end;
while (bbs>0) do putBits(0);
putLn('</Data>');
putLn('  <Title>Temporary title</Title>');
putLn('  <Prompt>Temporary prompt</Prompt>');
putLn('</CiscoIPPhoneImage>');
uppWriteFile(bfq,bfp,bfi);
End;

{$include filter2.pas}BEGIN;doMain;END.