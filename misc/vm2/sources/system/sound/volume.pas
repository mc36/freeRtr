{$heap 127k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

const entriesMax=256;
Var
  pipe:LongInt;
  entryNum:LongInt;
  entryCur:array[1..entriesMax] of LongInt;
  entryMin:array[1..entriesMax] of LongInt;
  entryMax:array[1..entriesMax] of LongInt;
  entryNam:array[1..entriesMax] of String;


Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function exchange(Var buf;siz:LongInt):LongInt;
Label f1;
Var i,o:LongInt;
Begin;
pipeLineSend(pipe,buf,siz);
f1:
relequish;
siz:=1024*32;
if (pipeLineRecv(pipe,buf,siz)<>0) then siz:=0;
if (siz<1) then begin;
  pipeLineStats(pipe,o,i,i);
  if (o=0) then immErr('sound device closed connection!');
  goto f1;
  end;
exchange:=siz;
End;

Function conv(i:LongInt):String;
Var
  a:String;
  ab0:byte absolute a;
Begin;
a:=BStr(i);
while (ab0<10) do a:=' '+a;
conv:=a;
End;




Label f1,f2;
Var
  buf:array[1..1024*16] of byte;
  i,o,p:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
BEGIN;
WriteLn('volume controller v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<1) then immErr('using: vol.code <process> [num] [value]');
a:=paramStr(1);
i:=BVal(a);
if (i=0) then i:=BugOS_findProcNam(a);
if (i=0) then immErr('process not found!');

if (pipeLineCreate(pipe,i,65536,true)<>0) then immErr('error opening pipeline!');

buf[1]:=1;
p:=exchange(buf,1);
WriteLn('device information:');
for i:=1 to p do write(chr(buf[i]));

buf[1]:=2;
o:=exchange(buf,1);
entryNum:=0;
p:=0;
while (p<o) do begin;
  if (entryNum>=entriesMax) then break;
  inc(entryNum);
  move(buf[p+1],entryMin[entryNum],sizeof(i));
  move(buf[p+5],entryMax[entryNum],sizeof(i));
  inc(p,8);
  ab0:=0;
  while (1=1) do begin;
    inc(p);
    i:=buf[p];
    if (i=0) then break;
    inc(ab0);
    ab[ab0]:=i;
    end;
  entryNam[entryNum]:=a;
  end;

o:=BVal(paramStr(2));
p:=BVal(paramStr(3));
if (o>=1) and (o<=entryNum) then begin;
  WriteLn('setting '+entryNam[o]+' to '+BStr(p)+'...');
  move(o,buf[2],sizeof(o));
  move(p,buf[6],sizeof(p));
  buf[1]:=4;
  exchange(buf,9);
  if (buf[1]=1) then a:='successful!' else a:='failed!';
  WriteLn(a);
  end;


entryCur[1]:=$03030303;
exchange(entryCur,1);

WriteLn('mixer values:');
WriteLn('       num       min       max      curr  name');
for i:=1 to entryNum do
 writeLn(conv(i)+conv(entryMin[i])+conv(entryMax[i])+conv(entryCur[i])+'  '+entryNam[i]);
END.