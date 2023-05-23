{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc crt.inc}

Var pipe,size,divisor:LongInt;

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

Procedure putPeak(num:LongInt);
Var
  i:LongInt;
  c:Char;
  cb:Byte absolute c;
Begin;
if (divisor<0) then begin;
  write(bstr(-num)+'  ');
  exit;
  end;
for i:=1 to 32 do begin;
  case i of
    1:textColor($0a);
    24:textColor($0e);
    28:textColor($0c);
    end;
  if (i>num) then cb:=176 else cb:=219;
  write(c);
  end;
End;


Label f1,f2;
Var
  buf:array[1..1024*32] of byte;
  bufI:array[1..1] of Integer absolute buf;
  i,o,p,l,r:LongInt;
  a:String;
BEGIN;
WriteLn('peak level v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<1) then immErr('using: peak.code <process> [divisor] [number]');
a:=paramStr(1);
i:=BVal(a);
if (i=0) then i:=BugOS_findProcNam(a);
if (i=0) then immErr('process not found!');

if (pipeLineCreate(pipe,i,65536,true)<>0) then immErr('error opening pipeline!');
buf[1]:=5;
exchange(buf,1);
move(buf,size,sizeof(size));
size:=size shr 2;

divisor:=BVal(paramStr(2));
if (divisor=0) then divisor:=200;
divisor:=divisor*size;

a:=paramStr(3);
if (a<>'') then begin;
  WriteLn('dialing '+a+'...');
  move(a,buf,sizeof(a));
  buf[1]:=9;
  exchange(buf,length(a)+1);
  if (buf[1]=1) then a:='successful!' else a:='failed!';
  WriteLn(a);
  end;

WriteLn('');
i:=0;
buf[1]:=7;
pipelineSend(pipe,buf,1);
f1:
if keypressed then begin;
  textColor(7);
  WriteLn('');
  Halt(0);
  end;
buf[1]:=7;
exchange(buf,1);
p:=1;l:=0;r:=0;
for o:=1 to size do begin;
  i:=bufI[p];
  if (i<0) then i:=-i;
  inc(l,i);
  inc(p);
  i:=bufI[p];
  if (i<0) then i:=-i;
  inc(r,i);
  inc(p);
  end;
l:=l div divisor;
r:=r div divisor;
putPeak(l);
textColor(7);
Write(' - ');
putPeak(r);
Write(#13);
goto f1;
END.