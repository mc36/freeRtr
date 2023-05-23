{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc crt.inc}

Var pipe,size:LongInt;

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

Label f1;
Var
  buf:array[1..1024*32] of byte;
  i,o,p,s:LongInt;
  a:String;
  f:xFile;
BEGIN;
WriteLn('raw player v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<2) then immErr('using: play.code <process> <file> [number]');
a:=paramStr(1);
i:=BVal(a);
if (i=0) then i:=BugOS_findProcNam(a);
if (i=0) then immErr('process not found!');

if (pipeLineCreate(pipe,i,65536,true)<>0) then immErr('error opening pipeline!');
buf[1]:=5;
exchange(buf,1);
move(buf,size,sizeof(size));

a:=paramStr(3);
if (a<>'') then begin;
  WriteLn('dialing '+a+'...');
  move(a,buf,sizeof(a));
  buf[1]:=9;
  exchange(buf,length(a)+1);
  if (buf[1]=1) then a:='successful!' else a:='failed!';
  WriteLn(a);
  end;

a:=paramStr(2);
WriteLn('playing '+a+'...');
if (xOpen(f,a,xGenFilMod_r)<>0) then immErr('error opening file!');
s:=xFileSize(f);
fillchar(buf,sizeof(buf),0);
buf[1]:=6;
pipeLineSend(pipe,buf,size+1);
Write('             /'+BStr(s)+'  (buffer='+BStr(size)+')'#13);
p:=0;
f1:
Write(BStr(p)+#13);
i:=s-p;
if keypressed then halt(2);
if (i<1) then Begin;
  fillchar(buf,sizeof(buf),0);
  buf[1]:=6;
  exchange(buf,size+1);
  WriteLn('');
  halt(0);
  end;
if (i>size) then i:=size;
xBlockRead(f,buf[2],i);
fillchar(buf[i+2],size-i,0);
buf[1]:=6;
exchange(buf,size+1);
inc(p,size);
goto f1;
END.