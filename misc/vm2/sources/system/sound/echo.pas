{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
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

Label f1,f2;
Var
  buf:array[1..1024*32] of byte;
  i,o,p:LongInt;
  a:String;
BEGIN;
WriteLn('echoer v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<1) then immErr('using: echo.code <process> [number]');
a:=paramStr(1);
i:=BVal(a);
if (i=0) then i:=BugOS_findProcNam(a);
if (i=0) then immErr('process not found!');

if (pipeLineCreate(pipe,i,65536,true)<>0) then immErr('error opening pipeline!');
buf[1]:=5;
exchange(buf,1);
move(buf,size,sizeof(size));

a:=paramStr(2);
if (a<>'') then begin;
  WriteLn('dialing '+a+'...');
  move(a,buf,sizeof(a));
  buf[1]:=9;
  exchange(buf,length(a)+1);
  if (buf[1]=1) then a:='successful!' else a:='failed!';
  WriteLn(a);
  end;

fillchar(buf,sizeof(buf),0);
buf[1]:=8;
pipelineSend(pipe,buf,size+1);
p:=0;
i:=size;
f1:
Write(BStr(p)+'  (buffer='+BStr(i)+')'#13);
if keypressed then goto f2;
buf[1]:=8;
i:=exchange(buf,size+1);
move(buf,buf[2],i);
inc(p,i);
goto f1;
f2:
WriteLn('');
Halt(0);
END.