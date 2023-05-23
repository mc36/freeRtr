{$heap 63k}
{$stack 15k}
{$sysinc system.inc}
{$sysinc crt.inc}
{$sysinc filesys.inc}
{$sysinc pipeline.inc}
{$sysinc param.inc}

Function getWord(var a:string):string;
Var i:word;
Begin;
i:=pos(' ',a);
if (i<1) then i:=666;
getWord:=copy(a,1,i-1);
a:=copy(a,i+1,255);
End;

Procedure immErr(a:string);
Begin;
WriteLn(a);
halt(1);
End;


Label f1,f2,f3;
Var
  daemons:LongInt;
  a,b,c:String;
  i,o,p:LongInt;
  w:Word;
BEGIN;
WriteLn('daemon run v1.0, done by Mc at '#%date' '#%time'.');
b:=GetAllParameters;
a:=getWord(b);
daemons:=BVal(a);
if (daemons<>0) then a:=getWord(b);
if (a='') then begin;
  writeln('using: daemonrun.code [number] <filename> [parameters]');
  halt(1);
  end;
if (daemons<1) then daemons:=1;
WriteLn('executable: "'+a+'"');
WriteLn('parameters: "'+b+'"');
WriteLn('daemonizations: '+BStr(daemons));
i:=xExecInside(a,b,o,p);
if (i<>0) then begin;
  writeLn('error: '+BStr(i));
  halt(1);
  end;
WriteLn('started, pid='+BStr(o)+', pipe='+BStr(p));

c:='';
f1:
i:=200;
if (pipeLineRecv(p,a[1],i)<>0) then i:=0;
if (i>0) then begin;
  a[0]:=chr(i);
  c:=c+a;
  f2:
  i:=pos(#0#0#0#0,c);
  if (i>0) then begin;
    write(copy(c,1,i-1));
    c:=copy(c,i+4,666);
    dec(daemons);
    if (daemons>0) then goto f2;
    WriteLn('process daemonized!');
    Halt(0);
    end;
  o:=0;
  while (copy(c,length(c),666)=#0) do begin;
    c:=copy(c,1,length(c)-1);
    inc(o);
    end;
  Write(c);
  c:='';
  for i:=1 to o do c:=c+#0;
  goto f1;
  end;
while keypressed do begin;
  w:=ReadKey;
  pipeLineSend(p,w,sizeof(w));
  end;
pipeLineStats(p,o,i,i);
if (o=0) then begin;
  WriteLn('process terminated!');
  Halt(1);
  end;
relequish;
goto f1;
END.