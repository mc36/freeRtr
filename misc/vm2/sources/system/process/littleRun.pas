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


Label f1;
Var
  a,b,c:String;
  i,o,p:LongInt;
  w:Word;
BEGIN;
WriteLn('little run v1.0, done by Mc at '#%date' '#%time'.');
b:=GetAllParameters;
a:=getWord(b);
if (a='') then begin;
  writeln('using: littlerun.code <filename> [parameters]');
  halt(1);
  end;
WriteLn('executable: "'+a+'"');
WriteLn('parameters: "'+b+'"');
i:=xExecInside(a,b,o,p);
if (i<>0) then begin;
  writeLn('error: '+BStr(i));
  halt(1);
  end;
WriteLn('started, pid='+BStr(o)+', pipe='+BStr(p));
w:=$8001;pipeLineSend(p,w,sizeof(w));
w:=70;pipeLineSend(p,w,sizeof(w));
w:=20;pipeLineSend(p,w,sizeof(w));

f1:
i:=255;
if (pipeLineRecv(p,a[1],i)<>0) then i:=0;
if (i>0) then begin;
  a[0]:=chr(i);
  Write(a);
  goto f1;
  end;
while keypressed do begin;
  w:=ReadKey;
  pipeLineSend(p,w,sizeof(w));
  end;
pipeLineStats(p,o,i,i);
if (o=0) then begin;
  WriteLn('process terminated!');
  Halt(0);
  end;
relequish;
goto f1;
END.