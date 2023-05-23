{$heap 63k}
{$stack 15k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
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
  t:xtText;
  a,b,c:String;
  i,o,p:LongInt;
  w:Word;
BEGIN;
WriteLn('capture run v1.0, done by Mc at '#%date' '#%time'.');
b:=GetAllParameters;
c:=getWord(b);
a:=getWord(b);
if (a='') then immErr('using: capturerun.code <output> <filename> [parameters]');
WriteLn('executable: "'+a+'"');
WriteLn('parameters: "'+b+'"');
WriteLn('    output: "'+c+'"');
if (xCreate(c)<>0) then immErr('error creating output!');
if (xtOpen(t,c,false)<>0) then immErr('error opening output!');
i:=xExecInside(a,b,o,p);
if (i<>0) then immErr('error: '+xGetErrorName(i));
WriteLn('started, pid='+BStr(o)+', pipe='+BStr(p));
f1:
i:=255;
if (pipeLineRecv(p,a[1],i)<>0) then i:=0;
if (i>0) then begin;
  a[0]:=chr(i);
  xtWrite(t,a);
  Write(a);
  goto f1;
  end;
pipeLineStats(p,o,i,i);
if (o=0) then begin;
  xtClose(t);
  WriteLn('process terminated!');
  Halt(0);
  end;
while keypressed do begin;
  w:=ReadKey;
  pipeLineSend(p,w,sizeof(w));
  end;
relequish;
goto f1;
END.