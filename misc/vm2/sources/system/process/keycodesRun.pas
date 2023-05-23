{$heap 63k}
{$stack 15k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
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


Label f1,f2;
Var
  a,b,c:String;
  cb:array[0..1] of byte absolute c;
  cb0:byte absolute c;
  i,o,p:LongInt;
  w:Word;
BEGIN;
WriteLn('keycodes run v1.0, done by Mc at '#%date' '#%time'.');
b:=GetAllParameters;
cb0:=0;
f2:
a:=getWord(b);
i:=BVal(a);
if (a='0') or (i<>0) then begin;
  w:=i;
  move(w,cb[cb0+1],sizeof(w));
  inc(cb0,sizeof(w));
  goto f2;
  end;
if (a='') then immErr('using: keycodesrun.code <keycodes> <filename> [parameters]');
WriteLn('executable: "'+a+'"');
WriteLn('parameters: "'+b+'"');
WriteLn('  keycodes: '+BStr(length(c) shr 1)+' keystrokes');
i:=xExecInside(a,b,o,p);
if (i<>0) then immErr('error: '+xGetErrorName(i));
WriteLn('started, pid='+BStr(o)+', pipe='+BStr(p));
pipeLineSend(p,c[1],length(c));
f1:
i:=255;
if (pipeLineRecv(p,a[1],i)<>0) then i:=0;
if (i>0) then begin;
  a[0]:=chr(i);
  Write(a);
  goto f1;
  end;
while keypressed do begin;
  i:=ReadKey;
  i:=(i shl 8) or (i shr 8);
  pipeLineSend(p,i,2);
  end;
pipeLineStats(p,o,i,i);
if (o=0) then begin;
  WriteLn('process terminated!');
  Halt(0);
  end;
relequish;
goto f1;
END.