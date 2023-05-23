{$heap 63k}
{$stack 15k}
{$sysinc system.inc}
{$sysinc crt.inc}
{$sysinc filesys.inc}
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

Procedure doRun(a,b:String);
Label f1,f2;
Var
  i,o,p:LongInt;
  c:String;
  w:Word;
Begin;
WriteLn('');
WriteLn('"'+a+'" "'+b+'"');
i:=xExecInside(a,b,o,p);
if (i<>0) then begin;
  writeLn('error: '+BStr(i));
  exit;
  end;
c:='';
f1:
i:=200;
if (pipeLineRecv(p,a[1],i)<>0) then i:=0;
if (i>0) then begin;
  a[0]:=chr(i);
  c:=c+a;
  i:=pos(#0#0#0#0,c);
  if (i>0) then begin;
    write(copy(c,1,i-1));
    goto f2;
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
pipeLineStats(p,o,i,i);
if (o=0) then goto f2;
relequish;
goto f1;
f2:
pipeLineClose(p);
WriteLn('');
End;

Var
  t:xtText;
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('system loader v1.0, done by Mc at '#%date' '#%time'.');
a:=GetAllParameters;
if (a='') then immErr('using: sysload.code <process list>');
if (xtOpen(t,a,true)<>0) then immErr('error opening config file!');
while not xtEOF(t) do begin;
  a:=xtReadLn(t,666);
  i:=pos(';',a);
  if (i>0) then a:=copy(a,1,i-1);
  if (a='') then continue;
  doRun(getWord(a),a);
  end;
WriteLn('successfully finished!');
END.