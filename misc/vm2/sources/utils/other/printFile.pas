{$heap 15k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}



Procedure ImmErr(a:String);
Begin;
WriteLn('');
WriteLn(a);
Halt(2);
End;


Label f1;
Var
  buf:array[1..1024] of byte;
  i,o,q,r,p,s,t:LongInt;
  a:String;
  f:xFile;
BEGIN;
WriteLn('file printer v1.0, done by Mc at '#%date' '#%time'.');
if (ParamCount<>2) then immErr('using: printer.code <process> <file>');
a:=ParamStr(2);
WriteLn('file: '+a);
if (xOpen(f,a,xGenFilMod_r)<>0) then immErr('error opening file!');
a:=ParamStr(1);
q:=BVal(a);
WriteLn('process: '+a);
if (q=0) then q:=BugOS_findProcNam(a);
if (q=0) then immErr('process not found!');
WriteLn('pid: '+BStr(q));
if (pipeLineCreate(q,q,4096,false)<>0) then q:=0;
if (q=0) then immErr('error creating pipeline!');
pipeLineStats(q,o,i,r);
p:=0;
s:=xFileSize(f);
WriteLn('printing '+BStr(s)+' bytes...');
while (p<s) do begin;
  o:=s-p;
  if (o>sizeof(buf)) then o:=sizeof(buf);
  inc(p,o);
  xBlockRead(f,buf,o);
  while (pipeLineSend(q,buf,o)<>0) do begin;
    relequish;
    pipeLineStats(q,t,i,i);
    if (t=0) then immErr('pipeline closed by remote!');
    end;
  Write(#13+BStr(p));
  end;
repeat
  relequish;
  pipeLineStats(q,o,i,p);
  if (o=0) then immErr('pipeline closed by remote!');
  until (p=r);
WriteLn('');
for i:=1 to 16 do relequish;
i:=255;
if (pipeLineRecv(q,a[1],i)<>0) then i:=0;
if (i=0) then begin;
  WriteLn('successful!');
  Halt(0);
  end;
a[0]:=chr(i);
WriteLn('got error codes:');
WriteLn(a);
Halt(1);
END.