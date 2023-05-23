{$heap 15k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc bugos.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$include \sources\internet\kernel\utils\timer2.inc}
Type
  OneDataRecord=record
    last:LongInt;
    pid:LongInt;
    inter:LongInt;
    uid:LongInt;
    prog:String;
    param:String;
    end;
Var
  EntryDat:^array[1..1] of OneDataRecord;
  EntryNum:LongInt;

Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Procedure ReadUpList(a:String);
Label f1;
Const maxTime=82800;
Var
  t:xtText;
  b:String;
  i,o:LongInt;
  d:OneDataRecord;

function getWord:String;
begin;
while (copy(b,1,1)=' ') do b:=copy(b,2,255);
i:=pos(' ',b);
if (i<1) then i:=666;
getWord:=copy(b,1,i-1);
b:=copy(b,i+1,255);
while (copy(b,1,1)=' ') do b:=copy(b,2,255);
end;

procedure resize(n:LongInt);
var
  i:LongInt;
  p:Pointer;
begin;
i:=n*sizeof(OneDataRecord);
if (ExtendedMemoryResize(p,i)<i) then immErr('out of memory!');
EntryDat:=p^;
EntryNum:=n;
end;

Begin;
if (a='') then immErr('using: scheduler.code <config>');
WriteLn('reading '+a+' file...');
if (xtOpen(t,a,true)<>0) then immErr('error opening!');
f1:
if xtEOF(t) then begin;
  xtClose(t);
  WriteLn(BStr(EntryNum)+' entries readed!');
  if (EntryNum<1) then halt(0);
  exit;
  end;
b:=xtReadLn(t,255);
fillchar(d,sizeof(d),0);
i:=BVal(getWord);
if (i<1) then goto f1;
if (i>maxTime) then i:=maxTime;
d.inter:=i;
d.uid:=BVal(getWord);
a:=getWord;
if (a='') then goto f1;
d.prog:=a;
d.param:=b;
d.last:=CurrentTime;
resize(EntryNum+1);
EntryDat^[EntryNum]:=d;
goto f1;
End;

Procedure doOne(var d:OneDataRecord);
Begin;
if (d.pid<>0) then begin;
  if BugOS_ProcessExists(d.pid) then exit;
  WriteLn('process "'+d.prog+'" "'+d.param+'" terminated!');
  d.pid:=0;
  d.last:=currentTime;
  exit;
  end;
if (GetTimePast(d.last)<d.inter) then exit;
WriteLn('process "'+d.prog+'" "'+d.param+'" started!');
BugOS_SetOwnerInfo(d.uid);
if (xExecBgnd(d.prog,d.param,d.pid)<>0) then d.pid:=0;
d.last:=currentTime;
End;


Label f1;
Var
  i:LongInt;
  last:LongInt;
BEGIN;
WriteLn('scheduler v1.0, done by Mc at '#%date' '#%time'.');
EntryNum:=0;
timer2start;
inc(CurrentTime,ticksPerSec*60);
ReadUpList(paramStr(1));
last:=0;
BugOS_SignDaemoning;

f1:
timer2start;
if (GetTimePast(last)=0) then begin;
  relequish;
  goto f1;
  end;
last:=currentTime;
for i:=1 to EntryNum do doOne(EntryDat^[i]);
goto f1;
END.