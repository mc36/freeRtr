{$stack 1k}
{$heap 128k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc alap.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
Const
  OutputExt='.opt';

{$include utils.inc}
{$include memory.inc}
{$include asmopt1.inc}
{$include asmopt2.inc}
{$include asmopt3.inc}


Label f1,f2,f3;
Var
  includeD:array[1..8] of record
    n:String;
    p:LongInt;
    end;
  includeN:Byte;
  t,tt:xtText;
  a,b:String;
  i:LongInt;


Procedure DoRemoval;
Var i:LongInt;
Begin;
Write(#13'('+BStr(ProcsedLines)+') working...');
DoTheCommentRemoval;
repeat
  i:=RemovedLines;
  DoTheStackOverUsage;
  DoTheMultipleByOne;
  DoTheDuplicatedMove;
  until (i=RemovedLines);
for i:=1 to LinesNum do xtWriteLn(tt,AllLinesData[i]);
Write(#13'(       )                     '#13);
LinesNum:=0;
End;


BEGIN;
WriteLn('Assembly Optimizer v1.0, done by Mc at '#%date' '#%time'.');

a:=ParamStr(1);
if (a='') then ImmErr('using: asmOpt.code <filename>');
a:=xFExpand(a,1)+xFExpand(a,2);

xErase(a+OutputExt);
if (xCreate(a+OutputExt)<>0) then immErr('error creating target!');
if (xtOpen(tt,a+OutputExt,false)<>0) then immErr('error creating target!');

includeD[1].n:=a+'.asm';
includeD[1].p:=0;
includeN:=1;
RemovedLines:=0;
ProcsedLines:=0;
LinesNum:=0;

WriteLn('Optimizing '+includeD[includeN].n+'...');
Write('(       )');
fillchar(t,sizeof(t),0);
f1:
xtClose(t);
a:=includeD[includeN].n;
if (xtOpen(t,a,true)<>0) then immErr('error opening source ('+a+')!');
xtSetPos(t,includeD[includeN].p);
f2:
Write(#13'('+Bstr(ProcsedLines));
if xtEOF(t) then begin;
  dec(includeN);
  if (includeN<1) then goto f3;
  goto f1;
  end;
b:=xtReadLn(t,255);
inc(ProcsedLines);
a:=kicsi(xLevesz(b));
if IsThisBeg(a,'include') then begin;
  i:=pos(';',a);
  if (i<>0) then a:=copy(a,1,i-1);
  includeD[includeN].p:=xtGetPos(t);
  delete(a,1,7);
  a:=xLevesz(a);
  if (copy(a,2,1)<>':') and (copy(a,1,1)<>'\') then
   a:=xFExpand(includeD[includeN].n,1)+a;
  inc(includeN);
  if (includeN>8) then immErr('too many include files!');
  includeD[includeN].n:=a;
  includeD[includeN].p:=0;
  goto f1;
  end;
inc(LinesNum);
AllLinesData[LinesNum]:=a;
if (LinesNum<LinesMax) then goto f2;
DoRemoval;
goto f2;

f3:
DoRemoval;
xtClose(tt);
Write(#13);
WriteLn(BStr(RemovedLines)+' lines of '+BStr(ProcsedLines)+' removed.');
WriteLn('Successfully finished!');
END.