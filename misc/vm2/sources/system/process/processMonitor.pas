{$heap 63k}
{$stack 7k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc hex.inc}
{$sysinc filesys2.inc}
{$sysinc bugos.inc}
Type
  OneProcessRecord=record
    pid:LongInt;
    seq:LongInt;
    uid:LongInt;
    ppid:LongInt;
    right:LongInt;
    pages:LongInt;
    pipes:LongInt;
    files:LongInt;
    ticks:LongInt;
    runs:LongInt;
    param:String;
    name:String;
    end;
Var
  ProcessDat:^array[1..1] of OneProcessRecord;
  ProcessNum:LongInt;
  filterProcs:Boolean;
  ntry:OneProcessRecord;
  ScrSizX,ScrSizY:Word;
  RefreshScr:byte; {16-cursor, 32-listing, 64-details, 128-screen}
  ProcCur,ProcScr:LongInt;

Function conv2hex(i:LongInt):String;
Begin;
conv2hex:=byte2hextype(i shr 24)+byte2hextype(i shr 16)+byte2hextype(i shr 8)+byte2hextype(i);
End;

Procedure processResize;
Var
  i,o:LongInt;
  p:Pointer;
Begin;
o:=ProcessNum*sizeof(OneProcessRecord);
i:=ExtendedMemoryResize(p,o);
if (i<o) then begin;
  WriteLn('error allocating memory!');
  halt(1);
  end;
ProcessDat:=p^;
End;

Procedure RestartComputer;
Label f1;
Var i,o,p:LongInt;
Begin;
f1:
BugOS_totalSysInfo(p,i,o);
if (p<2) then Halt(0);
for i:=p-1 downto 0 do begin;
  o:=BugOS_findProcNum(i);
  BugOS_KillProcess(o);
  end;
BugOS_MyOwnerInfo(i,o);
if (i<>0) then Halt(0);
goto f1;
End;


Procedure ReadUpProcesses;
Var
  d:record
    nam:string;
    par:string;
    end;
  i,o,p,q,r:LongInt;
Begin;
BugOS_totalSysInfo(i,o,p);
ProcessNum:=i+1;
processResize;
fillchar(ntry,sizeof(ntry),0);
ntry.name:='___ system - kernel ___';
ntry.files:=p;
ntry.pipes:=o;
BugOS_totalMemInfo(i,o,p);
ntry.pages:=o;
ntry.param:='total='+alakit(i)+'  free='+alakit(p);
move(ntry,ProcessDat^[1],sizeof(ntry));
for r:=2 to ProcessNum do begin;
  ntry.seq:=r-1;
  q:=BugOS_findProcNum(r-2);
  ntry.pid:=q;
  BugOS_ProcessInfo(q,i,o,p);
  ntry.pages:=i;
  ntry.pipes:=o;
  ntry.files:=p;
  BugOS_ProcessName(q,d,i,o,p);
  ntry.uid:=i;
  ntry.ppid:=o;
  ntry.right:=p;
  ntry.name:=d.nam;
  ntry.param:=d.par;
  BugOS_ProcessStat(q,i,o);
  ntry.ticks:=i;
  ntry.runs:=o;
  move(ntry,ProcessDat^[r],sizeof(ntry));
  end;
End;

Procedure filterProcesses;
Var i,o,p:LongInt;
Begin;
BugOS_MyOwnerInfo(p,o);
o:=1;
for i:=2 to ProcessNum do begin;
  if (ProcessDat^[i].uid<>p) then continue;
  inc(o);
  ProcessDat^[o]:=ProcessDat^[i];
  end;
ProcessNum:=o;
processResize;
End;

Procedure SortProcesses;

function QuickReadOne(n:LongInt):String;
begin;
QuickReadOne:=conv2hex(ProcessDat^[n].pid);
end;

procedure QuickSwapOne(n1,n2:LongInt);
begin;
move(ProcessDat^[n1],ntry,sizeof(ntry));
move(ProcessDat^[n2],ProcessDat^[n1],sizeof(ntry));
move(ntry,ProcessDat^[n2],sizeof(ntry));
end;

{$sysinc quicksrt.inc}
Begin;
if (ProcessNum>1) then QuickSort(1,ProcessNum);
End;


Procedure AlignCheck;
Var i:LongInt;
Begin;
if (ProcCur>ProcessNum) then ProcCur:=ProcessNum;
if (ProcCur<1) then ProcCur:=1;
i:=ProcCur-1;
if (ProcScr>i) then ProcScr:=i;
i:=ProcCur-ScrSizY+7;
if (ProcScr<i) then ProcScr:=i;
End;


Procedure ReRead;
Begin;
ReadUpProcesses;
if filterProcs then filterProcesses;
SortProcesses;
AlignCheck;
End;

Label f1,f2,f3;
Var
  i,o:LongInt;
  a:String;
  w:Word;
BEGIN;
WriteLn('Process Monitor v1.0, done by Mc at '#%date' '#%time'.');

BugOS_MyOwnerInfo(i,o);
if (i<>0) or (o<>0) then begin;
  BugOS_dropPrivileges;
  WriteLn('privileges dropped...');
  filterProcs:=True;
  end else filterProcs:=false;

RefreshScr:=$ff;
ReRead;
ProcCur:=1;
ProcScr:=0;
f1:
if (RefreshScr and $80<>0) then begin;
  ConsoleSize(ScrSizX,ScrSizY);
  textColor($07);clrscr;
  textColor($2f);
  a:='keys:  alt+r=reread  alt+k=kill  alt+f=filter  ctrl+alt+esc=reboot';
  while (length(a)<ScrSizX) do a:=a+' ';
  a:=copy(a,1,ScrSizX-1);
  GotoXY(1,ScrSizY);Write(a);
  textColor($1f);
  a:='procID    name';
  while (length(a)<ScrSizX) do a:=a+' ';
  GotoXY(1,1);Write(a);
  RefreshScr:=$ff;
  end;
if (RefreshScr and $40<>0) then begin;
  textColor($17);
  if (ProcCur<1) or (ProcCur>ProcessNum) then begin;
    fillchar(ntry,sizeof(ntry),0)
    end else begin;
    move(ProcessDat^[ProcCur],ntry,sizeof(ntry));
    end;
  a:='cur='+BStr(ProcCur)+'/'+BStr(ProcessNum);
  while (length(a)<14) do a:=a+' ';
  a:=a+'seq='+BStr(ntry.seq);
  while (length(a)<23) do a:=a+' ';
  a:=a+'pid='+conv2hex(ntry.pid);
  a:=a+'  runs='+alakit(ntry.runs);
  while (length(a)<ScrSizX) do a:=a+' ';
  GotoXY(1,ScrSizY-5);Write(a);
  a:='ppid='+conv2hex(ntry.ppid);
  a:=a+'  uid='+conv2hex(ntry.uid);
  a:=a+'  right='+conv2hex(ntry.right);
  a:=a+'  ticks='+alakit(ntry.ticks);
  while (length(a)<ScrSizX) do a:=a+' ';
  GotoXY(1,ScrSizY-4);Write(a);
  a:='memory='+alakit(ntry.pages);
  a:=a+'  files='+alakit(ntry.files);
  a:=a+'  pipes='+alakit(ntry.pipes);
  while (length(a)<ScrSizX) do a:=a+' ';
  GotoXY(1,ScrSizY-3);Write(a);
  a:='prg="'+ntry.name+'"';
  while (length(a)<ScrSizX) do a:=a+' ';
  a:=copy(a,1,ScrSizX);
  GotoXY(1,ScrSizY-2);Write(a);
  a:='par="'+ntry.param+'"';
  while (length(a)<ScrSizX) do a:=a+' ';
  a:=copy(a,1,ScrSizX);
  GotoXY(1,ScrSizY-1);Write(a);
  RefreshScr:=RefreshScr or $10;
  end;
if (RefreshScr and $20<>0) then begin;
  for o:=1 to ScrSizY-7 do begin;
    GotoXY(1,o+1);
    i:=o+ProcScr;
    if (i=ProcCur) then textColor($2f) else textColor($07);
    if (i<1) or (i>ProcessNum) then a:='' else begin;
      move(ProcessDat^[i],ntry,sizeof(ntry));
      a:=ntry.name;
      a:=xFileName(a,2)+xFileName(a,3);
      a:=conv2hex(ntry.pid)+'  '+a;
      end;
    while (length(a)<ScrSizX) do a:=a+' ';
    a:=copy(a,1,ScrSizX);
    Write(a);
    end;
  RefreshScr:=RefreshScr or $10;
  end;
if (RefreshScr and $10<>0) then gotoXY(11,1+ProcCur-ProcScr);
RefreshScr:=0;

f2:
w:=ReadKey;
o:=ProcCur;
case w of
  $8001:begin; RefreshScr:=$ff;goto f1; end;
  $801d:goto f3;{f10}
  $0478:goto f3;{alt+x}
  $046b:begin;{alt+k}
    BugOS_KillProcess(ProcessDat^[ProcCur].pid);
    RefreshScr:=$60;
    ReRead;
    goto f1;
    end;
  $0466:begin;{alt+f}
    filterProcs:=not filterProcs;
    RefreshScr:=$60;
    ReRead;
    goto f1;
    end;
  $0472:begin;{alt+r}
    RefreshScr:=$60;
    ReRead;
    goto f1;
    end;
  $8605:RestartComputer;{ctrl+alt+esc}
  $800c:dec(ProcCur);{up}
  $800d:inc(ProcCur);{down}
  $8008:ProcCur:=1;{home}
  $8009:ProcCur:=ProcessNum;{end}
  $800a:dec(ProcCur,ScrSizY shr 1);{pgup}
  $800b:inc(ProcCur,ScrSizY shr 1);{pgdn}
  end;
AlignCheck;
if (o<>ProcCur) then begin;
  RefreshScr:=$60;
  goto f1;
  end;
goto f2;
f3:
GotoXY(1,ScrSizY);
textColor($07);
WriteLn('');
END.