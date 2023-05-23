{$heap 7k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc param.inc}
{$sysinc crt.inc}

Const maxPrint=255;
Var
  buf:array[1..1024] of byte;
  procS,pipeS:LongInt;
  procC,pipeC:LongInt;
  nam,par:String;
  uid:LongInt;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Procedure delayBit;
Var i:LongInt;
Begin;
for i:=1 to 128 do relequish;
End;


Label f1,f2,f3,f4,f5,f6;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
  w:Word;

Procedure addLine(a:String);
Var i:LongInt;
Begin;
a:=a+#13#10;
i:=length(a);
move(a[1],buf[o+1],i);
inc(o,i);
End;

Procedure startSession;
Begin;
WriteLn('creating session...');
a:='i'+a;
pipeLineBegListen;
if (xExecBgnd(GetMyFullFileName,a,procC)<>0) then procC:=0;
if (procC=0) then immErr('error starting intermediate process!');
WriteLn('intermediate process is '+BStr(procC)+'...');
delayBit;
if (pipeLineGetIncoming(pipeC)<>0) then pipeC:=0;
if (pipeC=0) then immErr('intermediate process does not respond!');
pipeLineEndListen;
p:=not procC;
pipeLineStats(pipeC,p,i,o);
if (p<>procC) then immErr('someone wanted to hack us!');
i:=sizeof(o);
pipeLineRecv(pipeC,o,i);
if (i<>sizeof(o)) then immErr('intermediate process speaks silly!');
pipeLineClose(pipeC);
delayBit;
a:=BStr(o);
WriteLn('suspender process is '+a+'...');
End;



BEGIN;
WriteLn('suspender v1.0, done by Mc at '#%date' '#%time'.');

a:=GetAllParameters;
if (ab0<=1) then begin;
  f1:
  WriteLn('using: suspend.code <command><processname> [parameters]');
  WriteLn('commands: b=beginSession c=continue s=foregroundStart t=backgroundStart');
  Halt(2);
  end;
a[1]:=lowCase(a[1]);
i:=ab[1];
a:=copy(a,2,255);
case i of
  $69:begin; {i-internal}
    BugOS_MyProcessInfo(i,o,p);
    if (pipeLineCreate(p,o,4096,false)<>0) then p:=0;
    if (p=0) then immErr('error creating pipeline!');
    a:='s'+a;
    if (xExecBgnd(GetMyFullFileName,a,o)<>0) then o:=0;
    if (o=0) then immErr('error starting session!');
    pipeLineSend(p,o,sizeof(o));
    delayBit;
    immErr('successfully finished!');
    end;
  $73:begin; {s-start}
    goto f2;
    end;
  $74:begin; {t-background}
    startSession;
    delayBit;
    WriteLn('successful!');
    exit;
    end;
  $62:begin; {b-begin}
    startSession;
    goto f5;
    end;
  $63:begin; {c-continue}
    goto f5;
    end;
  end;
goto f1;



f2:
BugOS_MyOwnerInfo(uid,i);
i:=pos(' ',a);
if (i<1) then i:=666;
par:=copy(a,i+1,255);
nam:=copy(a,1,i-1);
WriteLn('starting '+nam+' '+par+'...');
if (xExecInside(nam,par,procS,pipeS)<>0) then procS:=0;
BugOS_dropPrivileges;
if (procS=0) then immErr('error starting process!');
WriteLn('pid='+BStr(procS)+' pipe='+BStr(pipeS)+'...');
if pipeLineBegListen then immErr('failed to start listening!');
WriteLn('serving others...');
BugOS_SignDaemoning;

f3:
relequish;
i:=sizeof(buf);
pipeLineRecv(pipeS,buf,i);
if not BugOS_ProcessExists(procS) then immErr('process terminated!');
if (pipeLineGetIncoming(pipeC)<>0) then goto f3;
if (pipeLineStats(pipeC,procC,i,i)<>0) then procC:=0;
o:=not uid;
BugOS_ProcessName(procC,buf,o,i,i);
if (o<>uid) then begin;
  o:=0;
  addLine('you are not the owner of this process!');
  pipeLineSend(pipeC,buf,o);
  pipeLineClose(pipeC);
  goto f3;
  end;
o:=0;
addLine(' name: '+nam);
addLine('param: '+par);
addLine('  uid: '+BStr(uid));
addLine('  pid: '+BStr(procS));
addLine(' pipe: '+BStr(pipeS));
addLine('after this line, you are continuing the process...');
pipeLineSend(pipeC,buf,o);

f4:
relequish;
if (pipeLineStats(pipeC,i,o,p)<>0) then i:=0;
if (i=0) then begin;
  pipeLineClose(pipeC);
  goto f3;
  end;
if (p>16) then begin;
  if (p>sizeof(buf)) then p:=sizeof(buf);
  pipeLineRecv(pipeS,buf,p);
  if (p>0) then pipeLineSend(pipeC,buf,p);
  end;
if (pipeLineStats(pipeS,i,o,p)<>0) then i:=0;
if (i=0) and (o<1) then immErr('process terminated!');
if (p>16) then begin;
  if (p>sizeof(buf)) then p:=sizeof(buf);
  pipeLineRecv(pipeC,buf,p);
  if (p>0) then pipeLineSend(pipeS,buf,p);
  end;
goto f4;



f5:
BugOS_dropPrivileges;
procS:=BVal(a);
if (procS=0) then procS:=BugOS_findProcNam(a);
if (procS=0) then immErr('error finding process!');
WriteLn('continuing '+a+' ('+BStr(procS)+')...');
if (pipeLineCreate(pipeS,procS,65536,false)<>0) then pipeS:=0;
if (pipeS=0) then immErr('error creating pipeline!');
w:=$8001;
pipeLineSend(pipeS,w,sizeof(w));

f6:
relequish;
o:=1;
while keypressed do begin;
  w:=readKey;
  move(w,buf[o],sizeof(w));
  inc(o,2);
  end;
if (o>1) then pipeLineSend(pipeS,buf,o-1);
if (pipeLineStats(pipeS,i,o,p)<>0) then i:=0;
if (i=0) and (o<1) then begin;
  WriteLn('');
  immErr('pipeline closed!');
  exit;
  end;
if (o>maxPrint) then o:=maxPrint;
if (o>0) then pipeLineRecv(pipeS,ab[1],o);
if (o<1) then goto f6;
ab[0]:=o;
Write(a);
goto f6;
END.