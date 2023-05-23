{$heap 15k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
Var
  randS:array[0..255] of byte;
  randI:byte;
  randJ:byte;
  randK:String;
  randBytes:LongInt;

Procedure randomSetKey;
Var
  t,i,j:WORD;
  k:array[0..255] of byte;
Begin;
t:=Length(randK);
randI:=0;
randJ:=0;
j:=0;
for i:=0 to 255 do begin;
  randS[i]:=i;
  k[i]:=ord(randK[(i mod t)+1]);
  end;
for i:=0 to 255 do begin;
  j:=(j+randS[i]+k[i]) and $ff;
  t:=randS[i];
  randS[i]:=randS[j];
  randS[j]:=t;
  end;
End;

Function randomGetByte:byte;
Var i:byte;
Begin;
randI:=randI+1;
randJ:=randJ+randS[randI];
i:=randS[randI];
randS[randI]:=randS[randJ];
randS[randJ]:=i;
i:=randS[randI]+randS[randJ];
randomGetByte:=randS[i];
End;

Procedure randomRebuild;
Var i:Word;
Begin;
fillchar(randK,sizeof(randK),0);
randK:='';
for i:=1 to 128 do randK:=randK+chr(randomGetByte);
End;

Function doOnePipe(pipe:LongInt):Boolean;
Var
  buf:array[1..1024] of byte;
  i,o,p:LongInt;
Begin;
doOnePipe:=True;
pipeLineStats(pipe,i,o,p);
if (i=0) then exit;
doOnePipe:=False;
dec(p,16);
if (p<1) then exit;
if (p>sizeof(buf)) then p:=sizeof(buf);
for i:=1 to p do buf[i]:=randomGetByte;
pipeLineSend(pipe,buf,p);
inc(randBytes,p);
if (randBytes<16*1024*1024) then exit;
randomSetKey;
randomRebuild;
End;


Label f1;
Const
  randomKeyDatafilename='c:\system\randomNumber.seed';
  connectMax=256;
Var
  connectDat:array[1..connectMax] of LongInt;
  connectCur:LongInt;
  lastDay:LongInt;
  f:xFile;
  i,o,p:LongInt;
BEGIN;
WriteLn('random number generator v1.0, done by Mc at '#%date' '#%time'.');
WriteLn('reading random seed...');
xCreate(randomKeyDatafilename);
xSetRight(randomKeyDatafilename,0,xRights_OwnRead+xRights_OwnWrite);
xOpen(f,randomKeyDatafilename,xGenFilMod_rw);
xSeek(f,0);
if (xBlockRead(f,randK,sizeof(randK))<>0) then randK:='initial data';
randomSetKey;
randomRebuild;
xSeek(f,0);
xBlockWrite(f,randK,sizeof(randK));
xClose(f);

connectCur:=0;
if (pipeLineBegListen<>0) then begin;
  WriteLn('error start listening...');
  Halt(1);
  end;
WriteLn('serving others...');
BugOS_SignDaemoning;

f1:
for i:=1 to connectCur do if doOnePipe(connectDat[i]) then begin;
  pipeLineClose(connectDat[i]);
  connectDat[i]:=connectDat[connectCur];
  dec(connectCur);
  goto f1;
  end;
if (pipeLineGetIncoming(i)<>0) then begin;
  BugOS_KernelUptime(i,o,p);
  if (i=lastDay) then begin; Relequish;goto f1; end;
  lastDay:=i;
  xOpen(f,randomKeyDatafilename,xGenFilMod_rw);
  xBlockWrite(f,randK,sizeof(randK));
  xClose(f);
  goto f1;
  end;
if (connectCur>=connectMax) then begin;
  pipeLineClose(i);
  goto f1;
  end;
inc(connectCur);
connectDat[connectCur]:=i;
goto f1;
END.