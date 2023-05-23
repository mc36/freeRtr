{$heap 31k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc param.inc}

Var
  pipe:LongInt;
  path:String;
  mode:LongInt; {1-overwrite, 2-skip, 3-continue}
Const
  endOfFileChar=26;



Procedure doSend;
Var
  buf:array[1..1024] of byte;
  p,s:LongInt;
  i,o:LongInt;
  f:xFile;
Begin;
WriteLn('sending '+path+'...');
if (xOpen(f,path,xGenFilMod_r)<>0) then begin;
  WriteLn('error opening file!');
  exit;
  end;
s:=xFileSize(f);
WriteLn('going to send '+BStr(s)+' bytes...');
p:=0;
while (p<s) do begin;
  Write(BStr(p)+#13);
  o:=s-p;
  if (o>sizeof(buf)) then o:=sizeof(buf);
  xBlockRead(f,buf,o);
  while (pipeLineSend(pipe,buf,o)<>0) do relequish;
  inc(p,o);
  end;
xClose(f);
buf[1]:=endOfFileChar;
while (pipeLineSend(pipe,buf,1)<>0) do relequish;
WriteLn('successfully finished!');
End;




Procedure doReceive;
Label f1;
Var
  buf:array[1..1024] of byte;
  run:Boolean;
  i,o,p,s:LongInt;
  f:xFile;
Begin;
WriteLn('receiving '+path+'...');
i:=xCreate(path);
if (mode=2) and (i<>0) then begin;
  WriteLn('file already exists, skipping...');
  exit;
  end;
if (xOpen(f,path,xGenFilMod_rw)<>0) then begin;
  WriteLn('error opening file!');
  exit;
  end;
if (mode=1) then begin;
  xTruncate(f);
  p:=0;
  end else begin;
  p:=xFileSize(f);
  xSeek(f,p);
  WriteLn('continuing after '+BStr(p)+' bytes...');
  end;
s:=0;
f1:
relequish;
p:=sizeof(buf);
if (pipeLineRecv(pipe,buf,p)<>0) then p:=0;
if (p<1) then goto f1;
run:=true;
for i:=p downto 1 do if (buf[i]=endOfFileChar) then begin;
  run:=false;
  p:=i-1;
  end;
xBlockWrite(f,buf,p);
inc(s,p);
Write(BStr(s)+#13);
if run then goto f1;
xClose(f);
WriteLn(BStr(s)+' bytes received.');
WriteLn('successfully finished!');
End;




Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('ascii file transfer v1.0, done by Mc at '#%date' '#%time'.');

BugOS_MyProcessInfo(i,o,i);
if (pipeLineCreate(pipe,o,65536,false)<>0) then begin;
  WriteLn('error creating pipeline!');
  exit;
  end;

a:=GetAllParameters;
o:=0;
case lowCase(a[1]) of
  'r':o:=1;
  't':o:=2;
  end;
case lowCase(a[2]) of
  'o':mode:=1;
  's':mode:=2;
  'c':mode:=3;
  end;
path:=copy(a,3,255);
if (o=0) or (mode=0) then begin;
  WriteLn('using: protocol.code <direction><existing><pathname>');
  WriteLn(' mode: t=transmit, r=receive');
  WriteLn('exist: c=continue, o=overwrite, s=skip');
  exit;
  end;

if (o=1) then doReceive else doSend;
END.