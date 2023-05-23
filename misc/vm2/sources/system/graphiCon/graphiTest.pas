{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc pipeline.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
Type
  OneMouseRecord=record
    bu:Byte;
    mx:LongInt;
    my:LongInt;
    end;
  OneDisplayRecord=record
    x:LongInt;
    y:LongInt;
    d:array[1..1] of byte;
    end;
Var
  pipeDsp,pipeKey,pipeMos:LongInt;
  buf:array[1..1024*16] of byte;
  mos:OneMouseRecord absolute buf;
  dsp:OneDisplayRecord absolute buf;
  key:Word absolute buf;
  scrX,scrY:LongInt;
  posX,posY:LongInt;
  oldX,oldY:LongInt;
  butt,colr:LongInt;

Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Procedure doPipe(var pipe:LongInt;i:LongInt);
Var a:String;
Begin;
a:=paramStr(i);
i:=BVal(a);
if (i=0) then i:=BugOS_findProcNam(a);
if (i=0) then exit;
if (pipeLineCreate(pipe,i,65536,true)<>0) then pipe:=0;
End;

Procedure dspSend(s:LongInt);
Label f1;
Begin;
s:=(s*3)+8;
f1:
if (pipeLineSend(pipeDsp,buf,s)<>0) then begin;
  relequish;
  goto f1;
  end;
End;

Label f1,f2;
Const
  colSiz=60;
  PtrSiz=5;
Var i,o,p:LongInt;
BEGIN;
WriteLn('graphic tester v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<3) then immErr('using: tester.code <video-process> [<key-process> [mouse-process]]');
WriteLn('opening display...');
doPipe(pipeDsp,1);
if (pipeDsp=0) then immErr('error opening display!');
WriteLn('opening keyboard...');
doPipe(pipeKey,2);
WriteLn('opening mouse...');
doPipe(pipeMos,3);
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(pipeDsp,buf,i)<>0) then i:=0;
if (i<>8) then immErr('display process not responding!');
move(buf[1],scrX,sizeof(scrX));
move(buf[5],scrY,sizeof(scrY));
posX:=0;posY:=0;
oldX:=0;oldY:=0;
colr:=$ff;butt:=0;
WriteLn('screen size: '+BStr(scrX)+'x'+BStr(scrY)+'...');

fillchar(buf,sizeof(buf),$ff);
dsp.x:=0;
dsp.y:=0;
dspSend(scrX);
dsp.y:=scrY-1;
dspSend(scrX);
fillchar(buf,sizeof(buf),0);
dsp.x:=0;
fillChar(buf[9],3,$ff);
fillChar(buf[scrX*3+6],3,$ff);
for i:=1 to scrY-2 do begin;
  dsp.y:=i;
  dspSend(scrX);
  end;
fillchar(buf,sizeof(buf),0);
dsp.x:=(scrX-3*colSiz-20) shr 1;
dsp.y:=(scrY-256) shr 1;
for p:=0 to 255 do begin;
  o:=9;
  for i:=1 to colSiz do begin;
    buf[o]:=p;
    inc(o,3);
    end;
  inc(o,31);
  for i:=1 to colSiz do begin;
    buf[o]:=p;
    inc(o,3);
    end;
  inc(o,31);
  for i:=1 to colSiz do begin;
    buf[o]:=p;
    inc(o,3);
    end;
  inc(dsp.y);
  dspSend(colSiz*3+20);
  end;

f1:
fillchar(buf,sizeof(buf),0);
dsp.x:=oldX;
dsp.y:=oldY;
for i:=1 to ptrSiz do begin;
  dspSend(ptrSiz);
  inc(dsp.y);
  end;
move(colr,buf[9],3);
dsp.x:=posX;
dsp.y:=posY;
for i:=1 to ptrSiz do begin;
  dspSend(ptrSiz);
  inc(dsp.y);
  end;
p:=9;
for o:=1 to ptrSiz do begin;
  move(colr,buf[p],3);
  inc(p,3);
  end;
dsp.x:=posX;
dsp.y:=posY;
dspSend(ptrSiz);
oldX:=posX;
oldY:=posY;
f2:
key:=0;
i:=sizeof(buf);
if (pipeLineRecv(pipeKey,buf,i)<>0) then i:=0;
if (i>0) then begin;
  if (key=$8005) then halt(0);
  end;
i:=sizeof(buf);
if (pipeLineRecv(pipeMos,buf,i)<>0) then i:=0;
if (i>0) then begin;
  if (mos.bu<>butt) then colr:=((colr shr 8) or (colr shl 16)) and $ffffff;
  inc(posX,mos.mx);
  inc(posY,mos.my);
  if (posX<0) then posX:=0;
  if (posX>=scrX) then posX:=scrX-1;
  if (posY<0) then posY:=0;
  if (posY>=scrY) then posY:=scrY-1;
  butt:=mos.bu;
  end;

relequish;
if (oldX<>posX) or (oldY<>posY) then goto f1;
goto f2;
END.