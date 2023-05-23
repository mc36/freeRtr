{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc pipeline.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
Type
  OneDisplayRecord=record
    x:LongInt;
    y:LongInt;
    d:array[1..3] of byte;
    end;
Var
  pipeUp,pipeDn:LongInt;
  act,scrX,scrY:LongInt;
  buf:array[1..1024*16] of byte;
  dsp:OneDisplayRecord absolute buf;

Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Procedure dspSend(s:LongInt);
Label f1;
Begin;
f1:
if (pipeLineSend(pipeUp,buf,s)<>0) then begin;
  relequish;
  goto f1;
  end;
End;

Procedure xchg1(var buf1,buf2);
Var
  b1:array[1..1] of byte absolute buf1;
  b2:array[1..1] of byte absolute buf2;
  b:byte;
Begin;
b:=b1[1];b1[1]:=b2[1];b2[1]:=b;
b:=b1[2];b1[2]:=b2[2];b2[2]:=b;
b:=b1[3];b1[3]:=b2[3];b2[3]:=b;
End;

Procedure xchg2(var buf1,buf2);
Begin;
asm varAddr trg buf1;
asm AddrLod trg [trg];
asm varAddr src buf2;
asm AddrLod src [src];
asm movR duwuw a [trg];
asm movR duwuw b [src];
asm movW duwuw [trg] b;
asm movW duwuw [src] a;
asm add ? src 2;
asm add ? trg 2;
asm movR dubub a [trg];
asm movR dubub b [src];
asm movW dubub [trg] b;
asm movW dubub [src] a;
End;


Label f1,f2,f3,f4;
Var
  i,o,p:LongInt;
  a:String;
BEGIN;
WriteLn('display rotate v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<>2) then immErr('using: rotate.code <video-process> <l/r>');
a:=paramStr(2);
act:=-1;
if (a='l') then act:=1;
if (a='r') then act:=0;
if (act<0) then immErr('no action selected!');
WriteLn('opening display...');
a:=paramStr(1);
i:=BVal(a);
if (i=0) then i:=BugOS_findProcNam(a);
if (pipeLineCreate(pipeUp,i,65536,true)<>0) then pipeUp:=0;
if (pipeUp=0) then immErr('error opening pipeline!');
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(pipeUp,buf,i)<>0) then i:=0;
if (i<>8) then immErr('process not responding!');
move(buf[1],scrX,sizeof(scrX));
move(buf[5],scrY,sizeof(scrY));
WriteLn('screen size: '+BStr(scrX)+'x'+BStr(scrY)+'...');
if (act<>0) then a:='left' else a:='right';
WriteLn('action: '+a);

Write('waiting for upper level...');
BugOS_SignDaemoning;
pipeLineBegListen;
while (pipeLineGetIncoming(pipeDn)<>0) do relequish;
pipeLineEndListen;
move(scrY,buf[1],sizeof(scrY));
move(scrX,buf[5],sizeof(scrX));
pipeLineSend(pipeDn,buf,sizeof(i)*2);
WriteLn(' done!');

f1:
p:=sizeof(buf);
pipeLineRecv(pipeDn,buf,p);
if (p<1) then begin;
  pipeLineStats(pipeDn,o,i,i);
  if (o=0) then immErr('upper exited!');
  pipeLineStats(pipeUp,o,i,i);
  if (o=0) then immErr('lower exited!');
  relequish;
  goto f1;
  end;
i:=dsp.x;dsp.x:=dsp.y;dsp.y:=i;
if (act<>0) then dsp.y:=scrY-dsp.y-1 else dsp.x:=scrX-dsp.x-1;
{,(p-sizeof(i)+sizeof(i)) div 3);}
o:=sizeof(i)+sizeof(i)+1;
while (o<p) do begin;
  move(buf[o],dsp.d,3);
  dspSend(sizeof(dsp));
  if (act=0) then inc(dsp.y) else dec(dsp.y);
  inc(o,3);
  end;
goto f1;
END.