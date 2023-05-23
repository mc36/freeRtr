{$heap 127k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$include \sources\internet\kernel\utils\timer2.inc}

Const proggyName='line emulator v1.0, done by Mc at '#%date' '#%time'.';




Type OnePeerRecord=record
  buf:array[1..16*1024] of Integer;
  pipe:LongInt;
  cmd:LongInt;
  end;
Var
  lastTick,ticksPerPack,samplesPerPack:LongInt;
  wires,noise:LongInt;
  randS:array[0..255] of byte;
  randI:byte;
  randJ:byte;


Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Procedure randomSetKey(randK:String);
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




Function loggint2up(var loc:OnePeerRecord;p:LongInt):Boolean;
Begin;
loggint2up:=false;
if (loc.pipe<>0) then exit;
WriteLn('upper logged in!');
fillchar(loc,sizeof(loc),0);
loc.pipe:=p;
loggint2up:=true;
End;

Procedure releq2up(var loc:OnePeerRecord);
Var
  buf:array[1..32*1024] of byte;
  i,o,p:LongInt;
  a:String;

Procedure addNoise;
Begin;
if (noise<1) then exit;
for i:=1 to samplesPerPack shl 1 do begin;
  inc(o,2);
  inc(buf[o],randomGetByte mod noise);
  end;
End;

Begin;
if (loc.pipe=0) then exit;
if (loc.cmd<>0) then exit;
o:=sizeof(buf);
pipeLineRecv(loc.pipe,buf,o);
if (o<1) then begin;
  pipeLineStats(loc.pipe,o,i,i);
  if (o<>0) then exit;
  WriteLn('upper logged out!');
  pipeLineClose(loc.pipe);
  loc.pipe:=0;
  exit;
  end;
case buf[1] of
  8:begin; {play and record sample}
    addNoise;
    move(buf[2],loc,samplesPerPack shl 2);
    loc.cmd:=2;
    end;
  6:begin; {play sample}
    addNoise;
    move(buf[2],loc,samplesPerPack shl 2);
    loc.cmd:=1;
    end;
  7:begin; {record sample}
    loc.cmd:=2;
    end;
  1:begin; {get card identification}
    a:=proggyName;
    a:=a+#13#10'wires='+BStr(wires);
    a:=a+' noise='+BStr(noise);
    a:=a+'/32k block='+BStr(samplesPerPack*4);
    a:=a+' pps='+BStr(ticksPerSec div ticksPerPack)+#13#10;
    pipeLineSend(loc.pipe,a[1],length(a));
    end;
  2:begin; {get mixer names}
    a:=#0#0#0#0#0#0#0#0'no available volumebars'#0;
    pipeLineSend(loc.pipe,a[1],length(a));
    end;
  3:begin; {get mixer values}
    a:=#0#0#0#0;
    pipeLineSend(loc.pipe,a[1],length(a));
    end;
  4:begin; {set mixer values}
    a:=#0;
    pipeLineSend(loc.pipe,a[1],length(a));
    end;
  5:begin; {get buffer info}
    i:=samplesPerPack shl 2;
    pipeLineSend(loc.pipe,i,sizeof(i));
    end;
  9:begin; {dial a number}
    i:=0;
    pipeLineSend(loc.pipe,i,1);
    end;
  10:begin; {hangup call}
    i:=0;
    pipeLineSend(loc.pipe,i,1);
    end;
  end;
End;

Procedure purge2up(var loc,rem:OnePeerRecord);
Var i,o:LongInt;
Begin;
case loc.cmd of
  2:begin; {record}
    pipeLineSend(loc.pipe,rem,samplesPerPack shl 2);
    end;
  1:begin; {play}
    i:=$01010101;
    pipeLineSend(loc.pipe,i,1);
    end;
  0:exit;
  end;
loc.cmd:=0;
End;

Procedure mixWires(var buffer1,buffer2);
Var
  buf1:array[1..1] of Integer absolute buffer1;
  buf2:array[1..1] of Integer absolute buffer2;
  i,o:LongInt;
Begin;
if (wires>=4) then exit;
for o:=1 to samplesPerPack shl 1 do begin;
  i:=buf1[o]+buf2[o];
  i:=i div 2;
  buf1[o]:=i;
  buf2[o]:=i;
  end;
End;


Label f1;
Var
  p1,p2:OnePeerRecord;
  i:LongInt;
BEGIN;
WriteLn(proggyName);
wires:=BVal(paramStr(1));
noise:=BVal(paramStr(2));
if (wires<2) then immErr('using: linemu.code <wires> [noise]');

fillchar(p1,sizeof(p1),0);
fillchar(p2,sizeof(p2),0);
timer2start;
ticksPerPack:=ticksPerSec div 32;
if (ticksPerPack<1) then ticksPerPack:=1;
samplesPerPack:=(44100*ticksPerPack) div ticksPerSec;
lastTick:=currentTime;
randomSetKey('noise');
pipeLineBegListen;
BugOS_SignDaemoning;

f1:
relequish;
timer2start;
while (pipeLineGetIncoming(i)=0) do begin;
  if loggint2up(p1,i) then goto f1;
  if loggint2up(p2,i) then goto f1;
  WriteLn('no more slots!');
  pipeLineClose(i);
  end;
releq2up(p1);
releq2up(p2);
if (abs(currentTime-lastTick)<ticksPerPack) then goto f1;
lastTick:=currentTime;
if (p1.cmd+p2.cmd<1) then goto f1;
mixWires(p1,p2);
purge2up(p1,p2);
purge2up(p2,p1);
i:=samplesPerPack shl 2;
fillchar(p1,i,0);
fillchar(p2,i,0);
goto f1;
END.