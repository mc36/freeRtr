{$heap 15k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}

Type OneConnectionRecord=record
  ctrl:LongInt;                 {cotnrol pipeline}
  data:LongInt;                 {data pipeline}
  cur:LongInt;                  {current: 1=dtr, 2=rts}
  dtr:LongInt;                  {number of dtr changes}
  rts:LongInt;                  {number of rts changes}
  closes:LongInt;               {number of closes}
  end;
Const ConnectionNum=32;
Var ConnectionDat:array[0..ConnectionNum-1] of OneConnectionRecord;

Procedure doOne(var loc,rem:OneConnectionRecord);
Label f1;
Var
  bufB:array[1..1024] of Byte;
  bufD:array[1..1] of LongInt absolute bufB;
  bufD1:LongInt absolute bufB;
  i,o,p:LongInt;
Begin;
if (loc.ctrl=0) then exit;
pipeLineStats(loc.ctrl,i,o,p);
if (i=0) then begin;
  f1:
  WriteLn('connection closed!');
  pipeLineClose(loc.data);
  pipeLineClose(loc.ctrl);
  loc.data:=0;
  loc.ctrl:=0;
  inc(loc.closes);
  exit;
  end;
pipeLineStats(loc.data,i,o,p);
if (i=0) then goto f1;
if (p>0) then begin;
  if (p>sizeof(bufB)) then p:=sizeof(bufB);
  if (pipeLineRecv(rem.data,bufB,p)=0) then pipeLineSend(loc.data,bufB,p);
  end;
p:=sizeof(bufB);
if (pipeLineRecv(loc.ctrl,bufD,p)<>0) then exit;
if (p<sizeof(bufD1)) then exit;
case bufD1 of
  00:begin; {read line status counters}
    bufD[2]:=0; {overrun errors}
    bufD[3]:=0; {parity errors}
    bufD[4]:=0; {framing errors}
    bufD[5]:=rem.closes; {break detects}
    if (rem.ctrl<>0) then i:=$00 else i:=$08;
    bufD[6]:=i; {current line status}
    rem.closes:=0;
    i:=6*sizeof(bufD1);
    end;
  01:begin; {read modem status counters}
    bufD[2]:=rem.rts; {cts changes}
    bufD[3]:=rem.dtr; {dsr changes}
    bufD[4]:=0; {ring indicator changes}
    bufD[5]:=rem.dtr; {data carrier detect changes}
    i:=0;
    if (rem.cur and 1<>0) then i:=i or $0a;
    if (rem.cur and 2<>0) then i:=i or $01;
    bufD[6]:=i; {current modem status}
    rem.rts:=0;
    rem.dtr:=0;
    i:=6*sizeof(bufD1);
    end;
  02:begin; {read modem control status}
    bufD[2]:=loc.cur; {current modem control}
    i:=2*sizeof(bufD1);
    end;
  03:begin; {set modem control value}
    i:=bufD[2] and $03;
    if (i and $01<>loc.cur and $01) then inc(loc.dtr);
    if (i and $02<>loc.cur and $02) then inc(loc.rts);
    loc.cur:=i;
    i:=1*sizeof(bufD1);
    end;
  04:begin; {read line status}
    bufD[2]:=28800; {line speed (bit/sec)}
    bufD[3]:=0; {line speed high dword}
    bufD[4]:=8; {byte length in bits}
    bufD[5]:=0; {parity}
    bufD[6]:=1; {stop bits}
    bufD[7]:=0; {send break}
    i:=7*sizeof(bufD1);
    end;
  05:begin; {write line status}
    i:=1*sizeof(bufD1);
    end;
  06:begin; {read flow control}
    bufD[2]:=1; {used flow control}
    i:=2*sizeof(bufD1);
    end;
  07:begin; {write flow control}
    i:=1*sizeof(bufD1);
    end;
  08:begin; {driver buffer status}
    bufD[2]:=0; {bytes waiting in rx buffer}
    bufD[3]:=0; {bytes waiting in tx buffer}
    i:=3*sizeof(bufD1);
    end;
  09:begin; {clear driver rx buffer}
    i:=1*sizeof(bufD1);
    end;
  10:begin; {clear driver tx buffer}
    i:=1*sizeof(bufD1);
    end;
  11:begin; {clear driver rx and tx buffers}
    i:=1*sizeof(bufD1);
    end;
  else goto f1;
  end;
pipeLineSend(loc.ctrl,bufD,i);
End;

Procedure doNew(p1:LongInt);
Label f1;
Var i,o,p,p2:LongInt;
Begin;
p2:=0;
for i:=0 to ConnectionNum-1 do begin;
  if (ConnectionDat[i].ctrl=p1) then exit;
  if (ConnectionDat[i].data=p1) then exit;
  end;
i:=ConnectionNum;
pipeLineSend(p1,i,sizeof(i));
o:=16;
while (1=1) do begin;
  dec(o);
  if (o<0) then goto f1;
  i:=sizeof(p);
  if (pipeLineRecv(p1,p,i)<>0) then i:=0;
  if (i=sizeof(p)) then break;
  relequish;
  end;
if (p<0) or (p>=ConnectionNum) then goto f1;
if (ConnectionDat[p].ctrl<>0) then goto f1;
o:=16;
while (1=1) do begin;
  dec(o);
  if (o<0) then goto f1;
  i:=sizeof(p2);
  if (pipeLineRecv(p1,p2,i)<>0) then i:=0;
  if (i=sizeof(p2)) then break;
  relequish;
  end;
ConnectionDat[p].ctrl:=p1;
ConnectionDat[p].data:=p2;
WriteLn('new connection...');
exit;
f1:
pipeLineClose(p1);
pipeLineClose(p2);
End;


Label f1;
Var i:LongInt;
BEGIN;
WriteLn('serialEmu v1.0, done by Mc at '#%date' '#%time'.');

if (pipeLineBegListen<>0) then begin;
  WriteLn('error start listening...');
  Halt(1);
  end;
fillchar(ConnectionDat,sizeof(ConnectionDat),0);
WriteLn('serving others...');
BugOS_SignDaemoning;

f1:
for i:=0 to ConnectionNum-1 do doOne(ConnectionDat[i],ConnectionDat[i xor 1]);
if (pipeLineGetIncoming(i)<>0) then begin;
  relequish;
  goto f1;
  end;
doNew(i);
goto f1;
END.