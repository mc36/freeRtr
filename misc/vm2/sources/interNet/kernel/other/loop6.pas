{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}

Var
  prgPipe:LongInt;
  localNet:String;
  gatwyNet:String;



Procedure ProcessNewPipe(pip:LongInt);
Label f1;
Var
  buf:array[1..1024] of byte;
  a,b:string;
  i,o,p:LongInt;
Begin;
pipeLineStats(pip,p,i,o);
BugOS_ProcessName(p,buf,i,i,o);
if (o and $40=0) then goto f1;
i:=128;
if (pipeLineRecv(pip,b[1],i)<>0) then i:=0;
b[0]:=chr(i);
a:=kicsi(copy(b,1,8));
b:=copy(b,9,255);
if (a='adradd--') then begin;
  a:='ok';
  goto f1;
  end;
if (a='adrread-') then begin;
  a:='adrdata'#0#0#0#0;
  goto f1;
  end;
if (a='param6--') then begin;
  a:='param6'#0#0#0+#$fe#$80#0#0#0#0#0#0#0#0#0#0#0#0#0#0+localNet+gatwyNet
    +#255#255#255#255#255#255#255#255#255#255#255#255#255#255#255#255;
  goto f1;
  end;
if (a='data6---') and (prgPipe=0) then begin;
  a:='data6';
  pipeLineSend(pip,a[1],length(a));
  prgPipe:=pip;
  pipeLineStats(prgPipe,i,i,i);
  exit;
  end;

a:='error';
f1:
pipeLineSend(pip,a[1],length(a));
pipeLineClose(pip);
End;



Label f1;
Var
  buf:array[1..1024*4] of byte;
  a:String;
  lastTest:LongInt;
  i,o,p:LongInt;
BEGIN;
WriteLn('loopback interface v1.0, done by Mc at '#%date' '#%time'.');

localNet:='1234567890123456';
gatwyNet:=localNet;

if string2ipAddr(paramStr(1),localNet[1]) then begin;
  WriteLn('using: loop.code <myaddr> [gateaddr]');
  halt(1);
  end;
if string2ipAddr(paramStr(2),gatwyNet[1]) then gatwyNet:=#$fe#$80#0#0#0#0#0#0#0#0#0#0#0#0#0#0;

pipeLineBegListen;
BugOS_SignDaemoning;

f1:
o:=sizeof(buf);
if (pipeLineRecv(prgPipe,buf,o)=0) then goto f1;
relequish;
while (pipeLineGetIncoming(o)=0) do ProcessNewPipe(o);
if (prgPipe<>0) then begin;
  if (pipeLineStats(prgPipe,lastTest,o,p)<>0) then lastTest:=0;
  if (lastTest=0) then begin;
    pipeLineClose(prgPipe);
    prgPipe:=0;
    end;
  end;
goto f1;
END.