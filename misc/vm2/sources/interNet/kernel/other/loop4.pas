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
  localNet:LongInt;
  gatwyNet:LongInt;



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
if (a='arpadd--') then begin;
  a:='ok';
  goto f1;
  end;
if (a='arpread-') then begin;
  a:='arpdata'#0#0#0#0;
  goto f1;
  end;
if (a='param---') then begin;
  b[0]:=#4;
  move(localNet,b[1],4);
  a:='param'+b;
  move(gatwyNet,b[1],4);
  a:=a+b+#255#255#255#255;
  goto f1;
  end;
if (a='data----') and (prgPipe=0) then begin;
  a:='data';
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

i:=length(IPv4addressPrefix)+1;
if string2ipAddr(paramStr(1),a[1]) then begin;
  WriteLn('using: loop.code <myaddr> [gateaddr]');
  halt(1);
  end;
move(a[i],localNet,sizeof(localNet));
if string2ipAddr(paramStr(2),a[1]) then a:=IPv4addressPrefix+#192#168#254#254;
move(a[i],gatwyNet,sizeof(gatwyNet));

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