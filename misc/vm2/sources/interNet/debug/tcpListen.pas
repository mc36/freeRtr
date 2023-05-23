{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc hex.inc}
{$sysinc crt.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}

Label f1,f2;
Var
  i,o,p:LongInt;
  a:String;
BEGIN;
TCPfindProcess;
i:=BVal(ParamStr(1));

pipeLineBegListen;

if TCPlistenOnPort(p,4096,a,i) then exit;
WriteLn('listening on '+ipAddr2string(a)+' '+BStr(i)+'...');

while (pipeLineGetIncoming(p)<>0) do begin;
  relequish;
  if keypressed then exit;
  end;
pipeLineEndListen;
Write('incoming connection...');

f1:
if TCPlookConnected(p,a,i,o) then begin;
  if keypressed then exit;
  relequish;
  if (p<>0) then goto f1;
  WriteLn(' failed!');
  exit;
  end;
WriteLn(#8#8#8' from '+ipAddr2string(a)+' '+BStr(i)+'...');

f2:
if keypressed then begin;
  i:=readKey;
  case i of
    $8002:i:=9;
    $8003:i:=8;
    $8004:i:=13;
    $8204:i:=10;
    $8005:i:=27;
    end;
  if (i and $8000<>0) then exit;
  pipeLineSend(p,i,1);
  end;
i:=128;
if (pipeLineRecv(p,a[1],i)<>0) then i:=0;
a[0]:=chr(i);
if (i<1) then begin;
  pipeLineStats(p,o,i,i);
  if (o=0) then exit;
  end;
write(a);
relequish;
goto f2;
END.