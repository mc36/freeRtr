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

{$include \sources\internet\kernel\utils\timer2.inc}

Label f1,f2,f3;
Var
  buf:array[1..8*1024] of byte;
  i,o,p,c1,c2,t:LongInt;
  a:String;
BEGIN;
TCPfindProcess;
i:=BVal(ParamStr(2));
string2ipAddr(ParamStr(1),a);
if (i=0) then i:=7;

Write('connecting to '+ipAddr2string(a)+' '+BStr(i)+'...');
TCPbeginConnect(p,65536,a,i);
f1:
if TCPlookConnected(p,a,i,o) then begin;
  if keypressed then exit;
  relequish;
  if (p<>0) then goto f1;
  WriteLn(' failed!');
  exit;
  end;
WriteLn(' ok!');
WriteLn('local side is '+ipAddr2string(a)+' '+BStr(i)+'...');
WriteLn('rx tx');

timer2start;
c1:=0;
c2:=0;
t:=CurrentTime;
f2:
pipeLineStats(p,i,i,o);
if (pipeLineSend(p,buf,o)<>0) then o:=0;
if (o>0) then begin; inc(c2,o);goto f2; end;
f3:
i:=sizeof(buf);
if (pipeLineRecv(p,buf,i)<>0) then i:=0;
if (i>0) then begin; inc(c1,i);goto f3; end;
relequish;
timer2start;
if (GetTimePast(t)=0) then goto f2;
WriteLn(BStr(c1)+' '+BStr(c2));
c1:=0;
c2:=0;
t:=CurrentTime;
if keypressed then exit;
goto f2;
END.