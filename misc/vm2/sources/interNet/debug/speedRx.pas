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

Label f1,f2;
Var
  buf:array[1..8*1024] of byte;
  i,o,p,c,t:LongInt;
  a:String;
BEGIN;
TCPfindProcess;
i:=BVal(ParamStr(2));
string2ipAddr(ParamStr(1),a);
if (i=0) then i:=19;

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

timer2start;
c:=0;
t:=CurrentTime;
f2:
i:=sizeof(buf);
if (pipeLineRecv(p,buf,i)=0) then begin; inc(c,i);goto f2; end;
relequish;
timer2start;
if (GetTimePast(t)=0) then goto f2;
WriteLn(BStr(c));
c:=0;
t:=CurrentTime;
if keypressed then exit;
goto f2;
END.