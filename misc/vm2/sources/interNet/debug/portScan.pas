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
  i,o,p,q:LongInt;
  a,b:String;
BEGIN;
TCPfindProcess;
string2ipAddr(ParamStr(1),a);

WriteLn('scanning ports at '+ipAddr2string(a)+'...');
q:=0;
p:=0;
f1:
pipeLineClose(p);
inc(q);
if (q>65535) then goto f2;
write(BStr(q)+#13);
TCPbeginConnect(p,4096,a,q);
while TCPlookConnected(p,b,i,o) do begin;
  if keypressed then goto f2;
  relequish;
  if (p=0) then goto f1;
  end;
WriteLn(BStr(q));
goto f1;
f2:
write('       '#13);
END.