{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc hex.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}


Function generateWakeOnLanPacket(var a:String):String;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  b:String;
  i,o:LongInt;
Begin;
generateWakeOnLanPacket:='';
kicserel(':','',a);
kicserel('-','',a);
a:=kicsi(a);
if (length(a)<>12) then exit;
for o:=1 to 6 do begin;
  b:=copy(a,o*2-1,2);
  i:=BVal('$'+b);
  if (kicsi(byte2hextype(i))<>b) then exit;
  ab[o]:=i;
  end;
ab[0]:=6;
b:=#255#255#255#255#255#255;
for i:=1 to 16 do b:=b+a;
generateWakeOnLanPacket:=b;
for i:=1 to 6 do a:=a+'-'+byte2hextype(ab[i]);
a:=copy(a,8,666);
End;


Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;


Var
  i,o,p,q,r:LongInt;
  a,b,c:String;
BEGIN;
WriteLn('wakeup-on-lan v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');

if (paramCount<2) then immErr('using: wol.code <macaddr> <ipaddr> [port]');

q:=BVal(paramStr(3));
if (q=0) then q:=9;
if string2ipAddr(paramStr(2),b) then immErr('error in ip address!');

a:=paramStr(1);
c:=generateWakeOnLanPacket(a);
if (c='') then immErr('error in mac address!');
WriteLn('will wake up '+a+'...');

move(b,a,sizeof(a));
i:=0;
if UDPlistenOnPort(p,4096,a,i) then exit;
WriteLn('listening on '+ipAddr2string(a)+' '+BStr(i)+'...');
WriteLn('will send to '+ipAddr2string(b)+' '+BStr(q)+'...');

WriteLn('sending magic packet...');
pipeLineStats(p,i,i,r);
UDPsendPacket(p,b,q,c[1],length(c));
repeat
  relequish;
  pipeLineStats(p,q,i,o);
  if (q=0) then immErr('pipeline closed!');
  until (r=o);
WriteLn('successful!');
END.