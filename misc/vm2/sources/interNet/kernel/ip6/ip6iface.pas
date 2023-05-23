{$heap 15k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$sysinc inet_addr.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
exit;
End;

Procedure doQuery(i:longInt;var a:String);
Label f1;
Var o,p:LongInt;
Begin;
relequish;
if (pipeLineCreate(o,i,4096,true)<>0) then exit;
pipeLineSend(o,a[1],length(a));
p:=0;
f1:
relequish;
inc(p);
if (p>16) then begin;
  pipeLineClose(o);
  immErr('error communicating process!');
  end;
i:=250;
if (pipeLineRecv(o,a[1],i)<>0) then i:=0;
if (i<1) then goto f1;
pipeLineClose(o);
a[0]:=chr(i);
End;


Var
  a,b:String;
  ab:array[0..1] of byte absolute a;
  bb:array[0..1] of byte absolute b;
  i,o,p,q:LongInt;
BEGIN;
WriteLn('ip6 interface info v1.0, done by Mc at '#%date' '#%time'.');
a:=ParamStr(1);
if (a='') then immErr('using: ip6iface.code <process name/id>');
p:=BVal(a);
if (p=0) then p:=BugOS_findProcNam(a);
if (p=0) then immErr('process not found!');

a:='param6--';
doQuery(p,a);
if (copy(a,1,6)<>'param6') then immErr('invalid response');
a:=copy(a,7,255);
WriteLn('source address type: '+byte2hextype(ord(a[1])));
WriteLn('target address type: '+byte2hextype(ord(a[2])));
move(a[3],b,sizeof(b));
a:=copy(a,length(b)+4,255);
write('mac address: ');
for i:=1 to length(b) do write(byte2hextype(bb[i]));
WriteLn('');
WriteLn('link address: '+ipAddr2string(a[1]));
WriteLn('ip address: '+ipAddr2string(a[17]));
WriteLn('gateway ip: '+ipAddr2string(a[33]));
WriteLn('netmask: '+ipAddr2string(a[49]));
Write('dns:');
a:=copy(a,65,255);
while (a<>'') do begin;
  Write(' '+ipAddr2string(a[1]));
  a:=copy(a,17,255);
  end;
WriteLn('');

a:='adrread-1234';
i:=1;move(i,a[9],sizeof(i));
doQuery(p,a);
if (copy(a,1,7)<>'adrdata') then immErr('invalid response');
move(a[8],o,sizeof(o));
WriteLn('address cache: '+BStr(o));

for i:=1 to o do begin;
  a:='adrread-1234';
  move(i,a[9],sizeof(i));
  doQuery(p,a);
  if (copy(a,1,7)<>'adrdata') then immErr('invalid response');
  Write(BStr(i)+': ');
  for q:=28 to length(a) do Write(byte2hextype(ab[q]));
  WriteLn(' '+ipAddr2string(a[12]));
  end;
END.