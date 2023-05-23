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

function adr(var ad):string;
var a:string;
begin;
a:=IPv4addressPrefix;
move(ad,a[length(a)+1],16);
adr:=ipAddr2string(a[1]);
end;

Var
  a:String;
  i,o,p,q:LongInt;
BEGIN;
WriteLn('ip4 interface info v1.0, done by Mc at '#%date' '#%time'.');
a:=ParamStr(1);
if (a='') then immErr('using: ip4iface.code <process name/id>');
p:=BVal(a);
if (p=0) then p:=BugOS_findProcNam(a);
if (p=0) then immErr('process not found!');

a:='param---';
doQuery(p,a);
if (copy(a,1,5)<>'param') then immErr('invalid response');
WriteLn('ip address: '+adr(a[6]));
WriteLn('gateway ip: '+adr(a[10]));
WriteLn('netmask: '+adr(a[14]));
Write('dns:');
a:=copy(a,18,255);
while (a<>'') do begin;
  Write(' '+adr(a[1]));
  a:=copy(a,5,255);
  end;
WriteLn('');

a:='arpread-1234';
i:=1;move(i,a[9],sizeof(i));
doQuery(p,a);
if (copy(a,1,7)<>'arpdata') then immErr('invalid response');
move(a[8],o,sizeof(o));
WriteLn('address cache: '+BStr(o));

for i:=1 to o do begin;
  a:='arpread-1234';
  move(i,a[9],sizeof(i));
  doQuery(p,a);
  if (copy(a,1,7)<>'arpdata') then immErr('invalid response');
  move(a[12],o,sizeof(o));
  Write(BStr(i)+': ');
  for q:=16 to length(a) do Write(byte2hextype(ord(a[q])));
  WriteLn(' '+adr(o));
  end;
END.