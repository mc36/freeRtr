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

Label f1;
Var
  i,o,p,q:LongInt;
  a,b,c:String;
BEGIN;
TCPfindProcess;
q:=BVal(ParamStr(2));
string2ipAddr(ParamStr(1),a);

i:=q;
if UDPlistenOnPort(p,4096,b,i) then exit;
WriteLn('listening on '+ipAddr2string(b)+' '+BStr(i)+'...');
WriteLn('will send to '+ipAddr2string(a)+' '+BStr(q)+'...');
move(a,c,sizeof(a));

f1:
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
  UDPsendPacket(p,c,q,i,1);
  end;
if (p=0) then exit;
i:=128;
if UDPreceivePacket(p,b,o,a[1],i) then begin;
  relequish;
  goto f1;
  end;
a[0]:=chr(i);
write(a);
goto f1;
END.