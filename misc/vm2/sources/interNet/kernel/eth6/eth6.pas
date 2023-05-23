{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc crt.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$include \sources\internet\kernel\utils\checksum.inc}
{$include \sources\internet\kernel\utils\timer.inc}

{$include eth.inc}
{$include arp.inc}
{$include ip.inc}
{$include cfg_user.inc}
{$include cfg_dhcp.inc}


Procedure ProcessNewPipe(pip:LongInt);
Label f1;
Var
  buf:array[1..1024] of byte;
  a,b:string;
  i,o,p:LongInt;
  e:OneEtherAddrRec;
  n:OneInterAddrRec;
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
  move(b[1],n,sizeof(n));
  move(b[17],e,sizeof(e));
  ArpCacheAppend(n,e);
  a:='ok';
  goto f1;
  end;
if (a='adrread-') then begin;
  move(b[1],i,sizeof(i));
  move(ArpCacheNum,b[1],sizeof(ArpCacheNum));
  b[0]:=chr(sizeof(ArpCacheNum));
  a:='adrdata'+b;
  if (i>=1) and (i<=ArpCacheNum) then begin;
    b[0]:=#16;move(ArpCacheDat^[i].i,b[1],16);a:=a+b;
    b[0]:=#6;move(ArpCacheDat^[i].e,b[1],6);a:=a+b;
    end;
  goto f1;
  end;
if (a='param6--') then begin;
  b[0]:=#6;
  move(localEth,b[1],6);
  a:='param6'#1#2#6+b;
  b[0]:=#16;
  move(linkiNet,b[1],16);a:=a+b;
  move(localNet,b[1],16);a:=a+b;
  move(gatwyNet,b[1],16);a:=a+b;
  move(ntmskNet,b[1],16);a:=a+b;
  a:=a+dnsServs;
  goto f1;
  end;
if (a='data6---') and (prgPipe=0) then begin;
  a:='data6';
  pipeLineSend(pip,a[1],length(a));
  prgPipe:=pip;
  pipeLineStats(prgPipe,prgProc,i,o);
  exit;
  end;

a:='error';
f1:
pipeLineSend(pip,a[1],length(a));
pipeLineClose(pip);
End;

Label using,f1,f2,f3;
Var
  pck:OnePacketRecord;
  pck2:OneUpperPacketRec;
  eth:OneEtherAddrRec;
  net:OneEtherAddrRec;
  a:String;
  lastTest:LongInt;
  i,o,p:LongInt;
BEGIN;
WriteLn('ethernet media access v1.0, done by Mc at '#%date' '#%time'.');
if (ParamCount<2) then begin;
  using:
  WriteLn('using: eth.code <process> <mode> [parameters]');
  WriteLn('modes:');
  WriteLn('dhcpa - automatic configuration with Dinamic Host Config Protocol address');
  WriteLn('dhcpp - automatic configuration with Dinamic Host Config Protocol prefix');
{
  WriteLn('router - automatic configuration with Router Advertisements');
}
  WriteLn('user - manual configuration: <myIP> <gateway> <netmask> [dns1]...');
  Halt(1);
  end;

ArpCacheNum:=0;
ethernetOpen;
a:=ParamStr(2);
a:=kicsi(a);
if (a='user') then Configure_user else
if (a='dhcpa') then Configure_dhcp(1) else
if (a='dhcpp') then Configure_dhcp(2) else
goto using;

prgPipe:=0;
gatwyEth:=broadEth;
lastTest:=-99999;
lastSent:=0;
ArpCacheAppend(gatwyNet,gatwyEth);
ArpCacheDat^[1].t:=lastTest;

if (pipeLineBegListen<>0) then begin;
  WriteLn('failed to start listening!');
  Halt(2);
  end;
DisplayLocalAddresses;
BugOS_SignDaemoning;

f1:
o:=sizeof(pck);
if (pipeLineRecv(ethPipe,pck,o)=0) then begin;
  i:=readWordMSB(pck.t);
  if (i<>$86dd) then begin;
    WriteLn('got invalid type ($'+byte2hextype(i shr 8)+byte2hextype(i)+') packet from '+convEtherAddr(pck.a));
    goto f1;
    end;
  pck2.t:=0;
  move(pck.d,pck2.d,o);
  pipeLineSend(prgPipe,pck2,o+12);
  lastSent:=prgProc;
  goto f1;
  end;
f2:
o:=sizeof(pck2);
if (pipeLineRecv(prgPipe,pck2,o)=0) then begin;
  if (pck2.t=2) then begin;
    move(pck2.d,eth,sizeof(eth));
    ArpCacheAppend(pck2.a,eth);
    goto f2;
    end;
  if (pck2.t<>0) then begin;
    WriteLn('got invalid packet from upper!');
    goto f2;
    end;
  if EqualIPmask(pck2.a,localNet,ntmskNet) then begin;
    f3:
    i:=ArpCacheFind(pck2.a);
    if (i<1) then begin;
      ArpCachePutBad(pck2.a);
      eth:=broadEth;
      lastTest:=-99999;
      end else eth:=ArpCacheDat^[i].e;
    end else begin;
    if (readWordMSB(pck2.a)=$fe80) then goto f3;
    eth:=gatwyEth;
    if EqualIPaddr(pck2.a,broadNet) then eth:=broadEth;
    end;
  pck.a:=eth;
  WriteWordMSB(pck.t,$86dd);
  move(pck2.d,pck.d,o);
  pipeLineSend(ethPipe,pck,o-12);
  lastSent:=ethProc;
  goto f2;
  end;
relequish;
while (pipeLineGetIncoming(o)=0) do ProcessNewPipe(o);
if (getTimePast(lastTest)<5) then goto f1;
for o:=ArpCacheNum downto 1 do if SendArpReq(ArpCacheDat^[o]) then ArpCacheDelete(o);
while keypressed do case readkey of
  $0478:exit; {alt+x}
  $0461:DisplayLocalAddresses; {alt+a}
  $0463:DisplayArpCache; {alt+c}
  end;
if (prgPipe<>0) then begin;
  if (pipeLineStats(prgPipe,lastTest,o,p)<>0) then lastTest:=0;
  if (lastTest=0) then begin;
    pipeLineClose(prgPipe);
    prgPipe:=0;
    end;
  end;
BugOS_KernelUptime(i,lastTest,o);
goto f1;
END.