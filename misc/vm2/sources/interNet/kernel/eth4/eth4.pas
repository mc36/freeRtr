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
{$include cfg_bootp.inc}
{$include cfg_dhcp.inc}


Procedure ProcessNewPipe(pip:LongInt);
Label f1;
Var
  buf:array[1..1024] of byte;
  a,b:string;
  i,o,p:LongInt;
  e:OneEtherAddrRec;
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
  move(b[1],i,sizeof(i));
  move(b[5],e,sizeof(e));
  ArpCacheAppend(i,e);
  a:='ok';
  goto f1;
  end;
if (a='arpread-') then begin;
  move(b[1],i,sizeof(i));
  move(ArpCacheNum,b[1],sizeof(ArpCacheNum));
  b[0]:=chr(sizeof(ArpCacheNum));
  a:='arpdata'+b;
  if (i>=1) and (i<=ArpCacheNum) then begin;
    b[0]:=#4;move(ArpCacheDat^[i].i,b[1],4);a:=a+b;
    b[0]:=#6;move(ArpCacheDat^[i].e,b[1],6);a:=a+b;
    end;
  goto f1;
  end;
if (a='param---') then begin;
  b[0]:=#4;
  move(localNet,b[1],4);
  a:='param'+b;
  move(gatwyNet,b[1],4);a:=a+b;
  move(ntmskNet,b[1],4);a:=a+b;
  a:=a+dnsServs;
  goto f1;
  end;
if (a='data----') and (prgPipe=0) then begin;
  a:='data';
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

Label using,f1,f2;
Var
  pck:OnePacketRecord;
  eth:OneEtherAddrRec;
  a:String;
  lastTest:LongInt;
  i,o,p:LongInt;
BEGIN;
WriteLn('ethernet media access v1.0, done by Mc at '#%date' '#%time'.');
if (ParamCount<2) then begin;
  using:
  WriteLn('using: eth.code <process> <mode> [parameters]');
  WriteLn('modes:');
{
  WriteLn('rarp  - automatic configuration with Reverse Address Resolution Protocol');
}
  WriteLn('bootp - automatic configuration with Boot Protocol');
  WriteLn('dhcp  - automatic configuration with Dinamic Host Config Protocol');
  WriteLn('user  - manual configuration: <myIP> <gateway> <netmask> [dns1]...');
  Halt(1);
  end;

ArpCacheNum:=0;
ethernetOpen;
a:=ParamStr(2);
a:=kicsi(a);
if (a='user') then Configure_user else
if (a='bootp') then Configure_bootp else
if (a='dhcp') then Configure_dhcp else
goto using;

prgPipe:=0;
gatwyEth:=broadEth;
lastTest:=-99999;
lastSent:=0;
ArpCacheAppend(gatwyNet,gatwyEth);
ArpCacheDat^[1].t:=lastTest;

if (pipeLineBegListen<>0) then immErr('failed to start listening!');
DisplayLocalAddresses;
BugOS_SignDaemoning;

f1:
o:=sizeof(pck);
if (pipeLineRecv(ethPipe,pck,o)=0) then begin;
  i:=readWordMSB(pck.t);
  case i of
    $800:begin;
      i:=0;
      move(i,pck.a[5],4);
      pipeLineSend(prgPipe,pck.a[5],o-4);
      lastSent:=prgProc;
      end;
    $806:GotOneArpPacket(pck);
    else WriteLn('got invalid type ($'+byte2hextype(i shr 8)+byte2hextype(i)+') packet from '+convEtherAddr(pck.a));
    end;
  goto f1;
  end;
f2:
o:=sizeof(pck);
if (pipeLineRecv(prgPipe,pck.a[5],o)=0) then begin;
  move(pck.a[5],p,sizeof(p));
  if (p and ntmskNet<>localNet and ntmskNet) then begin;
    eth:=gatwyEth;
    if (p=$ffffffff) then eth:=broadEth;
    end else begin;
    i:=ArpCacheFind(p);
    if (i<1) then begin;
      ArpCachePutBad(p);
      eth:=broadEth;
      lastTest:=-99999;
      end else eth:=ArpCacheDat^[i].e;
    end;
  pck.a:=eth;
  WriteWordMSB(pck.t,$800);
  pipeLineSend(ethPipe,pck,o+4);
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