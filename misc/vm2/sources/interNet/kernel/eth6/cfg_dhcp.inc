Procedure Configure_dhcp(md:LongInt);
Const
  UDPport_DHCPs=547;
  UDPport_DHCPc=546;
Type
  dhcpMessageHeader=record
    typ:Byte;                   {message type}
    tid:array[1..3] of byte;    {transaction id}
    end;
Var
  bufD:array[1..1024] of byte;
  bufS:LongInt;
  bufP:LongInt;
  bufH:dhcpMessageHeader absolute bufD;
  duid:String;
  suid:String;

Function getOpt(var p:LongInt):String;
Var
  i,o,q:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
q:=bufP;
p:=-1;
if (bufP>=bufS) then exit;
p:=ReadWordMSB(bufD[bufP+1]);
o:=ReadWordMSB(bufD[bufP+3]);
inc(bufP,4);
if (o>255) then i:=255 else i:=o;
ab0:=i;
move(bufD[bufP+1],ab[1],i);
inc(bufP,o);
case p of
  3:bufP:=q+16; {ia na}
  4:bufP:=q+8; {ia ta}
  5:bufP:=q+12+sizeof(OneInterAddrRec); {ia addr}
  25:bufP:=q+16; {ia pd}
  end;
getOpt:=a;
End;

Procedure addOpt(p:LongInt;a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
WriteWordMSB(bufD[bufS+1],p);
WriteWordMSB(bufD[bufS+3],ab0);
inc(bufS,4);
move(ab[1],bufD[bufS+1],ab0);
inc(bufS,ab0);
End;

Function mbdOpt(p:LongInt;a:String):String;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
a:='1234'+a;
WriteWordMSB(a[1],p);
WriteWordMSB(a[3],ab0-4);
mbdOpt:=a;
End;

Function findOpt(o:LongInt):String;
Label f1;
Var
  a:String;
  i:LongInt;
Begin;
findOpt:='';
bufP:=sizeof(dhcpMessageHeader);
f1:
a:=getOpt(i);
if (i<0) then exit;
if (i<>o) then goto f1;
findOpt:=a;
End;

Label f1,f2,f3,f4;
Var
  a1,a2:OneInterAddrRec;
  ah:OneEtherAddrRec;
  retry,time:LongInt;
  a,b:String;
  i,o,p:LongInt;
Begin;
duid[0]:=#6;
move(localEth,duid[1],120);
duid:=#0#3#0#1+duid;

retry:=0;
f1:
inc(retry);
if (retry>64) then immErr('too much try!');
bufS:=sizeof(bufH);
bufH.typ:=1;
addOpt(1,duid);                 {client id}
addOpt(8,#0#0);                 {elapsed time}
a[0]:=#4;
move(ethProc,a[1],sizeof(ethProc));
case md of
  1:begin; i:=3;a:=a+#0#0#0#0#0#0#0#0; end;     {address}
  2:begin; i:=25;a:=a+#0#0#0#0#0#0#0#0; end;    {prefix}
  end;
addOpt(i,a);                    {empty address record}
fillchar(gatwyEth,sizeof(gatwyEth),$ff);
fillchar(gatwyNet,sizeof(gatwyNet),0);
WriteWordMSB(gatwyNet.p1,$ff02);
WriteLongMSB(gatwyNet.p4,$10002);
sendUDPpacket(gatwyEth,linkiNet,gatwyNet,UDPport_DHCPc,UDPport_DHCPs,bufS,bufD);
WriteLn('DHCP sent solicit');
BugOS_KernelUptime(i,time,o);
f2:
relequish;
if (getTimePast(time)>5) then goto f1;
receiveUDPpacket(ah,a1,a2,i,o,bufS,bufD);
if (i<>UDPport_DHCPs) then goto f2;
if (o<>UDPport_DHCPc) then goto f2;
if (bufH.typ<>2) then goto f2;
suid:=findOpt(2);
if (suid='') then goto f2;

retry:=0;
f3:
inc(retry);
if (retry>64) then immErr('too much try!');
bufS:=sizeof(bufH);
bufH.typ:=3;
addOpt(1,duid);                 {client id}
addOpt(2,suid);                 {client id}
addOpt(8,#0#0);                 {elapsed time}
a[0]:=#4;
move(ethProc,a[1],sizeof(ethProc));
case md of
  1:begin; i:=3;a:=a+#0#0#0#0#0#0#0#0; end;     {address}
  2:begin; i:=25;a:=a+#0#0#0#0#0#0#0#0; end;    {prefix}
  end;
addOpt(i,a);                    {empty address record}
fillchar(gatwyEth,sizeof(gatwyEth),$ff);
fillchar(gatwyNet,sizeof(gatwyNet),0);
WriteWordMSB(gatwyNet.p1,$ff02);
WriteLongMSB(gatwyNet.p4,$10002);
sendUDPpacket(gatwyEth,linkiNet,gatwyNet,UDPport_DHCPc,UDPport_DHCPs,bufS,bufD);
WriteLn('DHCP sent request');
BugOS_KernelUptime(i,time,o);

f4:
relequish;
if (getTimePast(time)>5) then goto f3;
receiveUDPpacket(ah,a1,a2,i,o,bufS,bufD);
if (i<>UDPport_DHCPs) then goto f4;
if (o<>UDPport_DHCPc) then goto f4;
if (bufH.typ<>7) then goto f2;
a:=findOpt(2);
if (suid<>a) then goto f4;

case md of
  1:begin; i:=5;o:=1; end;      {address}
  2:begin; i:=26;o:=10; end;    {prefix}
  end;
a:=findOpt(i);
if (a='') then goto f4;
gatwyEth:=ah;
gatwyNet:=a1;
fillchar(ntmskNet,sizeof(ntmskNet),$ff);
move(a[o],localNet,sizeof(localNet));
dnsServs:=findOpt(23);
End;
