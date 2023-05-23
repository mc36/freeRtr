{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc crt.inc}
{$sysinc alap.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$include \sources\internet\kernel\utils\checksum.inc}
{$include \sources\internet\kernel\utils\timer.inc}

{$include routing.inc}
{$include ifaces.inc}
{$include ip.inc}

Procedure ProcessNewPipe(pip:LongInt);
Label f1;

function cip(i:LongInt):String;var a:String;begin; a[0]:=#4;move(i,a[1],4);cip:=IPv4addressPrefix+a; end;

Var
  buf:array[1..1024] of byte;
  a,b:string;
  i,o,p,q,r,s:LongInt;
Begin;
pipeLineStats(pip,p,i,o);
BugOS_ProcessName(p,buf,i,i,o);
if (o and $40=0) then goto f1;
i:=128;
if (pipeLineRecv(pip,b[1],i)<>0) then i:=0;
b[0]:=chr(i);
a:=kicsi(copy(b,1,8));
b:=copy(b,9,255);
if (a='param---') then begin;
  a:='param'+cip(LocalAddr);
  goto f1;
  end;
if (a='patred--') then begin;
  move(b[1],i,sizeof(i));
  if (i>=1) and (i<=RoutesNum) then begin;
    a:=cip(RoutesDat[i].SrcIP)+cip(RoutesDat[i].SrcMK)+
       cip(RoutesDat[i].TrgIP)+cip(RoutesDat[i].TrgMK)+
       cip(RoutesDat[i].Final);
    end else a:='';
  b[0]:=#8;
  move(i,b[1],4);
  move(RoutesNum,b[5],4);
  a:='path'+b+a;
  goto f1;
  end;
if (a='patadd--') then begin;
  move(b[1],i,4);
  move(b[17],o,4);
  move(b[33],p,4);
  move(b[49],q,4);
  move(b[65],r,4);
  move(b[81],s,4);
  if AddOneRoute(i,o,p,q,r,s) then a:=#1#0#0#0 else a:=#0#0#0#0;
  a:='add'+a;
  goto f1;
  end;
if (a='patdel--') then begin;
  move(b[1],i,4);
  if DelOneRoute(i) then a:=#1#0#0#0 else a:=#0#0#0#0;
  a:='del'+a;
  goto f1;
  end;
if (a='ifcred--') then begin;
  move(b[1],i,4);
  if (i>=1) and (i<=IfacesNum) then begin;
    a:=cip(IfacesDat[i].LocalIP)+cip(IfacesDat[i].GateWay)+
       cip(IfacesDat[i].NetMask);
    b[0]:=#4;move(IfacesDat[i].pid,b[1],4);
    a:=a+b+IfacesDat[i].Name
    end else a:='';
  b[0]:=#8;
  move(i,b[1],4);
  move(IfacesNum,b[5],4);
  a:='iface'+b+a;
  goto f1;
  end;
if (a='ifcadd--') then begin;
  move(b[1],i,4);
  b:=copy(b,5,255);
  if AddOneIface(i,b) then a:=#1#0#0#0 else a:=#0#0#0#0;
  a:='add'+a;
  goto f1;
  end;
if (a='ifcdel--') then begin;
  move(b[1],i,4);
  if DelOneIface(i) then a:=#1#0#0#0 else a:=#0#0#0#0;
  a:='del'+a;
  goto f1;
  end;
if (a='getopt--') then begin;
  move(b[1],i,4);
  case i of
    1:a:=chr(UpdateTTL)+#0#0#0'update TTL when forwarding';
    2:a:=chr(RoutePacks)+#0#0#0'forward packets between interfaces';
    3:a:=chr(ResendPack)+#0#0#0'resend misrouted packets on interface';
    4:a:=chr(RouteBcast)+#0#0#0'forward broadcast packets between interfaces';
    5:a:=chr(SendErrors)+#0#0#0'generate ICMP error reporting packets';
    else a:='';
    end;
  b[0]:=#8;
  move(i,b[1],4);
  i:=5;
  move(i,b[5],4);
  a:='option'+b+a;
  goto f1;
  end;
if (a='setopt--') then begin;
  move(b[1],i,4);
  move(b[5],o,4);
  a:=#0#0#0#0;
  case i of
    1:UpdateTTL:=(o=1);
    2:RoutePacks:=(o=1);
    3:ResendPack:=(o=1);
    4:RouteBcast:=(o=1);
    5:SendErrors:=(o=1);
    else a:=#1#0#0#0;
    end;
  a:='set'+a;
  goto f1;
  end;
if (a='data----') and (PrtclPipe=0) then begin;
  a:='data';
  pipeLineSend(pip,a[1],length(a));
  PrtclPipe:=pip;
  pipeLineStats(PrtclPipe,PrtclProc,i,o);
  exit;
  end;

a:='error';
f1:
pipeLineSend(pip,a[1],length(a));
pipeLineClose(pip);
End;

Label f1,f2;
Var
  pck:OneProtocolPacketRec;
  a:String;
  lastTest:LongInt;
  i,o,p:LongInt;
BEGIN;
WriteLn('internet protocol 4 v1.0, done by Mc at '#%date' '#%time'.');

if (pipeLineBegListen<>0) then begin;
  WriteLn('failed to start listening!');
  Halt(2);
  end;
RoutesNum:=0;
IfacesNum:=0;
PrtclPipe:=0;
PrtclProc:=0;
GetDefaults;
lastTest:=-1;
lastSent:=0;
BugOS_SignDaemoning;
f1:
for i:=IfacesNum downto 1 do ProcessIfaces(i,IfacesDat[i]);
f2:
o:=sizeof(pck);
if (pipeLineRecv(PrtclPipe,pck,o)=0) then begin;
  ProcessPrtcol(pck,o-33);
  goto f2;
  end;
if (pipeLineGetIncoming(o)=0) then begin;
  relequish;
  repeat
    ProcessNewPipe(o);
    until (pipeLineGetIncoming(o)<>0);
  end;
relequish;
if (getTimePast(lastTest)<5) then goto f1;
while keypressed do case readkey of
  $0478:exit; {alt+x}
  $0461:begin; {alt+a}
    WriteLn('local address: '+convIpAddr(LocalAddr,false));
    WriteLn('   interfaces: '+BStr(IfacesNum));
    WriteLn('       routes: '+BStr(RoutesNum));
    end;
  $0469:begin; {alt+i}
    WriteLn('interfaces:');
    for i:=1 to IfacesNum do WriteLn(
     convIpAddr(IfacesDat[i].LocalIP,true)+' '+
     convIpAddr(IfacesDat[i].GateWay,true)+' '+
     convIpAddr(IfacesDat[i].NetMask,true)+' '+
     IfacesDat[i].name
     );
    end;
  $0472:begin; {alt+r}
    WriteLn('routes:');
    for i:=1 to RoutesNum do WriteLn(
     convIpAddr(RoutesDat[i].SrcIP,true)+' '+
     convIpAddr(RoutesDat[i].SrcMK,true)+' '+
     convIpAddr(RoutesDat[i].TrgIP,true)+' '+
     convIpAddr(RoutesDat[i].TrgMK,true)+' '+
     convIpAddr(RoutesDat[i].Final,true)
     );
    end;
  end;
if (PrtclPipe<>0) then begin;
  if (pipeLineStats(PrtclPipe,lastTest,o,p)<>0) then lastTest:=0;
  if (lastTest=0) then begin;
    pipeLineClose(PrtclPipe);
    PrtclPipe:=0;
    end;
  end;
BugOS_KernelUptime(i,lastTest,o);
goto f1;
END.