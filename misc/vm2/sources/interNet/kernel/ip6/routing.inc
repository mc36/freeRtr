Type
  OneInternetAddress=record
    p1:LongWord;
    p2:LongWord;
    p3:LongWord;
    p4:LongWord;
    end;

Var
  LocalAddr:OneInternetAddress; {local ip address}
  LocalHost:OneInternetAddress; {0::1}
  UndefAddr:OneInternetAddress; {0::0}
  AllNodAdr:OneInternetAddress; {ff02::1}
  AllRtrAdr:OneInternetAddress; {ff02::2}
  SolictAdr:OneInternetAddress; {ff02::1:ffxx:xxxx}
  UpdateTTL:Boolean;            {update ttl field}
  RoutePacks:Boolean;           {route packets}
  ResendPack:Boolean;           {resend packet on same interface}
  RouteBcast:Boolean;           {route broadcast packets}
  SendErrors:Boolean;           {send icmp messages}
  BroadLimit:Byte;              {broadcast routing limit}
  PrtclPipe:LongInt;            {upper protocol pipeline}
  PrtclProc:LongInt;            {upper protocol process}
  lastSent:LongInt;             {last sent to process}
  LastAdvertise:LongInt;        {last time advertising sent}

Type
  OneRouteDescriptorRec=record
    SrcIP:OneInternetAddress;   {source ip (anded)}
    SrcMK:OneInternetAddress;   {source mask}
    TrgIP:OneInternetAddress;   {target ip (anded)}
    TrgMK:OneInternetAddress;   {target mask}
    Final:OneInternetAddress;   {final destination}
    end;
  OneIfaceDescriptorRec=record
    LinkiIP:OneInternetAddress; {link ip address}
    LocalIP:OneInternetAddress; {local ip address}
    NetMask:OneInternetAddress; {netmask}
    GateWay:OneInternetAddress; {gateway ip}
    mcastIP:OneInternetAddress; {multicast ip}
    bcastIP:OneInternetAddress; {broadcast ip}
    macAddr:array[1..32] of byte;       {hardware address}
    macSize:Byte;               {size of mac address}
    macSrcT:Byte;               {type of soruce mac address}
    macTrgT:Byte;               {type of target mac address}
    prfxLen:Byte;               {prefix length}
    Name:String[15];            {name of device}
    pid:LongInt;                {process id}
    pipe:LongInt;               {pipeline id}
    end;
Const
  MaxRoutes=512;
  MaxIfaces=128;
Var
  RoutesDat:Array[1..MaxRoutes] of OneRouteDescriptorRec;
  IfacesDat:Array[1..MaxIfaces] of OneIfaceDescriptorRec;
  RoutesNum:LongInt;
  IfacesNum:LongInt;


Procedure GetDefaults;
Var
  LocalHostBytes:array[1..16] of byte absolute LocalHost;
  AllNodAdrBytes:array[1..16] of byte absolute AllNodAdr;
  AllRtrAdrBytes:array[1..16] of byte absolute AllRtrAdr;
  SolictAdrBytes:array[1..16] of byte absolute SolictAdr;
Begin;
fillchar(UndefAddr,sizeof(UndefAddr),0);
fillchar(LocalHost,sizeof(LocalHost),0);
fillchar(AllNodAdr,sizeof(AllNodAdr),0);
fillchar(AllRtrAdr,sizeof(AllRtrAdr),0);
fillchar(SolictAdr,sizeof(SolictAdr),0);
AllNodAdrBytes[1]:=$ff;AllNodAdrBytes[2]:=$02;AllNodAdrBytes[16]:=$01;
AllRtrAdrBytes[1]:=$ff;AllRtrAdrBytes[2]:=$02;AllRtrAdrBytes[16]:=$02;
LocalHostBytes[16]:=1;
SolictAdrBytes[1]:=$ff;SolictAdrBytes[2]:=$02;SolictAdrBytes[12]:=1;SolictAdrBytes[13]:=$ff;
LocalAddr:=LocalHost;
UpdateTTL:=True;
RoutePacks:=False;
ResendPack:=False;
RouteBcast:=False;
SendErrors:=False;
BroadLimit:=8;
End;

Function EqualIPaddr(var a1,a2:OneInternetAddress):Boolean;
Begin;
EqualIPaddr:=False;
if (a1.p4<>a2.p4) then exit;
if (a1.p1<>a2.p1) then exit;
if (a1.p2<>a2.p2) then exit;
if (a1.p3<>a2.p3) then exit;
EqualIPaddr:=True;
End;

Function EqualIPmask(var a1,a2,a3:OneInternetAddress):Boolean;
Begin;
EqualIPmask:=False;
if (a1.p4 and a3.p4<>a2.p4 and a3.p4) then exit;
if (a1.p1 and a3.p1<>a2.p1 and a3.p1) then exit;
if (a1.p2 and a3.p2<>a2.p2 and a3.p2) then exit;
if (a1.p3 and a3.p3<>a2.p3 and a3.p3) then exit;
EqualIPmask:=True;
End;



Procedure FindOneRoute(var src,trg:OneInternetAddress;var ifc:LongInt;var adr:OneInternetAddress);
{if: 0=localhost, -1=broadcast, -2=noRoute, -3=discard}
Label loc,non;
Begin;
adr:=trg;
for ifc:=1 to RoutesNum do
 if EqualIPmask(src,RoutesDat[ifc].SrcIP,RoutesDat[ifc].SrcMK) then
 if EqualIPmask(trg,RoutesDat[ifc].TrgIP,RoutesDat[ifc].TrgMK) then begin;
  adr:=RoutesDat[ifc].Final;
  if EqualIPaddr(adr,UndefAddr) then adr:=trg;
  break;
  end;
if EqualIPaddr(adr,LocalHost) then begin; loc:ifc:=0;exit; end;
if EqualIPaddr(adr,LocalAddr) then goto loc;
if EqualIPaddr(adr,UndefAddr) then goto non;
ifc:=ReadWordMSB(adr);
if (ifc and $ff00=$ff00) then begin; ifc:=-1;exit; end;
if (ifc and $ffc0=$fe80) then begin;
  for ifc:=1 to IfacesNum do if EqualIPaddr(adr,IfacesDat[ifc].linkiIP) then goto loc;
  non:
  ifc:=-3;
  exit;
  end;
if EqualIPaddr(src,UndefAddr) then goto loc;
ifc:=ReadWordMSB(src);
if (ifc and $ff00=$ff00) then goto loc;
if (ifc and $ffc0=$fe80) then goto non;
for ifc:=1 to IfacesNum do begin;
  if EqualIPaddr(adr,IfacesDat[ifc].LocalIP) then goto loc;
  if EqualIPaddr(adr,IfacesDat[ifc].GateWay) then exit;
  if EqualIPmask(adr,IfacesDat[ifc].mcastIP,IfacesDat[ifc].NetMask) then exit;
  end;
ifc:=-2;
End;
