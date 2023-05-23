Var
  LocalAddr:LongInt;            {local ip address}
  LocalHost:LongInt;            {127.0.0.1}
  Ipv6begin:LongInt;            {0.0.255.255}
  UpdateTTL:Boolean;            {update ttl field}
  RoutePacks:Boolean;           {route packets}
  ResendPack:Boolean;           {resend packet on same interface}
  RouteBcast:Boolean;           {route broadcast packets}
  SendErrors:Boolean;           {send icmp messages}
  PrtclPipe:LongInt;            {upper protocol pipeline}
  PrtclProc:LongInt;            {upper protocol process}
  lastSent:LongInt;             {last sent to process}

Type
  OneRouteDescriptorRec=record
    SrcIP:LongInt;              {source ip (anded)}
    SrcMK:LongInt;              {source mask}
    TrgIP:LongInt;              {target ip (anded)}
    TrgMK:LongInt;              {target mask}
    Final:LongInt;              {final destination}
    end;
  OneIfaceDescriptorRec=record
    LocalIP:LongInt;            {local ip address}
    NetMask:LongInt;            {netmask}
    GateWay:LongInt;            {gateway ip}
    mcastIP:LongInt;            {multicast ip}
    bcastIP:LongInt;            {broadcast ip}
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
  LocalHostBytes:array[1..4] of byte absolute LocalHost;
  Ipv6beginBytes:array[1..4] of byte absolute Ipv6begin;
Begin;
LocalHost:=0;
LocalHostBytes[1]:=127;
LocalHostBytes[4]:=1;
Ipv6begin:=0;
Ipv6beginBytes[3]:=255;
Ipv6beginBytes[4]:=255;
LocalAddr:=LocalHost;
UpdateTTL:=True;
RoutePacks:=True;
ResendPack:=False;
RouteBcast:=False;
SendErrors:=False;
End;

Function convIpAddr(var d:LongInt;pad:Boolean):String;
Var a:String;
Begin;
a:=IPv4addressPrefix;
move(d,a[13],sizeof(d));
a:=ipAddr2string(a[1]);
if pad then while (length(a)<15) do a:=' '+a;
convIpAddr:=a;
End;



Procedure FindOneRoute(src,trg:LongInt;var ifc,adr:LongInt);
{if=0-->special case depends on ad; 0=localhost, 1=broadcast, 2=noRoute}
Label loc;
Begin;
adr:=trg;
for ifc:=1 to RoutesNum do
 if (src and RoutesDat[ifc].SrcMK=RoutesDat[ifc].SrcIP) then
 if (trg and RoutesDat[ifc].TrgMK=RoutesDat[ifc].TrgIP) then begin;
  adr:=RoutesDat[ifc].Final;
  if (adr=$00000000) or (adr=$ffffffff) then adr:=trg;
  break;
  end;
if (adr=LocalHost) or (adr=LocalAddr) then begin; loc:ifc:=0;adr:=0;exit; end;
if (adr=$00000000) or (adr=$ffffffff) then begin; ifc:=0;adr:=1;exit; end;
if (src=$00000000) or (src=$ffffffff) then goto loc;
for ifc:=1 to IfacesNum do begin;
  if (adr=IfacesDat[ifc].LocalIP) then goto loc;
  if (adr=IfacesDat[ifc].GateWay) then exit;
  if (adr and IfacesDat[ifc].NetMask=IfacesDat[ifc].mcastIP) then exit;
  if (adr=IfacesDat[ifc].mcastIP) or (adr=IfacesDat[ifc].bcastIP) then goto loc;
  end;
ifc:=0;
adr:=2;
End;
