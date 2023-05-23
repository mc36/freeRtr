Type
  OneGeneralDescriptorRec=record
    siz:Byte;        {size in bytes}
    typ:Byte;        {descriptor type}
    dat:array[1..256] of byte;
    end;
  OneDeviceDescriptorRec=record
    siz:Byte;        {size in bytes}
    typ:Byte;        {descriptor type}
    verUSB:word;     {USB specification release number; 210h=2.10}
    devClass:byte;   {device class code}
    devSubClass:byte;{device subclass code}
    devProtocol:byte;{device protocol code}
    maxPackSiz:byte; {maximum packet size for endpoint zero (8, 16, 32, 64)}
    idVendor:word;   {vendor ID}
    idProduct:word;  {product ID}
    verDev:word;     {device release number in binary-coded decimal}
    strMfgr:byte;    {index of string descriptor describing manufacturer}
    strPrdt:byte;    {index of string descriptor describing product}
    strSrnm:byte;    {index of string descriptor describing serial number}
    maxConfigs:byte; {number of possible configurations}
    end;
  OneConfigureDescriptorRec=record
    siz:Byte;        {size in bytes}
    typ:Byte;        {descriptor type}
    totalLen:word;   {total length: config, if, endpt; class-vendor-specific}
    numIfaces:byte;  {number of interfaces supported by this configuration}
    configValue:byte;{value to use to the SetConfiguration() request}
    strIndex:byte;   {index of string descriptor for this configuration}
    attribs:byte;    {configuration characteristics: bit7: reserved (set to 1)}
                     {bit6: self-powered; bit5: remote-wakeup; bit4..0: reserved (reset to 0)}
    maxPower:byte;   {maximum power consumption expressed in 2mA units}
    end;
  OneInterfaceDescriptorRec=record
    siz:Byte;        {size in bytes}
    typ:Byte;        {descriptor type}
    intrface:byte;   {number of interface}
    alternate:byte;  {select alternate setting for the interface}
    numEndPts:byte;  {number of endpoints used by this interface}
    ifClass:byte;    {interface class code}
    ifSubClass:byte; {interface subclass code}
    ifProtocol:byte; {interface protocol code}
    strIndex:byte;   {index of string descriptor for this interface}
    end;
  OneEndpointDescriptorRec=record
    siz:Byte;        {size in bytes}
    typ:Byte;        {descriptor type}
    address:byte;    {bit 3..0: the endpoint number}
                     {bit 6..4: reserved, reset to 0}
                     {bit 7: direction: 0 = OUT, 1=IN}
    attribs:byte;    {bit 1..0: transfer type: 00=control, 01=isochronous,}
                     {                         10=bulk, 11=interrupt}
    maxPackSiz:Word; {maximum packet size for sending or receiving}
    interval:Byte;   {Interval for polling endpoint for data transfers}
    end;
  OneDescriptorListRecord=array[1..256] of LongInt;



Function doParse(var lst:OneDescriptorListRecord;t:Byte):LongInt;
Var
  d:OneGeneralDescriptorRec;
  i,o,p,q:LongInt;
Begin;
fillchar(lst,sizeof(lst),0);
p:=1;
q:=0;
while (p<=bufferS) do begin;
  move(bufferB[p],d,sizeof(d));
  i:=d.siz;
  if (i<2) then i:=2;
  if (d.typ=t) then begin;
    inc(q);
    lst[q]:=p;
    end;
  inc(p,i);
  end;
doParse:=q;
End;




Function doDump:Boolean;
Var
  dscD:OneGeneralDescriptorRec;
  devD:OneDeviceDescriptorRec;
  cfgD:OneConfigureDescriptorRec;
  ifcD:OneInterfaceDescriptorRec;
  eptD:OneEndpointDescriptorRec;
  ifcL:OneDescriptorListRecord;
  eptL:OneDescriptorListRecord;
  txtL:OneDescriptorListRecord;
  ifcN,eptN,txtN:LongInt;
  i,o,p,q,r:LongInt;
  a,b:String;

function convVer(i:LongInt):String;
Begin;
convVer:=BStr(i shr 12)+BStr((i shr 8) and 15)+'.'+BStr((i shr 4) and 15)+BStr(i and 15);
End;

procedure addTxt(n:LongInt);
var i:LongInt;
begin;
if (n<1) then exit;
for i:=1 to txtN do if (txtL[i]=n) then exit;
inc(txtN);
txtL[txtN]:=n;
end;

Begin;
doDump:=True;
if getDesc(1,0) then exit;
move(bufferD,devD,sizeof(devD));
if (bufferS<devD.siz) then exit;
if (devD.typ<>1) then exit;

txtN:=0;
WriteLn('usb version: '+convVer(ReadWordLSB(devD.verUSB)));
WriteLn('class: '+BStr(devD.devClass));
WriteLn('subclass: '+BStr(devD.devSubClass));
WriteLn('protocol: '+BStr(devD.devProtocol));
WriteLn('max packet size: '+BStr(devD.maxPackSiz));
WriteLn('vendor: '+BStr(ReadWordLSB(devD.idVendor)));
WriteLn('product: '+BStr(ReadWordLSB(devD.idProduct)));
WriteLn('device version: '+convVer(ReadWordLSB(devD.verDev)));
WriteLn('configurations: '+BStr(devD.maxConfigs));
addTxt(devD.strMfgr);
addTxt(devD.strPrdt);
addTxt(devD.strSrnm);
driverCur:=DriverMax+1;
driverPar:='';
config:=0;
for q:=1 to devD.maxConfigs do begin;
  WriteLn('- configuration #'+BStr(q)+':');
  ifcN:=0;
  eptN:=0;
  if getDesc(2,q-1) then continue;
  if (doParse(ifcL,2)<1) then continue;
  move(bufferB[ifcL[1]],cfgD,sizeof(cfgD));
  if (doParse(ifcL,4)<1) then continue;
  if (doParse(eptL,5)<1) then continue;
  WriteLn('  total length: '+BStr(cfgD.totalLen));
  WriteLn('  interfaces: '+BStr(cfgD.numIfaces));
  WriteLn('  value to select: '+BStr(cfgD.configValue));
  WriteLn('  self power: '+BStr(cfgD.attribs shr 7));
  WriteLn('  remote wakeup: '+BStr((cfgD.attribs shr 6) and 1));
  WriteLn('  power consumption: '+BStr(2*cfgD.maxPower));
  addTxt(cfgD.strIndex);
  for r:=1 to cfgD.numIfaces do begin;
    WriteLn('  - interface '+BStr(r)+':');
    b:=BStr(driverPid)+' '+BStr(device)+' '+BStr(speed);
    inc(ifcN);
    move(bufferB[ifcL[ifcN]],ifcD,sizeof(ifcD));
    WriteLn('    interface number: '+BStr(ifcD.intrface));
    WriteLn('    value to select: '+BStr(ifcD.alternate));
    WriteLn('    endpoints: '+BStr(ifcD.numEndPts));
    WriteLn('    class: '+BStr(ifcD.ifClass));
    WriteLn('    subclass: '+BStr(ifcD.ifSubClass));
    WriteLn('    protocol: '+BStr(ifcD.ifProtocol));
    addTxt(ifcD.strIndex);
    for p:=1 to ifcD.numEndPts do begin;
      WriteLn('    - endpoint '+BStr(p)+':');
      inc(eptN);
      move(bufferB[eptL[eptN]],eptD,sizeof(eptD));
      a:=BStr(eptD.address and 15);
      b:=b+' '+a;
      WriteLn('      endpoint number: '+a);
      if (eptD.address and $80=0) then a:='out' else a:='in';
      b:=b+a[1];
      WriteLn('      direction: '+a);
      case eptD.attribs and 3 of
        0:a:='control';
        1:a:='Isochronous';
        2:a:='bulk';
        3:a:='interrupt';
        end;
      b:=b+a[1];
      WriteLn('      transfer type: '+a);
      case (eptD.attribs shr 2) and 3 of
        0:a:='no synchronization';
        1:a:='asynchronous';
        2:a:='adaptive';
        3:a:='synchronous';
        end;
      WriteLn('      synchronization type: '+a);
      case (eptD.attribs shr 4) and 3 of
        0:a:='data';
        1:a:='feedback';
        2:a:='implicit feedback data';
        3:a:='reserved';
        end;
      WriteLn('      usage type: '+a);
      i:=ReadWordLSB(eptD.maxPackSiz);
      a:=BStr(i and $7ff);
      WriteLn('      max packet size: '+a);
      b:=b+a;
      WriteLn('      transaction/microframe: '+BStr(((i shr 12) and 3)+1));
      WriteLn('      poll interval: '+BStr(eptD.interval));
      end;
    p:=(ifcD.ifClass shl 16) or (ifcD.ifSubClass shl 8) or ifcD.ifProtocol;
    for o:=1 to driverCur-1 do begin;
      if (driverDat^[o].typ<>p) then continue;
      driverCur:=o;
      driverPar:=b;
      config:=cfgD.configValue;
      iface:=ifcD.alternate;
      break;
      end;
    end;
  end;
WriteLn('strings ('+BStr(txtN)+') :');
for q:=1 to txtN do begin;
  if getDesc(3,txtL[q]) then continue;
  move(bufferB,dscD,sizeof(dscD));
  if (dscD.typ<>3) then continue;
  if (bufferS<dscD.siz) then continue;
  dec(dscD.siz,2);
  a:='';
  p:=1;
  while (p<dscD.siz) do begin;
    i:=dscD.dat[p];
    a:=a+chr(i);
    inc(p,2);
    end;
  WriteLn('"'+a+'"');
  end;
doDump:=False;
End;
