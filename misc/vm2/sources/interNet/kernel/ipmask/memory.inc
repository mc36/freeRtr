Type
  OneConnectionRecord=record
    prot:LongInt;               {protocol: 1=udp, 2=tcp}
    time:LongInt;               {time of last action}
    locA:OneAddressRecord;      {local source address}
    locP:LongInt;               {local source port}
    extA:OneAddressRecord;      {external target address}
    extP:LongInt;               {external target port}
    emuP:LongInt;               {external source port}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  LocalAddr:OneAddressRecord absolute LocalAddress;
  LocalMask:OneAddressRecord;
  XtrnlAddr:OneAddressRecord;
  WholeMask:OneAddressRecord;
  PrtclPipe:LongInt;
  PrtclProc:LongInt;



Function ResizeMem(n:LongInt):Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
ResizeMem:=True;
i:=n*sizeof(OneConnectionRecord);
if (ExtendedMemoryResize(p,i)<i) then exit;
ConnectionNum:=n;
ConnectionDat:=p^;
ResizeMem:=False;
End;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;



Function IsAddressInMask(a1,a2,a3:OneAddressRecord):Boolean;
Begin;
IsAddressInMask:=False;
if (a1.a4 and a3.a4<>a2.a4 and a3.a4) then exit;
if (a1.a3 and a3.a3<>a2.a3 and a3.a3) then exit;
if (a1.a2 and a3.a2<>a2.a2 and a3.a2) then exit;
if (a1.a1 and a3.a1<>a2.a1 and a3.a1) then exit;
IsAddressInMask:=True;
End;

Function FindOneLocalSide(locA,extA:OneAddressRecord;locP,extP,prot:LongInt):LongInt;
Label f1;
Var
  i:LongInt;
  adr:OneAddressRecord;
Begin;
for i:=1 to ConnectionNum do begin;
  if (ConnectionDat^[i].prot<>prot) then continue;
  if (ConnectionDat^[i].locP<>locP) then continue;
  if (ConnectionDat^[i].extP<>extP) then continue;
  adr:=ConnectionDat^[i].locA;
  if (adr.a4<>locA.a4) then continue;
  if (adr.a3<>locA.a3) then continue;
  if (adr.a2<>locA.a2) then continue;
  if (adr.a1<>locA.a1) then continue;
  adr:=ConnectionDat^[i].extA;
  if (adr.a4<>extA.a4) then continue;
  if (adr.a3<>extA.a3) then continue;
  if (adr.a2<>extA.a2) then continue;
  if (adr.a1<>extA.a1) then continue;
  goto f1;
  end;
i:=0;
f1:
FindOneLocalSide:=i;
End;

Function FindOneExternalSide(extA:OneAddressRecord;extP,emuP,prot:LongInt):LongInt;
Label f1;
Var
  i:LongInt;
  adr:OneAddressRecord;
Begin;
for i:=1 to ConnectionNum do begin;
  if (ConnectionDat^[i].prot<>prot) then continue;
  if (ConnectionDat^[i].emuP<>emuP) then continue;
  if (ConnectionDat^[i].extP<>extP) then continue;
  adr:=ConnectionDat^[i].extA;
  if (adr.a4<>extA.a4) then continue;
  if (adr.a3<>extA.a3) then continue;
  if (adr.a2<>extA.a2) then continue;
  if (adr.a1<>extA.a1) then continue;
  goto f1;
  end;
i:=0;
f1:
FindOneExternalSide:=i;
End;






Procedure displayInformation;
Begin;
WriteLn('  link process id: '+BStr(LowerProcess));
WriteLn(' link pipeline id: '+BStr(LowerPipeline));
WriteLn(' upper process id: '+BStr(PrtclProc));
WriteLn('upper pipeline id: '+BStr(PrtclPipe));
WriteLn('      connections: '+BStr(ConnectionNum));
WriteLn('         local ip: '+ipAddr2string(localAddr));
WriteLn('       local mask: '+ipAddr2string(localMask));
WriteLn('      external ip: '+ipAddr2string(xtrnlAddr));
End;

Procedure displayConnectinos;
Var
  d:OneConnectionRecord;
  a:String;
  i:LongInt;
Begin;
WriteLn('protocol    source                                target');
for i:=1 to ConnectionNum do begin;
  d:=ConnectionDat^[i];
  case d.prot of
    1:a:='tcp ';
    2:a:='udp ';
    else a:='??? ';
    end;
  a:=a+ipAddr2string(d.locA)+' '+BStr(d.locP);
  while (length(a)<50) do a:=a+' ';
  a:=a+ipAddr2string(d.extA)+' '+BStr(d.extP);
  WriteLn(a);
  end;
End;
