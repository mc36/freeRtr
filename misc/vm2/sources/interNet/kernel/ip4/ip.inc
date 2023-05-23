Var
  PacketSequences:LongInt;
Type
  OneIPheaderRecord=record
    VerIHL:byte;                {version & ihl}
    SerTyp:byte;                {type of service}
    TotLen:word;                {Total Length}
    Idntfy:word;                {id}
    FlgOfs:word;                {flag & fragment offset}
    LivTim:byte;                {time to live}
    Protcl:byte;                {protocol}
    HedSum:word;                {header checksum}
    SrcAdr:longint;             {source address}
    TrgAdr:longint;             {target address}
    end;
  OneICMPheaderRecord=record
    Typ:Byte;
    Cod:Byte;
    Sum:Word;
    Idn:Word;
    Seq:Word;
    end;
Type
  OneInterfacePacketRec=record
    a:LongInt;
    d:array[1..4*1024] of byte;
    end;
  OneProtocolPacketRec=record
    a1:LongInt;
    a2:LongInt;
    a3:LongInt;
    a4:LongInt;
    b1:LongInt;
    b2:LongInt;
    b3:LongInt;
    b4:LongInt;
    p:Byte;
    d:Array[1..4*1024] of byte;
    end;


Procedure SendInternetPacket(var pck:OneInterfacePacketRec;prot,siz,adr1,adr2,ti,ta:LongInt);
Var
  ip:OneIPheaderRecord;
Begin;
inc(PacketSequences);
ip.VerIHL:=$45;
ip.SerTyp:=$00;
WriteWordMSB(ip.TotLen,siz+20);
ip.Idntfy:=PacketSequences;
ip.FlgOfs:=0;
ip.LivTim:=255;
ip.Protcl:=prot;
ip.HedSum:=0;
ip.SrcAdr:=adr2;
ip.TrgAdr:=adr1;
ip.HedSum:=not CalculateSum(ip,sizeof(ip));
move(ip,pck.d,sizeof(ip));
if (ti=0) then begin;
  FindOneRoute(LocalAddr,adr1,ti,ta);
  if (ti=0) then exit;
  end;
pck.a:=ta;
lastSent:=IfacesDat[ti].pid;
pipeLineSend(IfacesDat[ti].pipe,pck,siz+24);
End;

Procedure SendIcmpErrorPacket(var pck:OneInterfacePacketRec;typ,cod:Byte;adr:LongInt;a:String);
Var
  ip:OneIPheaderRecord;
  icmp:OneICMPheaderRecord;
  i,o:LongInt;
Begin;
if not SendErrors then exit;
move(pck.d,ip,sizeof(ip));
o:=((ip.VerIHL and $0f) shl 2)+8;
i:=ReadWordMSB(ip.TotLen);
if (o<i) then o:=i;
i:=ReadWordMSB(ip.FlgOfs);
if (i and $3fff<>0) then exit;
if (ip.Protcl=1) then exit;
move(pck.d,pck.d[29],o);
icmp.Typ:=typ;
icmp.Cod:=cod;
icmp.Sum:=0;
icmp.Idn:=0;
icmp.Seq:=0;
move(icmp,pck.d[21],sizeof(icmp));
icmp.Sum:=not CalculateSum(pck.d[21],o+8);
move(icmp,pck.d[21],sizeof(icmp));
SendInternetPacket(pck,1,o+8,adr,LocalAddr,0,0);
WriteLn('ICMP sends '+a+' to '+convIpAddr(adr,false));
End;


Procedure ProcessIfaces(num:LongInt;var dat:OneIfaceDescriptorRec);
Label lh,f1;
Var
  siz,beg:LongInt;
  ifc,adr:LongInt;
  a:String;
  i,o:LongInt;
  ipck:OneInterfacePacketRec;
  ppck:OneProtocolPacketRec;
  ip:OneIPheaderRecord;
  icmp:OneICMPheaderRecord;
Begin;
f1:
siz:=sizeof(ipck);
if (pipeLineRecv(dat.pipe,ipck,siz)<>0) then begin;
  pipeLineStats(dat.pipe,o,i,i);
  if (o=0) then DelOneIface(num);
  exit;
  end;
dec(siz,4)
move(ipck.d,ip,sizeof(ip));
if (ip.VerIHL and $f0<>$40) then begin;
  WriteLn('IP got a packet with invalid version!');
  goto f1;
  end;
i:=ReadWordMSB(ip.TotLen);
if (i>siz) then begin;
  WriteLn('IP got a packet with invalid total length!');
  goto f1;
  end;
siz:=i;
beg:=(ip.VerIHL and $0f) shl 2;
if (beg<20) or (beg>siz) then begin;
  WriteLn('IP got a packet with invalid header length!');
  goto f1;
  end;
if (CalculateSum(ipck.d,beg)<>$ffff) then begin;
  WriteLn('IP got a packet with invalid header checksum!');
  goto f1;
  end;
dec(siz,beg);
i:=ip.srcAdr;
if (i=dat.mcastIP) or (i=dat.bcastIP) then begin;
  WriteLn('IP got a packet from broadcast address!');
  goto f1;
  end;
if (ip.LivTim=0) then begin;
  WriteLn('IP got a packet with zero ttl!');
  goto f1;
  end;
FindOneRoute(ip.srcAdr,ip.trgAdr,ifc,adr);
if (ifc<>0) then begin;
  ipck.a:=adr;
  if not RoutePacks then goto f1;
  if (ifc=num) then if not ResendPack then goto f1;
  if UpdateTTL then begin;
    dec(ip.LivTim);
    if (ip.LivTim<1) then begin;
      SendIcmpErrorPacket(ipck,11,0,ip.srcAdr,'time to live exceeded in transit');
      goto f1;
      end;
    ip.hedSum:=0;
    move(ip,ipck.d,sizeof(ip));
    ip.hedSum:=not CalculateSum(ipck.d,beg);
    move(ip,ipck.d,sizeof(ip));
    end;
  lastSent:=IfacesDat[ifc].pid;
  pipeLineSend(IfacesDat[ifc].pipe,ipck,beg+siz+4);
  goto f1;
  end;
if (adr=0) then begin;
  lh:
  i:=ReadWordMSB(ip.FlgOfs);
  if (i and $2000<>0) then begin;
    WriteLn('IP got a packet from '+convIpAddr(ip.srcAdr,false)+' with more fragments!');
    goto f1;
    end;
  if (i and $1fff<>0) then begin;
    WriteLn('IP got a packet from '+convIpAddr(ip.srcAdr,false)+' with fragment offset!');
    goto f1;
    end;
  if (ip.Protcl<>1) then begin;
    ppck.a1:=0;
    ppck.b1:=0;
    ppck.a2:=0;
    ppck.b2:=0;
    ppck.a3:=Ipv6begin;
    ppck.b3:=Ipv6begin;
    ppck.a4:=ip.srcAdr;
    ppck.b4:=ip.trgAdr;
    ppck.p:=ip.Protcl;
    move(ipck.d[beg+1],ppck.d,siz);
    lastSent:=PrtclProc;
    pipeLineSend(PrtclPipe,ppck,siz+33);
    goto f1;
    end;
  if (siz<sizeof(icmp)) then begin;
    WriteLn('ICMP got a too short packet from '+convIpAddr(ip.srcAdr,false)+'!');
    goto f1;
    end;
  if (CalculateSum(ipck.d[beg+1],siz)<>$ffff) then begin;
    WriteLn('ICMP got a packet with invalid header checksum from '+convIpAddr(ip.srcAdr,false)+'!');
    goto f1;
    end;
  move(ipck.d[beg+1],icmp,sizeof(icmp));
  a:='unknown (type=$'+byte2hextype(icmp.typ)+'; code=$'+byte2hextype(icmp.cod)+')';
  case icmp.typ of
    8:if (icmp.cod=0) then begin;
      move(ipck.d[beg+9],ipck.d[29],siz);
      icmp.typ:=0;
      icmp.cod:=0;
      icmp.sum:=0;
      move(icmp,ipck.d[21],sizeof(icmp));
      icmp.sum:=not CalculateSum(ipck.d[21],siz);
      move(icmp,ipck.d[21],sizeof(icmp));
      SendInternetPacket(ipck,1,siz,ip.srcAdr,LocalAddr,0,0);
      a:='echo request';
      end;
    0:if (icmp.cod=0) then begin;
      a:='echo reply';
      end;
    3:case icmp.cod of
      0:a:='network unreachable';
      1:a:='host unreachable';
      2:a:='protocol unreachable';
      3:a:='port unreachable';
      4:a:='fragmentation needed and don''t fragment set';
      5:a:='source route failed';
      end;
    11:case icmp.cod of
      0:a:='time to live exceeded in transit';
      1:a:='fragment reassembly time exceeded';
      end;
    12:if (icmp.cod=0) then a:='parameter problem';
    4:if (icmp.cod=0) then a:='source quench';
    5:case icmp.cod of
      0:a:='redirect datagrams for network';
      1:a:='redirect datagrams for host';
      2:a:='redirect datagrams for type of service and network';
      3:a:='redirect datagrams for type of service and host';
      end;
    13:if (icmp.cod=0) then a:='timestamp request';
    14:if (icmp.cod=0) then a:='timestamp reply';
    15:if (icmp.cod=0) then a:='information request';
    16:if (icmp.cod=0) then a:='information reply';
    end;
  WriteLn('ICMP got '+a+' from '+convIpAddr(ip.srcAdr,false)+'!');
  goto f1;
  end;
if (adr=1) then begin;
  if not RouteBcast then goto lh;
  o:=siz+beg+4;
  ipck.a:=$ffffffff;
  for i:=1 to IfacesNum do if (i<>num) then pipeLineSend(IfacesDat[i].pipe,ipck,o);
  goto lh;
  end;
if (adr=2) then begin;
  SendIcmpErrorPacket(ipck,3,1,ip.srcAdr,'host unreachable');
  goto f1;
  end;

WriteLn('IP encountered internal error!');
End;

Procedure ProcessPrtcol(var ppck:OneProtocolPacketRec;siz:LongInt);
Label f1;
Var
  ipck:OneInterfacePacketRec;
  i,o:LongInt;
Begin;
if (ppck.a1<>0) then goto f1;
if (ppck.a2<>0) then goto f1;
if (ppck.a3<>Ipv6begin) then goto f1;
FindOneRoute(LocalAddr,ppck.a4,i,o);
if (i<>0) then begin;
  move(ppck.d,ipck.d[21],siz);
  SendInternetPacket(ipck,ppck.p,siz,ppck.a4,ppck.b4,i,o);
  exit;
  end;
if (o=0) then begin;
  lastSent:=PrtclProc;
  pipeLineSend(PrtclPipe,ppck,siz+33);
  exit;
  end;
if (o=1) then begin;
  WriteLn('upper wants to broadcast!');
  exit;
  end;
if (o=2) then begin;
  f1:
  WriteLn('IP has no route to '+convIpAddr(ppck.a4,false)+'!');
  exit;
  end;

WriteLn('IP encountered internal error!');
End;
