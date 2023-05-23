Var
  PacketSequences:LongInt;
Type
  OneIPheaderRecord=record
    VerTCFL:longword;           {version & traffiClass & flowLabel}
    PayLen:word;                {payload length}
    NxtHdr:byte;                {next header}
    LivTim:byte;                {time to live}
    SrcAdr:OneInternetAddress;  {source address}
    TrgAdr:OneInternetAddress;  {target address}
    end;
  OneIPextensionRecord=record
    nextTyp:Byte;               {type of next header}
    extSize:Byte;               {size of header -1 /8}
    end;
  OneICMPheaderRecord=record
    Typ:Byte;
    Cod:Byte;
    Sum:Word;
    Res:LongWord;
    end;
Type
  OneInterfacePacketRec=record
    t:LongInt;
    a:OneInternetAddress;
    d:array[1..4*1024] of byte;
    end;
  OneProtocolPacketRec=record
    a:OneInternetAddress;
    b:OneInternetAddress;
    p:Byte;
    d:Array[1..4*1024] of byte;
    end;


Procedure SendInternetPacket(var pck:OneInterfacePacketRec;prot,siz,ti:LongInt;adrS,adrT,ta:OneInternetAddress);
Var
  ip:OneIPheaderRecord;
Begin;
WriteLongMSB(ip.VerTCFL,$60000000);
WriteWordMSB(ip.PayLen,siz);
ip.NxtHdr:=prot;
ip.LivTim:=255;
ip.SrcAdr:=adrS;
ip.TrgAdr:=adrT;
move(pck.d[1],pck.d[sizeof(ip)+1],siz);
inc(siz,sizeof(ip));
move(ip,pck.d,sizeof(ip));
if (ti<1) then begin;
  FindOneRoute(LocalAddr,adrT,ti,ta);
  if (ti<1) then exit;
  end;
pck.a:=ta;
pck.t:=0;
lastSent:=IfacesDat[ti].pid;
pipeLineSend(IfacesDat[ti].pipe,pck,siz+20);
End;

Procedure SendIcmpErrorPacket(var pck:OneInterfacePacketRec;typ,ti,res,siz:LongInt;var adrS,adrT,ta:OneInternetAddress;a:String;error:Boolean);
Var
  icmp:OneICMPheaderRecord;
  ip:OneIPheaderRecord;
  i,o:LongInt;
Begin;
if error then begin;
  if not SendErrors then exit;
  move(pck.d,ip,sizeof(ip));
  if (ip.NxtHdr=58) then exit;
  if (siz>256) then siz:=256;
  end;
WriteWordMSB(icmp.typ,typ);
WriteLongMSB(icmp.res,res);
icmp.sum:=0;
move(pck.d[1],pck.d[sizeof(icmp)+1],siz);
inc(siz,sizeof(icmp));
move(icmp,pck.d,sizeof(icmp));
icmp.sum:=not TestPseudoSum(adrS,adrT,pck.d,58,siz);
move(icmp,pck.d,sizeof(icmp));
WriteLn('ICMP sends '+a+' to '+ipAddr2string(adrT));
SendInternetPacket(pck,58,siz,ti,adrS,adrT,ta);
End;

Procedure addLinkAddress(var pck:OneInterfacePacketRec;var siz:LongInt;num,typ:LongInt);
Var i:LongInt;
Begin;
i:=IfacesDat[num].macSize;
if (i<1) then exit;
i:=(i+9) and $ffffff8;
pck.d[siz+1]:=typ;
pck.d[siz+2]:=i shr 3;
move(IfacesDat[num].macAddr,pck.d[siz+3],i);
inc(siz,i);
End;

Procedure addPrefixLength(var pck:OneInterfacePacketRec;var siz:LongInt;num:LongInt);
Var i:LongInt;
Begin;
pck.d[siz+1]:=3;
pck.d[siz+2]:=4;
pck.d[siz+3]:=IfacesDat[num].prfxLen;
pck.d[siz+4]:=$c0;
i:=-1;
move(i,pck.d[siz+5],sizeof(i));
move(i,pck.d[siz+9],sizeof(i));
i:=0;
move(i,pck.d[siz+13],sizeof(i));
move(IfacesDat[num].mcastIP,pck.d[siz+17],sizeof(OneInternetAddress));
inc(siz,32);
End;

Procedure ProcessIfaces(num:LongInt;var dat:OneIfaceDescriptorRec);
Label f1,rr,lh,upp,icm,nhd;
Var
  a:String;
  i,o:LongInt;
  beg,siz,osz:LongInt;
  ipck:OneInterfacePacketRec;
  ppck:OneProtocolPacketRec;
  icmp:OneICMPheaderRecord;
  ext:OneIPextensionRecord;
  adr,adr2:OneInternetAddress;
  ip:OneIPheaderRecord;

Procedure addCache(o:LongInt;var adr:OneInternetAddress);
Var i,s:LongInt;
Label f1;
Begin;
s:=siz;
f1:
if (s<8) then exit;
if (ipck.d[beg+1]<>o) then begin;
  i:=ipck.d[beg+2];
  if (i<1) then exit;
  inc(beg,i);
  dec(s,i);
  goto f1;
  end;
ipck.t:=2;
ipck.a:=adr;
i:=IfacesDat[num].macSize;
move(ipck.d[beg+3],ipck.d,i);
pipeLineSend(IfacesDat[num].pipe,ipck,i+20);
End;

Begin;
f1:
siz:=sizeof(ipck);
if (pipeLineRecv(dat.pipe,ipck,siz)<>0) then begin;
  pipeLineStats(dat.pipe,o,i,i);
  if (o=0) then DelOneIface(num);
  exit;
  end;
dec(siz,20);
if (ipck.t=1) then begin;
  o:=sizeof(adr);
  move(ipck.a,ipck.d,sizeof(adr));
  move(ipck.a,adr2,sizeof(adr2));
  move(SolictAdr,adr2,sizeof(SolictAdr)-3);
  addLinkAddress(ipck,o,num,IfacesDat[num].macSrcT);
  SendIcmpErrorPacket(ipck,$8700,num,0,o,IfacesDat[num].LocalIP,adr2,ipck.a,'neighbor solicitation',false);
  goto f1;
  end;
if (ipck.t<>0) then begin;
  WriteLn('IP got invalid link packet from '+dat.name);
  goto f1;
  end;
if (siz<sizeof(ip)) then begin;
  WriteLn('IP got a too short packet!');
  goto f1;
  end;
move(ipck.d,ip,sizeof(ip));
i:=ReadLongMSB(ip.VerTCFL);
if (i shr 28<>6) then begin;
  WriteLn('IP got a packet with invalid version!');
  goto f1;
  end;
if (ip.LivTim=0) then begin;
  WriteLn('IP got a packet with zero ttl!');
  goto f1;
  end;
i:=ReadWordMSB(ip.PayLen);
if (i+sizeof(ip)>siz) then begin;
  WriteLn('IP got a packet with invalid payload length from '+ipAddr2string(ip.srcAdr));
  goto f1;
  end;
siz:=i;
beg:=sizeof(ip);
osz:=siz;
rr:
FindOneRoute(ip.SrcAdr,ip.TrgAdr,i,adr);
if (i>0) then begin;
  ipck.a:=adr;
  if not RoutePacks then goto f1;
  if (i=num) then if not ResendPack then goto f1;
  if UpdateTTL then begin;
    dec(ip.LivTim);
    if (ip.LivTim<1) then begin;
      SendIcmpErrorPacket(ipck,$0300,0,0,osz,LocalAddr,ip.srcAdr,ip.srcAdr,'time to live exceeded in transit',true);
      goto f1;
      end;
    end;
  lastSent:=IfacesDat[i].pid;
  pipeLineSend(IfacesDat[i].pipe,ipck,beg+siz+20);
  goto f1;
  end;
if (i=0) then begin;
  lh:
  if (ip.NxtHdr in [$06,$11]) then begin;
    upp:
    ppck.a:=ip.SrcAdr;
    ppck.b:=ip.TrgAdr;
    ppck.p:=ip.NxtHdr;
    move(ipck.d[beg+1],ppck.d,siz);
    lastSent:=PrtclProc;
    pipeLineSend(PrtclPipe,ppck,siz+33);
    goto f1;
    end;
  if (ip.NxtHdr=58) then begin;
    if (siz<sizeof(icmp)) then begin;
      WriteLn('ICMP got too short packet from '+ipAddr2string(ip.srcAdr));
      goto f1;
      end;
    if (TestPseudoSum(ip.srcAdr,ip.trgAdr,ipck.d[beg+1],58,siz)<>$ffff) then begin;
      WriteLn('ICMP got wrong checksum from '+ipAddr2string(ip.srcAdr));
      goto f1;
      end;
    move(ipck.d[beg+1],icmp,sizeof(icmp));
    inc(beg,sizeof(icmp));
    dec(siz,sizeof(icmp));
    if EqualIPaddr(ip.trgAdr,IfacesDat[num].linkiIP) then adr:=ip.trgAdr else adr:=LocalAddr;
    a:='unknown (type='+byte2hextype(icmp.typ)+'; code='+byte2hextype(icmp.cod)+')';
    case icmp.typ of
      135:begin; {neighbor solicitation}
        a:='neighbor solicitation';
        move(ipck.d[beg+1],adr,sizeof(adr));
        dec(siz,sizeof(adr));
        inc(beg,sizeof(adr));
        addCache(IfacesDat[num].macSrcT,ip.srcAdr);
        if not EqualIPaddr(adr,IfacesDat[num].LocalIP) then
         if not EqualIPaddr(adr,IfacesDat[num].LinkiIP) then goto f1;
        move(adr,ipck.d[1],sizeof(adr));
        o:=sizeof(adr);
        addLinkAddress(ipck,o,num,IfacesDat[num].macTrgT);
        i:=$60;
        if RoutePacks then inc(i,$80);
        SendIcmpErrorPacket(ipck,$8800,num,i shl 24,o,adr,ip.srcAdr,ip.srcAdr,'neighbor advertisement',false);
        end;
      136:begin; {neighbor advertisement}
        a:='neighbor advertisement';
        move(ipck.d[beg+1],adr,sizeof(adr));
        dec(siz,sizeof(adr));
        inc(beg,sizeof(adr));
        addCache(IfacesDat[num].macTrgT,adr);
        end;
      137:begin; {redirect}
        a:='redirect for '+ipAddr2string(ipck.d[beg+17])+' to '+ipAddr2string(ipck.d[beg+1]);
        end;
      128:begin; {echo request}
        a:='echo request';
        move(ipck.d[beg+1],ipck.d,siz);
        SendIcmpErrorPacket(ipck,$8100,0,readLongMSB(icmp.res),siz,adr,ip.srcAdr,ip.srcAdr,'echo reply',false);
        end;
      134:begin; {router advertisement}
        a:='router advertisement';
        dec(siz,8);
        inc(beg,8);
        addCache(IfacesDat[num].macSrcT,ip.srcAdr);
        end;
      133:begin; {router solicitation}
        a:='router solicitation';
        addCache(IfacesDat[num].macSrcT,ip.srcAdr);
        if not RoutePacks then goto icm;
        o:=0;
        move(o,ipck.d[1],sizeof(o));
        move(o,ipck.d[5],sizeof(o));
        o:=8;
        addLinkAddress(ipck,o,num,IfacesDat[num].macTrgT);
        addPrefixLength(ipck,o,num);
        SendIcmpErrorPacket(ipck,$8600,num,$ffc0ffff,o,adr,ip.srcAdr,ip.srcAdr,'router advertisement',false);
        end;
      141:begin; {inverse neighbor solicitation}
        a:='inverse neighbor solicitation';
        end;
      142:begin; {inverse neighbor advertisement}
        a:='inverse neighbor advertisement';
        end;
      129:begin; {echo reply}
        a:='echo reply';
        end;
      1:begin; {destination unreachable}
        case icmp.cod of
          0:a:='no route to destination';
          1:a:='administratively prohibited';
          3:a:='address unreachable';
          4:a:='port unreachable';
          end;
        end;
      2:begin; {packet too big}
        a:='packet too big (mtu='+BStr(ReadLongMSB(icmp.Res))+')';
        end;
      3:begin; {time exceeded}
        case icmp.cod of
          0:a:='hop limit exceeded in transit';
          1:a:='fragment reassembly time exceeded';
          end;
        end;
      4:begin; {parameter problem}
        case icmp.cod of
          0:a:='erroneous header field (ptr='+BStr(ReadLongMSB(icmp.Res))+')';
          1:a:='unrecognized next header (ptr='+BStr(ReadLongMSB(icmp.Res))+')';
          2:a:='unrecognized option (ptr='+BStr(ReadLongMSB(icmp.Res))+')';
          end;
        end;
      end;
    icm:
    WriteLn('ICMP got '+a+' from '+ipAddr2string(ip.srcAdr));
    goto f1;
    end;
  move(ipck.d[beg+1],ext,sizeof(ext));
  case ip.NxtHdr of
    0:begin; {hop-by-hop}
      goto nhd;
      end;
    43:Begin; {routing}
      if (ipck.d[beg+4]<1) then goto nhd;
      if not RoutePacks then goto f1;
      if (ipck.d[beg+3]<>0) then begin;
        WriteLn('IP got unknown routing header from '+ipAddr2string(ip.srcAdr));
        goto f1;
        end;
      o:=ipck.d[beg+4]-(ipck.d[beg+2] shr 1);
      if (o<0) then begin;
        WriteLn('IP got buggy routing header from '+ipAddr2string(ip.srcAdr));
        goto f1;
        end;
      dec(ipck.d[beg+4]);
      o:=(o+1)*8;
      move(ipck.d[beg+o],adr2,sizeof(adr2));
      move(ip.TrgAdr,ipck.d[beg+o],sizeof(adr2));
      move(ipck.d,ip,sizeof(ip));
      ip.TrgAdr:=adr2;
      move(ip,ipck.d,sizeof(ip));
      goto rr;
      end;
    44:begin; {fragment}
      WriteLn('IP got fragment header from '+ipAddr2string(ip.srcAdr));
      goto f1;
      end;
    59:begin; {nothing}
      goto f1;
      end;
    60:begin; {destination options}
      WriteLn('IP got destination options from '+ipAddr2string(ip.srcAdr));
      goto nhd;
      end;
    else begin;
      goto upp;
      SendIcmpErrorPacket(ipck,$0401,0,beg,osz,LocalAddr,ip.srcAdr,ip.srcAdr,'unrecognized next header',true);
      goto f1;
      end;
    end;
  nhd:
  i:=(ext.extSize+1)*8;
  if (i>siz) then begin;
    SendIcmpErrorPacket(ipck,$0400,0,beg,osz,LocalAddr,ip.srcAdr,ip.srcAdr,'erroneous header field',true);
    goto f1;
    end;
  inc(beg,i);
  dec(siz,i);
  ip.NxtHdr:=ext.nextTyp;
  goto lh;
  end;
if (i=-1) then begin;
  if not RouteBcast then goto lh;
  if ((ReadWordMSB(adr) shr 8) and $f<=BroadLimit) then goto lh;
  o:=siz+beg+20;
  ipck.a:=UndefAddr;
  for i:=1 to IfacesNum do if (i<>num) then pipeLineSend(IfacesDat[i].pipe,ipck,o);
  goto lh;
  end;
if (i=-2) then begin;
  SendIcmpErrorPacket(ipck,$0100,0,0,osz,LocalAddr,ip.srcAdr,ip.srcAdr,'host unreachable',true);
  goto f1;
  end;
if (i=-3) then goto f1;

WriteLn('IP encountered internal error!');
End;

Procedure SendAdvertisements(num:LongInt);
Var
  ipck:OneInterfacePacketRec;
  i,o:LongInt;
Begin;
o:=0;
move(o,ipck.d[1],sizeof(o));
move(o,ipck.d[5],sizeof(o));
o:=8;
addLinkAddress(ipck,o,num,IfacesDat[num].macTrgT);
addPrefixLength(ipck,o,num);
SendIcmpErrorPacket(ipck,$8600,num,$ffc0ffff,o,IfacesDat[num].LinkiIP,AllNodAdr,UndefAddr,'router advertisement',false);
End;

Procedure ProcessPrtcol(var ppck:OneProtocolPacketRec;siz:LongInt);
Label f1;
Var
  ipck:OneInterfacePacketRec;
  i,o:LongInt;
Begin;
FindOneRoute(LocalAddr,ppck.a,o,ipck.a);
if (o>0) then begin;
  move(ppck.d,ipck.d,siz);
  SendInternetPacket(ipck,ppck.p,siz,o,ppck.b,ppck.a,ipck.a);
  exit;
  end;
if (o=0) then begin;
  lastSent:=PrtclProc;
  pipeLineSend(PrtclPipe,ppck,siz+33);
  exit;
  end;
if (o=-1) then begin;
  WriteLn('upper wants to broadcast!');
  exit;
  end;
if (o=-2) then begin;
  WriteLn('IP has no route to '+ipAddr2string(ppck.a)+'!');
  exit;
  end;
if (o=-3) then begin;
  exit;
  end;

WriteLn('IP encountered internal error!');
End;
