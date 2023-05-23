Const
  UDPport_BOOTPs=67;
  UDPport_BOOTPc=68;
Const
  Op_BOOTP_req=1;
  Op_BOOTP_rep=2;
  dhcpMagicCookie=$63825363;
Const
  Op_DHCP_discover=1;
  Op_DHCP_offer=2;
  Op_DHCP_request=3;
  Op_DHCP_decline=4;
  Op_DHCP_ack=5;
  Op_DHCP_nak=6;
  Op_DHCP_release=7;
  Op_DHCP_inform=8;
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
Type
  OneUDPheaderRecord=record
    Src:Word;                   {source port}
    Trg:Word;                   {traget port}
    Len:Word;                   {length}
    Sum:Word;                   {checksum}
    end;
Type
  OneBOOTPheaderRecord=record
    op:Byte;
    htype:Byte;
    hlen:Byte;
    hops:Byte;
    xid:LongInt;
    secs:word;
    flags:word;
    ciadr:LongInt;
    yiadr:LongInt;
    siadr:LongInt;
    giadr:LongInt;
    chadr:array[1..16] of byte;
    sname:array[1..64] of char;
    fname:array[1..128] of char;
    magic:LongInt;
    end;
Type
  OnePseudoIPheaderRecord=record
    sou:LongInt;
    tar:LongInt;
    zro:Byte;
    prt:Byte;
    len:Word;
    sum:Word;
    end;
Var
  PacketSequences:LongInt;
  leaseTimeValue:LongInt;

Function TestPseudoSum(a,b,p,siz:LongInt;var dat):Word;
Var ph:OnePseudoIPheaderRecord;
Begin;
ph.sou:=a;
ph.tar:=b;
ph.zro:=0;
ph.prt:=p;
WriteWordMSB(ph.len,siz);
ph.sum:=CalculateSum(dat,siz);
TestPseudoSum:=CalculateSum(ph,SizeOf(ph));
End;

Procedure bootpAddOption(var buffer;var p:LongInt;t:LongInt;a:String);
Var buf:array[1..1] of byte absolute buffer;
Begin;
inc(p,sizeof(OneBOOTPheaderRecord));
inc(p);
buf[p]:=t;
inc(p);
move(a,buf[p],sizeof(a));
inc(p,length(a));
dec(p,sizeof(OneBOOTPheaderRecord));
End;

Procedure sendUDPpacket(adr:OneEtherAddrRec;as,at,ps,pt,siz:LongInt;var buffer);
Var
  buf:array[1..1] of byte absolute buffer;
  ip:OneIPheaderRecord;
  udp:OneUDPheaderRecord;
Begin;
move(buf,buf[sizeof(ip)+sizeof(udp)+9],siz);
move(adr,buf,sizeof(adr));
writeWordMSB(buf[7],$800);
inc(PacketSequences);
ip.VerIHL:=$45;
ip.SerTyp:=0;
writeWordMSB(ip.TotLen,sizeof(ip)+sizeof(udp)+siz);
ip.Idntfy:=PacketSequences;
ip.FlgOfs:=0;
ip.LivTim:=$ff;
ip.Protcl:=$11;
ip.HedSum:=0;
ip.SrcAdr:=as;
ip.TrgAdr:=at;
ip.HedSum:=not CalculateSum(ip,sizeof(ip));
move(ip,buf[9],sizeof(ip));
writeWordMSB(udp.Src,ps);
writeWordMSB(udp.Trg,pt);
writeWordMSB(udp.Len,sizeof(udp)+siz);
udp.Sum:=0;
move(udp,buf[9+sizeof(ip)],sizeof(udp));
udp.Sum:=not TestPseudoSum(as,at,$11,sizeof(udp)+siz,buf[9+sizeof(ip)]);
move(udp,buf[9+sizeof(ip)],sizeof(udp));
pipeLineSend(ethPipe,buf,sizeof(ip)+sizeof(udp)+9+siz);
End;

Procedure receiveUDPpacket(var adr:OneEtherAddrRec;var as,at,ps,pt,siz:LongInt;var buffer);
Label f1;
Var
  buf:array[1..1] of byte absolute buffer;
  ip:OneIPheaderRecord;
  udp:OneUDPheaderRecord;
  i,o:LongInt;
Begin;
f1:
siz:=-1;
i:=1024;
if (pipeLineRecv(ethPipe,buf,i)<>0) then i:=0;
if (i<1) then exit;
if (readWordMSB(buf[7])<>$800) then goto f1;
move(buf,adr,sizeof(adr));
move(buf[9],ip,sizeof(ip));
if (ip.VerIHL<>$45) then goto f1;
if (ip.Protcl<>$11) then goto f1;
o:=ReadWordMSB(ip.TotLen);
if (o>i-8) then goto f1;
dec(o,sizeof(ip));
move(buf[9+sizeof(ip)],udp,sizeof(udp));
i:=ReadWordMSB(udp.Len);
if (i>o) then goto f1;
dec(i,sizeof(udp));
if (i<1) then goto f1;
as:=ip.SrcAdr;
at:=ip.TrgAdr;
ps:=readWordMSB(udp.Src);
pt:=readWordMSB(udp.Trg);
move(buf[sizeof(ip)+sizeof(udp)+9],buf,i);
siz:=i;
End;



Procedure parseBootpOptions(var buffer;var dhcptyp:LongInt;var sid:String);
Label f1,f2;
Var
  buf:array[1..1] of byte absolute buffer;
  i,o,p:LongInt;
  a:String;
Begin;
p:=sizeof(OneBOOTPheaderRecord);
f1:
if (p>=sizeof(OneBOOTPheaderRecord)+60) then exit;
inc(p);
i:=buf[p];
if (i=0) then goto f1;
if (i=255) then exit;
inc(p);
move(buf[p],a,sizeof(a));
inc(p,length(a));
case i of
  1:move(a[1],ntmskNet,sizeof(ntmskNet));
  3:move(a[1],gatwyNet,sizeof(gatwyNet));
  6:dnsServs:=copy(a,1,8);
  51:move(a[1],leaseTimeValue,sizeof(leaseTimeValue));
  53:dhcptyp:=Ord(a[1]);
  54:sid:=a;
  end;
goto f1;
End;
