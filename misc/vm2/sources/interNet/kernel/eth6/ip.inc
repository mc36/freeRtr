Type
  OneIPheaderRecord=record
    VerTCFL:longword;           {version & traffiClass & flowLabel}
    PayLen:word;                {payload length}
    NxtHdr:byte;                {next header}
    LivTim:byte;                {time to live}
    SrcAdr:OneInterAddrRec;     {source address}
    TrgAdr:OneInterAddrRec;     {target address}
    end;
Type
  OneUDPheaderRecord=record
    Src:Word;                   {source port}
    Trg:Word;                   {traget port}
    Len:Word;                   {length}
    Sum:Word;                   {checksum}
    end;
Type
  OnePseudoIPheaderRecord=record
    sou:OneInterAddrRec;
    tar:OneInterAddrRec;
    zro:Byte;
    prt:Byte;
    len:Word;
    sum:Word;
    end;

Function TestPseudoSum(a,b:OneInterAddrRec;p,siz:LongInt;var dat):Word;
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

Procedure sendUDPpacket(adr:OneEtherAddrRec;as,at:OneInterAddrRec;ps,pt,siz:LongInt;var buffer);
Var
  buf:array[1..1] of byte absolute buffer;
  ip:OneIPheaderRecord;
  udp:OneUDPheaderRecord;
Begin;
move(buf,buf[sizeof(ip)+sizeof(udp)+9],siz);
move(adr,buf,sizeof(adr));
writeWordMSB(buf[7],$86dd);
WriteLongMSB(ip.VerTCFL,6 shl 28);
WriteWordMSB(ip.PayLen,sizeof(udp)+siz);
ip.NxtHdr:=$11;
ip.LivTim:=$ff;
ip.SrcAdr:=as;
ip.TrgAdr:=at;
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

Procedure receiveUDPpacket(var adr:OneEtherAddrRec;var as,at:OneInterAddrRec;var ps,pt,siz:LongInt;var buffer);
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
if (readWordMSB(buf[7])<>$86dd) then goto f1;
move(buf,adr,sizeof(adr));
move(buf[9],ip,sizeof(ip));
if (ReadLongMSB(ip.VerTCFL) shr 28<>6) then goto f1;
if (ip.NxtHdr<>$11) then goto f1;
o:=ReadWordMSB(ip.PayLen);
if (o+sizeof(ip)>i-8) then goto f1;
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
