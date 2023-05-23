Type
  OnePseudoIPheaderRecord=record
    sou:OneAddressRecord;
    tar:OneAddressRecord;
    zro:Byte;
    prt:Byte;
    len:Word;
    sum:Word;
    end;

Function TestPseudoSum(Var ip:OnePacketRecord):Word;
Var ph:OnePseudoIPheaderRecord;
Begin;
ph.sou:=ip.a;
ph.tar:=ip.b;
ph.zro:=0;
ph.prt:=ip.p;
WriteWordMSB(ph.len,ip.s);
ph.sum:=CalculateSum(ip.d,ip.s);
TestPseudoSum:=CalculateSum(ph,SizeOf(ph));
End;
