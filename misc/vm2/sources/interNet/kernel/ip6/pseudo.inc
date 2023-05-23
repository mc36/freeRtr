Type
  OnePseudoIPheaderRecord=record
    sou:OneInternetAddress;
    tar:OneInternetAddress;
    zro:Byte;
    prt:Byte;
    len:Word;
    sum:Word;
    end;

Function TestPseudoSum(var src,trg:OneInternetAddress;var buf;prt,siz:Word):Word;
Var ph:OnePseudoIPheaderRecord;
Begin;
ph.sou:=src;
ph.tar:=trg;
ph.zro:=0;
ph.prt:=prt;
WriteWordMSB(ph.len,siz);
ph.sum:=CalculateSum(buf,siz);
TestPseudoSum:=CalculateSum(ph,SizeOf(ph));
End;
