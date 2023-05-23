Type
  OneTCPheaderRecord=record
    PrtSrc:Word;                {source port}
    PrtTrg:Word;                {target port}
    PckSeq:LongInt;             {data sequence}
    PckAck:LongInt;             {ack sequence}
    DatOfs:Byte;                {upper 4 bits}
    Flags:Byte;                 {lower 6 bits}
    Window:Word;                {window size}
    ChkSum:Word;                {checksum}
    Urgent:Word;                {urgent pointer}
    end;
  OneUDPheaderRecord=record
    Src:Word;                   {source port}
    Trg:Word;                   {traget port}
    Len:Word;                   {length}
    Sum:Word;                   {checksum}
    end;




Procedure gotOnePacket(var pck:OnePacketRecord;proto:LongInt);
Label f1,f2;
Var
  tcp:OneTCPheaderRecord;
  udp:OneUDPheaderRecord;
  rul:OneRuleRecord;
  prtSrc,prtTrg,prtPro:LongInt;
  i,o,p:LongInt;

Function goodRul(var d:OneRuleRecord):Boolean;
Begin;
goodRul:=False;
if (d.protocol and prtPro=0) then exit;
if (d.srcPrtF>prtSrc) then exit;
if (d.srcPrtL<prtSrc) then exit;
if (d.trgPrtF>prtTrg) then exit;
if (d.trgPrtL<prtTrg) then exit;
if compareAddrMsk(pck.a,d.srcAddr,d.srcMask) then exit;
if compareAddrMsk(pck.b,d.trgAddr,d.trgMask) then exit;
p:=d.action;
goodRul:=True;
End;

Begin;
{$ifdef debug}WriteLn('got '+ipAddr2string(pck.a)+' --> '+ipAddr2string(pck.b));{$endif}
prtPro:=0;
case proto of
  1:begin; {tcp}
    if (pck.s<sizeof(tcp)) then begin;
      WriteLn('TCP got a too short packet from '+ipAddr2string(pck.a));
      exit;
      end;
    if (TestPseudoSum(pck)<>$ffff) then begin;
      WriteLn('TCP got wrong checksum from '+ipAddr2string(pck.a));
      exit;
      end;
    move(pck.d,tcp,sizeof(tcp));
    o:=(tcp.DatOfs shr 4)*4;
    p:=pck.s-o;
    if (o<sizeof(tcp)) or (p<0) then begin;
      WriteLn('TCP got wrong data offset from '+ipAddr2string(pck.a));
      exit;
      end;
    move(pck.d[o+1],pck.d[sizeof(tcp)+1],p);
    tcp.Flags:=tcp.Flags and $3f;
    o:=sizeof(tcp);
    if (tcp.Flags and $20<>0) then begin; {urg}
      if (ReadWordMSB(tcp.Urgent)>=p) then begin;
        tcp.Flags:=tcp.Flags and $1f;
        tcp.Urgent:=0;
        end;
      end else tcp.Urgent:=0;
    if (tcp.Flags and $02<>0) then begin; {syn}
      move(pck.d[o+1],pck.d[o+5],p);
      pck.d[o+1]:=2;
      pck.d[o+2]:=4;
      WriteWordMSB(pck.d[o+3],1024);
      inc(o,4);
      prtPro:=2;
      end else prtPro:=4;
    tcp.DatOfs:=(o shr 2) shl 4;
    tcp.window:=$6060;
    pck.s:=o+p;
    prtTrg:=ReadWordMSB(tcp.prtTrg);
    prtSrc:=ReadWordMSB(tcp.prtSrc);
    end;
  2:begin; {udp}
    if (pck.s<sizeof(udp)) then begin;
      WriteLn('UDP got a too short packet from '+ipAddr2string(pck.a));
      exit;
      end;
    move(pck.d,udp,sizeof(udp));
    p:=ReadWordMSB(udp.len);
    if (p<sizeof(udp)) then begin;
      WriteLn('UDP got a too short packet from '+ipAddr2string(pck.a));
      exit;
      end;
    pck.s:=p;
    if (udp.sum=0) then begin;
      end else if (TestPseudoSum(pck)<>$ffff) then begin;
      WriteLn('UDP got wrong checksum from '+ipAddr2string(pck.a));
      exit;
      end;
    prtTrg:=ReadWordMSB(udp.trg);
    prtSrc:=ReadWordMSB(udp.src);
    prtPro:=1;
    end;
  else exit;
  end;

for i:=1 to RulesNumb do if goodRul(RulesData^[i]) then goto f1;
{$ifdef debug}WriteLn('no matching rule, discarding packet...');{$endif}
exit;

f1:
if (p=1) then goto f2;
if (p=0) then begin;
  {$ifdef debug}WriteLn('rule #'+BStr(i)+' firbid this, discarding packet...');{$endif}
  exit;
  end;
rul:=RulesData^[i];
if (p and $02<>0) then pck.a:=rul.nSrcAdr; {srcAddr}
if (p and $04<>0) then pck.b:=rul.nTrgAdr; {trgAddr}
if (p and $08<>0) then prtSrc:=rul.nSrcPrt; {srcPort}
if (p and $10<>0) then prtTrg:=rul.nTrgPrt; {trgPort}
f2:

case proto of
  1:begin; {tcp}
    WriteWordMSB(tcp.prtSrc,prtSrc);
    WriteWordMSB(tcp.prtTrg,prtTrg);
    tcp.ChkSum:=0;
    move(tcp,pck.d,sizeof(tcp));
    tcp.ChkSum:=not TestPseudoSum(pck);
    move(tcp,pck.d,sizeof(tcp));
    end;
  2:begin; {udp}
    WriteWordMSB(udp.src,prtSrc);
    WriteWordMSB(udp.trg,prtTrg);
    udp.sum:=0;
    move(udp,pck.d,sizeof(udp));
    udp.sum:=not TestPseudoSum(pck);
    move(udp,pck.d,sizeof(udp));
    end;
  else exit;
  end;

{$ifdef debug}WriteLn('sending '+ipAddr2string(pck.a)+' --> '+ipAddr2string(pck.b));{$endif}
if compareAddress(pck.b,localAddr) then begin;
  exchangeAddr(pck.a,pck.b);
  SendOneLowerPacket(pck);
  exit;
  end;
pipeLineSend(PrtclPipe,pck,pck.s+33);
lastSent:=PrtclProc;
End;



Procedure ProcessUpperStream(var pck:OnePacketRecord);
Label f1;
Var i,o:LongInt;
Begin;
if (PrtclPipe=0) then exit;
f1:
i:=sizeof(pck);
if (pipeLineRecv(PrtclPipe,pck,i)<>0) then i:=0;
if (i<1) then begin;
  if (pipeLineStats(PrtclPipe,o,i,i)<>0) then o:=0;
  if (o<>0) then exit;
  pipeLineClose(PrtclPipe);
  PrtclPipe:=0;
  exit;
  end;
pck.s:=i-33;
exchangeAddr(pck.a,pck.b);
if (pck.p=TCPprotocolNum) then gotOnePacket(pck,1) else
 if (pck.p=UDPprotocolNum) then gotOnePacket(pck,2) else
 WriteLn('got invalid protocol upper');
goto f1;
End;
