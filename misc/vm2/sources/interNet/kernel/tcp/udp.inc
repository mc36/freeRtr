Type
  OneUDPheaderRecord=record
    Src:Word;                   {source port}
    Trg:Word;                   {traget port}
    Len:Word;                   {length}
    Sum:Word;                   {checksum}
    end;

Procedure UDPsendPacket(adr:OneAddressRecord;loc,rem:Word;var datB;datS:word);
Var
  udp:OneUDPheaderRecord;
  pck:OnePacketRecord;
Begin;
WriteWordMSB(udp.src,loc);
WriteWordMSB(udp.trg,rem);
WriteWordMSB(udp.len,sizeof(udp)+datS);
udp.sum:=0;
pck.a:=adr;
pck.b:=LocalAddress;
pck.p:=UDPprotocolNum;
pck.s:=datS+sizeof(udp);
move(datB,pck.d[9],datS);
move(udp,pck.d,sizeof(udp));
udp.sum:=not TestPseudoSum(pck);
move(udp,pck.d,sizeof(udp));
SendOneLowerPacket(pck);
{$ifdef debug}WriteLn('UDP sends packet to '+ipAddr2string(adr));{$endif}
End;


Procedure UDPgotOnePacket(var pck:OnePacketRecord);
Var
  prtLoc,prtRem:Word;
  cmd:OneCommandRecord;
  con:OneConnectionStateRecord;
  conN:LongInt;
  udp:OneUDPheaderRecord;
  siz:LongInt;
Begin;
if (pck.s<sizeof(udp)) then begin;
  WriteLn('UDP got a too short packet from '+ipAddr2string(pck.a));
  exit;
  end;
move(pck.d,udp,sizeof(udp));
siz:=ReadWordMSB(udp.len);
if (siz<sizeof(udp)) then begin;
  WriteLn('UDP got a too short packet from '+ipAddr2string(pck.a));
  exit;
  end;
pck.s:=siz;
if (udp.sum=0) then begin;
  {$ifdef debug}WriteLn('UDP got without checksum from '+ipAddr2string(pck.a));{$endif}
  end else if (TestPseudoSum(pck)<>$ffff) then begin;
  WriteLn('UDP got wrong checksum from '+ipAddr2string(pck.a));
  exit;
  end;
dec(siz,sizeof(udp));
prtLoc:=ReadWordMSB(udp.trg);
prtRem:=ReadWordMSB(udp.src);
conN:=FindOneListening($82,prtLoc);
if (conN=0) then begin;
  WriteLn('UDP got packet from '+ipAddr2string(pck.a)+' for local port '+BStr(prtLoc));
  exit;
  end;
ConnectReadH(conN,con);
cmd.cmd:=5;
cmd.adr:=pck.a;
cmd.prt:=prtRem;
move(cmd,pck.d[9-sizeof(cmd)],sizeof(cmd));
pipeLineSend(con.pipeline,pck.d[9-sizeof(cmd)],sizeof(cmd)+siz);
{$ifdef debug}WriteLn('UDP got packet from '+ipAddr2string(pck.a));{$endif}
End;


Procedure protoGotOnePacket(var pck:OnePacketRecord);
Var
  cmd:OneCommandRecord;
  i,o:LongInt;
Begin;
o:=ProtocolPipes[pck.p];
if (o=0) then begin;
  WriteLn('got unknown protocol ($'+byte2hextype(pck.p)+') from '+ipAddr2string(pck.a));
  exit;
  end;
i:=pck.s;
fillchar(cmd,sizeof(cmd),0);
cmd.adr:=pck.a;
cmd.prt:=pck.p;
cmd.cmd:=5;
move(pck.d,pck.d[sizeof(cmd)+1],i);
move(cmd,pck.d,sizeof(cmd));
pipeLineSend(o,pck.d,sizeof(cmd)+i);
{$ifdef debug}WriteLn('proto #'+BStr(pck.p)+' got packet from '+ipAddr2string(pck.a));{$endif}
End;
