Const
  TCPflag_FIN=$01;
  TCPflag_SYN=$02;
  TCPflag_RST=$04;
  TCPflag_PSH=$08;
  TCPflag_ACK=$10;
  TCPflag_URG=$20;
  TCPflag_ECE=$40;
  TCPflag_CWR=$80;
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


Function TCPsendPacket(adr:OneAddressRecord;flg,loc,rem:Word;seq,ack:LongInt;var datB,optB;datS,optS:word):Boolean;
Var
  tcp:OneTCPheaderRecord;
  pck:OnePacketRecord;
Begin;
WriteWordMSB(tcp.PrtSrc,loc);
WriteWordMSB(tcp.PrtTrg,rem);
WriteLongMSB(tcp.PckSeq,seq);
WriteLongMSB(tcp.PckAck,ack);
tcp.DatOfs:=((optS+20) and $ffc) shl 2;
tcp.Flags:=flg;
tcp.Window:=$1010;
tcp.ChkSum:=0;
tcp.Urgent:=0;
pck.a:=adr;
pck.b:=LocalAddress;
pck.p:=TCPprotocolNum;
pck.s:=optS+datS+sizeof(tcp);
move(optB,pck.d[21],optS);
move(datB,pck.d[optS+21],datS);
move(tcp,pck.d,sizeof(tcp));
tcp.ChkSum:=not TestPseudoSum(pck);
move(tcp,pck.d,sizeof(tcp));
TCPsendPacket:=SendOneLowerPacket(pck);
End;

Procedure TCPgotOnePacket(var pck:OnePacketRecord);
Var
  prtLoc,prtRem:Word;
  posSeq,posAck:LongInt;
  cmd:OneCommandRecord;
  con:OneConnectionStateRecord;
  conN:LongInt;
  tcp:OneTCPheaderRecord;
  beg,siz:LongInt;
  flg:Byte;

Procedure readOptions;
Label f1,f2;
Const
  MaxSegMin=$10;
  MaxSegMax=$500;
Var
  i,o:LongInt;
Begin;
if (beg=sizeof(tcp)) then exit;
o:=sizeof(tcp);
f1:
if (o>=beg) then exit;
inc(o);
case pck.d[o] of
  0:exit;{eol}
  1:goto f1;{nop}
  2:begin;{mss}
    if (pck.d[o+1]<>4) then goto f2;
    i:=ReadWordMSB(pck.d[o+2]);
    inc(o,3);
    if (i<MaxSegMin) then i:=MaxSegMin;
    if (i>MaxSegMax) then i:=MaxSegMax;
    con.maxFrame:=i;
    {$ifdef debug}WriteLn('TCP got MSS='+BStr(i)+' from '+ipAddr2string(pck.a));{$endif}
    goto f1;
    end;
  3:begin;{ws}
    if (pck.d[o+1]<>3) then goto f2;
    inc(o,2);
    goto f1;
    end;
  4:begin;{sack-permit}
    if (pck.d[o+1]<>2) then goto f2;
    inc(o,1);
    goto f1;
    end;
  5:begin;{sack-data}
    inc(o,pck.d[o+1]-1);
    goto f1;
    end;
  6:begin;{echo-request}
    if (pck.d[o+1]<>6) then goto f2;
    inc(o,5);
    goto f1;
    end;
  7:begin;{echo-reply}
    if (pck.d[o+1]<>6) then goto f2;
    inc(o,5);
    goto f1;
    end;
  8:begin;{timestamp}
    if (pck.d[o+1]<>10) then goto f2;
    inc(o,9);
    goto f1;
    end;
  9:begin;{pocp}
    if (pck.d[o+1]<>2) then goto f2;
    inc(o,1);
    goto f1;
    end;
  10:begin;{posp}
    if (pck.d[o+1]<>3) then goto f2;
    inc(o,2);
    goto f1;
    end;
  11:begin;{cc}
    if (pck.d[o+1]<>6) then goto f2;
    inc(o,5);
    goto f1;
    end;
  12:begin;{cc.new}
    if (pck.d[o+1]<>6) then goto f2;
    inc(o,5);
    goto f1;
    end;
  13:begin;{cc.echo}
    if (pck.d[o+1]<>6) then goto f2;
    inc(o,5);
    goto f1;
    end;
  14:begin;{acr}
    if (pck.d[o+1]<>3) then goto f2;
    inc(o,2);
    goto f1;
    end;
  15:begin;{acd}
    inc(o,pck.d[o+1]-1);
    goto f1;
    end;
  19:begin;{md5sum}
    if (pck.d[o+1]<>18) then goto f2;
    inc(o,17);
    goto f1;
    end;
  end;
f2:
WriteLn('TCP got invalid option from '+ipAddr2string(pck.a));
exit;
End;


Label f1;
Var
  i,o:LongInt;
  b:Boolean;
Begin;
if (pck.s<sizeof(tcp)) then begin;
  WriteLn('TCP got a too short packet from '+ipAddr2string(pck.a));
  exit;
  end;
if (TestPseudoSum(pck)<>$ffff) then begin;
  WriteLn('TCP got wrong checksum from '+ipAddr2string(pck.a));
  exit;
  end;
move(pck.d,tcp,sizeof(tcp));
beg:=(tcp.DatOfs shr 4)*4;
siz:=pck.s-beg;
if (beg<sizeof(tcp)) or (siz<0) then begin;
  WriteLn('TCP got wrong data offset from '+ipAddr2string(pck.a));
  exit;
  end;
flg:=tcp.Flags and (TCPflag_FIN+TCPflag_SYN+TCPflag_RST+TCPflag_ACK);
prtLoc:=ReadWordMSB(tcp.PrtTrg);
prtRem:=ReadWordMSB(tcp.PrtSrc);
posSeq:=ReadLongMSB(tcp.PckSeq);
posAck:=ReadLongMSB(tcp.PckAck);
if (flg=TCPflag_ACK) then begin;
  conN:=FindOneConnection(1,pck.a,prtLoc,prtRem);
  if (conN=0) then begin;
    WriteLn('TCP got unwanted ACK from '+ipAddr2string(pck.a));
    TCPsendPacket(pck.a,TCPflag_RST+TCPflag_ACK,prtLoc,prtRem,posAck,posSeq,i,i,0,0);
    exit;
    end;
  ConnectReadH(conN,con);
  f1:
  o:=con.sequRem-posSeq;
  if (o<-TransmitBufMax) or (o>TransmitBufMax) then begin;
    WriteLn('TCP got bad positioned DATA from '+ipAddr2string(pck.a));
    TCPsendPacket(pck.a,TCPflag_RST+TCPflag_ACK,prtLoc,prtRem,posAck,posSeq,i,i,0,0);
    exit;
    end;
  i:=posAck-con.sequLoc;
  if (i<-TransmitBufMax) or (i>con.bufSize) then begin;
    WriteLn('TCP got bad positioned ACK from '+ipAddr2string(pck.a));
    TCPsendPacket(pck.a,TCPflag_RST+TCPflag_ACK,prtLoc,prtRem,posAck,posSeq,i,i,0,0);
    exit;
    end;
  {$ifdef debug}WriteLn('TCP got ACK from '+ipAddr2string(pck.a)+'; ack='+BStr(i)+'; seq='+BStr(o)+'; siz='+BStr(siz));{$endif}
  readOptions;
  if (con.stat=5) then begin;
    con.stat:=2;
    cmd.cmd:=3;
    cmd.adr:=con.remAddr;
    cmd.prt:=con.portRem;
    cmd.dat:=con.portLoc;
    pipeLineSend(con.pipeline,cmd,sizeof(cmd));
    {$ifdef debug}WriteLn('pipeline now connected with '+ipAddr2string(pck.a));{$endif}
    end;
  if (con.stat=2) then begin;
    con.stat:=3;
    con.txBegan:=-1;
    con.lastTx:=CurrentTime;
    con.lastRx:=CurrentTime;
    con.retries:=retry_TCP_DAT;
    con.timeOut:=timot_TCP_DAT;
    end;
  if (con.stat<>3) then begin;
    WriteLn('TCP got unwanted ACK from '+ipAddr2string(pck.a));
    exit;
    end;
  b:=false;
  if (o<0) then begin;
    {$ifdef debug}WriteLn('TCP got '+BStr(-o)+' bytes future DATA from '+ipAddr2string(pck.a));{$endif}
    end else if (siz>o) then begin;
    if (pipeLineSend(con.pipeline,pck.d[beg+o+1],siz-o)<>0) then begin;
      {$ifdef debug}WriteLn('TCP got '+BStr(siz-o)+' bytes DATA from '+ipAddr2string(pck.a)+', but dropped it!');{$endif}
      exit;
      end;
    lastSent:=con.process;
    inc(con.sequRem,siz-o);
    b:=true;
    {$ifdef debug}WriteLn('TCP got '+BStr(siz-o)+' bytes DATA from '+ipAddr2string(pck.a));{$endif}
    end;
  if (i>0) then begin;
    dec(con.bufSize,i);
    dec(con.bufSent,i);
    inc(con.sequLoc,i);
    ConnectReadD(conN,con);
    move(con.buf[i+1],con.buf,con.bufSize);
    ConnectWriteD(conN,con);
    if (con.txBegan>-1) then begin;
      o:=1+GetTimePast(con.txBegan);
      if (o>timot_TCP_DAT) then o:=timot_TCP_DAT;
      con.timeOut:=o;
      con.txBegan:=-1;
      end;
    o:=con.MaxOutgo+512;
    if (o<TransmitBufMax) then con.MaxOutgo:=o;
    {$ifdef debug}WriteLn('TCP got '+BStr(i)+' bytes ACK from '+ipAddr2string(pck.a));{$endif}
    end;
  if b then begin;
    TCPsendPacket(con.remAddr,TCPflag_ACK,con.portLoc,con.portRem,con.sequLoc+con.BufSent,con.sequRem,i,i,0,0);
    con.lastTx:=CurrentTime;
    {$ifdef debug}WriteLn('TCP sends ACK to '+ipAddr2string(pck.a));{$endif}
    end;
  con.lastRx:=CurrentTime;
  con.retries:=retry_TCP_DAT;
  ConnectWriteH(conN,con);
  exit;
  end;
if (flg=0) then begin;
  conN:=FindOneConnection(1,pck.a,prtLoc,prtRem);
  if (conN=0) then begin;
    WriteLn('TCP got unwanted DATA from '+ipAddr2string(pck.a));
    TCPsendPacket(pck.a,TCPflag_RST+TCPflag_ACK,prtLoc,prtRem,posAck,posSeq,i,i,0,0);
    exit;
    end;
  ConnectReadH(conN,con);
  posAck:=con.sequLoc;
  goto f1;
  end;
if (flg and TCPflag_RST<>0) then begin;
  conN:=FindOneConnection(1,pck.a,prtLoc,prtRem);
  if (conN=0) then begin;
    WriteLn('TCP got unknown RST from '+ipAddr2string(pck.a));
    exit;
    end;
  ConnectDelete(conN);
  {$ifdef debug}WriteLn('TCP got RST from '+ipAddr2string(pck.a));{$endif}
  exit;
  end;
if (flg and TCPflag_FIN<>0) then begin;
  conN:=FindOneConnection(1,pck.a,prtLoc,prtRem);
  if (conN=0) then begin;
    WriteLn('TCP got unwanted FIN+ACK from '+ipAddr2string(pck.a));
    TCPsendPacket(pck.a,TCPflag_RST+TCPflag_ACK,prtLoc,prtRem,posAck,posSeq+1,i,i,0,0);
    exit;
    end;
  ConnectReadH(conN,con);
  if (con.stat=4) then begin;
    TCPsendPacket(pck.a,TCPflag_ACK,prtLoc,prtRem,posAck,posSeq+1,i,i,0,0);
    ConnectDelete(conN);
    {$ifdef debug}WriteLn('TCP got FIN+ACK from '+ipAddr2string(pck.a));{$endif}
    exit;
    end;
  if (con.stat<>3) then begin;
    WriteLn('TCP got unwanted FIN+ACK from '+ipAddr2string(pck.a));
    exit;
    end;
  if (siz>0) then begin;
    {$ifdef debug}WriteLn('TCP got FIN+ACK from '+ipAddr2string(pck.a)+', with data!');{$endif}
    goto f1;
    end;
  TCPsendPacket(pck.a,TCPflag_ACK+TCPflag_FIN,prtLoc,prtRem,posAck,posSeq+1,i,i,0,0);
  ConnectDelete(conN);
  {$ifdef debug}WriteLn('TCP got FIN+ACK from '+ipAddr2string(pck.a));{$endif}
  exit;
  end;
if (flg=TCPflag_SYN+TCPflag_ACK) then begin;
  conN:=FindOneConnection(1,pck.a,prtLoc,prtRem);
  if (conN=0) then begin;
    WriteLn('TCP got unwanted SYN+ACK from '+ipAddr2string(pck.a));
    TCPsendPacket(pck.a,TCPflag_RST+TCPflag_ACK,prtLoc,prtRem,posAck,posSeq+1,i,i,0,0);
    exit;
    end;
  ConnectReadH(conN,con);
  if (con.sequLoc<>posAck) then begin;
    WriteLn('TCP got bad positioned SYN+ACK from '+ipAddr2string(pck.a));
    exit;
    end;
  if (con.stat=2) and (con.sequRem=posSeq+1) then begin;
    {$ifdef debug}WriteLn('TCP got SYN+ACK again from '+ipAddr2string(pck.a));{$endif}
    con.lastTx:=-99999;
    ConnectWriteH(conN,con);
    exit;
    end;
  if (con.stat<>1) then begin;
    WriteLn('TCP got unwanted SYN+ACK from '+ipAddr2string(pck.a));
    exit;
    end;
  readOptions;
  con.sequRem:=posSeq+1;
  con.stat:=2;
  con.retries:=retry_TCP_SYN*2;
  con.timeOut:=timot_TCP_SYN;
  con.lastTx:=-99999;
  ConnectWriteH(conN,con);
  {$ifdef debug}WriteLn('TCP got SYN+ACK from '+ipAddr2string(pck.a));{$endif}
  cmd.cmd:=3;
  cmd.adr:=LocalAddress;
  cmd.prt:=con.portLoc;
  cmd.dat:=0;
  pipeLineSend(con.pipeline,cmd,sizeof(cmd));
  {$ifdef debug}WriteLn('pipeline now connected with '+ipAddr2string(pck.a));{$endif}
  exit;
  end;
if (flg=TCPflag_SYN) then begin;
  conN:=FindOneConnection(1,pck.a,prtLoc,prtRem);
  if (conN<>0) then begin;
    ConnectReadH(conN,con);
    con.lastTx:=-99999;
    ConnectWriteH(conN,con);
    {$ifdef debug}WriteLn('TCP got SYN again from '+ipAddr2string(pck.a));{$endif}
    exit;
    end;
  conN:=FindOneListening($81,prtLoc);
  if (conN=0) then begin;
    WriteLn('TCP got SYN from '+ipAddr2string(pck.a)+' for local port '+BStr(prtLoc));
    TCPsendPacket(pck.a,TCPflag_RST+TCPflag_ACK,prtLoc,prtRem,random($ffffffff),posSeq+1,i,i,0,0);
    exit;
    end;
  ConnectReadH(conN,con);
  if (pipeLineCreate(o,con.process,con.sequLoc,false)<>0) then begin;
    WriteLn('TCP got SYN from '+ipAddr2string(pck.a)+' but failed to open new pipeline!');
    TCPsendPacket(pck.a,TCPflag_RST+TCPflag_ACK,prtLoc,prtRem,random($ffffffff),posSeq+1,i,i,0,0);
    exit;
    end;
  conN:=ConnectionNum+1;
  if ResizeConnectionMemory(conN) then begin;
    pipeLineClose(o);
    WriteLn('TCP got SYN from '+ipAddr2string(pck.a)+' but failed to allocate memory!');
    TCPsendPacket(pck.a,TCPflag_RST+TCPflag_ACK,prtLoc,prtRem,random($ffffffff),posSeq+1,i,i,0,0);
    exit;
    end;
  makeNewConnectRec($01,pck.a,prtLoc,prtRem,con);
  readOptions;
  con.pipeline:=o;
  con.portLoc:=prtLoc;
  con.sequRem:=posSeq+1;
  con.stat:=5;
  con.retries:=retry_TCP_SYN;
  con.timeOut:=timot_TCP_SYN;
  con.lastTx:=-99999;
  ConnectWriteH(conN,con);
  {$ifdef debug}WriteLn('TCP got SYN from '+ipAddr2string(pck.a));{$endif}
  exit;
  end;

WriteLn('TCP got unknown packet from '+ipAddr2string(pck.a));
End;
