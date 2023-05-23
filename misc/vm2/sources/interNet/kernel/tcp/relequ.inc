Procedure relequish2proto(var pipe:LongInt;proto:LongInt);
Var
  pck:OnePacketRecord;
  cmd:OneCommandRecord;
  i,o,p:LongInt;
Begin;
if (pipe=0) then exit;
o:=sizeof(pck);
if (pipeLineRecv(pipe,pck.d,o)<>0) then o:=0;
if (o<1) then begin;
  pipeLineStats(pipe,o,i,i);
  if (o<>0) then exit;
  pipeLineClose(pipe);
  pipe:=0;
  exit;
  end;
dec(o,sizeof(cmd));
if (o<0) then exit;
move(pck.d,cmd,sizeof(cmd));
pck.a:=cmd.adr;
pck.b:=LocalAddress;
pck.p:=proto;
pck.s:=o;
move(pck.d[sizeof(cmd)+1],pck.d,o);
SendOneLowerPacket(pck);
{$ifdef debug}WriteLn('proto#'+BStr(proto)+' sends packet to '+ipAddr2string(adr));{$endif}
End;


Procedure relequish2connection(conN:LongInt);
Label f1,f2,f3,f4;
Var
  pck:OnePacketRecord;
  con:OneConnectionStateRecord;
  cmd:OneCommandRecord;
  i,o,p:LongInt;
  b:Boolean;
Begin;
ConnectReadH(conN,con);
if (con.bufSize=0) then begin;
  pipeLineStats(con.pipeline,o,p,i);
  if (con.stat=2) and (p>0) then begin;
    con.stat:=3;
    con.txBegan:=-1;
    con.lastTx:=CurrentTime;
    con.lastRx:=CurrentTime;
    con.retries:=retry_TCP_DAT;
    con.timeOut:=timot_TCP_DAT;
    ConnectWriteH(conN,con);
    end;
  if (o=con.process) then goto f1;
  if (con.typ<>1) then begin; ConnectDelete(conN);exit; end;
  if (con.stat<>3) then begin;
    f3:
    {$ifdef debug}WriteLn('TCP sends RST to '+ipAddr2string(con.remAddr));{$endif}
    TCPsendPacket(con.remAddr,TCPflag_RST+TCPflag_ACK,con.portLoc,con.portRem,con.sequLoc,con.sequRem,i,i,0,0);
    ConnectDelete(conN);
    exit;
    end;
  if (p>0) then goto f1;
  con.stat:=4;
  con.retries:=retry_TCP_FIN;
  con.timeOut:=timot_TCP_FIN;
  con.bufSize:=1;
  con.bufSent:=1;
  con.lastTx:=-99999;
  ConnectWriteH(conN,con);
  {$ifdef debug}WriteLn('pipeline closed by user with '+ipAddr2string(con.remAddr));{$endif}
  f1:
  end;
if (con.typ=1) and (con.stat=3) then begin;
  b:=false;
  i:=TransmitBufSize-con.bufSize;
  if (i>16) then begin;
    ConnectReadD(conN,con);
    if (pipeLineRecv(con.pipeline,con.buf[con.bufSize+1],i)<>0) then i:=0;
    if (i>0) then begin;
      inc(con.bufSize,i);
      ConnectWriteD(conN,con);
      b:=true;
      end;
    end;
  if (con.bufSent<con.bufSize) and (con.bufSent<con.MaxOutgo) then begin;
    {$ifdef debug}WriteLn('TCP has data to send to '+ipAddr2string(con.remAddr));{$endif}
    b:=true;
    end;
  if (con.bufSize>0) then if (GetTimePast(con.lastTx)>=con.timeOut) then begin;
    {$ifdef debug}WriteLn('TCP encountered TX timeout with '+ipAddr2string(con.remAddr));{$endif}
    con.bufSent:=0;
    i:=con.timeOut*2;
    if (i>timot_TCP_DAT) then i:=timot_TCP_DAT;
    con.timeOut:=i;
    con.txBegan:=-1;
    con.MaxOutgo:=$200;
    b:=true;
    end;
  if (GetTimePast(con.lastRx)>timot_TCP_DAT) then begin;
    {$ifdef debug}WriteLn('TCP encountered RX timeout with '+ipAddr2string(con.remAddr));{$endif}
    dec(con.retries);
    if (con.retries<1) then goto f3;
    con.lastRx:=CurrentTime;
    con.txBegan:=-1;
    b:=true;
    end;
  if (GetTimePast(con.lastTx)>timot_TCP_DAT) then begin;
    {$ifdef debug}WriteLn('TCP encountered TX timeout with '+ipAddr2string(con.remAddr));{$endif}
    con.txBegan:=-1;
    b:=true;
    end;
  if b then begin;
    ConnectReadD(conN,con);
    if (con.bufSent=0) then con.txBegan:=CurrentTime;
    repeat
      i:=con.bufSize;
      if (i>con.MaxOutgo) then i:=con.MaxOutgo;
      dec(i,con.bufSent);
      if (i>con.maxFrame) then i:=con.maxFrame;
      if (i<0) then i:=0;
      o:=TCPflag_ACK;
      if (con.bufSize<PushTresholdBytes) then if (con.bufSent+i>=con.bufSize) then o:=TCPflag_ACK+TCPflag_PSH;
      if TCPsendPacket(con.remAddr,o,con.portLoc,con.portRem,con.sequLoc+con.BufSent,con.sequRem,con.buf[con.BufSent+1],i,i,0) then goto f4;
      inc(con.bufSent,i);
      {$ifdef debug}WriteLn('TCP sends '+BStr(i)+' bytes DATA to '+ipAddr2string(con.remAddr));{$endif}
      until (con.bufSent>=con.bufSize) or (con.bufSent>=con.MaxOutgo);
    f4:
    con.lastTx:=CurrentTime;
    end;
  ConnectWriteH(conN,con);
  exit;
  end;
case con.typ of
  $01:begin;
    if (GetTimePast(con.lastTx)<con.timeOut) then exit;
    {$ifdef debug}WriteLn('TCP encountered timeout with '+ipAddr2string(con.remAddr));{$endif}
    dec(con.retries);
    if (con.retries<1) then goto f3;
    con.lastTx:=CurrentTime;
    case con.stat of
      1:begin;
        WriteLongMSB(i,$02040404);
        TCPsendPacket(con.remAddr,TCPflag_SYN,con.portLoc,con.portRem,con.sequLoc-1,0,i,i,0,4);
        con.timeOut:=timot_TCP_SYN;
        {$ifdef debug}WriteLn('TCP sends SYN to '+ipAddr2string(con.remAddr));{$endif}
        end;
      2:begin;
        TCPsendPacket(con.remAddr,TCPflag_ACK,con.portLoc,con.portRem,con.sequLoc,con.sequRem,i,i,0,0);
        con.timeOut:=timot_TCP_SYN;
        {$ifdef debug}WriteLn('TCP sends ACK to '+ipAddr2string(con.remAddr));{$endif}
        end;
      4:begin;
        TCPsendPacket(con.remAddr,TCPflag_FIN+TCPflag_ACK,con.portLoc,con.portRem,con.sequLoc,con.sequRem,i,i,0,0);
        con.timeOut:=timot_TCP_FIN;
        {$ifdef debug}WriteLn('TCP sends FIN+ACK to '+ipAddr2string(con.remAddr));{$endif}
        end;
      5:begin;
        WriteLongMSB(i,$02040404);
        TCPsendPacket(con.remAddr,TCPflag_SYN+TCPflag_ACK,con.portLoc,con.portRem,con.sequLoc-1,con.sequRem,i,i,0,4);
        con.timeOut:=timot_TCP_SYN;
        {$ifdef debug}WriteLn('TCP sends SYN+ACK to '+ipAddr2string(con.remAddr));{$endif}
        end;
      else goto f2;
      end;
    ConnectWriteH(conN,con);
    exit;
    end;
  $82:begin;
    o:=sizeof(pck);
    if (pipeLineRecv(con.pipeline,pck.d,o)<>0) then o:=0;
    dec(o,sizeof(cmd));
    if (o<0) then exit;
    move(pck.d,cmd,sizeof(cmd));
    UDPsendPacket(cmd.adr,con.portLoc,cmd.prt,pck.d[sizeof(cmd)+1],o);
    end;
  $00:begin;
    ConnectReadD(conN,con);
    o:=sizeof(cmd)-con.maxFrame;
    if (o>0) then begin;
      dec(con.stat);
      if (con.stat<1) then begin;
        f2:
        ConnectDelete(conN);
        exit;
        end;
      if (pipeLineRecv(con.pipeline,con.buf[con.maxFrame+1],o)<>0) then o:=0;
      inc(con.maxFrame,o);
      ConnectWriteH(conN,con);
      ConnectWriteD(conN,con);
      exit;
      end;
    con.stat:=0;
    con.maxFrame:=0;
    move(con.buf,cmd,sizeof(cmd));
    BugOS_ProcessName(con.process,con.buf,i,i,o);
    o:=o and $40;
    case cmd.cmd of
      1:begin;
        makeNewConnectRec($01,cmd.adr,0,cmd.prt,con);
        con.stat:=1;
        con.retries:=retry_TCP_SYN;
        con.timeOut:=timot_TCP_SYN;
        con.lastTx:=-99999;
        end;
      2:begin;
        if (o=0) then cmd.prt:=0;
        makeNewConnectRec($81,cmd.adr,cmd.prt,0,con);
        move(cmd.adr,i,sizeof(i));
        con.sequLoc:=i;
        cmd.cmd:=3;
        cmd.adr:=LocalAddress;
        cmd.prt:=con.portLoc;
        cmd.dat:=0;
        pipeLineSend(con.pipeline,cmd,sizeof(cmd));
        end;
      4:begin;
        if (o=0) then cmd.prt:=0;
        makeNewConnectRec($82,cmd.adr,cmd.prt,0,con);
        cmd.cmd:=3;
        cmd.adr:=LocalAddress;
        cmd.prt:=con.portLoc;
        cmd.dat:=0;
        pipeLineSend(con.pipeline,cmd,sizeof(cmd));
        end;
      7:begin;
        if (o=0) then goto f2;
        o:=cmd.prt;
        if (o<=0) or (o>=255) then goto f2;
        if (ProtocolPipes[o]<>0) then goto f2;
        ProtocolPipes[o]:=con.pipeline;
        cmd.cmd:=3;
        cmd.adr:=LocalAddress;
        cmd.prt:=o;
        cmd.dat:=0;
        pipeLineSend(con.pipeline,cmd,sizeof(cmd));
        con.pipeline:=0;
        ConnectWriteH(conN,con);
        goto f2;
        end;
      else goto f2;
      end;
    {$ifdef debug}WriteLn('incoming pipeline sent the command.');{$endif}
    ConnectWriteH(conN,con);
    exit;
    end;
  $81:;
  else ConnectDelete(conN);
  end;
End;
