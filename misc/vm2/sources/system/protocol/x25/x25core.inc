Procedure relequishToX25;
Label f1;
Var
  buf:array[1..2048] of byte;
  tcp:OneTCPcommandHeader;
  con:OneConnectionRecord;
  num,lcn,typ,siz:LongInt;
  i,o,p:LongInt;

Procedure sendPipe(a:String);
Begin;
pipeLineSend(con.pipe,a[1],length(a));
con.time:=CurrentTime;
End;

Procedure procAck(rx:LongInt);
Begin;
if ((rx-1) and modMsk<>con.txSeq) then exit;
con.txSeq:=(con.txSeq+1) and modMsk;
con.siz:=0;
End;

Begin;
siz:=receivePacket(buf);
if (siz<1) then exit;
packetGetAddr(buf,lcn,typ);
if (typ and 1=0) then begin; {data}
  {$ifdef debug2}writeln('lcn '+BStr(lcn)+' got DATA...');{$endif}
  num:=findLCNnumber(lcn);
  if (num<1) then goto f1;
  con:=ConnectionDat^[num];
  if not (con.stat in [6,7]) then goto f1;
  con.stat:=7;
  if packetGetPRS(buf,i,o,p) then goto f1;
  procAck(o);
  if (p<>con.rxSeq) then begin;
    sendRcvRdy(con);
    con.time:=currentTime;
    con.retry:=16;
    exit;
    end;
  con.rxSeq:=(con.rxSeq+1) and modMsk;
  pipeLineSend(con.pipe,buf[i+1],siz-i);
  con.time:=currentTime;
  con.retry:=16;
  sendRcvRdy(con);
  ConnectionDat^[num]:=con;
  exit;
  end;
case typ and $1f of
  $01:begin; {rr}
    {$ifdef debug2}writeln('lcn '+BStr(lcn)+' got RR...');{$endif}
    num:=findLCNnumber(lcn);
    if (num<1) then goto f1;
    con:=ConnectionDat^[num];
    if not (con.stat in [6,7]) then goto f1;
    con.stat:=7;
    if packetGetPR(buf,i,o) then goto f1;
    procAck(o);
    if (con.siz=0) then con.time:=currentTime else con.time:=-9999;
    con.retry:=16;
    ConnectionDat^[num]:=con;
    exit;
    end;
  $05:begin; {rnr}
    {$ifdef debug2}writeln('lcn '+BStr(lcn)+' got RNR...');{$endif}
    num:=findLCNnumber(lcn);
    if (num<1) then goto f1;
    con:=ConnectionDat^[num];
    if not (con.stat in [6,7]) then goto f1;
    con.stat:=7;
    if packetGetPR(buf,i,o) then goto f1;
    procAck(o);
    con.time:=currentTime;
    con.retry:=16;
    ConnectionDat^[num]:=con;
    exit;
    end;
  $09:begin; {rnr}
    {$ifdef debug2}writeln('lcn '+BStr(lcn)+' got REJ...');{$endif}
    num:=findLCNnumber(lcn);
    if (num<1) then goto f1;
    con:=ConnectionDat^[num];
    if not (con.stat in [6,7]) then goto f1;
    con.stat:=7;
    if packetGetPR(buf,i,o) then goto f1;
    procAck(o);
    con.time:=-9999;
    con.retry:=16;
    ConnectionDat^[num]:=con;
    exit;
    end;
  end;
case typ of
  $0b:begin; {call request}
    {$ifdef debug2}writeln('lcn '+BStr(lcn)+' got call request...');{$endif}
    if (listenerPipe=0) then goto f1;
    num:=findLCNnumber(lcn);
    if (num>0) then begin;
      con:=ConnectionDat^[num];
      sendCallAcpt(con);
      exit;
      end;
    fillchar(con,sizeof(con),0);
    con.lcn:=lcn;
    if ResizeMem(ConnectionNum+1) then goto f1;
    num:=ConnectionNum;
    if (pipeLineCreate(con.pipe,listenerProc,listenerSize,false)<>0) then begin;
      pipeLineClose(con.pipe);
      goto f1;
      end;
    p:=3;
    packetGetNums(buf,p,con);
    packetGetString(buf,p);
    packetGetUser(buf,p,siz,con);
    fillchar(tcp,sizeof(tcp),0);
    tcp.cmd:=3;
    pipeLineSend(con.pipe,tcp,sizeof(tcp));
    sendPipe('src:'+con.src+#13#10);
    sendPipe('trg:'+con.trg+#13#10);
    sendPipe('usr:'+con.usr+#13#10);
    con.stat:=7;
    con.time:=currentTime;
    con.retry:=16;
    sendPipe('conn'#13#10);
    sendCallAcpt(con);
    ConnectionDat^[num]:=con;
    exit;
    end;
  $0f:begin; {call accept}
    {$ifdef debug2}writeln('lcn '+BStr(lcn)+' got call accept...');{$endif}
    num:=findLCNnumber(lcn);
    if (num<1) then begin;
      f1:
      fillchar(con,sizeof(con),0);
      con.lcn:=lcn;
      sendClearReq(con,0,0);
      exit;
      end;
    con:=ConnectionDat^[num];
    con.stat:=7;
    con.time:=currentTime;
    con.retry:=16;
    sendPipe('conn'#13#10);
    ConnectionDat^[num]:=con;
    exit;
    end;
  $13:begin; {clear request}
    {$ifdef debug2}writeln('lcn '+BStr(lcn)+' got clear request...');{$endif}
    fillchar(con,sizeof(con),0);
    con.lcn:=lcn;
    sendClearConf(con);
    num:=findLCNnumber(lcn);
    if (num<1) then exit;
    con:=ConnectionDat^[num];
    if (con.stat=6) then sendPipe('err: '+BStr(buf[4])+' '+BStr(buf[5])+#13#10);
    con.stat:=666;
    ConnectionDat^[num]:=con;
    exit;
    end;
  $17:begin; {clear confirmation}
    {$ifdef debug2}writeln('lcn '+BStr(lcn)+' got clear confirmation...');{$endif}
    num:=findLCNnumber(lcn);
    if (num<1) then exit;
    con:=ConnectionDat^[num];
    con.stat:=666;
    ConnectionDat^[num]:=con;
    exit;
    end;
  $1f:begin; {reset confirmation}
    {$ifdef debug2}writeln('lcn '+BStr(lcn)+' got reset confirmation...');{$endif}
    num:=findLCNnumber(lcn);
    if (num<1) then goto f1;
    con:=ConnectionDat^[num];
    con.rxSeq:=0;
    con.txSeq:=0;
    ConnectionDat^[num]:=con;
    exit;
    end;
  $1b:begin; {reset request}
    {$ifdef debug2}writeln('lcn '+BStr(lcn)+' got reset request...');{$endif}
    num:=findLCNnumber(lcn);
    if (num<1) then goto f1;
    con:=ConnectionDat^[num];
    con.rxSeq:=0;
    con.txSeq:=0;
    ConnectionDat^[num]:=con;
    sendResetConf(con);
    exit;
    end;
  $fb:begin; {restart request}
    {$ifdef debug2}writeln('got restart request: cause='+BStr(buf[4])+' diag='+BStr(buf[5])+'...');{$endif}
    for i:=1 to ConnectionNum do ConnectionDat^[i].stat:=666;
    sendRestartConf;
    exit;
    end;
  $ff:begin; {restart confirmation}
    {$ifdef debug2}writeln('got restart confirmation...');{$endif}
    for i:=1 to ConnectionNum do ConnectionDat^[i].stat:=666;
    exit;
    end;
  end;
WriteLn('got unknown packet: lcn='+BStr(lcn)+' type=$'+byte2hextype(typ));
End;
