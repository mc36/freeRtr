Function doConn(var con:OneConnectionRecord;conN:LongInt):Boolean;

Procedure send(a:String);
Begin;
pipeLineSend(con.tcp,a[1],length(a));
End;

Label f1;
Var
  buf:array[1..512] of byte;
  i,o,p:LongInt;
  a,b:String;
Begin;
if (GetTimePast(con.time)>5*60) then begin;
  WriteLn('connection timed out with '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  doConn:=True;
  exit;
  end;
doConn:=False;
case con.stat of
  3:begin; {connected}
    if not BugOS_ProcessExists(con.proc) then begin;
      f1:
      con.stat:=0;
      exit;
      end;
    pipeLineStats(con.tcp,i,o,p);
    if (i=0) then goto f1;
    if (p>512) then begin;
      p:=sizeof(buf);
      pipeLineRecv(con.emu1,buf,p);
      pipeLineSend(con.tcp,buf,p);
      if (p>0) then con.time:=CurrentTime;
      end;
    pipeLineStats(con.emu1,i,o,p);
    if (i=0) then goto f1;
    if (p>512) then begin;
      p:=sizeof(buf);
      pipeLineRecv(con.tcp,buf,p);
      pipeLineSend(con.emu1,buf,p);
      if (p>0) then con.time:=CurrentTime;
      end;
    pipeLineStats(con.term,i,o,p);
    if (i=0) then goto f1;
    if (p>512) then begin;
      p:=sizeof(buf);
      pipeLineRecv(con.emu2,buf,p);
      pipeLineSend(con.term,buf,p);
      end;
    pipeLineStats(con.emu2,i,o,p);
    if (i=0) then goto f1;
    if (p>512) then begin;
      p:=sizeof(buf);
      pipeLineRecv(con.term,buf,p);
      pipeLineSend(con.emu2,buf,p);
      end;
    end;
  2:begin; {enter}
    p:=sizeof(buf);
    if (pipeLineRecv(con.tcp,buf,p)<>0) then p:=0;
    if (p<>1) then begin;
      pipeLineStats(con.tcp,i,o,p);
      if (i=0) then goto f1;
      exit;
      end;
    if (buf[1]<>32) then exit;
    send(#13#10#13#10);
    if (pipeLineCreate(con.emu1,terminalFilter,65536,false)<>0) then goto f1;
    if (pipeLineCreate(con.emu2,terminalFilter,65536,false)<>0) then goto f1;
    pipeLineSend(con.emu1,con.emu2,sizeof(con.emu2));
    BugOS_SetOwnerInfo(userIDtoGive);
    if (xExecInside(startProcNam,startProcPar,con.proc,con.term)<>0) then goto f1;
    BugOS_SetOwnerInfo(0);
    con.stat:=3;
    end;
  1:begin; {init}
    if TCPlookConnected(con.tcp,con.adr,con.prt,i) then begin;
      doConn:=(con.tcp=0);
      exit;
      end;
    WriteLn('connection accepted from '+ipAddr2string(con.adr)+' '+BStr(con.prt));
    con.stat:=2;
    pipeLineStats(con.tcp,o,i,i);
    p:=0;
    for i:=ProtocolNum downto 1 do if (o=ProtocolDat[i].proc) then p:=i;
    if (p>0) then a:=ProtocolDat[p].name else begin;
      a:='unknown';
      p:=1;
      end;
    send(initString);
    send('protocol: '+a+#13#10);
    send('server: '+ipAddr2string(ProtocolDat[p].addr)+' '+BStr(ProtocolDat[p].port)+#13#10);
    send('client: '+ipAddr2string(con.adr)+' '+BStr(con.prt)+#13#10);
    send('press space to log in!');
    end;
  0:begin; doConn:=True;exit; end; {disconnect}
  else begin; doConn:=True;exit; end;
  end;
End;
