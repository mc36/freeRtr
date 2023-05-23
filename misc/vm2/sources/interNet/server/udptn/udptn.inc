Function doConn(var con:OneConnectionRecord;conN:LongInt):Boolean;

Procedure send(a:String);
Begin;
UDPsendPacket(serverPipe,con.adr,con.prt,a[1],length(a));
End;

Label f1;
Var
  buf:array[1..512] of byte;
  i,o,p:LongInt;
  a,b:String;
Begin;
if (GetTimePast(con.time)>5*60) then begin;
  send(#13#10#13#10'timeout, session closed!'#13#10);
  WriteLn('connection timed out with '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  doConn:=True;
  exit;
  end;
doConn:=False;
case con.stat of
  2:begin; {connected}
    if not BugOS_ProcessExists(con.proc) then begin;
      f1:
      send(#13#10#13#10'process terminated, session closed!'#13#10);
      con.stat:=0;
      exit;
      end;
    p:=sizeof(buf);
    pipeLineRecv(con.emu1,buf,p);
    if (p>0) then UDPsendPacket(serverPipe,con.adr,con.prt,buf,p);
    if (p>0) then con.time:=CurrentTime;
    pipeLineStats(con.emu1,i,o,p);
    if (i=0) then goto f1;
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
  1:begin; {enter}
    end;
  0:begin; doConn:=True;exit; end; {disconnect}
  else begin; doConn:=True;exit; end;
  end;
End;



Procedure doPack(var con:OneConnectionRecord;var buffer;siz:LongInt);

Procedure send(a:String);
Begin;
UDPsendPacket(serverPipe,con.adr,con.prt,a[1],length(a));
End;

Label f1;
Var buf:array[1..1] of byte absolute buffer;
Begin;
case con.stat of
  2:begin; {connected}
    pipeLineSend(con.emu1,buf,siz);
    con.time:=CurrentTime;
    end;
  1:begin; {enter}
    if (siz<>1) then exit;
    if (buf[1]<>32) then exit;
    send(#13#10#13#10);
    if (pipeLineCreate(con.emu1,terminalFilter,65536,false)<>0) then goto f1;
    if (pipeLineCreate(con.emu2,terminalFilter,65536,false)<>0) then goto f1;
    pipeLineSend(con.emu1,con.emu2,sizeof(con.emu2));
    BugOS_SetOwnerInfo(userIDtoGive);
    if (xExecInside(startProcNam,startProcPar,con.proc,con.term)<>0) then goto f1;
    BugOS_SetOwnerInfo(0);
    con.stat:=2;
    end;
  else begin;
    f1:
    con.stat:=0;
    end;
  end;
End;



Procedure doNew(var buffer;siz,prt:LongInt;var adr:OneTCPaddressRecord);

Procedure send(a:String);
Begin;
UDPsendPacket(serverPipe,adr,prt,a[1],length(a));
End;

Var
  buf:array[1..1] of byte absolute buffer;
  con:OneConnectionRecord;
Begin;
if (siz<>1) or (buf[1]<>13) then begin;
  send('press enter to start session!'#13#10);
  exit;
  end;
if ResizeMem(ConnectionNum+1) then begin;
  send('server is too busy!'#13#10);
  WriteLn('failed to allocate memory!');
  exit;
  end;
fillchar(con,sizeof(con),0);
con.stat:=1;
con.adr:=adr;
con.prt:=prt;
con.time:=CurrentTime;
ConnectionDat^[ConnectionNum]:=con;
WriteLn('connection accepted from '+ipAddr2string(adr)+' '+BStr(prt));
send('server: '+ipAddr2string(serverAddr)+' '+BStr(serverPort)+#13#10);
send('client: '+ipAddr2string(adr)+' '+BStr(prt)+#13#10);
send('press space to log in!');
End;
