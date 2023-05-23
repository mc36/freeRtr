Procedure gotIncoming(p:LongInt);
Var
  i,o:LongInt;
  a:String;
Begin;
pipeLineStats(p,o,i,i);
if (o=TCPprocessId) then begin;
  o:=FindConnectByStat(0);
  if (o<1) then begin;
    WriteLn('out of connection slots!');
    pipeLineClose(p);
    exit;
    end;
  ConnectionDat^[o].tcpP:=p;
  ConnectionDat^[o].stat:=1;
  ConnectionDat^[o].time:=currentTime;
  exit;
  end;
o:=FindConnectByStat(2);
if (o<1) then begin;
  {$ifdef debug}
  WriteLn('refused connection from upper layer...');
  {$endif}
  pipeLineClose(p);
  exit;
  end;
ConnectionDat^[o].uprP:=p;
ConnectionDat^[o].stat:=3;
a:=ipAddr2string(ConnectionDat^[o].addr)+' '+BStr(ConnectionDat^[o].port);
a:='12341234'#0#0#0#0#0#0#0#0+chr(o)+#255'stun with '+a+#0;
i:=1;move(i,a[1],sizeof(i));
i:=1500;move(i,a[5],sizeof(i));
pipeLineSend(p,a[1],length(a));
{$ifdef debug}
WriteLn('accepted connection from upper layer...');
{$endif}
End;



Procedure releq2upp(var con:OneConnectionRecord);
Var
  buf:array[1..4*1024] of byte;
  i,o,p:LongInt;
Begin;
p:=sizeof(buf);
if (pipeLineRecv(con.uprP,buf,p)<>0) then p:=0;
if (p<1) then begin;
  pipeLineStats(con.uprP,o,i,i);
  if (o<>0) then exit;
  WriteLn('upper exited with '+ipAddr2string(con.addr)+' '+BStr(con.port));
  con.stat:=999;
  exit;
  end;
sendDataPack(con.tcpP,con.grup,p,buf);
con.time:=currentTime;
End;



Procedure relequishConn(var con:OneConnectionRecord);
Label f1;
Var
  buf:array[1..4*1024] of byte;
  i,o,p:LongInt;
  a:String;
Begin;
if (con.stat=0) then exit;
if (getTimePast(con.time)>60*5) then begin;
  WriteLn('timeout with '+ipAddr2string(con.addr)+' '+BStr(con.port));
  con.stat:=999;
  end;
case con.stat of
  3:begin; {header}
    releq2upp(con);
    i:=receiveHeader(con.tcpP,o);
    if (i>0) then begin;
      con.stat:=4;
      con.size:=i;
      con.grup:=o;
      con.time:=currentTime;
      exit;
      end;
    if (o<>-1) then goto f1;
    end;
  4:begin; {packet}
    releq2upp(con);
    if (pipeLineStats(con.tcpP,p,i,o)<>0) then p:=0;
    if (p=0) then begin;
      f1:
      WriteLn('connection lost with '+ipAddr2string(con.addr)+' '+BStr(con.port));
      con.stat:=999;
      exit;
      end;
    if (i<con.size) then exit;
    i:=con.size;
    if (pipeLineRecv(con.tcpP,buf,i)<>0) then i:=0;
    if (i<>con.size) then goto f1;
    pipeLineSend(con.uprP,buf,con.size);
    con.time:=currentTime;
    con.stat:=3;
    end;
  2:; {start}
  1:begin; {init}
    if TCPlookConnected(con.tcpP,con.addr,con.port,i) then begin;
      if (con.tcpP=0) then con.stat:=999;
      exit;
      end;
    WriteLn('connection accepted from '+ipAddr2string(con.addr)+' '+BStr(con.port));
    a:=con.cmnd;
    {$ifdef debug}WriteLn('starting '+a+'...');{$endif}
    i:=pos(' ',a);
    if (i<1) then i:=666;
    if (xExecBgnd(copy(a,1,i-1),copy(a,i+1,255),o)<>0) then o:=0;
    if (o=0) then begin;
      WriteLn('failed to start upper process!');
      con.stat:=999;
      exit;
      end;
    con.stat:=2;
    con.time:=currentTime;
    con.grup:=1;
    end;
  0:; {free}
  else begin;
    pipeLineClose(con.uprP);
    pipeLineClose(con.tcpP);
    con.uprP:=0;
    con.tcpP:=0;
    con.stat:=0;
    end;
  end;
End;
