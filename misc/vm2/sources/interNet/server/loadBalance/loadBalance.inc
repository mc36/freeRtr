Function doConn(var con:OneConnectionRecord):Boolean;
Label f1,f2;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  buf:array[1..4*1024] of byte;
  i,o:LongInt;
  p1,p2,t1,t2,r1,r2:LongInt;
Begin;
doConn:=False;
if (GetTimePast(con.time)>60*5) then begin;
  con.stat:=0;
  doConn:=True;
  exit;
  end;
case con.stat of
  4:begin; {connected}
    pipeLineStats(con.pipe1,p1,r1,t1);
    pipeLineStats(con.pipe2,p2,r2,t2);
    if (p1=0) then if (r1<1) then goto f1;
    if (p2=0) then if (r2<1) then goto f1;
    if (r1>t2) then r1:=t2;
    if (r1>sizeof(buf)) then r1:=sizeof(buf);
    if (r1>0) then begin;
      pipeLineRecv(con.pipe1,buf,r1);
      pipeLineSend(con.pipe2,buf,r1);
      con.time:=CurrentTime;
      end;
    if (r2>t1) then r2:=t1;
    if (r2>sizeof(buf)) then r2:=sizeof(buf);
    if (r2>0) then begin;
      pipeLineRecv(con.pipe2,buf,r2);
      pipeLineSend(con.pipe1,buf,r2);
      con.time:=CurrentTime;
      end;
    end;
  3:begin; {connecting}
    if TCPlookConnected(con.pipe2,a,i,o) then begin;
      if (con.pipe2<>0) then exit;
      WriteLn('connection refused by #'+BStr(con.which)+', will try later!');
      targetTime[targetNxt]:=currentTime;
      targetStat[con.which]:=2;
      con.stat:=2;
      exit;
      end;
    WriteLn('connection accepted by #'+BStr(con.which)+', local side is '+ipAddr2string(a)+' '+BStr(i));
    targetStat[con.which]:=0;
    con.time:=CurrentTime;
    con.stat:=4;
    end;
  2:begin; {reconnect}
    p1:=0;
    f2:
    inc(p1);
    if (p1>targetNum) then begin;
      WriteLn('no active servers at now!');
      con.stat:=0;
      exit;
      end;
    targetNxt:=(targetNxt mod targetNum)+1;
    if (targetStat[targetNxt]=2) then goto f2;
    if (targetStat[targetNxt]=1) then begin;
      targetTime[targetNxt]:=currentTime;
      targetStat[targetNxt]:=2;
      Write('trying: ');
      end;
    WriteLn('connecting to #'+BStr(targetNxt));
    con.which:=targetNxt;
    TCPbeginConnect(con.pipe2,65536,targetAddr[targetNxt],targetPort[targetNxt]);
    con.time:=CurrentTime;
    con.stat:=3;
    end;
  1:begin; {init}
    if TCPlookConnected(con.pipe1,con.adr,con.prt,i) then begin;
      doConn:=(con.pipe1=0);
      exit;
      end;
    WriteLn('connection accepted from '+ipAddr2string(con.adr)+' '+BStr(con.prt));
    con.stat:=2;
    end;
  0:goto f1; {disconnect}
  else begin; f1: con.stat:=0;doConn:=True;exit; end;
  end;
End;
