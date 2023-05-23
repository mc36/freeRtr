Function doConn(var con:OneConnectionRecord):Boolean;
Label f1;
Var
  buf:array[1..1024] of byte;
  i,o,p:LongInt;
  a,b:String;
Begin;
doConn:=False;
if (con.stat=0) then exit;
if (GetTimePast(con.time)>5*60) then begin;
  WriteLn('connection timed out with '+ipAddr2string(con.addr)+' '+BStr(con.port));
  a:='connection timed out!'#13#10;
  pipeLineSend(con.pipe,a[1],length(a));
  doConn:=True;
  exit;
  end;
case con.stat of
  2:begin; {connect}
    pipeLineStats(con.pipe,p,i,o);
    if (p=0) then goto f1;
    if (o>sizeof(buf)) then o:=sizeof(buf);
    if (o>64) then pipeLineRecv(con.serD,buf,o) else o:=0;
    if (o>0) then pipeLineSend(con.pipe,buf,o);
    pipeLineStats(con.serD,p,i,o);
    if (o>sizeof(buf)) then o:=sizeof(buf);
    if (o>64) then pipeLineRecv(con.pipe,buf,o) else o:=0;
    if (o>0) then pipeLineSend(con.serD,buf,o);
    end;
  1:begin; {init}
    if TCPlookConnected(con.pipe,con.addr,con.port,i) then begin;
      doConn:=(con.pipe=0);
      exit;
      end;
    WriteLn('connection accepted from '+ipAddr2string(con.addr)+' '+BStr(con.port));
    con.time:=currentTime;
    con.stat:=2;
    end;
  0:;; {disconnect}
  else begin; f1:con.stat:=666;doConn:=True;exit; end;
  end;
End;
