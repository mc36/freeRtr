Function doConn(var con:OneConnectionRecord):Boolean;
Var
  buf:array[1..1024] of byte;
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
  3:begin; {data}
    pipeLineStats(con.pipe,i,o,p);
    if (i=0) then con.stat:=0;
    if (o<con.size) then exit;
    con.stat:=2;
    move(con.head,buf,sizeof(con.head));
    o:=con.size;
    pipeLineRecv(con.pipe,buf[sizeof(con.head)+1],o);
    if (o<>con.size) then con.stat:=0;
    processReq(con,buf);
    end;
  2:begin; {head}
    pipeLineStats(con.pipe,i,o,p);
    if (i=0) then con.stat:=0;
    if (o<sizeof(con.head)) then exit;
    o:=sizeof(con.head);
    pipeLineRecv(con.pipe,con.head,o);
    if (o<>sizeof(con.head)) then con.stat:=0;
    con.size:=ReadLongMSB(con.head.siz);
    if (con.size<1) or (con.size>sizeof(buf)) then con.stat:=0;
    con.stat:=3;
    end;
  1:begin; {init}
    if TCPlookConnected(con.pipe,con.adr,con.prt,i) then begin;
      doConn:=(con.pipe=0);
      exit;
      end;
    WriteLn('connection accepted from '+ipAddr2string(con.adr)+' '+BStr(con.prt));
    con.stat:=2;
    end;
  0:begin; doConn:=True;exit; end; {disconnect}
  else begin; doConn:=True;exit; end;
  end;
End;
