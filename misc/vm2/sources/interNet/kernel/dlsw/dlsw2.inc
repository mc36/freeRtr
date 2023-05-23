Procedure relequishConn(var con:OneConnectionRecord);
Label f1;
Var
  buf:array[1..4*1024] of byte;
  hdr:OneDLSWheaderRecord;
  i,o,p:LongInt;
  a:String;
Begin;
if (getTimePast(con.time)>60*5) then begin;
  WriteLn('timeout with '+ipAddr2string(con.addr));
  con.stat:=999;
  end;
case con.stat of
  4:begin; {data}
    pipeLineStats(con.pipe2,o,i,i);
    if (o=0) then goto f1;
    pipeLineStats(con.pipe1,p,i,o);
    if (p=0) then begin;
      con.stat:=3;
      exit;
      end;
    if (i<con.hdrSiz) then exit;
    o:=con.hdrSiz;
    pipeLineRecv(con.pipe1,buf,o);
    if (o<>con.hdrSiz) then goto f1;
    con.stat:=3;
    con.time:=currentTime;
    move(con.hdrDat,hdr,sizeof(hdr));
    case hdr.msgType of
      $06,$14:begin; {data frames}
        move(buf[3],buf[7],o);
        move(hdr.srcMac,buf,sizeof(hdr.srcMac));
        pipeLineSend(con.uppr,buf,o+4);
        exit;
        end;
      $20:if (ReadWordMSB(buf[3])=$1520) then begin; {capabilities exchange}
        WriteWordMSB(buf[1],4);
        WriteWordMSB(buf[3],$1521);
        hdr.version:=$31;
        hdr.headLen:=sizeof(hdr);
        WriteWordMSB(hdr.dataLen,4);
        hdr.msgType:=$20;
        hdr.protID:=$42;
        hdr.headNum:=1;
        hdr.oldMsgTp:=$49;
        hdr.direct:=2;
        pipeLineSend(con.pipe2,hdr,sizeof(hdr));
        pipeLineSend(con.pipe2,buf,4);
        exit;
        end;
      end;
    WriteLn('got unknown message type: $'+byte2hextype(hdr.msgType));
    exit;
    end;
  3:begin; {header}
    pipeLineStats(con.pipe2,o,i,i);
    if (o=0) then goto f1;
    pipeLineStats(con.pipe1,p,i,o);
    if (p=0) then exit;
    if (i<sizeof(hdr)) then exit;
    i:=sizeof(hdr);
    pipeLineRecv(con.pipe1,con.hdrDat,i);
    if (i<>sizeof(hdr)) then goto f1;
    move(con.hdrDat,hdr,sizeof(hdr));
    con.hdrSiz:=ReadWordMSB(hdr.dataLen);
    con.stat:=4;
    con.time:=currentTime;
    end;
  2:begin; {wait}
    if TCPlookConnected(con.pipe2,a,i,o) then begin;
      if (con.pipe2<>0) then exit;
      f1:
      WriteLn('connection lost with '+ipAddr2string(con.addr));
      con.stat:=999;
      exit;
      end;
    WriteLn('accepted, local side is '+ipAddr2string(a)+' '+BStr(i)+'...');
    p:=4;
    WriteWordMSB(buf[3],$1520);
    addTLVdata(buf,p,$81,#0#0#11);      {vendor id}
    addTLVdata(buf,p,$82,#2#0);         {version}
    addTLVdata(buf,p,$83,#0#20);        {window size}
    addTLVdata(buf,p,$86,#255#255#255#255#255#255#255#255#255#255#255#255#255#255#255#255);      {sap list control}
    addTLVdata(buf,p,$84,proggyName);   {software name}
    addTLVdata(buf,p,$87,#2);           {tcp connections}
    addTLVdata(buf,p,$85,#0);           {mac exclusive}
    addTLVdata(buf,p,$88,#0);           {netbios exclusive}
    addTLVdata(buf,p,$8c,#1);           {multicast version}
    addTLVdata(buf,p,$8b,#0#0#11);      {vendor id}
    WriteWordMSB(buf[1],p);
    fillchar(hdr,sizeof(hdr),0);
    hdr.version:=$31;
    hdr.headLen:=sizeof(hdr);
    WriteWordMSB(hdr.dataLen,p);
    hdr.remDLC:=Random($8fffffff);
    hdr.remDLCpid:=Random($8fffffff);
    hdr.msgType:=$20;
    hdr.flowCtrl:=0;
    hdr.protID:=$42;
    hdr.headNum:=1;
    hdr.oldMsgTp:=$49;
    hdr.direct:=1;
    pipeLineSend(con.pipe2,hdr,sizeof(hdr));
    pipeLineSend(con.pipe2,buf,p);
    con.stat:=3;
    con.time:=currentTime;
    end;
  1:begin; {init}
    WriteLn('connecting to '+ipAddr2string(con.addr)+' '+BStr(servPort));
    pipeLineClose(con.pipe2);
    TCPbeginConnect(con.pipe2,65536,con.addr,servPort);
    con.stat:=2;
    con.time:=currentTime;
    end;
  0:begin; {free}
    if (getTimePast(con.time)<60) then exit;
    con.stat:=1;
    con.time:=currentTime;
    end;
  else begin;
    pipeLineClose(con.pipe1);
    pipeLineClose(con.pipe2);
    con.pipe1:=0;
    con.pipe2:=0;
    con.time:=currentTime;
    con.stat:=0;
    end;
  end;
End;
