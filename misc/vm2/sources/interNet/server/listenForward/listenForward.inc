Type
  OneConnectionRecord=record
    pipe:LongInt;               {pipeline id}
    time:LongInt;               {time of last action}
    addr:OneTCPaddressRecord;   {remote address}
    port:LongInt;               {remote port}
    conn:LongInt;               {connection id}
    stat:LongInt;               {status: 1-accept, 2-connect, 3-data, 4-wait}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  ConnectionCur:LongInt;
  uplinkPipe:LongInt;
  uplinkLast:LongInt;
  uplinkHdrDat:array[1..5] of byte;
  uplinkHdrSiz:LongInt;


Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Function ResizeMem(n:LongInt):Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
ResizeMem:=True;
i:=n*sizeof(OneConnectionRecord);
if (ExtendedMemoryResize(p,i)<i) then exit;
ConnectionNum:=n;
ConnectionDat:=p^;
ResizeMem:=False;
End;

Function findOneConnect(id:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do if (id=ConnectionDat^[i].conn) then goto f1;
i:=0;
f1:
findOneConnect:=i;
End;

Procedure deleteOneConnect(n:LongInt);
Begin;
pipeLineClose(ConnectionDat^[n].pipe);
ConnectionDat^[n]:=ConnectionDat^[ConnectionNum];
ResizeMem(ConnectionNum-1);
End;


Function uplinkGetPackType(i:LongInt):String;
Var a:String;
Begin;
case i of
  0:a:='discardReq';
  1:a:='echoReq';
  2:a:='echoRep';
  3:a:='openConn';
  4:a:='acceptConn';
  5:a:='closeConn';
  6:a:='sendData';
  else a:='unknown:'+BStr(i);
  end;
uplinkGetPackType:=a;
End;

Function uplinkBufferFull:Boolean;
Var i,o,p:LongInt;
Begin;
pipeLineStats(uplinkPipe,p,i,o);
uplinkBufferFull:=(o<2048);
End;

Procedure uplinkSendFrame(typ,id,siz:LongInt;var data);
Var buf:array[1..2*1024] of byte;
Begin;
if (siz>1536) then immErr('wants to send too big data packet!');
buf[1]:=typ;
WriteWordMSB(buf[2],id);
WriteWordMSB(buf[4],siz);
move(data,buf[6],siz);
pipeLineSend(uplinkPipe,buf,siz+5);
{$ifdef debug}WriteLn('tx '+uplinkGetPackType(typ)+' chan='+BStr(id)+' size='+BStr(siz));{$endif}
End;

Function uplinkRecvFrame(var typ:LongInt;var id:LongInt;var data):LongInt;
Var i,o,p:LongInt;
Begin;
uplinkRecvFrame:=-1;
pipeLineStats(uplinkPipe,i,o,p);
if (i=0) then begin;
  WriteLn('remote closed connection!');
  Halt(1);
  end;
i:=sizeof(uplinkHdrDat)-uplinkHdrSiz;
if (i>0) then begin;
  if (pipeLineRecv(uplinkPipe,uplinkHdrDat[uplinkHdrSiz+1],i)<>0) then i:=0;
  inc(uplinkHdrSiz,i);
  exit;
  end;
p:=ReadWordMSB(uplinkHdrDat[4]);
if (p>1536) then immErr('wants to receive too big data packet!');
if (o<p) then exit;
uplinkHdrSiz:=0;
i:=p;
if (pipeLineRecv(uplinkPipe,data,i)<>0) then i:=0;
if (i<>p) then immErr('invalid number of bytes readed!');
typ:=uplinkHdrDat[1];
id:=ReadWordMSB(uplinkHdrDat[2]);
uplinkRecvFrame:=p;
{$ifdef debug}WriteLn('rx '+uplinkGetPackType(typ)+' chan='+BStr(id)+' size='+BStr(p));{$endif}
End;



Function uplinkDoUsualWork(var typ:LongInt;var id:LongInt;var data):LongInt;
Var
  p,s:LongInt;
  buf:array[1..2*1024] of byte;
Begin;
uplinkDoUsualWork:=-1;
s:=uplinkRecvFrame(typ,id,buf);
if (s<0) then exit;
uplinkLast:=currentTime;
case typ of
  0:; {discard}
  1:uplinkSendFrame(2,id,s,buf); {echo request}
  2:; {echo reply}
  5:begin; {close connection}
    p:=findOneConnect(id);
    if (p<1) then exit;
    deleteOneConnect(p);
    end;
  6:begin; {send data}
    p:=findOneConnect(id);
    if (p<1) then begin;
      uplinkSendFrame(5,id,0,s);
      exit;
      end;
    pipeLineSend(ConnectionDat^[p].pipe,buf,s);
    end;
  else begin;
    move(buf,data,s);
    uplinkDoUsualWork:=s;
    end;
  end;
End;



Procedure releq2conn;
Label f1,f2;
Var
  con:OneConnectionRecord;
  i,o,p:LongInt;
  buf:array[1..1024] of byte;
Begin;
inc(ConnectionCur);
if (ConnectionCur>ConnectionNum) then begin;
  ConnectionCur:=0;
  if (GetTimePast(uplinkLast)<60) then exit;
  uplinkSendFrame(1,random($10000),0,i);
  uplinkLast:=currentTime;
  exit;
  end;
if (ConnectionNum<1) then exit;
con:=ConnectionDat^[ConnectionCur];
case con.stat of
  3:begin; {connected}
    i:=sizeof(buf);
    if (pipeLineRecv(con.pipe,buf,i)<>0) then i:=0;
    if (i>0) then begin;
      uplinkSendFrame(6,con.conn,i,buf);
      goto f1;
      end;
    pipeLineStats(con.pipe,p,i,o);
    if (p<>0) then goto f1;
    uplinkSendFrame(5,con.conn,0,i);
    goto f2;
    end;
  1:begin; {accept}
    if TCPlookConnected(con.pipe,con.addr,con.port,i) then begin;
      if (con.pipe<>0) then exit;
      goto f2;
      end;
    WriteLn('connection accepted from '+ipAddr2string(con.addr)+' '+BStr(con.port));
    move(con.addr,buf,sizeof(con.addr));
    WriteWordMSB(buf[sizeof(con.addr)+1],con.port);
    uplinkSendFrame(3,con.conn,sizeof(con.addr)+2,buf);
    con.stat:=4;
    end;
  2:begin; {connect}
    if TCPlookConnected(con.pipe,buf,i,o) then begin;
      if (con.pipe<>0) then exit;
      uplinkSendFrame(5,con.conn,0,i);
      goto f2;
      end;
    WriteLn('connection accepted by target, local side is '+ipAddr2string(buf)+' '+BStr(i));
    WriteWordMSB(buf[sizeof(con.addr)+1],i);
    uplinkSendFrame(4,con.conn,sizeof(con.addr)+2,buf);
    con.stat:=3;
    end;
  4:; {wait}
  else begin;
    f2:
    deleteOneConnect(ConnectionCur);
    exit;
    end;
  end;

f1:
ConnectionDat^[ConnectionCur]:=con;
End;
