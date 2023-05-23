Function doConn(var con,connEmpty:OneConnectionRecord;num:LongInt):Boolean;
Label f1,f2;
Var
  pck1,pck2,pck3:OnePacketRecord;
  num1,num2,num3,num4,num5,num6:BigNumContextRecord;
  tcpDat:OneTCPcommandHeader;
  i,o,p,q:LongInt;
  a,b,c:String;

Procedure informClientNL(a:String);
Label f1;
Var i,o,p:LongInt;
Begin;
o:=16;
f1:
if (pipeLineSend(con.pipeL,a[1],length(a))=0) then exit;
pipeLineStats(con.pipeL,i,p,p);
if (i=0) then exit;
dec(o);
if (o<0) then exit;
relequish;
goto f1;
End;

Procedure informClient(a:String);
Begin;
informClientNL(a+#13#10);
End;

Procedure informClientBin(a:String;siz:LongInt;var buffer);
Var
  buf:array[1..1] of byte absolute buffer;
  i:LongInt;
Begin;
informClientNL(a);
a:='';
for i:=1 to siz do begin;
  a:=a+byte2hextype(buf[i]);
  if (length(a)<100) then continue;
  informClientNL(a);
  a:='';
  end;
informClient(a);
End;

Procedure changeSeqOrder(var con:OneConnectionRecord;add:LongInt);
Begin;
End;

{$include tls3.inc}
{$include tls4.inc}

Begin;
if (GetTimePast(con.time)>5*60) then goto f2;
doConn:=False;
case con.stat of
  77:begin; {normal data}
    if not recvPackRecLay(con,pck1) then begin;
      if (pck1.t=21) then begin; {alert}
        send_closing;
        con.stat:=0;
        exit;
        end;
      if (pck1.t<>23) then goto f2;
      con.recSiz:=pck1.s;
      con.recPos:=0;
      move(pck1.d,con.recDat,pck1.s);
      con.stat:=76;
      exit;
      end;
    pipeLineStats(con.pipeT,i,o,p);
    if (p<1024) then exit;
    i:=768;
    pipeLineRecv(con.pipeL,pck1,i);
    if (i>0) then begin;
      sendPackRecLay(con,23,pck1,i);
      exit;
      end;
    pipeLineStats(con.pipeL,i,o,p);
    if (i=con.proc) then exit;
    send_closing;
    end;
  76:begin; {sending data}
    pipeLineStats(con.pipeL,i,o,p);
    if (i<>con.proc) then begin; send_closing;exit; end;
    if (p<1024) then exit;
    dec(p,16);
    i:=con.recSiz-con.recPos;
    if (p>i) then p:=i;
    if (p<1) then begin;
      con.recSiz:=0;
      con.recPos:=0;
      con.stat:=77;
      exit;
      end;
    if (pipeLineSend(con.pipeL,con.recDat[con.recPos+1],p)<>0) then p:=0;
    con.time:=currentTime;
    inc(con.recPos,p);
    end;
  4:begin; {listening}
    con.time:=currentTime;
    pipeLineStats(con.pipeL,i,o,p);
    if (i=con.proc) then exit;
    WriteLn('listening closed on port '+ipAddr2string(con.addr)+' '+BStr(con.port));
    goto f2;
    end;
  1:; {session id cache}
  2:begin; {get tcp request}
    if TCPlookConnected(con.pipeT,con.addr,o,p) then begin;
      if (con.pipeT=0) then goto f2;
      exit;
      end;
    WriteLn('connection accepted from '+ipAddr2string(con.addr)+' '+BStr(o)+' to '+BStr(p));
    fillchar(tcpDat,sizeof(tcpDat),0);
    tcpDat.cmd:=3;
    move(con.addr,tcpDat.adr,sizeof(tcpDat.adr));
    tcpDat.prt:=o;
    tcpDat.dat:=p;
    con.port:=p;
    o:=0;
    for i:=1 to ConnectionNum do begin;
      if (ConnectionDat^[i].stat<>4) then continue;
      if (ConnectionDat^[i].port<>p) then continue;
      o:=i;
      break;
      end;
    if (o=0) then goto f1;
    con.proc:=ConnectionDat^[o].proc;
    move(ConnectionDat^[o].addr,i,sizeof(i));
    if (pipeLineCreate(con.pipeL,con.proc,i,false)<>0) then goto f2;
    pipeLineSend(con.pipeL,tcpDat,sizeof(tcpDat));
    serv_init;
    con.stat:=100;
    end;
  3:begin; {get local request}
    pipeLineStats(con.pipeL,i,o,p);
    if (o<sizeof(tcpDat)) then exit;
    i:=sizeof(tcpDat);
    pipeLineRecv(con.pipeL,tcpDat,i);
    if (i<>sizeof(tcpDat)) then exit;
    if (tcpDat.cmd=2) then begin;
      BugOS_ProcessName(con.proc,pck1,i,i,o);
      if (o and $40=0) then tcpDat.prt:=0;
      move(tcpDat.adr,i,sizeof(i));
      if TCPlistenOnPort(con.pipeT,i,tcpDat.adr,tcpDat.prt) then goto f2;
      con.addr:=tcpDat.adr;
      con.port:=tcpDat.prt;
      fillchar(tcpDat,sizeof(tcpDat),0);
      tcpDat.cmd:=3;
      move(con.addr,tcpDat.adr,sizeof(tcpDat.adr));
      tcpDat.prt:=con.port;
      pipeLineSend(con.pipeL,tcpDat,sizeof(tcpDat));
      WriteLn('listening opened on port '+ipAddr2string(con.addr)+' '+BStr(con.port));
      con.stat:=4;
      exit;
      end;
    if (tcpDat.cmd<>1) then goto f2;
    con.addr:=tcpDat.adr;
    con.port:=tcpDat.prt;
    pipeLineStats(con.pipeL,i,o,p);
    if TCPbeginConnect(con.pipeT,65536,con.addr,con.port) then goto f2;
    con.stat:=5;
    end;
  5:begin; {connecting}
    if TCPlookConnected(con.pipeT,a,p,o) then begin;
      if (con.pipeT=0) then goto f2;
      exit;
      end;
    fillchar(tcpDat,sizeof(tcpDat),0);
    tcpDat.cmd:=3;
    move(a,tcpDat.adr,sizeof(tcpDat.adr));
    tcpDat.prt:=p;
    tcpDat.dat:=0;
    pipeLineSend(con.pipeL,tcpDat,sizeof(tcpDat));
    informClient('locTCP:'+ipAddr2string(tcpDat.adr)+' '+BStr(tcpDat.prt));
    informClient('remTCP:'+ipAddr2string(con.addr)+' '+BStr(con.port));
    clnt_init;
    clnt_hllT('',false);
    end;
  6:begin; {serverHello}
    if recvPackHndshk(con,pck1) then exit;
    clnt_hllR(false);
    end;
  7:begin; {certificate}
    if recvPackHndshk(con,pck1) then exit;
    clnt_cert;
    end;
  8:begin; {server key exchange}
    if recvPackHndshk(con,pck1) then exit;
    clnt_kex;
    end;
  9:begin; {server hello done}
    if recvPackHndshk(con,pck1) then exit;
    clnt_done;
    end;
  10:begin; {change cipher spec}
    if recvPackRecLay(con,pck1) then exit;
    clnt_chg;
    end;
  11:begin; {finished}
    if recvPackHndshk(con,pck1) then exit;
    clnt_fnsh;
    end;
  12:begin; {change cipher spec}
    if recvPackRecLay(con,pck1) then exit;
    clnt_chgRe;
    end;
  13:begin; {finished}
    if recvPackHndshk(con,pck1) then exit;
    clnt_fnshRe;
    end;
  78:begin; {closing}
    if recvPackRecLay(con,pck1) then exit;
    got_closing;
    end;
  100:begin; {clientHello}
    if recvPackRecLay(con,pck1) then exit;
    if (con.vers shr 8<>2) then begin;
      CryptoBegHasher(con.macLRa,sha1algoNum,'','');
      CryptoBegHasher(con.macLRb,md5algoNum,'','');
      move(pck1.d,con.recDat[6],pck1.s);
      con.recPos:=pck1.s+5;
      if recvPackHndshk(con,pck1) then goto f2;
      end;
    serv_hello('',false);
    end;
  101:begin; {client key exchange}
    if recvPackHndshk(con,pck1) then exit;
    serv_kex;
    end;
  102:begin; {change cipher spec}
    if recvPackRecLay(con,pck1) then exit;
    serv_chg;
    end;
  103:begin; {finished}
    if recvPackHndshk(con,pck1) then exit;
    serv_fnsh;
    end;
  104:begin; {change cipher spec}
    if recvPackRecLay(con,pck1) then exit;
    serv_chgRe;
    end;
  105:begin; {finished}
    if recvPackHndshk(con,pck1) then exit;
    serv_fnshRe;
    end;
  0:begin; f2:doConn:=True;exit; end; {disconnect}
  else begin; f1:con.err:='bug';doConn:=True;exit; end;
  end;
End;
