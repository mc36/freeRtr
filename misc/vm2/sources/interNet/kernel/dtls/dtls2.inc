Function doConn(var con,connEmpty:OneConnectionRecord;num:LongInt):Boolean;
Label f1,f2,f3;
Var
  pck1,pck2,pck3:OnePacketRecord;
  num1,num2,num3,num4,num5,num6:BigNumContextRecord;
  tcpDat:OneTCPcommandHeader;
  i,o,p,q:LongInt;
  a,b,c:String;

Procedure informClient(a:String);
Label f1;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
Begin;
o:=16;
f1:
if (pipeLineSend(con.pipeL,ab[1],ab0)=0) then exit;
pipeLineStats(con.pipeL,i,p,p);
if (i=0) then exit;
dec(o);
if (o<0) then exit;
relequish;
goto f1;
End;

Procedure informClientBin(a:String;siz:LongInt;var buffer);
Label f1;
Var
  buf:array[1..1] of byte absolute buffer;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
move(buf,buf[ab0+1],siz);
move(ab[1],buf,ab0);
inc(siz,ab0);
o:=16;
f1:
if (pipeLineSend(con.pipeL,buf,siz)=0) then exit;
pipeLineStats(con.pipeL,i,p,p);
if (i=0) then exit;
dec(o);
if (o<0) then exit;
relequish;
goto f1;
End;

Procedure changeSeqOrder(var con:OneConnectionRecord;add:LongInt);
Var i:LongInt;
Begin;
i:=con.hndsLR;
con.hndsLR:=con.hndsRL+add;
con.hndsRL:=i;
End;

{$include \sources\internet\kernel\tls\tls3.inc}
{$include \sources\internet\kernel\tls\tls4.inc}

Begin;
if (GetTimePast(con.time)>5*60) then goto f2;
doConn:=False;
case con.stat of
  77:begin; {normal data}
    if not recvPackRecLay(con,pck1) then begin;
      if (pck1.t=21) then begin; {alert}
        send_closing;
        resendHndshkBuf(con);
        con.stat:=0;
        exit;
        end;
      if (pck1.t<>23) then goto f2;
      pipeLineSend(con.pipeL,pck1.d,pck1.s);
      exit;
      end;
    i:=sizeof(pck1);
    pipeLineRecv(con.pipeL,pck1,i);
    if (i>0) then begin;
      sendPackRecLay(con,23,pck1,i);
      exit;
      end;
    pipeLineStats(con.pipeL,i,o,p);
    if (i=con.proc) then exit;
    send_closing;
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
    if (pipeLineCreate(con.pipeL,con.proc,i,true)<>0) then goto f2;
    pipeLineSend(con.pipeL,tcpDat,sizeof(tcpDat));
    serv_init;
    con.vers:=0;
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
    if (tcpDat.cmd<>6) then goto f2;
    con.addr:=tcpDat.adr;
    con.port:=tcpDat.prt;
    pipeLineStats(con.pipeL,i,o,p);
    if RTPbeginConnect(con.pipeT,65536,con.addr,con.port) then goto f2;
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
    informClient('locUDP:'+ipAddr2string(tcpDat.adr)+' '+BStr(tcpDat.prt));
    informClient('remUDP:'+ipAddr2string(con.addr)+' '+BStr(con.port));
    clnt_init;
    clnt_hllT('',true);
    end;
  6:begin; {serverHello}
    if resendHndshkBuf(con) then goto f2;
    if recvPackHndshk(con,pck1) then exit;
    clnt_hllR(true);
    end;
  7:begin; {certificate}
    if resendHndshkBuf(con) then goto f2;
    if recvPackHndshk(con,pck1) then exit;
    clnt_cert;
    end;
  8:begin; {server key exchange}
    if resendHndshkBuf(con) then goto f2;
    if recvPackHndshk(con,pck1) then exit;
    clnt_kex;
    end;
  9:begin; {server hello done}
    if resendHndshkBuf(con) then goto f2;
    if recvPackHndshk(con,pck1) then exit;
    clnt_done;
    end;
  10:begin; {change cipher spec}
    if resendHndshkBuf(con) then goto f2;
    if recvPackRecLay(con,pck1) then exit;
    clnt_chg;
    end;
  11:begin; {finished}
    if resendHndshkBuf(con) then goto f2;
    if recvPackHndshk(con,pck1) then exit;
    clnt_fnsh;
    end;
  12:begin; {change cipher spec}
    if resendHndshkBuf(con) then goto f2;
    if recvPackRecLay(con,pck1) then exit;
    clnt_chgRe;
    end;
  13:begin; {finished}
    if resendHndshkBuf(con) then goto f2;
    if recvPackHndshk(con,pck1) then exit;
    clnt_fnshRe;
    resendHndshkBuf(con);
    end;
  78:begin; {closing}
    if resendHndshkBuf(con) then goto f2;
    if recvPackRecLay(con,pck1) then exit;
    got_closing;
    end;
  100:begin; {clientHello}
    if resendHndshkBuf(con) then goto f2;
    if recvPackHndshk(con,pck1) then exit;
    serv_hello(getClientAddrHash(con),true);
    end;
  101:begin; {client key exchange}
    if resendHndshkBuf(con) then goto f2;
    if recvPackHndshk(con,pck1) then exit;
    serv_kex;
    end;
  102:begin; {change cipher spec}
    if resendHndshkBuf(con) then goto f2;
    if recvPackRecLay(con,pck1) then exit;
    serv_chg;
    end;
  103:begin; {finished}
    if resendHndshkBuf(con) then goto f2;
    if recvPackHndshk(con,pck1) then exit;
    serv_fnsh;
    resendHndshkBuf(con);
    end;
  104:begin; {change cipher spec}
    if resendHndshkBuf(con) then goto f2;
    if recvPackRecLay(con,pck1) then exit;
    serv_chgRe;
    end;
  105:begin; {finished}
    if resendHndshkBuf(con) then goto f2;
    if recvPackHndshk(con,pck1) then exit;
    serv_fnshRe;
    resendHndshkBuf(con);
    end;
  0:begin; f2:doConn:=True;exit; end; {disconnect}
  else begin; f1:con.err:='bug';doConn:=True;exit; end;
  end;
End;
