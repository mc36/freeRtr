Function doConn(var con:OneConnectionRecord;conN:LongInt):Boolean;
Label f1;
Var
  buf:array[1..4*1024] of byte;
  tcp:OneTCPcommandHeader;
  i,o:LongInt;
  p1,p2,t1,t2,r1,r2:LongInt;
  a:String;
  b:Byte;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;

function getByte:LongInt;
Begin;
inc(con.pos);
getByte:=EventsDat[con.pos];
End;

Begin;
doConn:=False;
if (GetTimePast(con.time)>60*5) then begin;
  con.stat:=0;
  doConn:=True;
  exit;
  end;
case con.stat of
  200:begin; {connected}
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
  7:begin; {begin pascal string}
    pipeLineStats(con.pipe2,i,r1,t1);
    if (i=0) then goto f1;
    if (r1<sizeof(b)) then exit;
    i:=sizeof(b);
    if (pipeLineRecv(con.pipe2,b,i)<>0) then exit;
    if (i<1) then exit;
    con.tmp:=b;
    con.stat:=4;
    end;
  6:begin; {test next characetr}
    pipeLineStats(con.pipe2,i,r1,t1);
    if (i=0) then goto f1;
    if (r1<sizeof(b)) then exit;
    i:=sizeof(b);
    if (pipeLineRecv(con.pipe2,b,i)<>0) then exit;
    if (i<1) then exit;
    con.stat:=3;
    if (b=con.tmp) then exit;
    WriteLn('failed to connect!');
    goto f1;
    end;
  5:begin; {wait for character}
    pipeLineStats(con.pipe2,i,r1,t1);
    if (i=0) then goto f1;
    if (r1<sizeof(b)) then exit;
    i:=sizeof(b);
    if (pipeLineRecv(con.pipe2,b,i)<>0) then exit;
    if (i<1) then exit;
    if (b=con.tmp) then con.stat:=3;
    end;
  4:begin; {receive bytes}
    if (con.tmp<1) then begin; con.stat:=3;exit; end;
    pipeLineStats(con.pipe2,i,r1,t1);
    if (i=0) then goto f1;
    if (r1<con.tmp) then exit;
    i:=con.tmp;
    if (pipeLineRecv(con.pipe2,buf,i)<>0) then exit;
    if (i<con.tmp) then exit;
    con.stat:=3;
    end;
  3:begin; {next command}
    pipeLineStats(con.pipe2,i,r1,t1);
    if (i=0) then goto f1;
    con.time:=CurrentTime;
    i:=getByte;
    if (con.pos>EventsNum) then goto f1;
    case i of
      255:begin; {end}
        fillchar(tcp,sizeof(tcp),0);
        tcp.cmd:=3;
        tcp.adr:=serverAddr;
        tcp.prt:=serverPort;
        tcp.dat:=serverPort;
        pipeLineSend(con.pipe1,tcp,sizeof(tcp));
        con.stat:=200;
        end;
      0:begin; {sendAddr}
        i:=getByte;
        move(con.adr,a,sizeof(con.adr));
        if (i=0) then a:=ipAddr2string(a) else a:=chr(ab[i-1]);
        pipeLineSend(con.pipe2,a[1],length(a));
        end;
      1:begin; {sendPort}
        i:=getByte;
        WriteLongMSB(a,con.prt);
        if (i=0) then a:=BStr(con.prt) else a:=chr(ab[i-1]);
        pipeLineSend(con.pipe2,a[1],length(a));
        end;
      2:begin; {sendText}
        ab0:=getByte;
        for i:=1 to ab0 do ab[i]:=getByte;
        pipeLineSend(con.pipe2,a[1],length(a));
        end;
      3:begin; {recvBytes}
        con.tmp:=getByte;
        con.stat:=4;
        end;
      4:begin; {waitFor}
        con.tmp:=getByte;
        con.stat:=5;
        end;
      5:begin; {testNext}
        con.tmp:=getByte;
        con.stat:=6;
        end;
      6:begin; {recvPasStr}
        con.stat:=7;
        end;
      7:begin; {recvAscZstr}
        con.stat:=5;
        con.tmp:=0;
        end;
      else begin; Writeln('bug!');goto f1; end;
      end;
    end;
  2:begin; {connect}
    if TCPlookConnected(con.pipe2,a,i,o) then begin;
      if (con.pipe2=0) then goto f1;
      exit;
      end;
    WriteLn('local side is '+ipAddr2string(a)+' '+BStr(i)+'...');
    con.stat:=3;
    con.pos:=0;
    end;
  1:begin; {request}
    pipeLineStats(con.pipe1,i,r1,t1);
    if (i=0) then goto f1;
    if (r1<sizeof(tcp)) then exit;
    i:=sizeof(tcp);
    pipeLineRecv(con.pipe1,tcp,i);
    if (i<sizeof(tcp)) then goto f1;
    if (tcp.cmd<>1) then goto f1;
    con.adr:=tcp.adr;
    con.prt:=tcp.prt;
    WriteLn('connecting '+ipAddr2string(con.adr)+' '+BStr(con.prt)+'...');
    TCPbeginConnect(con.pipe2,t1*2+128,serverAddr,serverPort);
    con.stat:=2;
    end;
  0:goto f1; {disconnect}
  else begin; f1: con.stat:=0;doConn:=True;exit; end;
  end;
End;
