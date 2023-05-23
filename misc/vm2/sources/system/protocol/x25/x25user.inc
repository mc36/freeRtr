Procedure relequishToLOC;
Var
  con:OneConnectionRecord;
  tcp:OneTCPcommandHeader;
  buf:array[1..1024] of byte;


Function doer(var con:OneConnectionRecord):Boolean;
Label f1,f2;
Var
  a,b:String;
  i,o,p:LongInt;

Procedure send(a:String);
Begin;
pipeLineSend(con.pipe,a[1],length(a));
con.time:=CurrentTime;
con.retry:=16;
End;

Function input(var a:string):Boolean;
Label f1,f2;
Var
  i,o:LongInt;
  c:Char;
  b:Byte absolute c;
Begin;
f1:
input:=true;
i:=sizeof(b);
if (pipeLineRecv(con.pipe,b,i)<>0) then i:=0;
if (i<1) then begin;
  pipeLineStats(con.pipe,o,i,i);
  if (o=0) then con.stat:=0;
  exit;
  end;
con.time:=CurrentTime;
if (b=10) then goto f1;
if (b=13) then begin;
  input:=false;
  exit;
  end;
if (b=8) then begin;
  if (a='') then exit;
  a:=copy(a,1,length(a)-1);
  send(#8' '#8);
  goto f1;
  end;
if (b=27) then begin;
  o:=length(a);
  a:='';
  for i:=1 to o do a:=a+#8;
  for i:=1 to o do a:=a+' ';
  for i:=1 to o do a:=a+#8;
  send(a);
  a:='';
  goto f1;
  end;
c:=lowCase(c);
if (b>=$30) and (b<=$39) then goto f2;
if (b>=$61) and (b<=$66) then goto f2;
goto f1;
f2:
a:=a+c;
send(c);
goto f1;
End;


Begin;
if (GetTimePast(con.time)>5*60) then begin;
  send('timeout!'#13#10);
  goto f1;
  end;
doer:=false;
case con.stat of
  7:begin; {connection established}
    if (con.siz<1) then begin;
      i:=channelSiz;
      if (pipeLineRecv(con.pipe,con.buf,i)<>0) then i:=0;
      con.siz:=i;
      if (i<1) then begin;
        pipeLineStats(con.pipe,i,o,p);
        if (i=0) then goto f2;
        exit;
        end;
      con.time:=-9999;
      con.retry:=16;
      end;
    if (GetTimePast(con.time)<60) then exit;
    con.time:=currentTime;
    if (con.siz<1) then begin;
      sendRcvRdy(con);
      exit;
      end;
    dec(con.retry);
    if (con.retry<0) then goto f2;
    sendData(con);
    end;
  6:begin; {initiating connection}
    if (GetTimePast(con.time)<60) then exit;
    con.time:=currentTime;
    dec(con.retry);
    if (con.retry<0) then goto f2;
    sendCallReq(con);
    end;
  1:begin; {wait for tcp command}
    pipeLineStats(con.pipe,i,o,p);
    if (i=0) then goto f1;
    if (o<sizeof(tcp)) then exit;
    i:=sizeof(tcp);
    if (pipeLineRecv(con.pipe,tcp,i)<>0) then i:=0;
    if (i<>sizeof(tcp)) then exit;
    con.time:=CurrentTime;
    case tcp.cmd of
      1:begin;
        fillchar(tcp,sizeof(tcp),0);tcp.cmd:=3;
        pipeLineSend(con.pipe,tcp,sizeof(tcp));
        send('src:');
        con.stat:=2;
        exit;
        end;
      2:begin;
        if (listenerPipe<>0) then goto f1;
        pipeLineStats(con.pipe,p,i,o);
        BugOS_ProcessName(p,buf,i,o,p);
        if (p and $40=0) then goto f1;
        fillchar(tcp,sizeof(tcp),0);tcp.cmd:=3;
        pipeLineSend(con.pipe,tcp,sizeof(tcp));
        move(tcp.adr,listenerSize,sizeof(listenerSize));
        listenerPipe:=con.pipe;
        pipeLineStats(listenerPipe,listenerProc,i,o);
        con.pipe:=0;
        goto f1;
        end;
      end;
    goto f1;
    end;
  2:begin; {source address}
    if input(con.src) then exit;
    send(#13#10'trg:');
    con.stat:=3;
    end;
  3:begin; {target address}
    if input(con.trg) then exit;
    con.usr:='01000000';
    send(#13#10'usr:'+con.usr);
    con.stat:=4;
    end;
  4:begin; {user data}
    if input(con.usr) then exit;
    send(#13#10);
    con.stat:=5;
    end;
  5:begin; {find empty lcn}
    i:=findEmptyLCNnum;
    if (i=0) then exit;
    send('lcn:'+BStr(i)+#13#10);
    con.lcn:=i;
    con.stat:=6;
    con.retry:=10;
    con.time:=currentTime;
    sendCallReq(con);
    end;
  666:begin; {error}
    f2:
    con.stat:=666;
    doer:=true;
    end;
  else begin; f1:con.stat:=0;doer:=true; end;
  end;
End;


Var i,o,p,q,r:LongInt;
Begin;
while (pipeLineGetIncoming(i)=0) do begin;
  if ResizeMem(ConnectionNum+1) then begin;
    pipeLineClose(i);
    exit;
    end;
  fillchar(con,sizeof(con),0);
  con.stat:=1;
  con.pipe:=i;
  con.time:=CurrentTime;
  con.retry:=16;
  ConnectionDat^[ConnectionNum]:=con;
  end;
for i:=ConnectionNum downto 1 do if doer(ConnectionDat^[i]) then begin;
  con:=ConnectionDat^[i];
  if (con.stat>=6) then sendClearReq(con,0,0);
  pipeLineClose(con.pipe);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  end;

pipeLineStats(listenerPipe,o,i,i);
if (o=listenerProc) then exit;
listenerPipe:=0;
listenerProc:=0;
End;
