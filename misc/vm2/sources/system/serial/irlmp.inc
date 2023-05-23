Procedure upperClearStates;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do pipeLineClose(ConnectionDat[i].pipe);
upperQueueSiz:=0;
ConnectionNum:=0;
End;



Procedure upperSendCommand(src,trg,op:LongInt;a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
if (op<0) then a:='12'+a else a:='123'+a;
ab[1]:=trg and $7f;
ab[2]:=src and $7f;
if (op>=0) then begin;
  ab[3]:=op;
  inc(ab[1],$80);
  end;
if (upperQueueSiz>=upperQueueMax) then exit;
{$ifdef debug3}writeln('tx: dlsap='+BStr(ab[1] and $7f)+' slsap='+BStr(ab[2] and $7f)+' op='+BStr(ab[3]));{$endif}
inc(upperQueueSiz);
upperQueueBuf[upperQueueSiz]:=a;
if (packetOutputSiz=0) then lastSent:=-99999;
End;



Procedure upperGotOnePacket(var buffer;size:LongInt);
Label f1;
Var
  con:OneConnectionRecord;
  buf:array[1..1] of byte absolute buffer;
  i,o,p:LongInt;
  a:String;
Begin;
if (buf[1] and $80=0) then begin;
  {$ifdef debug3}writeln('got data frame...');{$endif}
  p:=findOneConnection(buf[1] and $7f,buf[2] and $7f);
  if (p=0) then begin;
    WriteLn('got command for unknown connection!');
    goto f1;
    end;
  con:=ConnectionDat[p];
  if con.tty then begin;
    inc(con.wndR,buf[3]);
    inc(con.wndL);
    o:=3;
    end else o:=2;
  pipeLineSend(con.pipe,buf[o+1],size-o);
  ConnectionDat[p]:=con;
  exit;
  end;
{$ifdef debug3}writeln('rx: dlsap='+BStr(buf[1] and $7f)+' slsap='+BStr(buf[2] and $7f)+' op='+BStr(buf[3]));{$endif}
if (buf[3]=1) then begin;       {connect}
  {$ifdef debug3}writeln('got connect...');{$endif}
  p:=findOneConnection(-1,buf[1] and $7f);
  if (p=0) then begin;
    WriteLn('connection request for invalid port '+BStr(buf[2]));
    goto f1;
    end;
  con:=ConnectionDat[p];
  con.wndR:=buf[5];
  con.adrR:=buf[1] and $7f;
  con.stat:=100;
  i:=0;
  pipeLineSend(con.pipe,i,1);
  {$ifdef debug3}writeln('sending connect confirm...');{$endif}
  if con.tty then a:=#14 else a:='';
  upperSendCommand(con.adrL,con.adrR,$81,#0+a);
  ConnectionDat[p]:=con;
  exit;
  end;
p:=findOneConnection(buf[1] and $7f,buf[2] and $7f);
if (p=0) then begin;
  i:=buf[3];
  if (i and $7f=2) then exit;
  WriteLn('got command for unknown connection!');
  f1:
  {$ifdef debug3}writeln('sending disconnect...');{$endif}
  upperSendCommand(buf[1],buf[2],2,#1);
  exit;
  end;
con:=ConnectionDat[p];
case buf[3] of
  2:begin;      {disconnect}
    {$ifdef debug3}writeln('got disconnect...');{$endif}
    con.stat:=666;
    ConnectionDat[p]:=con;
    exit;
    end;
  $81:begin;    {connect confirm}
    {$ifdef debug3}writeln('got connect confirm...');{$endif}
    if (con.stat<>3) then goto f1;
    con.wndR:=buf[5];
    i:=0;
    pipeLineSend(con.pipe,i,1);
    con.stat:=100;
    ConnectionDat[p]:=con;
    exit;
    end;
  3:begin;      {access mode}
    {$ifdef debug3}writeln('got access mode...');{$endif}
    {$ifdef debug3}writeln('sending access confirm...');{$endif}
    upperSendCommand(con.adrL,con.adrR,$83,#0+chr(buf[5]));
    exit;
    end;
  $83:begin;    {access mode confirm}
    {$ifdef debug3}writeln('got access mode confirm...');{$endif}
    exit;
    end;
  end;
WriteLn('got unknown command');
con.stat:=666;
ConnectionDat[p]:=con;
End;



Function doOneUpperPeer(var con:OneConnectionRecord):Boolean;
Label f1;
Var
  i,o,p:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
doOneUpperPeer:=False;
if (upperQueueSiz>upperQueueMax shr 1) then exit;
case con.stat of
  100:begin;    {connected}
    if con.tty then if (con.wndR<1) then exit;
    o:=dataSize-8;
    pipeLineRecv(con.pipe,ab[1],o);
    if (o<1) then begin;
      pipeLineStats(con.pipe,p,i,o);
      if (o=0) then goto f1;
      exit;
      end;
    ab0:=o;
    {$ifdef debug3}writeln('sending data...');{$endif}
    if con.tty then begin;
      i:=con.wndL;
      if (i>63) then i:=63;
      a:='1'+a;
      ab[1]:=i;
      dec(con.wndL,i);
      dec(con.wndR);
      end;
    upperSendCommand(con.adrL,con.adrR,-1,a);
    end;
  3:begin;      {wait connect confirm}
    pipeLineStats(con.pipe,p,i,o);
    if (o=0) then goto f1;
    end;
  2:begin;      {listen}
    pipeLineStats(con.pipe,p,i,o);
    if (o=0) then goto f1;
    end;
  1:begin;      {command}
    pipeLineStats(con.pipe,p,i,o);
    if (o=0) then goto f1;
    if (i<2) then exit;
    o:=2;
    pipeLineRecv(con.pipe,a,o);
    if (o<>2) then goto f1;
    con.adrR:=ab[1];
    if (ab0 in [3,4]) then begin;
      con.tty:=true;
      dec(ab0,2);
      end;
    case ab0 of
      1:begin;  {connect}
        repeat
          con.adrL:=random(120)+1;
          i:=findOneConnection(con.adrL,con.adrR);
          until (i<>0);
        {$ifdef debug3}writeln('sending connect...');{$endif}
        if con.tty then a:=#14 else a:='';
        upperSendCommand(con.adrL,con.adrR,1,#0+a);
        con.stat:=3;
        end;
      2:begin;  {listen}
        con.adrL:=-1;
        con.stat:=2;
        {$ifdef debug3}writeln('listening on port '+BStr(con.adrL));{$endif}
        end;
      else goto f1;
      end;
    end;
  else begin;
    f1:
    doOneUpperPeer:=True;
    end;
  end;
End;



Procedure doUpperWork;
Var
  con:OneConnectionRecord;
  i,o:LongInt;
Begin;
for i:=ConnectionNum downto 1 do if doOneUpperPeer(ConnectionDat[i]) then begin;
  {$ifdef debug3}writeln('upper closed connection');{$endif}
  con:=ConnectionDat[i];
  pipeLineClose(con.pipe);
  if (con.stat>2) then upperSendCommand(con.adrL,con.adrR,2,#1);
  ConnectionDat[i]:=ConnectionDat[ConnectionNum];
  dec(ConnectionNum);
  end;
if (ConnectionNum>=ConnectionMax) then exit;
while (pipeLineGetIncoming(o)=0) do begin;
  fillchar(con,sizeof(con),0);
  con.pipe:=o;
  con.stat:=1;
  inc(ConnectionNum);
  ConnectionDat[ConnectionNum]:=con;
  {$ifdef debug3}writeln('accepted from upper');{$endif}
  end;
End;
