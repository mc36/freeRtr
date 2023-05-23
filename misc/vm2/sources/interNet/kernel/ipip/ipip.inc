Procedure gotNewIncomingConnection(pipe:LongInt);
Label f1;
Var
  i,o,p:LongInt;
  a:String;
Begin;
for i:=1 to ConnectionNum do if (ConnectionDat[i].pipe=0) then goto f1;
pipeLineClose(pipe);
exit;
f1:
ConnectionDat[i].pipe:=pipe;
a:=ipAddr2string(ConnectionDat[i].addr);
WriteLn('upper with '+a+' logged in!');
a:='ipip with '+a;
BugOS_MyProcessInfo(i,o,p);
i:=i xor p xor o;
p:=((i shr 24) xor (i shr 16) xor (i shr 8) xor i) and $ff;
a:='12341234'#0#0#0#0#0#0#0#0+chr(p)+#255+a+#0;
i:=1;move(i,a[1],sizeof(i));
i:=1400;move(i,a[5],sizeof(i));
pipeLineSend(pipe,a[1],length(a));
End;



Procedure relequish2connection(var con:OneConnectionRecord);
Var
  buf:array[1..4096] of byte;
  i,o,p:LongInt;
Begin;
if (con.pipe=0) then exit;
p:=sizeof(buf);
if (pipeLineRecv(con.pipe,buf,p)<>0) then p:=0;
if (p<1) then begin;
  pipeLineStats(con.pipe,p,o,o);
  if (p<>0) then exit;
  pipeLineClose(con.pipe);
  con.pipe:=0;
  exit;
  end;
i:=ReadWordMSB(buf[2]);
o:=0;
case i of
  $800:begin; i:=1;o:=ip4pipe; end;
  $86dd:begin; i:=2;o:=ip6pipe; end;
  else begin;
    WriteLn('upper wanted to send unknown type to '+ipAddr2string(con.addr));
    exit;
    end;
  end;
if (con.mode and i=0) then begin;
  WriteLn('upper wanted to send unallowed type to '+ipAddr2string(con.addr));
  exit;
  end;
UDPsendPacket(o,con.addr,0,buf[4],p-3);
End;



Procedure gotOnePacket(var buffer;siz,typ:LongInt;var addr);
Var
  i,o,p:Longint;
  buf:array[1..1] of byte absolute buffer;
Begin;
p:=findOnePeerAddr(addr);
if (p<1) then begin;
  WriteLn('got packet from unknown host: '+ipAddr2string(addr));
  exit;
  end;
o:=ConnectionDat[p].pipe;
if (o=0) then begin;
  WriteLn('got packet for closed upper from '+ipAddr2string(addr));
  exit;
  end;
if (typ=$800) then i:=1 else i:=2;
if (ConnectionDat[p].mode and i=0) then begin;
  WriteLn('got packet with unallowed protocol from '+ipAddr2string(addr));
  exit;
  end;
buf[1]:=11;
WriteWordMSB(buf[2],typ);
pipeLineSend(o,buf,siz+3);
End;
