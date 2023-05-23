Procedure gotOnePacket(var addr;port:LongInt;var data;size:LongInt);
Var
  buf:array[1..1] of byte absolute data;
  i,o,p:LongInt;
  a,b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  con:OneConnectionRecord;
  usr:LongInt;
  act,typ:String;

Procedure qikRep(a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
a:='12345678'+a;
move(data,ab[1],8);
UDPsendPacket(servPipe,addr,port,a[1],ab0);
End;

Procedure adder(i:longint);
Var b,c:String;
Begin;
if parseXMLadd(i,b,c) then exit;
if (b=#13'tunnel'#13'action') then act:=c;
if (b=#13'tunnel'#13'type') then typ:=c;
End;

Procedure addCl;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
a:='Content-length: '+BStr(size-8)+#13#10;
move(buf[9],buf[ab0+9],size-8);
move(ab[1],buf[9],ab0);
inc(size,ab0);
End;

Procedure addStr(a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
move(ab[1],buf[size+1],ab0);
inc(size,ab0);
End;

Procedure sendInfo;
Var
  a:string;
  i,o:LongInt;
Begin;
size:=8;
o:=ConnectionDat^[usr].perV;
if isAddressIPv4mask(addr) then i:=4 else i:=6;
if (ConnectionDat^[usr].perV=4) then a:=ipAddr2string(ip4addr) else a:=ipAddr2string(ip6addr);
addStr('200 Success'#13#10);
addStr('<tunnel action="info" type="v'+BStr(o)+'udpv'+BStr(i)+'" lifetime="604800">');
addStr('<server>');
addStr('<address type="ipv4">'+ipAddr2string(ip4addr)+'</address>');
addStr('<address type="ipv6">'+ipAddr2string(ip6addr)+'</address>');
addStr('</server>');
addStr('<client>');
addStr('<address type="ipv'+BStr(o)+'">'+ipAddr2string(ConnectionDat^[usr].peer)+'</address>');
addStr('<keepalive interval="30">');
addStr('<address type="ipv'+BStr(o)+'">'+a+'</address>');
addStr('</keepalive>');
addStr('</client>');
addStr('</tunnel>'#13#10);
addCl;
UDPsendPacket(servPipe,addr,port,buf,size);
End;


Begin;
usr:=0;
for i:=1 to ConnectionNum do begin;
  if (ConnectionDat^[i].port<>port) then continue;
  if TCPcompareAddress(ConnectionDat^[i].addr,addr) then usr:=i;
  end;
if (usr<>0) then begin;
  con:=ConnectionDat^[usr];
  case con.stat of
    4:begin; {connected}
      if (con.perV=6) then begin;
        move(buf[1],buf[21],size);
        i:=0;
        move(i,buf,sizeof(i));
        move(con.peer,buf[5],sizeof(con.peer));
        inc(size,20);
        end else begin;
        move(buf[1],buf[5],size);
        move(con.peer[13],buf,4);
        inc(size,4);
        end;
      pipeLineSend(con.pipe,buf,size);
      con.timR:=currentTime;
      end;
    1:begin; {wait for acknowledge}
      {$ifdef debug}
      WriteLn('got possibly acknowledgement from '+ipAddr2string(addr)+' '+BStr(port));
      {$endif}
      dec(size,8);
      i:=size;
      if (i>255) then i:=255;
      move(buf[9],ab[1],i);
      ab0:=i;
      a:=kicsi(a);
      p:=pos(#13,a);
      if (p<1) then begin;
        WriteLn('got malformed tunnel req from '+ipAddr2string(addr)+' '+BStr(port));
        exit;
        end;
      ab0:=p-1;
      dec(size,p);
      inc(p,8);
      i:=pos(':',a);
      b:=copy(a,1,i-1);
      a:=copy(a,i+1,666);
      parseXMLcut(a);
      parseXMLcut(b);
      if (b<>'content-length') then begin;
        WriteLn('got invalid tunnel req from '+ipAddr2string(addr)+' '+BStr(port));
        exit;
        end;
      i:=BVal(a);
      if (i>size) then begin;
        WriteLn('got truncated tunnel req from '+ipAddr2string(addr)+' '+BStr(port));
        exit;
        end;
      size:=i;
      act:='';
      typ:='';
      parseXMLinit;
      for i:=p to p+size do adder(buf[i]);
      {$ifdef debug}
      WriteLn('action='+act+' type='+typ);
      {$endif}
      a:=kicsi(act);
      if (a='create') then begin; sendInfo;exit; end;
      if (a<>'accept') then begin;
        qikRep('302 invalid tunnel action requested'#13#10);
        WriteLn('got unknown tunnel req from '+ipAddr2string(addr)+' '+BStr(port));
        exit;
        end;
      con.stat:=2;
      qikRep('');
      end;
    2:; {wait for free}
    3:; {wait for upper}
    else con.stat:=999;
    end;
  ConnectionDat^[usr]:=con;
  exit;
  end;

o:=ReadLongLSB(data);
dec(size,8);
i:=size;
if (i>255) then i:=255;
move(buf[9],ab[1],i);
ab0:=i;
a:=kicsi(a);
b:='version=2.0.0';
if (copy(a,1,length(b))=b) then begin;
  {$ifdef debug}
  WriteLn('got version from '+ipAddr2string(addr)+' '+BStr(port));
  {$endif}
  qikRep('CAPABILITY TUNNEL=V4UDPV4 TUNNEL=V4UDPV6 TUNNEL=V6UDPV4 TUNNEL=V6UDPV6 AUTH=ANONYMOUS AUTH=PLAIN'#13#10);
  exit;
  end;
b:='authenticate anonymous';
if (copy(a,1,length(b))=b) then begin;
  {$ifdef debug}
  WriteLn('got auth anonym from '+ipAddr2string(addr)+' '+BStr(port));
  {$endif}
  qikRep('200 Success'#13#10);
  exit;
  end;
b:='authenticate plain';
if (copy(a,1,length(b))=b) then begin;
  {$ifdef debug}
  WriteLn('got auth req from '+ipAddr2string(addr)+' '+BStr(port));
  {$endif}
  qikRep('');
  exit;
  end;
if (o=$f0000002) and (ab0>1) and (ab[1]=0) then begin;
  {$ifdef debug}
  a:=copy(a,2,666);
  i:=pos(#0,a);
  WriteLn('got user='+copy(a,1,i-1)+' pass='+copy(a,i+1,666)+' from '+ipAddr2string(addr)+' '+BStr(port));
  {$endif}
  qikRep('200 Success'#13#10);
  exit;
  end;
b:='content-length';
if (copy(a,1,length(b))=b) then begin;
  {$ifdef debug}
  WriteLn('got tunnel req from '+ipAddr2string(addr)+' '+BStr(port));
  {$endif}
  p:=pos(#13,a);
  if (p<1) then begin;
    WriteLn('got malformed tunnel req from '+ipAddr2string(addr)+' '+BStr(port));
    exit;
    end;
  ab0:=p-1;
  dec(size,p);
  inc(p,8);
  a:=copy(a,pos(':',a)+1,666);
  parseXMLcut(a);
  i:=BVal(a);
  if (i>size) then begin;
    WriteLn('got truncated tunnel req from '+ipAddr2string(addr)+' '+BStr(port));
    exit;
    end;
  size:=i;
  act:='';
  typ:='';
  parseXMLinit;
  for i:=p to p+size do adder(buf[i]);
  {$ifdef debug}
  WriteLn('action='+act+' type='+typ);
  {$endif}
  if (kicsi(act)<>'create') then begin;
    qikRep('302 invalid tunnel action requested'#13#10);
    exit;
    end;
  a:=kicsi(copy(typ,1,5));
  p:=0;
  if (a='v4udp') then p:=4;
  if (a='v6udp') then p:=6;
  if (p<1) then begin;
    qikRep('302 invalid tunnel type requested'#13#10);
    exit;
    end;
  usr:=0;
  for i:=1 to ConnectionNum do begin;
    if (ConnectionDat^[i].stat<>0) then continue;
    if (ConnectionDat^[i].perV<>p) then continue;
    usr:=i;
    end;
  if (usr<1) then begin;
    qikRep('502 no free slots in this kind of address, try later'#13#10);
    WriteLn('failed to find free ipv'+BStr(p)+' address!');
    exit;
    end;
  con:=ConnectionDat^[usr];
  move(addr,con.addr,sizeof(con.addr));
  con.port:=port;
  con.stat:=1;
  con.timR:=currentTime;
  con.timT:=currentTime;
  ConnectionDat^[usr]:=con;
  sendInfo;
  exit;
  end;
WriteLn('got unknown request from '+ipAddr2string(addr)+' '+BStr(port));
End;



Procedure relequishConn(var con:OneConnectionRecord;usr:LongInt);
Var
  buf:array[1..2*1024] of byte;
  a,b:String;
  i,o:LongInt;
Begin;
if (con.stat=0) then exit;
if (getTimePast(con.timR)>60*5) then begin;
  WriteLn('timeout with '+ipAddr2string(con.addr)+' '+BStr(con.port));
  con.stat:=999;
  end;
case con.stat of
  4:begin; {connected}
    if (getTimePast(con.timT)>30) then begin;
      {$ifdef debug}
      WriteLn('sending keepalive packet...');
      {$endif}
      a:='ping';
      UDPsendPacket(servPipe,con.peer,7,a[1],length(a));
      con.timT:=currentTime;
      end;
    o:=sizeof(buf);
    if (pipeLineRecv(con.pipe,buf,o)<>0) then o:=0;
    if (o<1) then begin;
      pipeLineStats(con.pipe,o,i,i);
      if (o=0) then con.stat:=999;
      exit;
      end;
    if (con.perV=6) then begin;
      move(buf,i,sizeof(i));
      if (i<>0) then exit;
      dec(o,20);
      move(buf[21],buf,o);
      end else begin;
      dec(o,4);
      move(buf[5],buf,o);
      end;
    UDPsendPacket(servPipe,con.addr,con.port,buf,o);
    exit;
    end;
  3:; {waiting for upper to open}
  2:begin; {waiting for upper to be free}
    if (upperCon<>0) then exit;
    con.timR:=currentTime;
    con.timT:=currentTime;
    upperCon:=usr;
    upperTim:=currentTime;
    if (con.perV=4) then a:=ip4conf else a:=ip6conf;
    BugOS_MyProcessInfo(o,i,i);
    b:='iface add '+BStr(o)+' tsp'+BStr(usr)+' with '+ipAddr2string(con.addr)+' '+BStr(con.port);
    {$ifdef debug}
    WriteLn('executing '+a+' '+b+'...');
    {$endif}
    xExecBgnd(a,b,i);
    con.stat:=3;
    end;
  1:; {waiting for accept}
  0:; {free socket}
  else begin;
    pipeLineClose(con.pipe);
    con.pipe:=0;
    con.stat:=0;
    end;
  end;
End;



Procedure relequishUpper;
Begin;
if (upperCon=0) then exit;
if (getTimePast(upperTim)<15) then exit;
ConnectionDat^[upperCon].stat:=999;
upperCon:=0;
WriteLn('upper failed to open pipeline!');
End;
