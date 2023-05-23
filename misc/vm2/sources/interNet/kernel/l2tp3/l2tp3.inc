Const
  avpConnID=$90001;     {61}
  avpCapaLS=$90002;     {62}
  avpSessLc=$90003;     {63}
  avpSessRm=$90004;     {64}
  avpSessTp=$90007;     {68}
  avpTieBrk=$90009;     {5}


Procedure gotNewIncomingConnection(pipe:LongInt);
Label f1;
Var
  i,o,p:LongInt;
  a,b:String;
Begin;
for i:=1 to SessionNum do if (SessionDat[i].pipe=0) then goto f1;
pipeLineClose(pipe);
exit;
f1:
SessionDat[i].pipe:=pipe;
a:=ipAddr2string(SessionDat[i].addr)+' '+BStr(SessionDat[i].vcid);
WriteLn('upper with '+a+' logged in!');
a:='l2tp3 with '+a;
if (SessionDat[i].mode in [4,5]) then begin;
  b:='';
  for i:=1 to sizeof(macAddr) do b:=b+chr(macAddr[i]);
  for i:=1 to sizeof(macAddr) do b:=b+#255;
  i:=sizeof(macAddr);
  end else begin;
  b:=chr(macAddr[sizeof(macAddr)])+#255;
  i:=1;
  end;
a:='12341234'#0#0#0#0#0#0#0#0+b+a+#0;
move(i,a[1],sizeof(i));
i:=1400;move(i,a[5],sizeof(i));
pipeLineSend(pipe,a[1],length(a));
End;




Procedure relequish2sess(var ses:OneSessionRecord);
Var
  buf:array[1..4096] of byte;
  con:OneConnectionRecord;
  a,b:String;
  ab:array[0..128] of byte absolute a;
  ab0:byte absolute a;
  i,o,p,q:LongInt;

Function send:Boolean;
Begin;
send:=false;
if (con.bufS<>0) then exit;
if (con.stat<>100) then exit;
move(buf,con.bufD,p);
con.bufS:=p;
con.stat:=101;
ses.time:=currentTime;
ConnectionDat[ses.conn]:=con;
send:=true;
End;

Begin;
con:=ConnectionDat[ses.conn];
if (con.bufS<>0) then exit;
if (con.stat<>100) then exit;
case ses.stat of
  100:begin; {connected}
    end;
  0:begin; {none}
    {$ifdef debug}writeln('sending session request');{$endif}
    clearOneSess(ses);
    generateSessID(ses);
    ab0:=sizeof(ses.locI);
    move(ses.locI,ab[1],sizeof(ses.locI));
    b:=a;
    p:=0;
    placeOneAVP(buf,p,0,#0#10);         {type}
    i:=random($7fffffff);
    ab0:=sizeof(i);
    move(i,ab[1],sizeof(i));
    placeOneAVP(buf,p,15,a);            {serial number}
    a:=BStr(ses.vcid);
    placeOneAVP(buf,p,66,a);            {remote end id}
    placeOneAVP(buf,p,70,#0#0);         {sequencing}
    placeOneAVP(buf,p,71,#0#1);         {circuit status}
    placeOneAVP(buf,p,avpTieBrk,b+b);   {session tie breaker}
    a:=#0+chr(ses.mode);
    placeOneAVP(buf,p,avpSessTp,a);     {pw type}
    placeOneAVP(buf,p,avpSessLc,b);     {local session id}
    ab0:=sizeof(ses.remI);
    move(ses.remI,ab[1],sizeof(ses.remI));
    placeOneAVP(buf,p,avpSessRm,a);     {remote session id}
    if send then ses.stat:=1;
    end;
  1:begin; {sent reqest}
    end;
  2:begin; {got reqest}
    {$ifdef debug}writeln('sending session reply');{$endif}
    generateSessID(ses);
    ab0:=sizeof(ses.locI);
    move(ses.locI,ab[1],sizeof(ses.locI));
    b:=a;
    p:=0;
    placeOneAVP(buf,p,0,#0#11);         {type}
    i:=random($7fffffff);
    ab0:=sizeof(i);
    move(i,ab[1],sizeof(i));
    placeOneAVP(buf,p,15,a);            {serial number}
    a:=BStr(ses.vcid);
    placeOneAVP(buf,p,66,a);            {remote end id}
    placeOneAVP(buf,p,70,#0#0);         {sequencing}
    placeOneAVP(buf,p,71,#0#1);         {circuit status}
    placeOneAVP(buf,p,avpTieBrk,b+b);   {session tie breaker}
    a:=#0+chr(ses.mode);
    placeOneAVP(buf,p,avpSessTp,a);     {pw type}
    placeOneAVP(buf,p,avpSessLc,b);     {local session id}
    ab0:=sizeof(ses.remI);
    move(ses.remI,ab[1],sizeof(ses.remI));
    placeOneAVP(buf,p,avpSessRm,a);     {remote session id}
    if send then ses.stat:=3;
    end;
  3:begin; {sent reply}
    end;
  4:begin; {got reply}
    {$ifdef debug}writeln('sending session ok');{$endif}
    p:=0;
    placeOneAVP(buf,p,0,#0#12);         {type}
    i:=random($7fffffff);
    a:=BStr(ses.vcid);
    placeOneAVP(buf,p,70,#0#0);         {sequencing}
    placeOneAVP(buf,p,71,#0#1);         {circuit status}
    placeOneAVP(buf,p,24,#0#0#0#0);     {connect speed}
    ab0:=sizeof(ses.locI);
    move(ses.locI,ab[1],ab0);
    placeOneAVP(buf,p,avpSessLc,a);     {local session id}
    move(ses.remI,ab[1],ab0);
    placeOneAVP(buf,p,avpSessRm,a);     {remote session id}
    if send then ses.stat:=100;
    end;
  5:begin; {got disconnect}
    if (getTimePast(ses.time)<5) then exit;
    clearOneSess(ses);
    end;
  else begin;
    clearOneSess(ses);
    end;
  end;
if (ses.pipe=0) then exit;
p:=sizeof(buf);
if (pipeLineRecv(ses.pipe,buf,p)<>0) then p:=0;
if (p<1) then begin;
  pipeLineStats(ses.pipe,p,o,o);
  if (p<>0) then exit;
  pipeLineClose(ses.pipe);
  ses.pipe:=0;
  exit;
  end;
if (ses.stat<>100) then exit;
case ses.mode of
  11:begin;   {ip}
    move(buf[2],buf[5],p);
    inc(p,3);
    end;
  4,5:begin;  {vlan, eth}
    move(buf[sizeof(macAddr)+1],buf[sizeof(macAddr)*2+5],p);
    move(buf,buf[5],sizeof(macAddr));
    move(macAddr,buf[sizeof(macAddr)+5],sizeof(macAddr));
    inc(p,sizeof(macAddr)+4);
    end;
  6:begin;    {hdlc}
    move(buf,buf[5],p);
    inc(p,4);
    end;
  7:begin;    {ppp}
    move(buf,buf[3],p);
    inc(p,2);
    end;
  else exit;
  end;
move(ses.remI,buf,sizeof(i));
UDPsendPacket(pipe,ses.addr,0,buf,p);
End;




Procedure relequish2conn(var con:OneConnectionRecord);
Var
  buf:array[1..4096] of byte;
  a,b:String;
  ab:array[0..128] of byte absolute a;
  ab0:byte absolute a;
  i,o,p,q:LongInt;

Procedure mkHdr;
Begin;
move(buf,buf[17],p);
inc(p,16);
i:=0;
move(i,buf,sizeof(i));
buf[5]:=$c8;
buf[6]:=3;
WriteWordMSB(buf[7],p-4);
move(con.remI,buf[9],sizeof(i));
WriteWordMSB(buf[13],con.pckT);
WriteWordMSB(buf[15],con.pckR);
End;

Procedure savBuf;
Begin;
if (con.bufS>0) then exit;
move(buf,con.bufD,p);
con.bufS:=p;
con.rtry:=-1;
con.timT:=-99999;
End;

Begin;
if con.need then begin;
  con.need:=false;
  p:=0;
  mkHdr;
  UDPsendPacket(pipe,con.addr,0,buf,p);
  {$ifdef debug}writeln('sent ack');{$endif}
  exit;
  end;
if (con.bufS<1) then if (getTimePast(con.timR)>15) then begin;
  {$ifdef debug}writeln('sending hello request');{$endif}
  con.timR:=currentTime;
  p:=0;
  placeOneAVP(buf,p,0,#0#6);            {type}
  mkHdr;
  savBuf;
  end;
if (con.bufS>0) and (con.stat<=100) then if (getTimePast(con.timT)>5) then begin;
  inc(con.rtry);
  if (con.rtry>15) then begin;
    WriteLn('timeout sending to '+ipAddr2string(con.addr));
    clearOneConn(con);
    exit;
    end;
  UDPsendPacket(pipe,con.addr,0,con.bufD,con.bufS);
  con.timT:=currentTime;
  {$ifdef debug}writeln('resent packet');{$endif}
  end;
case con.stat of
  100:begin; {connected}
    end;
  101:begin; {prepare buffer}
    {$ifdef debug}writeln('sending session packet');{$endif}
    p:=con.bufS;
    move(con.bufD,buf,p);
    con.bufS:=0;
    con.stat:=100;
    mkHdr;
    savBuf;
    end;
  0:begin; {nothing happened}
    clearOneConn(con);
    if (con.bufS<>0) then exit;
    {$ifdef debug}writeln('sending start request');{$endif}
    generateConnID(con);
    con.stat:=1;
    ab0:=sizeof(con.locI);
    move(con.locI,ab[1],ab0);
    b:=a;
    p:=0;
    placeOneAVP(buf,p,0,#0#1);          {type}
    placeOneAVP(buf,p,2,#1#0);          {ver}
    placeOneAVP(buf,p,3,#0#0#0#0);      {framing}
    placeOneAVP(buf,p,5,b+b);           {tie breaker}
    placeOneAVP(buf,p,7,hostName);      {host name}
    placeOneAVP(buf,p,8,proggyName);    {vendor name}
    placeOneAVP(buf,p,10,#0#1);         {window}
    ab0:=sizeof(addr);
    move(addr,ab[1],sizeof(addr));
    if isAddressIPv4mask(addr) then a:=copy(a,length(IPv4addressPrefix)+1,666);
    placeOneAVP(buf,p,60,a);            {router id}
    placeOneAVP(buf,p,avpConnID,b);     {control conn id}
    a:=#0#5#0#4#0#6#0#7#0#11;
    placeOneAVP(buf,p,avpCapaLS,a);     {pw capa list}
    placeOneAVP(buf,p,$9000a,#0#1);     {draft version}
    mkHdr;
    savBuf;
    end;
  1:begin; {sent sccrq}
    end;
  2:begin; {got sccrq}
    if (con.bufS<>0) then exit;
    {$ifdef debug}writeln('sending start reply');{$endif}
    generateConnID(con);
    con.stat:=3;
    ab0:=sizeof(con.locI);
    move(con.locI,ab[1],ab0);
    b:=a;
    p:=0;
    placeOneAVP(buf,p,0,#0#2);          {type}
    placeOneAVP(buf,p,2,#1#0);          {ver}
    placeOneAVP(buf,p,3,#0#0#0#0);      {framing}
    placeOneAVP(buf,p,5,b+b);           {tie breaker}
    placeOneAVP(buf,p,7,hostName);      {host name}
    placeOneAVP(buf,p,8,proggyName);    {vendor name}
    placeOneAVP(buf,p,10,#0#1);         {window}
    ab0:=sizeof(addr);
    move(addr,ab[1],sizeof(addr));
    if isAddressIPv4mask(addr) then a:=copy(a,length(IPv4addressPrefix)+1,666);
    placeOneAVP(buf,p,60,a);            {router id}
    placeOneAVP(buf,p,avpConnID,b);     {control conn id}
    a:=#0#5#0#4#0#6#0#7#0#11;
    placeOneAVP(buf,p,avpCapaLS,a);     {pw capa list}
    placeOneAVP(buf,p,$9000a,#0#1);     {draft version}
    mkHdr;
    savBuf;
    end;
  3:begin; {sent sccrp}
    end;
  4:begin; {got sccrp}
    if (con.bufS<>0) then exit;
    {$ifdef debug}writeln('sending start ok');{$endif}
    con.stat:=100;
    p:=0;
    placeOneAVP(buf,p,0,#0#3);          {type}
    mkHdr;
    savBuf;
    clearSessOfConn(con);
    end;
  5:begin; {got stop}
    con.bufS:=0;
    end;
  else begin;
    clearOneConn(con);
    end;
  end;
End;



Procedure gotOnePacket(var buffer;siz:LongInt;var addr);
Label f1,f2,f3;
Var
  buf:array[1..1] of byte absolute buffer;
  con:OneConnectionRecord;
  ses:OneSessionRecord;
  num,i,o,p,q:Longint;
  a,b:String;
  ab:array[0..128] of byte absolute a;
  ab0:byte absolute a;
  typ,conn,seLoc,seRem,remId,pwTyp:LongInt;

Procedure discSess;
Begin;
{$ifdef debug}WriteLn('sending session disconnect');{$endif}
if (con.bufS<>0) then exit;
p:=0;
placeOneAVP(buf,p,0,#0#14);             {type}
ab0:=sizeof(i);
move(seLoc,ab[1],sizeof(i));
placeOneAVP(buf,p,64,a);                {remote session id}
move(seRem,ab[1],sizeof(i));
placeOneAVP(buf,p,63,a);                {local session id}
placeOneAVP(buf,p,1,#0#1'close');       {result code}
move(buf,con.bufD,p);
con.bufS:=p;
con.stat:=101;
ConnectionDat[num]:=con;
End;

Begin;
if (siz<4) then begin;
  WriteLn('got too small packet from '+ipAddr2string(addr));
  exit;
  end;
{$ifdef debug}WriteLn('got packet from '+ipAddr2string(addr));{$endif}
move(buf,i,sizeof(i));
if (i<>0) then begin;
  conn:=buf[4];
  if (conn<1) or (conn>SessionNum) then begin;
    WriteLn('got bad session id from '+ipAddr2string(addr));
    exit;
    end;
  ses:=SessionDat[conn];
  if (ses.locI<>i) then begin;
    WriteLn('got invalid session id from '+ipAddr2string(addr));
    exit;
    end;
  if not TCPcompareAddress(addr,ses.addr) then begin;
    WriteLn('got data from bad address '+ipAddr2string(addr));
    exit;
    end;
  if (ses.stat<>100) then begin;
    WriteLn('got for closed session from '+ipAddr2string(addr));
    exit;
    end;
  if (ses.pipe=0) then exit;
  case ses.mode of
    11:begin;   {ip}
      buf[4]:=11;
      p:=3;
      end;
    4,5:begin;  {vlan, eth}
      p:=10;
      end;
    6:begin;    {hdlc}
      p:=4;
      end;
    7:begin;    {ppp}
      buf[4]:=3;
      dec(p);
      buf[3]:=11;
      p:=2;
      end;
    else exit;
    end;
  dec(siz,p);
  if (siz<1) then begin;
    WriteLn('got truncated packet from '+ipAddr2string(addr));
    exit;
    end;
  pipeLineSend(ses.pipe,buf[p+1],siz);
  exit;
  end;
num:=findOnePeerAddr(addr);
if (num<1) then begin;
  WriteLn('got packet from unknown peer '+ipAddr2string(addr));
  exit;
  end;
con:=ConnectionDat[num];
if (buf[5]<>$c8) then begin;
  WriteLn('got unknown flags from '+ipAddr2string(addr));
  exit;
  end;
if (buf[6]<>3) then begin;
  WriteLn('got unknown version from '+ipAddr2string(addr));
  exit;
  end;
i:=ReadWordMSB(buf[7])+4;
if (i>siz) then begin;
  WriteLn('got truncated packet from '+ipAddr2string(addr));
  exit;
  end;
siz:=i;
move(buf[9],i,sizeof(i));
if (i=0) then if (con.stat=1) then i:=con.locI;
if (i<>con.locI) then begin;
  WriteLn('got unknown connect id from '+ipAddr2string(addr));
  exit;
  end;
i:=(ReadWordMSB(buf[13])-con.pckR) and $ffff;
if (i<>0) then begin;
  if (i>$8000) then a:='previous' else a:='future';
  WriteLn('got '+a+' packet from '+ipAddr2string(addr));
  exit;
  end;
if (con.bufS<>0) then i:=1 else i:=0;
if (ReadWordMSB(buf[15])=(con.pckT+i) and $ffff) then begin;
  {$ifdef debug}WriteLn('got good Nr field...');{$endif}
  if (con.bufS<>0) then inc(con.pckT);
  con.bufS:=0;
  con.rtry:=0;
  end;
inc(con.pckR);
con.timR:=currentTime;
con.need:=true;
ConnectionDat[num]:=con;
p:=16;
typ:=20;
conn:=0;
seRem:=0;
remId:=0;
pwTyp:=0;
b:='';
f1:
if (p>=siz) then goto f2;
a:=readOneAVP(buf,p,i);
case i of
  0:typ:=ReadWordMSB(ab[1]);
  avpTieBrk:b:=a;
  avpConnID:move(ab[1],conn,sizeof(conn));
  avpSessLc:move(ab[1],seLoc,sizeof(seLoc));
  avpSessRm:move(ab[1],seRem,sizeof(seRem));
  66:remId:=BVal(a);
  avpSessTp:pwTyp:=ReadWordMSB(ab[1]);
  end;
goto f1;
f2:

case typ of
  20:begin; {ack}
    {$ifdef debug}writeln('got ack');{$endif}
    dec(con.pckR);
    con.need:=false;
    ConnectionDat[num]:=con;
    exit;
    end;
  6:begin; {hello}
    {$ifdef debug}writeln('got hello');{$endif}
    exit;
    end;
  7,10:begin; {sess req}
    {$ifdef debug}writeln('got sess request');{$endif}
    conn:=findOneSessionVC(con,remId);
    if (conn<1) then begin; discSess;exit; end;
    ses:=SessionDat[conn];
    if (ses.stat=1) then begin;
      ab0:=sizeof(ses.locI);
      move(ses.locI,ab[1],ab0);
      a:=a+a;
      if (a<b) then exit;
      clearOneSess(ses);
      end;
    if (ses.stat<>0) then exit;
    ses.stat:=2;
    ses.remI:=seLoc;
    ses.time:=currentTime;
    SessionDat[conn]:=ses;
    exit;
    end;
  8,11:begin; {sess rep}
    {$ifdef debug}writeln('got sess reply');{$endif}
    conn:=findOneSessionID(con,seRem);
    if (conn<1) then begin; discSess;exit; end;
    ses:=SessionDat[conn];
    if (ses.stat<>1) then exit;
    ses.stat:=4;
    ses.remI:=seLoc;
    ses.time:=currentTime;
    SessionDat[conn]:=ses;
    exit;
    end;
  9,12:begin; {sess ok}
    {$ifdef debug}writeln('got sess ok');{$endif}
    conn:=findOneSessionID(con,seRem);
    if (conn<1) then begin; discSess;exit; end;
    ses:=SessionDat[conn];
    if (ses.stat<>3) then exit;
    ses.stat:=100;
    ses.time:=currentTime;
    SessionDat[conn]:=ses;
    exit;
    end;
  14:begin; {stop sess}
    {$ifdef debug}writeln('got stop sess');{$endif}
    conn:=findOneSessionID(con,seRem);
    if (conn<1) then exit;
    ses:=SessionDat[conn];
    ses.stat:=5;
    ses.time:=currentTime;
    SessionDat[conn]:=ses;
    exit;
    end;
  15,16:begin; {wen error, set link info}
    {$ifdef debug}writeln('got sess info');{$endif}
    conn:=findOneSessionID(con,seRem);
    if (conn<1) then begin; discSess;exit; end;
    ses:=SessionDat[conn];
    if (ses.stat<>100) then begin; discSess;exit; end;
    ses.time:=currentTime;
    exit;
    end;
  1:begin; {start req}
    {$ifdef debug}writeln('got start request');{$endif}
    if (con.stat=1) then begin;
      ab0:=sizeof(con.locI);
      move(con.locI,ab[1],ab0);
      a:=a+a;
      if (a<b) then exit;
      clearOneConn(con);
      inc(con.pckR);
      con.need:=true;
      end;
    if (con.stat<>0) then begin;
      WriteLn('got start for living connection from '+ipAddr2string(addr));
      exit;
      end;
    con.remI:=conn;
    con.stat:=2;
    con.timT:=-999999;
    ConnectionDat[num]:=con;
    exit;
    end;
  2:begin; {start rep}
    {$ifdef debug}writeln('got start reply');{$endif}
    if (con.stat<>1) then begin;
      WriteLn('got reply for living connection from '+ipAddr2string(addr));
      exit;
      end;
    con.remI:=conn;
    con.stat:=4;
    con.timT:=-999999;
    ConnectionDat[num]:=con;
    exit;
    end;
  3:begin; {start ok}
    {$ifdef debug}writeln('got start ok');{$endif}
    if (con.stat<>3) then begin;
      WriteLn('got ok for living connection from '+ipAddr2string(addr));
      exit;
      end;
    con.stat:=100;
    clearSessOfConn(con);
    ConnectionDat[num]:=con;
    exit;
    end;
  4:begin; {stop req}
    {$ifdef debug}writeln('got stop request');{$endif}
    clearSessOfConn(con);
    inc(con.rtry,3);
    con.stat:=5;
    con.bufS:=0;
    ConnectionDat[num]:=con;
    exit;
    end;
  else begin;
    WriteLn('got unknown type from '+ipAddr2string(addr));
    exit;
    end;
  end;

End;
