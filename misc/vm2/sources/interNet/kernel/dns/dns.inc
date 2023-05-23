Function doConn(var con:OneConnectionRecord;conN:LongInt):Boolean;
{0-none, 1-delete, 2-done}
Label f1,f2;
Const
  timeout=3;
  maxretry=5;
Var
  query:OneQuestionRecord;
  reply:OneResponseRecord;
  ip:OneTCPaddressRecord;
  pck:OnePacketRecord;
  phdr:OneQuestionPacketHeader;
  qhdr:OneQuestionHeader;
  i,o,p:LongInt;
  a,b:String;
Begin;
doConn:=False;
case con.stat of
  1:begin; {wait for question}
    i:=sizeof(query);
    if (pipeLineRecv(con.pipe,query,i)<>0) then i:=0;
    if (i<1) then begin;
      f2:
      pipeLineStats(con.pipe,i,o,p);
      if (i<>0) then exit;
      f1:
      con.stat:=0;
      doConn:=True;
      exit;
      end;
    case query.typ of
      1:begin; {name-->addr}
        {$ifdef debug}WriteLn('new query: address of '+query.nam);{$endif}
        if FindInCache4name2addr(query.nam,reply) then begin;
          reply.typ:=1;
          pipeLineSend(con.pipe,reply,sizeof(reply));
          {$ifdef debug}WriteLn('local cache hit');{$endif}
          exit;
          end;
        con.quest:=query.nam;
        con.stat:=2;
        end;
      2:begin; {addr-->name}
        {$ifdef debug}WriteLn('new query: name of '+ipAddr2string(query.nam));{$endif}
        if FindInCache4addr2name(query.nam,reply) then begin;
          reply.typ:=2;
          pipeLineSend(con.pipe,reply,sizeof(reply));
          {$ifdef debug}WriteLn('local cache hit');{$endif}
          exit;
          end;
        move(query.nam,con.quest,sizeof(con.quest));
        con.stat:=3;
        end;
      3:begin; {name-->mailer}
        {$ifdef debug}WriteLn('new query: mailer of '+query.nam);{$endif}
        if FindInCache4name2mail(query.nam,reply) then begin;
          reply.typ:=3;
          pipeLineSend(con.pipe,reply,sizeof(reply));
          {$ifdef debug}WriteLn('local cache hit');{$endif}
          exit;
          end;
        con.quest:=query.nam;
        con.stat:=4;
        end;
      else goto f1;
      end;
    con.time:=-99999;
    con.retry:=0;
    con.server:=128;
    con.reply:=NameServerList;
    end;
  2:begin; {name-->addr}
    if (GetTimePast(con.time)<timeout) then goto f2;
    inc(con.server);
    if (con.server>=length(con.reply) div sizeof(ip)) then begin;
      con.server:=0;
      inc(con.retry);
      end;
    if (con.retry>maxretry) then begin;
      reply.typ:=$81;
      reply.nam:=con.quest;
      pipeLineSend(con.pipe,reply,sizeof(reply));
      con.stat:=1;
      exit;
      end;
    phdr.id:=con.pipe and $ffff;
    WriteWordMSB(phdr.flag,Flags_RD);
    WriteWordMSB(phdr.qdc,1);
    WriteWordMSB(phdr.anc,0);
    WriteWordMSB(phdr.nsc,0);
    WriteWordMSB(phdr.arc,0);
    move(phdr,pck.d,sizeof(phdr));
    pck.s:=sizeof(phdr);
    a:=ConvertDomain2protocol(con.quest);
    move(a[1],pck.d[pck.s+1],length(a));
    inc(pck.s,length(a));
    if useIPv6addr then WriteWordMSB(qhdr.qtype,Type_AAAA) else WriteWordMSB(qhdr.qtype,Type_A);
    WriteWordMSB(qhdr.class,Class_IN);
    move(qhdr,pck.d[pck.s+1],sizeof(qhdr));
    inc(pck.s,sizeof(qhdr));
    move(con.reply[con.server*sizeof(ip)+1],ip,sizeof(ip));
    UDPsendPacket(listenPipeUdp,ip,remotePort,pck.d,pck.s);
    con.time:=CurrentTime;
    {$ifdef debug}WriteLn('sending query to '+ipAddr2string(ip));{$endif}
    end;
  3:begin; {addr-->name}
    if (GetTimePast(con.time)<timeout) then goto f2;
    inc(con.server);
    if (con.server>=length(con.reply) div sizeof(ip)) then begin;
      con.server:=0;
      inc(con.retry);
      end;
    if (con.retry>maxretry) then begin;
      reply.typ:=$82;
      move(con.quest,reply.dat,sizeof(reply.dat));
      pipeLineSend(con.pipe,reply,sizeof(reply));
      con.stat:=1;
      exit;
      end;
    phdr.id:=con.pipe and $ffff;
    WriteWordMSB(phdr.flag,Flags_RD);
    WriteWordMSB(phdr.qdc,1);
    WriteWordMSB(phdr.anc,0);
    WriteWordMSB(phdr.nsc,0);
    WriteWordMSB(phdr.arc,0);
    move(phdr,pck.d,sizeof(phdr));
    pck.s:=sizeof(phdr);
    a:=ConvertDomain2protocol(ConvertIPaddr2ptrName(con.quest));
    move(a[1],pck.d[pck.s+1],length(a));
    inc(pck.s,length(a));
    WriteWordMSB(qhdr.qtype,Type_PTR);
    WriteWordMSB(qhdr.class,Class_IN);
    move(qhdr,pck.d[pck.s+1],sizeof(qhdr));
    inc(pck.s,sizeof(qhdr));
    move(con.reply[con.server*sizeof(ip)+1],ip,sizeof(ip));
    UDPsendPacket(listenPipeUdp,ip,remotePort,pck.d,pck.s);
    con.time:=CurrentTime;
    {$ifdef debug}WriteLn('sending query to '+ipAddr2string(ip));{$endif}
    end;
  4:begin; {name-->mailer}
    if (GetTimePast(con.time)<timeout) then goto f2;
    inc(con.server);
    if (con.server>=length(con.reply) div sizeof(ip)) then begin;
      con.server:=0;
      inc(con.retry);
      end;
    if (con.retry>maxretry) then begin;
      reply.typ:=$83;
      reply.nam:=con.quest;
      pipeLineSend(con.pipe,reply,sizeof(reply));
      con.stat:=1;
      exit;
      end;
    phdr.id:=con.pipe and $ffff;
    WriteWordMSB(phdr.flag,Flags_RD);
    WriteWordMSB(phdr.qdc,1);
    WriteWordMSB(phdr.anc,0);
    WriteWordMSB(phdr.nsc,0);
    WriteWordMSB(phdr.arc,0);
    move(phdr,pck.d,sizeof(phdr));
    pck.s:=sizeof(phdr);
    a:=ConvertDomain2protocol(con.quest);
    move(a[1],pck.d[pck.s+1],length(a));
    inc(pck.s,length(a));
    WriteWordMSB(qhdr.qtype,Type_MX);
    WriteWordMSB(qhdr.class,Class_IN);
    move(qhdr,pck.d[pck.s+1],sizeof(qhdr));
    inc(pck.s,sizeof(qhdr));
    move(con.reply[con.server*sizeof(ip)+1],ip,sizeof(ip));
    UDPsendPacket(listenPipeUdp,ip,remotePort,pck.d,pck.s);
    con.time:=CurrentTime;
    {$ifdef debug}WriteLn('sending query to '+ipAddr2string(ip));{$endif}
    end;
  else begin; doConn:=True;exit; end;
  end;
End;





















Procedure doReply(var pck:OnePacketRecord;ip:OneTCPaddressRecord;prt:LongInt);
Label f1,f2,f3;
Var
  reply:OneResponseRecord;
  con:OneConnectionRecord;
  phdr:OneQuestionPacketHeader;
  qhdr:OneQuestionHeader;
  ahdr:OneAnswerHeader;
  i,o,p:LongInt;
  a,b,c:String;

Function findOwner:Boolean;
Label f1;
Begin;
a:=kicsi(reply.nam);
p:=ConnectionAct-1;
f1:
inc(p);
if (p>ConnectionNum) then begin;
  {$ifdef debug}WriteLn('owner not found');{$endif}
  findOwner:=True;
  exit;
  end;
if (ConnectionDat^[p].pipe and $ffff<>phdr.id) then goto f1;
if (ConnectionDat^[p].stat-1<>reply.typ and $7f) then goto f1;
case reply.typ of
  1:if (kicsi(ConnectionDat^[p].quest)<>a) then goto f1;
  2:if not TCPcompareAddress(ConnectionDat^[p].quest,reply.dat) then goto f1;
  3:if (kicsi(ConnectionDat^[p].quest)<>a) then goto f1;
  else goto f1;
  end;
{$ifdef debug}WriteLn('found owner at '+BStr(p));{$endif}
findOwner:=False;
End;

Begin;
{$ifdef debug}WriteLn('got reply from '+ipAddr2string(ip));{$endif}
move(pck.d,phdr,sizeof(phdr));
if (ReadWordMSB(phdr.qdc)<>1) then begin;
  WriteLn('got without question from '+ipAddr2string(ip));
  exit;
  end;
i:=ReadWordMSB(phdr.flag);
if (i and Flags_QR=0) then begin;
  WriteLn('got question from '+ipAddr2string(ip));
  exit;
  end;
p:=sizeof(phdr);
a:=GetPckProtocol2domain(pck,p);
move(pck.d[p+1],qhdr,sizeof(qhdr));
inc(p,sizeof(qhdr));
if (ReadWordMSB(qhdr.class)<>Class_IN) then begin;
  WriteLn('got with invalid qclass from '+ipAddr2string(ip));
  exit;
  end;
case ReadWordMSB(qhdr.qtype) of
  Type_A:begin;
    if useIPv6addr then begin;
      WriteLn('got with invalid qtype from '+ipAddr2string(ip));
      exit;
      end;
    reply.typ:=1;
    reply.nam:=a;
    {$ifdef debug}b:='address of '+a;{$endif}
    end;
  Type_AAAA:begin;
    if not useIPv6addr then begin;
      WriteLn('got with invalid qtype from '+ipAddr2string(ip));
      exit;
      end;
    reply.typ:=1;
    reply.nam:=a;
    {$ifdef debug}b:='address of '+a;{$endif}
    end;
  Type_PTR:begin;
    reply.typ:=2;
    if ConvertPtrName2ipAddr(a,reply.dat) then exit;
    {$ifdef debug}b:='name of '+ipAddr2string(reply.dat);{$endif}
    end;
  Type_MX:begin;
    reply.typ:=3;
    reply.nam:=a;
    {$ifdef debug}b:='mailer of '+a;{$endif}
    end;
  else begin;
    WriteLn('got with invalid qtype from '+ipAddr2string(ip));
    exit;
    end;
  end;
{$ifdef debug}WriteLn('reply to '+b);{$endif}
b:=kicsi(a);c:='';
for i:=1 to ReadWordMSB(phdr.anc) do begin;
  if (p>pck.s) then goto f3;
  a:=kicsi(GetPckProtocol2domain(pck,p));
  move(pck.d[p+1],ahdr,sizeof(ahdr));
  inc(p,sizeof(ahdr));
  if (ReadWordMSB(ahdr.class)<>Class_IN) then begin;
    inc(p,ReadWordMSB(ahdr.len));
    continue;
    end;
  o:=p;
  if (a=b) then case ReadWordMSB(ahdr.qtype) of
    Type_A:if (reply.typ=1) then begin;
      a:=IPv4addressPrefix;
      move(pck.d[p+1],a[length(a)+1],128);
      move(a[1],reply.dat,128);
      goto f2;
      end;
    Type_AAAA:if (reply.typ=1) then begin;
      move(pck.d[p+1],reply.dat,128);
      goto f2;
      end;
    Type_PTR:if (reply.typ=2) then begin;
      reply.nam:=GetPckProtocol2domain(pck,p);
      goto f2;
      end;
    Type_MX:if (reply.typ=3) then begin;
      inc(p,sizeof(word));
      a:=GetPckProtocol2domain(pck,p);
      if (length(c+a)<255) then c:=c+a+#0;
      end;
    Type_CNAME:b:=kicsi(GetPckProtocol2domain(pck,p));
    end;
  p:=ReadWordMSB(ahdr.len)+o;
  end;
if (reply.typ=3) then if (c<>'') then begin;
  reply.dat:=c;
  goto f2;
  end;
if (i and (Flags_AA or Flags_RA)<>0) then goto f1;
c:=#0;
for i:=1 to ReadWordMSB(phdr.nsc) do begin;
  if (p>pck.s) then goto f3;
  GetPckProtocol2domain(pck,p);
  move(pck.d[p+1],ahdr,sizeof(ahdr));
  inc(p,sizeof(ahdr));
  if (ReadWordMSB(ahdr.class)<>Class_IN) then begin;
    inc(p,ReadWordMSB(ahdr.len));
    continue;
    end;
  o:=p;
  case ReadWordMSB(ahdr.qtype) of
    Type_NS:c:=c+kicsi(GetPckProtocol2domain(pck,p))+#0;
    Type_SOA:c:=c+kicsi(GetPckProtocol2domain(pck,p))+#0;
    end;
  p:=ReadWordMSB(ahdr.len)+o;
  end;
b:='';
for i:=1 to ReadWordMSB(phdr.arc) do begin;
  if (p>pck.s) then goto f3;
  a:=#0+kicsi(GetPckProtocol2domain(pck,p))+#0;
  move(pck.d[p+1],ahdr,sizeof(ahdr));
  inc(p,sizeof(ahdr));
  if (ReadWordMSB(ahdr.class)<>Class_IN) then begin;
    inc(p,ReadWordMSB(ahdr.len));
    continue;
    end;
  o:=p;
  if (pos(a,c)<>0) then case ReadWordMSB(ahdr.qtype) of
    Type_A:begin;
      a:=IPv4addressPrefix;
      move(pck.d[p+1],a[length(a)+1],128);
      a[0]:=#16;
      b:=b+a;
      end;
    Type_AAAA:begin;
      move(pck.d[p+1],a[1],128);
      a[0]:=#16;
      b:=b+a;
      end;
    end;
  p:=ReadWordMSB(ahdr.len)+o;
  end;
if (b='') then goto f3;
{$ifdef debug}WriteLn('got redirect from '+ipAddr2string(ip));{$endif}
if findOwner then exit;
ConnectionDat^[p].reply:=b;
ConnectionDat^[p].server:=0;
ConnectionDat^[p].retry:=0;
ConnectionDat^[p].time:=-9999;
exit;

f1:
{$ifdef debug}WriteLn('got name error from '+ipAddr2string(ip));{$endif}
if findOwner then exit;
inc(reply.typ,$80);
pipeLineSend(ConnectionDat^[p].pipe,reply,sizeof(reply));
ConnectionDat^[p].stat:=1;
exit;

f2:
{$ifdef debug}WriteLn('got answer from '+ipAddr2string(ip));{$endif}
if findOwner then exit;
pipeLineSend(ConnectionDat^[p].pipe,reply,sizeof(reply));
ConnectionDat^[p].stat:=1;
fillchar(con,sizeof(con),0);
move(reply.nam,con.quest,sizeof(con.quest));
move(reply.dat,con.reply,sizeof(con.reply));
case reply.typ of
  1:con.stat:=100;
  2:con.stat:=100;
  3:con.stat:=101;
  else exit;
  end;
inc(ConnectionNxt);
if (ConnectionNxt>=ConnectionAct) then ConnectionNxt:=1;
ConnectionDat^[ConnectionNxt]:=con;
{$ifdef debug}WriteLn('answer stored in cache');{$endif}
exit;

f3:
{$ifdef debug}WriteLn('got truncated packet from '+ipAddr2string(ip));{$endif}
if findOwner then exit;
ConnectionDat^[p].time:=-9999;
exit;
End;






Procedure cleanUpCache;

Procedure doOne(var con:OneConnectionRecord);
Begin;
fillchar(con,sizeof(con),0);
End;

Var i:LongInt;
Begin;
for i:=1 to ConnectionAct-1 do doOne(ConnectionDat^[i]);
ConnectionNxt:=0;
{$ifdef debug}WriteLn('cache cleanup completed!');{$endif}
End;
