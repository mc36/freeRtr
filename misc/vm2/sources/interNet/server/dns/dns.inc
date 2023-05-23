Procedure doReply(var nam,dat:String;res:LongInt);
Var
  ip:OneTCPaddressRecord absolute dat;
  pck:OnePacketRecord;
  phdr:OneQuestionPacketHeader;
  qhdr:OneQuestionHeader;
  ahdr:OneAnswerHeader;

Procedure doer(var con:OneConnectionRecord);
Label f1;
Var
  i,o:LongInt;
  a,b:String;
  w:Word;
Begin;
{$ifdef debug}WriteLn('got answer from external resolver for '+ipAddr2string(con.adr)+' '+BStr(con.prt));{$endif}
phdr.id:=con.origI;
i:=con.origF or (Flags_QR or Flags_RA);
if (res and $80<>0) then i:=i or Respon_NAME;
WriteWordMSB(phdr.flag,i);
WriteWordMSB(phdr.qdc,1);
if (res and $80=0) then i:=1 else i:=0;
WriteWordMSB(phdr.anc,i);
phdr.nsc:=0;
phdr.arc:=0;
move(phdr,pck.d,sizeof(phdr));
pck.s:=sizeof(phdr);
case res and $7f of
  1:begin;
    a:=con.origQ;
    if isAddressIPv4mask(ip) then o:=Type_A else o:=Type_AAAA;
    end;
  2:begin;
    a:=ConvertIPaddr2ptrName(con.origQ);
    o:=Type_PTR;
    end;
  3:begin;
    a:=con.origQ;
    o:=Type_MX;
    end;
  else begin; con.stat:=0;exit; end;
  end;
a:=ConvertDomain2protocol(a);
move(a[1],pck.d[pck.s+1],length(a));
inc(pck.s,length(a));
WriteWordMSB(qhdr.qtype,o);
WriteWordMSB(qhdr.class,Class_IN);
move(qhdr,pck.d[pck.s+1],sizeof(qhdr));
inc(pck.s,sizeof(qhdr));
if (res and $80<>0) then goto f1;
WriteWordMSB(ahdr.qtype,o);
WriteWordMSB(ahdr.class,Class_IN);
WriteLongMSB(ahdr.ttl,86400);
case res and $7f of
  1:begin;
    if isAddressIPv4mask(ip) then begin;
      move(ip[sizeof(ip)-3],b[1],4);
      b[0]:=#4;
      end else begin;
      move(ip,b[1],sizeof(ip));
      b[0]:=#16;
      end;
    end;
  2:begin;
    b:=ConvertDomain2protocol(nam);
    end;
  3:begin;
    o:=0;
    while (1=1) do begin;
      i:=pos(#0,dat);
      if (i<1) then break;
      inc(o);
      b:=copy(dat,1,i-1);
      dat:=copy(dat,i+1,255);
      b:=#0#0+ConvertDomain2protocol(b);
      WriteWordMSB(b[1],o);
      move(a[1],pck.d[pck.s+1],length(a));
      inc(pck.s,length(a));
      WriteWordMSB(ahdr.len,length(b));
      move(ahdr,pck.d[pck.s+1],sizeof(ahdr));
      inc(pck.s,sizeof(ahdr));
      move(b[1],pck.d[pck.s+1],length(b));
      inc(pck.s,length(b));
      end;
    WriteWordMSB(phdr.anc,o);
    move(phdr,pck.d,sizeof(phdr));
    goto f1;
    end;
  else begin; con.stat:=0;exit; end;
  end;
move(a[1],pck.d[pck.s+1],length(a));
inc(pck.s,length(a));
WriteWordMSB(ahdr.len,length(b));
move(ahdr,pck.d[pck.s+1],sizeof(ahdr));
inc(pck.s,sizeof(ahdr));
move(b[1],pck.d[pck.s+1],length(b));
inc(pck.s,length(b));
f1:
if (pck.s>512) then begin;
  pck.s:=512;
  WriteWordMSB(phdr.flag,ReadWordMSB(phdr.flag) or Flags_TC);
  move(phdr,pck.d,sizeof(phdr));
  end;
case con.stat of
  4,5,6:begin;
    WriteWordMSB(w,pck.s);
    pipeLineSend(con.pipe,w,sizeof(w));
    pipeLineSend(con.pipe,pck.d,pck.s);
    con.stat:=2;
    end;
  100,101,102:begin;
    UDPsendPacket(listenPipeUdp,con.adr,con.prt,pck.d,pck.s);
    con.stat:=0;
    end;
  else begin; con.stat:=0;exit; end;
  end;
End;

Var i:LongInt;
Begin;
case res and $7f of
  1:begin;
    nam:=kicsi(nam);
    for i:=1 to ConnectionNum do begin;
      if not (ConnectionDat^[i].stat in [4,100]) then continue;
      if (ConnectionDat^[i].quest=nam) then doer(ConnectionDat^[i]);
      end;
    exit;
    end;
  2:begin;
    for i:=1 to ConnectionNum do begin;
      if not (ConnectionDat^[i].stat in [5,101]) then continue;
      if TCPcompareAddress(ip,ConnectionDat^[i].origQ) then doer(ConnectionDat^[i]);
      end;
    exit;
    end;
  3:begin;
    nam:=kicsi(nam);
    for i:=1 to ConnectionNum do begin;
      if not (ConnectionDat^[i].stat in [6,102]) then continue;
      if (ConnectionDat^[i].quest=nam) then doer(ConnectionDat^[i]);
      end;
    exit;
    end;
  else exit;
  end;
End;










Procedure BeginNextZoneFresh;
Var
  zhdr:OneZoneDescriptorRec;
  rhdr:OneZoneEntryHeaderRec;
  ahdr:OneAnswerHeader;
  con:OneConnectionRecord;
  i:LongInt;
  f:xFile;
Begin;
LastZoneTry:=CurrentTime;
if (ZonesNum<1) then exit;
LastZoneNum:=(LastZoneNum mod ZonesNum)+1;
xOpen(f,TempPath+ZonesDescriptorFile,xGenFilMod_r);
xSeek(f,(LastZoneNum-1)*sizeof(zhdr));
xBlockRead(f,zhdr,sizeof(zhdr));
xClose(f);
if (zhdr.typ<>2) then exit;
if (GetTimePast(zhdr.lastCheck)<fershIntrvl) then exit;
fillchar(con,sizeof(con),0);
move(zhdr.host[1],con.adr,sizeof(con.adr));
con.prt:=zhdr.hostPort;
con.stat:=200;
con.time:=CurrentTime;
con.origQ:=zhdr.name;
con.origI:=LastZoneNum;
for i:=1 to ConnectionNum do begin;
  if not (ConnectionDat^[i].stat in [200..220]) then continue;
  if (ConnectionDat^[i].origI=LastZoneNum) then exit;
  end;
if ResizeMem(ConnectionNum+1) then exit;
TCPbeginConnect(con.pipe,65536,con.adr,con.prt);
ConnectionDat^[ConnectionNum]:=con;
WriteLn('checking zone '+zhdr.name+', connecting to '+ipAddr2string(con.adr)+' '+BStr(con.prt));
End;

Procedure CheckForSoaChanged(var con:OneConnectionRecord;var pck:OnePacketRecord);
Label f1,f2;
Var
  rhdr:OneZoneEntryHeaderRec;
  phdr:OneQuestionPacketHeader;
  qhdr:OneQuestionHeader;
  ahdr:OneAnswerHeader;
  f:xFile;
  i,o,p:LongInt;
  a,b:String;
Begin;
{$ifdef debug}WriteLn('received soa for zone '+con.origQ+'...');{$endif}
con.stat:=0;
if (pck.s<sizeof(phdr)) then exit;
move(pck.d,phdr,sizeof(phdr));
if (ReadWordMSB(phdr.qdc)<>1) then exit;
o:=ReadWordMSB(phdr.anc);
i:=ReadWordMSB(phdr.flag);
if (i and Flags_QR=0) then exit;
if (i and Flags_RCODE<>Respon_OK) then exit;
p:=sizeof(phdr);a:='';
GetPckProtocol2domain(pck,p);
inc(p,sizeof(qhdr));
fillchar(ahdr,sizeof(ahdr),0);
f1:
inc(p,ReadWordMSB(ahdr.len));
dec(o);
if (o<0) then exit;
b:=kicsi(GetPckProtocol2domain(pck,p));
move(pck.d[p+1],ahdr,sizeof(ahdr));
if (ReadWordMSB(ahdr.qtype)<>Type_SOA) then goto f1;
if (ReadWordMSB(ahdr.class)<>Class_IN) then goto f1;
if (b<>kicsi(con.origQ)) then goto f1;
inc(p,sizeof(ahdr));
GetPckProtocol2domain(pck,p);
GetPckProtocol2domain(pck,p);
o:=ReadLongMSB(pck.d[p+1]);
xOpen(f,TempPath+OneZoneFilePostFix+BStr(con.origI),xGenFilMod_r);
if (xFileSize(f)<1) then begin;
  xClose(f);
  goto f2;
  end;
xBlockRead(f,rhdr,sizeof(rhdr));
xSeek(f,rhdr.nameSize+sizeof(rhdr));
i:=rhdr.dataSize;
xBlockRead(f,pck.d,i);
pck.s:=i;
xClose(f);
p:=sizeof(ahdr);
GetPckProtocol2domain(pck,p);
GetPckProtocol2domain(pck,p);
i:=ReadLongMSB(pck.d[p+1]);
if (i<>o) then goto f2;
{$ifdef debug}WriteLn('zone '+con.origQ+' is still up to date: '+BStr(o));{$endif}
UpdateLastTimeAndSerial(con.origI,1);
exit;
f2:
con.stat:=203;
End;

Procedure SaveReceivedZoneRR(var con:OneConnectionRecord;var pck1:OnePacketRecord);
Label f1;
Var
  pck2:OnePacketRecord;
  rhdr:OneZoneEntryHeaderRec;
  phdr:OneQuestionPacketHeader;
  qhdr:OneQuestionHeader;
  ahdr:OneAnswerHeader;
  i,o,p,q:LongInt;
  a,b:String;
Begin;
{$ifdef debug}WriteLn('received rr for zone '+con.origQ+'...');{$endif}
con.stat:=0;
if (pck1.s<sizeof(phdr)) then begin;
  WriteLn('got a too short packet from '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  exit;
  end;
move(pck1.d,phdr,sizeof(phdr));
if (ReadWordMSB(phdr.anc)<1) then begin;
  WriteLn('got no answer from '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  exit;
  end;
i:=ReadWordMSB(phdr.flag);
if (i and Flags_QR=0) then begin;
  WriteLn('got question from '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  exit;
  end;
if (i and Flags_RCODE<>Respon_OK) then begin;
  WriteLn('got error from '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  exit;
  end;
if (i and Flags_TC<>0) then begin;
  WriteLn('got truncated from '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  exit;
  end;
p:=sizeof(phdr);a:='';
for i:=1 to ReadWordMSB(phdr.qdc) do begin;
  GetPckProtocol2domain(pck1,p);
  inc(p,sizeof(qhdr));
  end;
for o:=1 to ReadWordMSB(phdr.anc) do begin;
  fillChar(rhdr,sizeof(rhdr),0);
  move(pck1.d[p+1],pck2.d[sizeof(rhdr)+1],sizeof(pck2.d));
  q:=p;
  b:=GetPckProtocol2domain(pck1,p);
  move(pck1.d[p+1],ahdr,sizeof(ahdr));
  rhdr.typ:=1;
  rhdr.nameSize:=p-q;
  rhdr.dataSize:=ReadWordMSB(ahdr.len)+sizeof(ahdr);
  rhdr.dataType:=ReadWordMSB(ahdr.qtype);
  rhdr.name2sort:=ConvertNameTypeForDomainSearch(b,rhdr.dataType);
  inc(p,rhdr.dataSize);
  if (p>pck1.s) then begin;
    WriteLn('got too big packet from '+ipAddr2string(con.adr)+' '+BStr(con.prt));
    exit;
    end;
  if (ReadWordMSB(ahdr.class)<>Class_IN) then continue;
  if (rhdr.dataType=Type_SOA) then if (con.fileP>0) then goto f1;
  i:=xFilePos(con.fileH);
  xBlockWrite(con.fileHH,i,sizeof(i));
  move(rhdr,pck2.d,sizeof(rhdr));
  xBlockWrite(con.fileH,pck2.d,rhdr.NameSize+rhdr.dataSize+sizeof(rhdr));
  inc(con.fileP);
  end;
if (p>pck1.s) then begin;
  WriteLn('got too big packet from '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  exit;
  end;
con.stat:=203;
exit;
f1:
xClose(con.fileH);
xClose(con.fileHH);
WriteLn('zone transferred from '+ipAddr2string(con.adr)+' '+BStr(con.prt)+', '+BStr(con.fileP+1)+' RRs received.');
a:=TempPath+OneZoneFilePostFix+BStr(con.origI);
xErase(a);xRename(a+CurrentDownZonePlus,a);
a:=a+ZoneSortedIndexPlus;
xErase(a);xRename(a+CurrentDownZonePlus,a);
Write('sorting zone '+con.origQ+'...');
SortRRsInOneZone(con.origI);
WriteLn(' done.');
UpdateLastTimeAndSerial(con.origI,0);
con.stat:=0;
End;











Function doConn(var con:OneConnectionRecord;conN:LongInt):Boolean;
Label f1,f2,f3;
Var
  i,o,p:LongInt;
  a,b:String;
  f:xFile;
  pck:OnePacketRecord;
  rhdr:OneZoneEntryHeaderRec;
  zhdr:OneZoneDescriptorRec;
  phdr:OneQuestionPacketHeader;
  qhdr:OneQuestionHeader;
  ahdr:OneAnswerHeader;

Procedure zoneOpen;
Var i:LongInt;
Begin;
xOpen(con.fileH,TempPath+OneZoneFilePostFix+BStr(con.zoneCur),xGenFilMod_r);
con.fileS:=xFileSize(con.fileH);
con.fileP:=0;
End;

Procedure zoneReadRR;
Label f1;
Var phdr:OneQuestionPacketHeader;
Begin;
if (xBlockRead(con.fileH,rhdr,sizeof(rhdr))<>0) then begin;
  f1:
  pck.s:=0;
  con.fileS:=0;
  con.fileP:=0;
  exit;
  end;
phdr.id:=con.origI;
WriteWordMSB(phdr.flag,con.origF or (Respon_OK or Flags_QR or Flags_RA or Flags_AA));
WriteWordMSB(phdr.anc,1);
phdr.qdc:=0;
phdr.nsc:=0;
phdr.arc:=0;
move(phdr,pck.d,sizeof(phdr));
pck.s:=rhdr.nameSize+rhdr.dataSize;
if (xBlockRead(con.fileH,pck.d[sizeof(phdr)+1],pck.s)<>0) then goto f1;
inc(con.fileP,pck.s+sizeof(rhdr));
inc(pck.s,sizeof(phdr));
End;

Procedure zoneSendRR;
Var i:Word;
Begin;
if (pck.s=0) then exit;
WriteWordMSB(i,pck.s);
pipeLineSend(con.pipe,i,sizeof(i));
pipeLineSend(con.pipe,pck.d,pck.s);
End;

Begin;
if (GetTimePast(con.time)>5*60) then begin;
  WriteLn('connection timed out with '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  doConn:=True;
  exit;
  end;
doConn:=False;
case con.stat of
  7:begin; {axfr tx}
    pipeLineStats(con.pipe,i,o,p);
    if (p<1024) then goto f1;
    con.time:=CurrentTime;
    if (con.fileP<con.fileS) then begin;
      zoneReadRR;
      if (rhdr.typ=1) then begin; zoneSendRR;exit; end;
      xClose(con.fileH);
      con.zoneCur:=rhdr.dataType;
      zoneOpen;
      zoneReadRR;
      exit;
      end;
    if (con.zoneBeg=con.zoneCur) then begin;
      xSeek(con.fileH,0);
      zoneReadRR;
      zoneSendRR;
      xClose(con.fileH);
      con.stat:=2;
      exit;
      end;
    xClose(con.fileH);
    xOpen(f,TempPath+ZonesDescriptorFile,xGenFilMod_r);
    xSeek(f,(con.zoneCur-1)*sizeof(zhdr));
    xBlockRead(f,zhdr,sizeof(zhdr));
    xClose(f);
    con.zoneCur:=zhdr.prntNum;
    zoneOpen;
    con.fileP:=zhdr.prntPos;
    xSeek(con.fileH,con.fileP);
    exit;
    end;
  3:begin; {wait for packet}
    pipeLineStats(con.pipe,i,o,p);
    if (o<con.fileS) then goto f1;
    i:=con.fileS;
    if (pipeLineRecv(con.pipe,pck.d,i)<>0) then i:=0;
    if (i<>con.fileS) then goto f2;
    con.time:=CurrentTime;
    pck.s:=i;
    i:=AnswerOnePacket(con,pck,true);
    case i of
      1:begin; zoneSendRR;con.stat:=2;exit; end;
      3,4,5:;
      2:begin;
        xOpen(f,TempPath+ZonesDescriptorFile+ZoneSortedIndexPlus,xGenFilMod_r);
        xSeek(f,(con.zoneCur-1)*sizeof(i));
        xBlockRead(f,i,sizeof(i));
        xClose(f);
        i:=(i div sizeof(zhdr))+1;
        con.zoneCur:=i;
        con.zoneBeg:=i;
        zoneOpen;
        con.stat:=7;
        exit;
        end;
      0:begin; con.stat:=2;exit; end;
      else goto f2;
      end;
    DNSresolvePut(i-2,con.origQ);
    con.stat:=i+1;
    exit;
    end;
  2:begin; {wait for size}
    pipeLineStats(con.pipe,i,o,p);
    if (o<2) then begin;
      f1:
      if (i<>0) then exit;
      WriteLn('connection lost with '+ipAddr2string(con.adr)+' '+BStr(con.prt));
      con.stat:=0;
      exit;
      end;
    con.time:=CurrentTime;
    i:=sizeof(word);
    if (pipeLineRecv(con.pipe,o,i)<>0) then i:=0;
    if (i<>sizeof(word)) then begin; f2:con.stat:=0;exit; end;
    con.fileS:=ReadWordMSB(o);
    if (con.fileS>sizeof(pck.d)) then begin;
      f3:
      WriteLn(ipAddr2string(con.adr)+' '+BStr(con.prt)+' wants to send too big packet');
      con.stat:=0;
      exit;
      end;
    con.stat:=3;
    end;
  4,5,6:begin; {tcp wait for resolved data}
    pipeLineStats(con.pipe,i,o,p);
    goto f1;
    end;
  100,101,102:exit; {udp wait for resolved data}
  1:begin; {init}
    if TCPlookConnected(con.pipe,con.adr,con.prt,i) then begin;
      doConn:=(con.pipe=0);
      exit;
      end;
    WriteLn('connection accepted from '+ipAddr2string(con.adr)+' '+BStr(con.prt));
    con.stat:=2;
    end;
  0:begin; doConn:=True;exit; end; {disconnect}
  200:begin; {connect}
    if TCPlookConnected(con.pipe,a,o,i) then begin;
      doConn:=(con.pipe=0);
      exit;
      end;
    WriteLn('connection accepted by '+ipAddr2string(con.adr)+' '+BStr(con.prt));
    con.stat:=201;
    WriteWordMSB(phdr.id,0);
    WriteWordMSB(phdr.flag,Opcode_QUERY);
    WriteWordMSB(phdr.qdc,1);
    WriteWordMSB(phdr.anc,0);
    WriteWordMSB(phdr.nsc,0);
    WriteWordMSB(phdr.arc,0);
    move(phdr,pck.d,sizeof(phdr));
    pck.s:=sizeof(phdr);
    a:=ConvertDomain2protocol(con.origQ);
    move(a[1],pck.d[pck.s+1],length(a));
    inc(pck.s,length(a));
    WriteWordMSB(qhdr.qtype,Type_SOA);
    WriteWordMSB(qhdr.class,Class_IN);
    move(qhdr,pck.d[pck.s+1],sizeof(qhdr));
    inc(pck.s,sizeof(qhdr));
    zoneSendRR;
    end;
  201:begin; {wait for soa size}
    pipeLineStats(con.pipe,i,o,p);
    if (o<2) then goto f1;
    con.time:=CurrentTime;
    i:=sizeof(word);
    if (pipeLineRecv(con.pipe,o,i)<>0) then i:=0;
    if (i<>sizeof(word)) then goto f2;
    con.fileS:=ReadWordMSB(o);
    if (con.fileS>sizeof(pck.d)) then goto f3;
    con.stat:=202;
    end;
  202:begin; {wait for soa data}
    pipeLineStats(con.pipe,i,o,p);
    if (o<con.fileS) then goto f1;
    i:=con.fileS;
    if (pipeLineRecv(con.pipe,pck.d,i)<>0) then i:=0;
    if (i<>con.fileS) then goto f2;
    con.time:=CurrentTime;
    pck.s:=i;
    CheckForSoaChanged(con,pck);
    if (con.stat=0) then exit;
    WriteLn('starting zone transfer with '+ipAddr2string(con.adr)+' '+BStr(con.prt));
    WriteWordMSB(phdr.id,0);
    WriteWordMSB(phdr.flag,Opcode_QUERY);
    WriteWordMSB(phdr.qdc,1);
    WriteWordMSB(phdr.anc,0);
    WriteWordMSB(phdr.nsc,0);
    WriteWordMSB(phdr.arc,0);
    move(phdr,pck.d,sizeof(phdr));
    pck.s:=sizeof(phdr);
    a:=ConvertDomain2protocol(con.origQ);
    move(a[1],pck.d[pck.s+1],length(a));
    inc(pck.s,length(a));
    WriteWordMSB(qhdr.qtype,Type_AXFR);
    WriteWordMSB(qhdr.class,Class_IN);
    move(qhdr,pck.d[pck.s+1],sizeof(qhdr));
    inc(pck.s,sizeof(qhdr));
    zoneSendRR;
    a:=TempPath+OneZoneFilePostFix+BStr(con.origI)+CurrentDownZonePlus;
    xErase(a);xCreate(a);xOpen(con.fileH,a,xGenFilMod_rw);
    a:=TempPath+OneZoneFilePostFix+BStr(con.origI)+ZoneSortedIndexPlus+CurrentDownZonePlus;
    xErase(a);xCreate(a);xOpen(con.fileHH,a,xGenFilMod_rw);
    con.fileP:=0;
    end;
  203:begin; {zonexfer packet size}
    pipeLineStats(con.pipe,i,o,p);
    if (o<2) then goto f1;
    con.time:=CurrentTime;
    i:=sizeof(word);
    if (pipeLineRecv(con.pipe,o,i)<>0) then i:=0;
    if (i<>sizeof(word)) then goto f2;
    con.fileS:=ReadWordMSB(o);
    if (con.fileS>sizeof(pck.d)) then goto f3;
    con.stat:=204;
    end;
  204:begin; {zonexfer packet data}
    pipeLineStats(con.pipe,i,o,p);
    if (o<con.fileS) then goto f1;
    i:=con.fileS;
    if (pipeLineRecv(con.pipe,pck.d,i)<>0) then i:=0;
    if (i<>con.fileS) then goto f2;
    con.time:=CurrentTime;
    pck.s:=i;
    SaveReceivedZoneRR(con,pck);
    end;
  else begin; doConn:=True;exit; end;
  end;
End;
