Function processL2CAPoptions(Var a:String):LongInt;

procedure addOpt(var a:string;t:longint;b:string);
Var ab:array[0..1] of byte absolute a;
begin;
inc(ab[0]);
ab[ab[0]]:=t;
inc(ab[0]);
ab[ab[0]]:=length(b);
a:=a+b;
end;

Label f1,f2;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
  b,c,d,e:String;
Begin;
b:='';
c:='';
d:='';
e:='';
f1:
if (ab0<1) then goto f2;
o:=ab[1];
move(ab[2],b,sizeof(b));
a:=copy(a,length(b)+3,666);
addOpt(c,o,b);
case o of
  1:;{mtu}
  2:;{flush}
  3:;{qos}
  4:begin; {retrans}
    if (copy(b,1,1)<>#0) then addOpt(d,o,#0);
    end;
  5:begin; {fcs}
    if (copy(b,1,1)<>#0) then addOpt(d,o,#0);
    end;
  6:;{flowspec}
  7:;{winsize}
  else addOpt(e,o,b);
  end;
goto f1;
f2:
a:=c;i:=0;
if (d<>'') then begin; a:=d;i:=1; end;
if (e<>'') then begin; a:=e;i:=3; end;
processL2CAPoptions:=i;
End;




Procedure hciGotAsyncPacket(var hnd:oneHCIconnectionRecord;var buffer);
Var
  buf:array[1..1024] of byte absolute buffer;
  lcp:oneLCPdataHeaderRecord absolute buffer;
  sig:oneSIGdataHeaderRecord absolute buffer;
  tcp:OneTCPcommandHeader absolute buffer;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p,q:LongInt;
Begin;
hnd.tim:=currentTime;
p:=ReadWordLSB(lcp.chn);
if (p=2) then begin;
  writeLn('got connectionless packet');
  exit;
  end;
o:=findOneLCPconnL(p);
if (o>0) then begin;
  i:=ReadWordLSB(lcp.len);
  pipeLineSend(lcpConnDat[o].pip,buf[sizeof(lcp)+1],i);
  lcpConnDat[o].tim:=currentTime;
  exit;
  end;
if (p<>1) then begin;
  writeLn('got for invalid channel');
  signDiscReq(hnd,-1,0,o);
  exit;
  end;
p:=ReadWordLSB(sig.len);
{$ifdef debug3}
write('sgnR:'+BStr(sig.cod));
for i:=1 to p do write(' '+byte2hextype(buf[sizeof(sig)+i]));
writeLn('');
{$endif}
case sig.cod of
  1:begin;
    {$ifdef debug4}writeLn('sigR: command reject');{$endif}
    end;
  2:begin;
    {$ifdef debug4}writeLn('sigR: connect request');{$endif}
    i:=readWordLSB(buf[sizeof(sig)+1]);
    o:=readWordLSB(buf[sizeof(sig)+3]);
    p:=findOneLCPconnHR(hnd.hnd,o);
    if (p<>0) then begin;
      writeLn('got duplicated connect request');
      lcpConnDat[p].tim:=currentTime;
      signConnRep(hnd,sig.id,lcpConnDat[p].loc,lcpConnDat[p].rem,0,0);
      exit;
      end;
    if (lcpConnNum>=maxLCPconns) then begin;
      writeLn('out of connection slots');
      signConnRep(hnd,sig.id,findUnusedConnID,o,4,0);
      exit;
      end;
    p:=findOnePSMconn(i);
    if (p=0) then begin;
      writeLn('unknown psm requested');
      signConnRep(hnd,sig.id,findUnusedConnID,o,2,0);
      exit;
      end;
    if (pipeLineCreate(q,psmConnDat[p].prc,8192,true)<>0) then q:=0;
    if (q=0) then begin;
      writeLn('failed to open pipeline');
      signConnRep(hnd,sig.id,findUnusedConnID,o,4,0);
      exit;
      end;
    inc(lcpConnNum);
    fillchar(lcpConnDat[lcpConnNum],sizeof(lcpConnDat[lcpConnNum]),0);
    lcpConnDat[lcpConnNum].adr:=hnd.adr;
    lcpConnDat[lcpConnNum].hnd:=hnd.hnd;
    lcpConnDat[lcpConnNum].psm:=i;
    lcpConnDat[lcpConnNum].loc:=findUnusedConnID;
    lcpConnDat[lcpConnNum].rem:=o;
    lcpConnDat[lcpConnNum].pip:=q;
    lcpConnDat[lcpConnNum].stt:=11;
    lcpConnDat[lcpConnNum].tim:=-9999999;
    signConnRep(hnd,sig.id,lcpConnDat[lcpConnNum].loc,lcpConnDat[lcpConnNum].rem,0,0);
    end;
  3:begin;
    {$ifdef debug4}writeLn('sigR: connect response');{$endif}
    i:=readWordLSB(buf[sizeof(sig)+1]);
    o:=readWordLSB(buf[sizeof(sig)+3]);
    q:=readWordLSB(buf[sizeof(sig)+5]);
    p:=findOneLCPconnL(o);
    if (p=0) then begin;
      writeLn('got for unknown connection');
      if (q=0) then signDiscReq(hnd,-1,i,o);
      exit;
      end;
    lcpConnDat[p].rem:=i;
    if (q=1) then begin;
      lcpConnDat[p].tim:=currentTime;
      lcpConnDat[p].try:=0;
      exit;
      end;
    if (q<>0) then begin;
      WriteLn('connection complete with error');
      lcpConnDat[p].stt:=0;
      exit;
      end;
    lcpConnDat[p].stt:=11;
    lcpConnDat[p].tim:=-9999999;
    end;
  4:begin;
    {$ifdef debug4}writeLn('sigR: configure request');{$endif}
    i:=readWordLSB(buf[sizeof(sig)+1]);
    o:=findOneLCPconnL(i);
    if (o=0) then begin;
      writeLn('got for unknown connection');
      signDiscReq(hnd,-1,0,i);
      exit;
      end;
    if (lcpConnDat[o].stt=32) and (lcpConnDat[o].rem<>0) then lcpConnDat[o].stt:=11;
    ab0:=p-4;
    move(buf[sizeof(sig)+5],ab[1],ab0);
    i:=processL2CAPoptions(a);
    signConfRep(hnd,sig.id,lcpConnDat[o].rem,i,a);
    if (i<>0) then exit;
    lcpConnDat[o].saw:=lcpConnDat[o].saw or 1;
    end;
  5:begin;
    {$ifdef debug4}writeLn('sigR: configure response');{$endif}
    i:=readWordLSB(buf[sizeof(sig)+1]);
    o:=findOneLCPconnL(i);
    if (o=0) then begin;
      writeLn('got for unknown connection');
      signDiscReq(hnd,-1,0,i);
      exit;
      end;
    i:=readWordLSB(buf[sizeof(sig)+5]);
    if (i<>0) then begin;
      lcpConnDat[o].stt:=3;
      exit;
      end;
    lcpConnDat[o].saw:=lcpConnDat[o].saw or 2;
    end;
  6:begin;
    {$ifdef debug4}writeLn('sigR: disconnect request');{$endif}
    i:=readWordLSB(buf[sizeof(sig)+1]);
    p:=readWordLSB(buf[sizeof(sig)+3]);
    signDiscRep(hnd,sig.id,i,p);
    o:=findOneLCPconnL(i);
    if (o=0) then begin;
      writeLn('got for unknown connection');
      exit;
      end;
    pipeLineClose(lcpConnDat[o].pip);
    lcpConnDat[o]:=lcpConnDat[lcpConnNum];
    dec(lcpConnNum);
    end;
  7:begin;
    {$ifdef debug4}writeLn('sigR: disconnect response');{$endif}
    i:=readWordLSB(buf[sizeof(sig)+3]);
    o:=findOneLCPconnL(i);
    if (o=0) then begin;
      writeLn('got for unknown connection');
      exit;
      end;
    pipeLineClose(lcpConnDat[o].pip);
    lcpConnDat[o]:=lcpConnDat[lcpConnNum];
    dec(lcpConnNum);
    end;
  8:begin;
    {$ifdef debug4}writeLn('sigR: echo request');{$endif}
    ab0:=p;
    move(buf[sizeof(sig)+1],ab[1],ab0);
    {$ifdef debug4}writeLn('sigT: echo response');{$endif}
    signSendCommand(hnd,9,sig.id,a);
    end;
  9:begin;
    {$ifdef debug4}writeLn('sigR: echo response');{$endif}
    end;
  10:begin;
    {$ifdef debug4}writeLn('sigR: info request');{$endif}
    i:=readWordLSB(buf[sizeof(sig)+1]);
    signInfoRep(hnd,sig.id,i,1);
    end;
  11:begin;
    {$ifdef debug4}writeLn('sigR: info response');{$endif}
    end;
  else begin;
    writeLn('unknown l2cap command');
    signCmdRej(hnd,sig.id,0,'');
    end;
  end;
End;





Function lcpReleq2oneConn(var lcp:oneLCPconnectionRecord):Boolean;
Label f1,f2,f3;
Var
  buf:array[1..2048] of byte;
  tcp:OneTCPcommandHeader absolute buf;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p,q:LongInt;
Begin;
lcpReleq2oneConn:=true;
if (lcp.hnd<>0) then begin;
  i:=0;
  if (lcp.chd<1) then inc(i);
  if (lcp.chd>hciConnNum) then inc(i);
  if (i=0) then if (hciConnDat[lcp.chd].hnd<>lcp.hnd) then inc(i);
  if (i<>0) then begin;
    i:=findOneHCIconnH(lcp.hnd);
    if (i=0) then goto f1;
    lcp.chd:=i;
    end;
  end;
case lcp.stt of
  1:begin; {ready}
    lcp.try:=0;
    if (lcp.chd=0) then goto f1;
    if (hciConnDat[lcp.chd].slt<localApkt) then exit;
    i:=sizeof(buf);
    pipeLineRecv(lcp.pip,buf,i);
    if (i>0) then begin;
      lcpSendData(hciConnDat[lcp.chd],lcp.rem,i,buf);
      lcp.tim:=currentTime;
      exit;
      end;
    pipeLineStats(lcp.pip,o,i,i);
    if (o<>0) then exit;
    lcp.stt:=3;
    end;
  2:begin; {closing}
    end;
  3:begin; {close begin}
    lcp.stt:=2;
    goto f2;
    end;
  0:begin; {done}
    f1:
    lcp.stt:=0;
    lcpReleq2oneConn:=false;
    exit;
    end;
  11:begin; {connreq}
    if (lcp.saw and 3<>3) then goto f3;
    fillchar(tcp,sizeof(tcp),0);
    move(lcp.adr,tcp.adr,sizeof(lcp.adr));
    tcp.prt:=lcp.psm;
    tcp.cmd:=3;
    pipeLineSend(lcp.pip,tcp,sizeof(tcp));
    lcp.stt:=1;
    end;
  21:begin; {wait for command}
    i:=sizeof(tcp);
    pipeLineRecv(lcp.pip,buf,i);
    if (i=0) then exit;
    lcp.tim:=currentTime;
    lcp.try:=0;
    case tcp.cmd of
      6:begin; {rtp.conn}
        move(tcp.adr,lcp.adr,sizeof(lcp.adr));
        lcp.psm:=tcp.prt;
        i:=findOneHCIconnA(lcp.adr);
        lcp.stt:=31;
        if (i<>0) then exit;
        if (hciConnNum>=maxHCIconns) then begin;
          WriteLn('hci connection table full');
          goto f1;
          end;
        inc(hciConnNum);
        fillchar(hciConnDat[hciConnNum],sizeof(hciConnDat[hciConnNum]),0);
        hciConnDat[hciConnNum].stt:=21;
        hciConnDat[hciConnNum].adr:=lcp.adr;
        hciConnDat[hciConnNum].tim:=-9999999;
        exit;
        end;
      4:begin; {udp.listen}
        if (psmConnNum>=maxPSMconns) then goto f1;
        if (findOnePSMconn(tcp.prt)<>0) then goto f1;
        inc(psmConnNum);
        fillchar(psmConnDat[psmConnNum],sizeof(psmConnDat[psmConnNum]),0);
        psmConnDat[psmConnNum].pip:=lcp.pip;
        psmConnDat[psmConnNum].psm:=tcp.prt;
        pipeLineStats(lcp.pip,o,i,i);
        psmConnDat[psmConnNum].prc:=o;
        lcp.pip:=0;
        fillchar(tcp,sizeof(tcp),0);
        move(localAddr,tcp.adr,sizeof(localAddr));
        tcp.prt:=psmConnDat[psmConnNum].psm;
        tcp.cmd:=3;
        pipeLineSend(psmConnDat[psmConnNum].pip,tcp,sizeof(tcp));
        goto f1;
        end;
      2:begin; {tcp.listen}
        fillchar(tcp,sizeof(tcp),0);
        move(localAddr,tcp.adr,sizeof(localAddr));
        tcp.prt:=localAmtu;
        tcp.dat:=localApkt;
        tcp.cmd:=3;
        pipeLineSend(lcp.pip,tcp,sizeof(tcp));
        hciStartInquiry(8,0);
        lcp.stt:=22;
        exit;
        end;
      1:begin; {tcp.conn}
        move(tcp.adr,lcp.adr,sizeof(lcp.adr));
        fillchar(tcp,sizeof(tcp),0);
        move(lcp.adr,tcp.adr,sizeof(lcp.adr));
        tcp.cmd:=3;
        pipeLineSend(lcp.pip,tcp,sizeof(tcp));
        if compareHCIaddress(lcp.adr,localAddr) then begin;
          pipeLineSend(lcp.pip,localName[1],length(localName));
          goto f1;
          end;
        hciNameRequest(lcp.adr);
        lcp.stt:=23;
        exit;
        end;
      end;
    writeLn('unknown request from upper!');
    goto f1;
    end;
  22:begin; {scanning}
    end;
  23:begin; {remote name request}
    end;
  31:begin; {wait for handle}
    lcp.try:=0;
    i:=lcp.chd;
    if (i<1) then i:=0;
    if (i>hciConnNum) then i:=0;
    if (i>0) then if not compareHCIaddress(lcp.adr,hciConnDat[i].adr) then i:=0;
    if (i=0) then i:=findOneHCIconnA(lcp.adr);
    if (i=0) then goto f1;
    lcp.chd:=i;
    if (hciConnDat[i].stt<>1) then exit;
    lcp.hnd:=hciConnDat[i].hnd;
    lcp.loc:=findUnusedConnID;
    lcp.stt:=32;
    goto f2;
    end;
  32:begin; {connreq}
    end;
  else begin;
    writeLn('bug!');
    goto f1;
    end;
  end;
f3:
if (getTimePast(lcp.tim)<5) then exit;
inc(lcp.try);
if (lcp.try>8) then begin;
  if (lcp.stt=2) then goto f1;
  if (lcp.loc=0) then goto f1;
  if (lcp.rem=0) then goto f1;
  lcp.stt:=2;
  lcp.try:=0;
  end;
f2:
lcp.tim:=currentTime;
case lcp.stt of
  11:begin; {confreq}
    if (lcp.chd=0) then goto f1;
    if (lcp.saw and 2<>0) then exit;
    if (hciConnDat[lcp.chd].slt<localApkt) then begin;
      dec(lcp.try);
      lcp.tim:=-9999999;
      exit;
      end;
    a:=#1#2#0#0;
    writeWordLSB(ab[3],localAmtu-sizeof(oneACLdataHeaderRecord));
    signConfReq(hciConnDat[lcp.chd],-1,lcp.rem,a);
    end;
  32:begin; {connreq}
    if (lcp.chd=0) then goto f1;
    signConnReq(hciConnDat[lcp.chd],-1,lcp.psm,lcp.loc);
    end;
  2:begin; {closing}
    if (lcp.chd=0) then goto f1;
    signDiscReq(hciConnDat[lcp.chd],-1,lcp.rem,lcp.loc);
    end;
  end;
End;



Procedure lcpReleq2conns;
Var i:LongInt;
Begin;
for i:=lcpConnNum downto 1 do begin;
  if lcpReleq2oneConn(lcpConnDat[i]) then continue;
  pipeLineClose(lcpConnDat[i].pip);
  lcpConnDat[i]:=lcpConnDat[lcpConnNum];
  dec(lcpConnNum);
  end;
End;
