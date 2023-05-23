Procedure hciGotEventPacket(var buffer);
Var
  buf:array[1..1024] of byte absolute buffer;
  hdr:oneEventHeaderRecord absolute buffer;
  adr:oneAddressRecord;
  a:String;
  ab:array[0..1] of byte absolute a;
  i,o,p,q:LongInt;


procedure gotSltChg(max:longint);
var i,r:longInt;
begin;
for i:=1 to max do begin;
  inc(p,2);
  inc(q,2);
  o:=ReadWordLSB(buf[p]);
  r:=findOneHCIconnH(o);
  if (r=0) then begin;
    writeLn('got slot for invalid handle');
    hciCloseConn(o,$13);
    continue;
    end;
  inc(hciConnDat[r].slt,ReadWordLSB(buf[q]));
  end;
end;

procedure gotInqRes;
var
  tcp:OneTCPcommandHeader;
  i,o,p:longInt;
begin;
for i:=1 to lcpConnNum do begin;
  if (lcpConnDat[i].stt<>22) then continue;
  p:=sizeof(hdr)+2;
  for o:=1 to buf[sizeof(hdr)+1] do begin;
    fillchar(tcp,sizeof(tcp),0);
    byteSwapHCIaddress(buf[p],tcp.adr);
    tcp.cmd:=3;
    pipeLineSend(lcpConnDat[i].pip,tcp,sizeof(tcp));
    inc(p,sizeof(adr));
    end;
  end;
end;

Begin;
case hdr.opc of
  1:begin;
    {$ifdef debug2}writeLn('evnt: inquiry complete');{$endif}
    for i:=1 to lcpConnNum do begin;
      if (lcpConnDat[i].stt<>22) then continue;
      lcpConnDat[i].stt:=0;
      end;
    end;
  2:begin;
    {$ifdef debug2}writeLn('evnt: inquiry result');{$endif}
    gotInqRes;
    end;
  3:begin;
    {$ifdef debug2}writeLn('evnt: connection complete');{$endif}
    byteSwapHCIaddress(buf[sizeof(hdr)+4],adr);
    p:=ReadWordLSB(buf[sizeof(hdr)+2]);
    o:=buf[sizeof(hdr)+1];
    i:=findOneHCIconnA(adr);
    if (i=0) then begin;
      WriteLn('connection complete with unknown peer');
      if (o=0) then hciCloseConn(p,$13);
      exit;
      end;
    if (o<>0) then begin;
      WriteLn('connection complete with error');
      hciConnDat[i]:=hciConnDat[hciConnNum];
      dec(hciConnNum);
      exit;
      end;
    hciConnDat[i].stt:=1;
    hciConnDat[i].slt:=localApkt;
    hciConnDat[i].tim:=currentTime;
    hciConnDat[i].hnd:=p and $fff;
    end;
  4:begin;
    {$ifdef debug2}writeLn('evnt: connection request');{$endif}
    byteSwapHCIaddress(buf[sizeof(hdr)+1],adr);
    if (buf[sizeof(hdr)+sizeof(adr)+4]<>1) then begin;
      WriteLn('invalid connection type');
      hciRefuseConn(adr,$11);
      exit;
      end;
    if (hciConnNum>=maxHCIconns) then begin;
      WriteLn('hci connection table full');
      hciRefuseConn(adr,$d);
      exit;
      end;
    inc(hciConnNum);
    fillchar(hciConnDat[hciConnNum],sizeof(hciConnDat[hciConnNum]),0);
    hciConnDat[hciConnNum].stt:=11;
    hciConnDat[hciConnNum].adr:=adr;
    hciConnDat[hciConnNum].tim:=currentTime;
    hciAcceptConn(adr);
    end;
  5:begin;
    {$ifdef debug2}writeLn('evnt: disconnect complete');{$endif}
    p:=ReadWordLSB(buf[sizeof(hdr)+2]);
    i:=findOneHCIconnH(p);
    if (i=0) then begin;
      WriteLn('disconnection complete with unknown peer');
      exit;
      end;
    closeHCIconnLCPs(p);
    hciConnDat[i]:=hciConnDat[hciConnNum];
    dec(hciConnNum);
    end;
  6:begin;
    {$ifdef debug2}writeLn('evnt: authentication complete');{$endif}
    end;
  7:begin;
    {$ifdef debug2}writeLn('evnt: remote name request complete');{$endif}
    byteSwapHCIaddress(buf[sizeof(hdr)+2],adr);
    ab[0]:=248;
    move(buf[sizeof(hdr)+sizeof(adr)+2],a[1],ab[0]);
    i:=pos(#0,a);
    if (i>0) then a:=copy(a,1,i-1);
    if (buf[sizeof(hdr)+1]<>0) then a:='';
    for i:=1 to lcpConnNum do begin;
      if (lcpConnDat[i].stt<>23) then continue;
      if not compareHCIaddress(lcpConnDat[i].adr,adr) then continue;
      pipeLineSend(lcpConnDat[i].pip,ab[1],ab[0]);
      lcpConnDat[i].stt:=0;
      end;
    end;
  8:begin;
    {$ifdef debug2}writeLn('evnt: encryption change');{$endif}
    end;
  9:begin;
    {$ifdef debug2}writeLn('evnt: change link key complete');{$endif}
    end;
  10:begin;
    {$ifdef debug2}writeLn('evnt: master link key complete');{$endif}
    end;
  11:begin;
    {$ifdef debug2}writeLn('evnt: read remote supported features complete');{$endif}
    end;
  12:begin;
    {$ifdef debug2}writeLn('evnt: read remote version complete');{$endif}
    end;
  13:begin;
    {$ifdef debug2}writeLn('evnt: qos setup complete');{$endif}
    end;
  14:begin;
    {$ifdef debug2}writeLn('evnt: command complete');{$endif}
    i:=ReadWordLSB(buf[sizeof(hdr)+2]);
    end;
  15:begin;
    {$ifdef debug2}writeLn('evnt: command status');{$endif}
    i:=ReadWordLSB(buf[sizeof(hdr)+3]);
    end;
  16:begin;
    {$ifdef debug2}writeLn('evnt: hardware error');{$endif}
    end;
  17:begin;
    {$ifdef debug2}writeLn('evnt: flush occured');{$endif}
    end;
  18:begin;
    {$ifdef debug2}writeLn('evnt: role changed');{$endif}
    end;
  19:begin;
    {$ifdef debug2}writeLn('evnt: completed packets');{$endif}
    p:=sizeof(hdr);
    q:=buf[sizeof(hdr)+1];
    q:=q*2+p;
    gotSltChg(buf[sizeof(hdr)+1]);
    end;
  20:begin;
    {$ifdef debug2}writeLn('evnt: mode changed');{$endif}
    end;
  21:begin;
    {$ifdef debug2}writeLn('evnt: return link keys');{$endif}
    end;
  22:begin;
    {$ifdef debug2}writeLn('evnt: link pin request');{$endif}
    byteSwapHCIaddress(buf[sizeof(hdr)+1],adr);
    hciEnterPin(adr,localPinCode);
    end;
  23:begin;
    {$ifdef debug2}writeLn('evnt: link key request');{$endif}
    byteSwapHCIaddress(buf[sizeof(hdr)+1],adr);
    hciRejectKey(adr);
    end;
  24:begin;
    {$ifdef debug2}writeLn('evnt: link key notify');{$endif}
    end;
  25:begin;
    {$ifdef debug2}writeLn('evnt: loopback');{$endif}
    end;
  26:begin;
    {$ifdef debug2}writeLn('evnt: buffer overflow');{$endif}
    end;
  27:begin;
    {$ifdef debug2}writeLn('evnt: max slot change');{$endif}
    p:=ReadWordLSB(buf[sizeof(hdr)+1]);
    i:=findOneHCIconnH(p);
    if (i=0) then begin;
      writeLn('got slot for invalid handle');
      hciCloseConn(p,$13);
      exit;
      end;
    hciConnDat[i].slt:=buf[sizeof(hdr)+3];
    end;
  28:begin;
    {$ifdef debug2}writeLn('evnt: clock offset');{$endif}
    end;
  29:begin;
    {$ifdef debug2}writeLn('evnt: packet type changed');{$endif}
    end;
  30:begin;
    {$ifdef debug2}writeLn('evnt: qos violation');{$endif}
    end;
  32:begin;
    {$ifdef debug2}writeLn('evnt: page scan repetition changed');{$endif}
    end;
  33:begin;
    {$ifdef debug2}writeLn('evnt: flow specification complete');{$endif}
    end;
  34:begin;
    {$ifdef debug2}writeLn('evnt: inquiry result with rssi');{$endif}
    gotInqRes;
    end;
  35:begin;
    {$ifdef debug2}writeLn('evnt: read remote extended features complete');{$endif}
    end;
  44:begin;
    {$ifdef debug2}writeLn('evnt: sync connection complete');{$endif}
    end;
  45:begin;
    {$ifdef debug2}writeLn('evnt: sync connection changed');{$endif}
    end;
  46:begin;
    {$ifdef debug2}writeLn('evnt: sniff subrating');{$endif}
    end;
  47:begin;
    {$ifdef debug2}writeLn('evnt: extended inquiry result');{$endif}
    gotInqRes;
    end;
  48:begin;
    {$ifdef debug2}writeLn('evnt: encryption key refresh complete');{$endif}
    end;
  49:begin;
    {$ifdef debug2}writeLn('evnt: io capability request');{$endif}
    end;
  50:begin;
    {$ifdef debug2}writeLn('evnt: io capability response');{$endif}
    end;
  51:begin;
    {$ifdef debug2}writeLn('evnt: user confirmation request');{$endif}
    end;
  52:begin;
    {$ifdef debug2}writeLn('evnt: user passkey request');{$endif}
    end;
  53:begin;
    {$ifdef debug2}writeLn('evnt: remote oob request');{$endif}
    end;
  54:begin;
    {$ifdef debug2}writeLn('evnt: simple pairing complete');{$endif}
    end;
  56:begin;
    {$ifdef debug2}writeLn('evnt: link supervision change');{$endif}
    end;
  57:begin;
    {$ifdef debug2}writeLn('evnt: enhanced flush complete');{$endif}
    end;
  59:begin;
    {$ifdef debug2}writeLn('evnt: user passkey notify');{$endif}
    end;
  60:begin;
    {$ifdef debug2}writeLn('evnt: keypress notify');{$endif}
    end;
  61:begin;
    {$ifdef debug2}writeLn('evnt: remote supported features');{$endif}
    end;
  64:begin;
    {$ifdef debug2}writeLn('evnt: physical link complete');{$endif}
    end;
  65:begin;
    {$ifdef debug2}writeLn('evnt: channel selected');{$endif}
    end;
  66:begin;
    {$ifdef debug2}writeLn('evnt: disconnect physical link');{$endif}
    end;
  67:begin;
    {$ifdef debug2}writeLn('evnt: physical link loss early warning');{$endif}
    end;
  68:begin;
    {$ifdef debug2}writeLn('evnt: physical link recovery');{$endif}
    end;
  69:begin;
    {$ifdef debug2}writeLn('evnt: logical link complete');{$endif}
    end;
  70:begin;
    {$ifdef debug2}writeLn('evnt: disconnect logical link complete');{$endif}
    end;
  71:begin;
    {$ifdef debug2}writeLn('evnt: flow spec modify complete');{$endif}
    end;
  72:begin;
    {$ifdef debug2}writeLn('evnt: data blocks complete');{$endif}
    p:=sizeof(hdr)+1;
    q:=buf[sizeof(hdr)+2];
    q:=q*2+p;
    gotSltChg(buf[sizeof(hdr)+1]);
    end;
  73:begin;
    {$ifdef debug2}writeLn('evnt: amp start test');{$endif}
    end;
  74:begin;
    {$ifdef debug2}writeLn('evnt: amp end test');{$endif}
    end;
  75:begin;
    {$ifdef debug2}writeLn('evnt: amp receiver report');{$endif}
    end;
  76:begin;
    {$ifdef debug2}writeLn('evnt: short range mode change complete');{$endif}
    end;
  77:begin;
    {$ifdef debug2}writeLn('evnt: amp status change');{$endif}
    end;
  else WriteLn('got invalid event: '+BStr(hdr.opc));
  end;
End;



Procedure hciReleq2device;
Var
  buf:array[1..1024*4] of byte;
  hdr:oneACLdataHeaderRecord absolute buf;
  i,o,p:LongInt;
Begin;
p:=sizeof(buf);
if (pipeLineRecv(localPipe,buf,p)<>0) then exit;
if (p<1) then exit;
{$ifdef debug1}
write('rx:');
for i:=1 to p do write(' '+byte2hextype(buf[i]));
writeLn('');
{$endif}
case hdr.hci of
  2:begin;
    o:=hdr.hnd and $fff;
    i:=findOneHCIconnH(o);
    if (i=0) then begin;
      writeLn('got data for invalid handle');
      hciCloseConn(o,$13);
      exit;
      end;
    hciGotAsyncPacket(hciConnDat[i],buf);
    end;
  4:hciGotEventPacket(buf);
  else writeLn('got invalid hci packet: '+BStr(hdr.hci));
  end;
End;



Function hciReleq2oneConn(var hnd:oneHCIconnectionRecord):Boolean;
Var i,o:LongInt;
Begin;
hciReleq2oneConn:=true;
i:=getTimePast(hnd.tim);
if (i>5) then hnd.slt:=localApkt;
if (i<30) then exit;
hnd.tim:=currentTime;
if (hnd.stt=21) then begin;
  writeLn('opening connection');
  hciStartConn(hnd.adr);
  exit;
  end;
for i:=1 to lcpConnNum do if (lcpConnDat[i].hnd=hnd.hnd) then exit;
closeHCIconnLCPs(hnd.hnd);
if (hnd.stt<>1) or (hnd.hnd=0) then begin;
  writeLn('clearing connection');
  hciReleq2oneConn:=false;
  exit;
  end;
writeLn('closing connection');
hciCloseConn(hnd.hnd,$13);
hnd.stt:=2;
End;


Procedure hciReleq2conns;
Var i:LongInt;
Begin;
for i:=hciConnNum downto 1 do begin;
  if hciReleq2oneConn(hciConnDat[i]) then continue;
  hciConnDat[i]:=hciConnDat[hciConnNum];
  dec(hciConnNum);
  end;
End;
