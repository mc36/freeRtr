Procedure clearOneConnectionRecord(var con:OneConnectionRecord);
Var
  buf:array[1..4096] of byte;
  hdr:OneGTPpacketHeaderRecord absolute buf;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
  addr:OneTCPaddressRecord;
  mode:LongInt;
  cmnd:String;
Begin;
if (con.stat>0) then begin;
  p:=sizeof(hdr);
  putOneTLVdata(buf,p,$14,#0); {nsapi}
  putOneTLVdata(buf,p,$13,#255); {teardown}
  hdr.flg:=$32;
  hdr.typ:=$14;
  WriteWordMSB(hdr.len,p-8);
  WriteWordMSB(hdr.seq,con.seqC);
  hdr.tei:=con.teiC;
  hdr.npd:=0;
  hdr.nxt:=0;
  UDPsendPacket(ctrlPip,con.ctrl,ctrlPrt,buf,p);
  end;
cmnd:=con.cmnd;
addr:=con.addr;
mode:=con.mode;
pipeLineClose(con.pipe);
fillchar(con,sizeof(con),0);
con.cmnd:=cmnd;
con.addr:=addr;
con.mode:=mode;
End;



Function doConn(var con:OneConnectionRecord):Boolean;
Label f1;
Var
  buf:array[1..4096] of byte;
  hdr:OneGTPpacketHeaderRecord absolute buf;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
Begin;
doConn:=false;
if (con.stat=0) then exit;
if (GetTimePast(con.timR)>60) then begin;
  doConn:=True;
  exit;
  end;
if (GetTimePast(con.timT)>15) then begin;
  sendEchoRequest(con.ctrl,ctrlPrt,con.seqD);
  updateOneSGSNaddr(con.ctrl,2);
  end;
case con.stat of
  3:begin; {connected}
    o:=sizeof(hdr);
    case con.mode of
      1:dec(o,2); {ppp}
      2,3:dec(o,1); {ipvX}
      end;
    p:=sizeof(buf);
    pipeLineRecv(con.pipe,buf[o+1],p);
    if (p<1) then begin;
      pipeLineStats(con.pipe,o,i,i);
      if (o<>0) then exit;
      goto f1;
      end;
    inc(p,o);
    hdr.flg:=$32;
    hdr.typ:=$ff;
    WriteWordMSB(hdr.len,p-8);
    hdr.tei:=con.teiD;
    WriteWordMSB(hdr.seq,con.seqD);
    hdr.npd:=0;
    hdr.nxt:=0;
    UDPsendPacket(dataPip,con.data,dataPrt,buf,p);
    inc(con.seqD);
    end;
  2:begin; {starting}
    con.timT:=currentTime;
    end;
  1:begin; {waiting}
    if (FindConnectByStat(2)<>0) then exit;
    a:=con.cmnd;
    {$ifdef debug}
    WriteLn('starting '+a+'...');
    {$endif}
    i:=pos(' ',a);
    if (i<1) then i:=666;
    if (xExecBgnd(copy(a,1,i-1),copy(a,i+1,255),o)<>0) then o:=0;
    con.stat:=2;
    exit;
    end;
  0:;; {disconnect}
  else begin; f1:con.stat:=666;doConn:=True;exit; end;
  end;
End;



Procedure doLowerD;
Var
  buf:array[1..4096] of byte;
  hdr:OneGTPpacketHeaderRecord absolute buf;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p,q:LongInt;
Begin;
p:=sizeof(buf);
if UDPreceivePacket(dataPip,a,i,buf,p) then exit;
if (dataPrt<>i) then begin;
  Writeln('got packet from invalid port!');
  exit;
  end;
q:=FindConnectByTeid(hdr.tei);
if (q<1) then begin;
  writeln('got packet for invalid teid!');
  exit;
  end;
if not TCPcompareAddress(a,ConnectionDat^[q].data) then begin;
  writeln('got packet from invalid address!');
  exit;
  end;
if (hdr.flg and $e0<>$20) then begin;
  WriteLn('got packet with invalid version!');
  exit;
  end;
if (hdr.flg and $10=0) then begin;
  WriteLn('got charging packet!');
  exit;
  end;
if (hdr.flg and 8<>0) then begin;
  WriteLn('got packet with extension headers!');
  exit;
  end;
if (hdr.typ<>$ff) then begin;
  WriteLn('got invalid packet type!');
  exit;
  end;
i:=ReadWordMSB(hdr.len)+8;
if (i>p) then begin;
  WriteLn('got truncated packet!');
  exit;
  end;
if (ConnectionDat^[q].stat<>3) then begin;
  WriteLn('got packet for not ready session!');
  exit;
  end;
o:=sizeof(hdr)+1;
case ConnectionDat^[q].mode of
  1:begin; {ppp}
    dec(o);
    buf[o]:=3;
    dec(o);
    buf[o]:=11;
    end;
  2,3:begin; {ipvX}
    dec(o);
    buf[o]:=11;
    end;
  end;
pipeLineSend(ConnectionDat^[q].pipe,buf[o],p-o+1);
End;



Procedure doLowerC;
Var
  buf:array[1..4096] of byte;
  hdr:OneGTPpacketHeaderRecord absolute buf;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p,q:LongInt;
  adrC,adrD:OneTCPaddressRecord;
  teiC,teiD,adrN,adrT,cause:LongInt;
  apn,imei,isdn:String;
Begin;
p:=sizeof(buf);
if UDPreceivePacket(ctrlPip,adrC,i,buf,p) then exit;
move(adrC,adrD,sizeof(adrD));
if (ctrlPrt<>i) then begin;
  Writeln('got packet from invalid port!');
  exit;
  end;
if (hdr.flg and $e0<>$20) then begin;
  WriteLn('got packet with invalid version!');
  exit;
  end;
if (hdr.flg and $10=0) then begin;
  WriteLn('got charging packet!');
  exit;
  end;
if (hdr.flg and 8<>0) then begin;
  WriteLn('got packet with extension headers!');
  exit;
  end;
i:=ReadWordMSB(hdr.len)+8;
if (i>p) then begin;
  WriteLn('got truncated packet!');
  exit;
  end;
p:=i;
{$ifdef debug}
WriteLn('got typ='+getMessageTypeName(hdr.typ)+' seq='+BStr(ReadWordMSB(hdr.seq))+' teid='+BStr(hdr.tei));
{$endif}
teiC:=0;
teiD:=0;
adrN:=0;
adrT:=0;
apn:='';
imei:='';
isdn:='';
cause:=-1;
o:=sizeof(hdr);
while (o<p) do begin;
  i:=getOneTLVdata(buf,o,a);
  {$ifdef debug}
  dumpOneBuffer('  '+getTypeLengthValueName(i),ab[1],ab0);
  {$endif}
  case i of
    $80:begin;
      case ReadWordMSB(ab[1]) and $fff of
        $001:adrT:=1;
        $121:adrT:=2;
        $157:adrT:=3;
        end;
      end;
    $85:begin;
      if (length(a)=4) then a:=IPv4addressPrefix+a;
      adrN:=(adrN and 1)+1;
      if (adrN=1) then move(ab[1],adrC,sizeof(adrC)) else move(ab[1],adrD,sizeof(adrD));
      end;
    $83:apn:=copy(a,2,ab[1]);
    $01:cause:=ab[1];
    $02:imei:=decodeOneTelNumber(a);
    $86:isdn:=decodeOneTelNumber(copy(a,2,666));
    $10:move(ab[1],teiD,sizeof(teiD));
    $11:move(ab[1],teiC,sizeof(teiC));
    end;
  end;
{$ifdef debug}
WriteLn('adrC='+ipAddr2string(adrC)+' adrD='+ipAddr2string(adrD)+' teiC='+BStr(teiC)+' teiD='+BStr(teiD)+' addr='+BStr(adrT)+' apn='+apn+' imei='+imei+' isdn='+isdn+' cause='+getCauseCodeName(cause));
{$endif}
case hdr.typ of
  $01:begin; {echo request}
    updateOneSGSNaddr(adrC,1);
    p:=sizeof(hdr);
    putOneTLVdata(buf,p,$e,#1); {recovery}
    hdr.flg:=$32;
    hdr.typ:=$02;
    WriteWordMSB(hdr.len,p-8);
    hdr.tei:=0;
    hdr.npd:=0;
    hdr.nxt:=0;
    UDPsendPacket(ctrlPip,adrC,ctrlPrt,buf,p);
    exit;
    end;
  $02:begin; {echo reply}
    updateOneSGSNaddr(adrC,1);
    exit;
    end;
  $10:begin; {create context}
    p:=0;
    if (teiC=0) then begin;
      WriteLn('got request without control teid!');
      exit;
      end;
    if (teiC=0) then begin;
      WriteLn('got request without data teid!');
      exit;
      end;
    if (imei='') then begin;
      WriteLn('got request without imei!');
      exit;
      end;
    p:=0;
    for i:=1 to ConnectionNum do begin;
      if (ConnectionDat^[i].stat=0) then continue;
      if (ConnectionDat^[i].imei=imei) then p:=i;
      end;
    if (p<>0) then if (ConnectionDat^[p].mode<>adrT) then begin;
      clearOneConnectionRecord(ConnectionDat^[p]);
      p:=0;
      end;
    if (p=0) then for i:=1 to ConnectionNum do begin;
      if (ConnectionDat^[i].stat<>0) then continue;
      if (ConnectionDat^[i].mode=adrT) then p:=i;
      end;
    if (p=0) then begin;
      WriteLn('no more slot for address type '+BStr(adrT));
      exit;
      end;
    q:=p;
    repeat
      nextTeid:=(nextTeid and $fffffff)+1;
      until (FindConnectByTeid(nextTeid)<1);
    p:=sizeof(hdr);
    putOneTLVdata(buf,p,$1,#128); {cause}
    putOneTLVdata(buf,p,$8,#254); {reordering}
    putOneTLVdata(buf,p,$e,#2); {recivery}
    ab0:=sizeof(nextTeid);
    move(nextTeid,ab[1],sizeof(nextTeid));
    putOneTLVdata(buf,p,$10,a); {teid data}
    putOneTLVdata(buf,p,$11,a); {teid control}
    putOneTLVdata(buf,p,$7f,a); {charging id}
    a:=convertOneAddress(ConnectionDat^[q].addr);
    case ConnectionDat^[q].mode of
      1:a:=#$f0#$01;
      2:a:=#$f1#$21+a;
      3:a:=#$f1#$57+a;
      else exit;
      end;
    putOneTLVdata(buf,p,$80,a); {end user address}
    putOneTLVdata(buf,p,$85,convertOneAddress(ctrlAdr)); {gsn address}
    putOneTLVdata(buf,p,$85,convertOneAddress(dataAdr)); {gsn address}
    putOneTLVdata(buf,p,$87,#$0#$b#$92#$1f); {qos}
    hdr.flg:=$32;
    hdr.typ:=$11;
    WriteWordMSB(hdr.len,p-8);
    hdr.tei:=teiC;
    hdr.npd:=0;
    hdr.nxt:=0;
    UDPsendPacket(ctrlPip,adrC,ctrlPrt,buf,p);
    if (ConnectionDat^[q].stat=0) then ConnectionDat^[q].stat:=1;
    ConnectionDat^[q].timR:=currentTime;
    ConnectionDat^[q].timT:=currentTime;
    ConnectionDat^[q].seqC:=ReadWordMSB(hdr.seq);
    ConnectionDat^[q].seqD:=0;
    ConnectionDat^[q].teiL:=nextTeid;
    ConnectionDat^[q].teiC:=teiC;
    ConnectionDat^[q].teiD:=teiD;
    ConnectionDat^[q].imei:=imei;
    ConnectionDat^[q].isdn:=isdn;
    ConnectionDat^[q].timR:=currentTime;
    ConnectionDat^[q].timT:=currentTime;
    move(adrC,ConnectionDat^[q].ctrl,sizeof(adrC));
    move(adrD,ConnectionDat^[q].data,sizeof(adrD));

{}{}WriteLn(bstr(q));{}{}{}{}
    exit;
    end;
  $14:begin; {delete context}
    q:=FindConnectByTeid(hdr.tei);
    if (q<1) then exit;
    if not TCPcompareAddress(adrC,ConnectionDat^[q].ctrl) then exit;
    p:=sizeof(hdr);
    putOneTLVdata(buf,p,$1,#128); {cause}
    hdr.flg:=$32;
    hdr.typ:=$15;
    WriteWordMSB(hdr.len,p-8);
    hdr.tei:=ConnectionDat^[q].teiC;
    hdr.npd:=0;
    hdr.nxt:=0;
    UDPsendPacket(ctrlPip,adrC,ctrlPrt,buf,p);
    ConnectionDat^[q].stat:=0;
    clearOneConnectionRecord(ConnectionDat^[q]);
    exit;
    end;
  $15:begin; {delete context}
    exit;
    end;
  else WriteLn('got unknown message type: '+BStr(hdr.typ));
  end;

End;
