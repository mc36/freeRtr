{{$define debug}
{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc param.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Const proggyName='gtp client v1.0';
Var
  upperPipe:LongInt;
  ctrlPip:LongInt;
  dataPip:LongInt;
  ctrlAdr:OneTCPaddressRecord;
  dataAdr:OneTCPaddressRecord;
  locAddr:OneTCPaddressRecord;
  ctrlPrt:LongInt;
  dataPrt:LongInt;
  ctrlTid:LongInt;
  dataTid:LongInt;
  gotAddr:String;
  curMode:LongInt; {1=ppp, 2=ipv4, 3-ipv6}
  ctrlSeq:LongInt;
  dataSeq:LongInt;
  lastSent:LongInt;
  lastGot:LongInt;

{$include gtp.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Procedure WaitForUpperLayer;
Var
  i,o,p:LongInt;
  a:String;
Begin;
Write('waiting for upper level...');
BugOS_SignDaemoning;
pipeLineBegListen;
while (pipeLineGetIncoming(upperPipe)<>0) do relequish;
pipeLineEndListen;
a:='gtp with '+ipAddr2string(ctrlAdr)+' '+BStr(ctrlPrt);
BugOS_MyProcessInfo(i,o,p);
i:=i xor p xor o;
p:=((i shr 24) xor (i shr 16) xor (i shr 8) xor i) and $ff;
a:='12341234'#0#0#0#0#0#0#0#0+chr(p)+#255+a+#0;
i:=1;move(i,a[1],sizeof(i));
i:=1400;move(i,a[5],sizeof(i));
pipeLineSend(upperPipe,a[1],length(a));
WriteLn(' done!');
End;



Procedure sendContextRequest;
Var
  buf:array[1..4096] of byte;
  hdr:OneGTPpacketHeaderRecord absolute buf;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
Begin;
p:=sizeof(hdr);
a:=encodeOneTelNumber(paramStr(1)+'????????????????');
putOneTLVdata(buf,p,$2,a); {imsi}
putOneTLVdata(buf,p,$e,#1); {recovery}
putOneTLVdata(buf,p,$f,#1); {selection mode}
ab0:=sizeof(i);
i:=1;
move(i,ab[1],sizeof(i));
putOneTLVdata(buf,p,$10,a); {teid data}
putOneTLVdata(buf,p,$11,a); {teid control}
putOneTLVdata(buf,p,$14,#0); {nsapi}
putOneTLVdata(buf,p,$1a,#8#0); {charging}
case curMode of
  1:i:=$f001; {ppp}
  2:i:=$f121; {ipv4}
  3:i:=$f157; {ipv6}
  end;
ab0:=2;
WriteWordMSB(ab[1],i);
putOneTLVdata(buf,p,$80,a); {address}
a:=paramStr(3);
putOneTLVdata(buf,p,$83,chr(ab0)+a); {apn}
ab0:=sizeof(locAddr);
move(locAddr,ab[1],sizeof(locAddr));
if isAddressIPv4mask(locAddr) then a:=copy(a,length(IPv4addressPrefix)+1,666);
putOneTLVdata(buf,p,$85,a); {gsn}
putOneTLVdata(buf,p,$85,a); {gsn}
a:=#$91+encodeOneTelNumber(paramStr(2));
putOneTLVdata(buf,p,$86,a); {msisdn}
putOneTLVdata(buf,p,$87,#$0#$b#$92#$1f); {qos}
hdr.flg:=$32;
hdr.typ:=$10;
WriteWordMSB(hdr.len,p-8);
hdr.tei:=0;
WriteWordMSB(hdr.seq,ctrlSeq);
hdr.npd:=0;
hdr.nxt:=0;
UDPsendPacket(ctrlPip,ctrlAdr,ctrlPrt,buf,p);
End;


Procedure releq2lowerC;
Label f1;
Var
  buf:array[1..4096] of byte;
  hdr:OneGTPpacketHeaderRecord absolute buf;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
  adrC,adrD:OneTCPaddressRecord;
  teiC,teiD,adrN,cause:LongInt;
  addr:String;
Begin;
p:=sizeof(buf);
if UDPreceivePacket(ctrlPip,a,i,buf,p) then exit;
if (ctrlPrt<>i) then begin;
  Writeln('got packet from invalid port!');
  exit;
  end;
if not TCPcompareAddress(a,ctrlAdr) then begin;
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
i:=ReadWordMSB(hdr.len)+8;
if (i>p) then begin;
  WriteLn('got truncated packet!');
  exit;
  end;
p:=i;
{$ifdef debug}
WriteLn('got typ='+getMessageTypeName(hdr.typ)+' seq='+BStr(ReadWordMSB(hdr.seq))+' teid='+BStr(hdr.tei));
{$endif}
move(ctrlAdr,adrC,sizeof(adrC));
move(dataAdr,adrD,sizeof(adrD));
teiC:=0;
teiD:=0;
adrN:=0;
cause:=-1;
addr:='x';
o:=sizeof(hdr);
while (o<p) do begin;
  i:=getOneTLVdata(buf,o,a);
  {$ifdef debug}
  dumpOneBuffer('  '+getTypeLengthValueName(i),ab[1],ab0);
  {$endif}
  case i of
    $80:begin;
      case ReadWordMSB(ab[1]) and $fff of
        $001:addr:=#1#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;
        $121:addr:=#2+IPv4addressPrefix+copy(a,3,666);
        $157:addr:=#3+copy(a,3,666);
        end;
      end;
    $85:begin;
      if (length(a)=4) then a:=IPv4addressPrefix+a;
      adrN:=(adrN and 1)+1;
      if (adrN=1) then move(ab[1],adrC,sizeof(adrC)) else move(ab[1],adrD,sizeof(adrD));
      end;
    $01:cause:=ab[1];
    $10:move(ab[1],teiD,sizeof(teiD));
    $11:move(ab[1],teiC,sizeof(teiC));
    end;
  end;
{$ifdef debug}
WriteLn('adrC='+ipAddr2string(adrC)+' adrD='+ipAddr2string(adrD)+' teiC='+BStr(teiC)+' teiD='+BStr(teiD)+' addr='+ipAddr2string(addr[2])+' cause='+getCauseCodeName(cause));
{$endif}

case hdr.typ of
  $01:begin; {echo request}
    p:=sizeof(hdr);
    putOneTLVdata(buf,p,$e,#1); {recovery}
    hdr.flg:=$32;
    hdr.typ:=$02;
    WriteWordMSB(hdr.len,p-8);
    hdr.tei:=0;
    hdr.npd:=0;
    hdr.nxt:=0;
    UDPsendPacket(ctrlPip,ctrlAdr,ctrlPrt,buf,p);
    lastGot:=currentTime;
    exit;
    end;
  $11:begin;
    lastGot:=currentTime;
    if (cause<>128) then immErr('got invalid cause: '+BStr(cause));
    a:=addr;
    if (ab[1]<>curMode) then immErr('got invalid address data!');
    gotAddr:=addr;
    ctrlTid:=teiC;
    dataTid:=teiD;
    move(adrC,ctrlAdr,sizeof(adrC));
    move(adrD,dataAdr,sizeof(adrD));
    exit;
    end;
  $14:begin;
    p:=sizeof(hdr);
    putOneTLVdata(buf,p,$1,#128); {cause}
    hdr.flg:=$32;
    hdr.typ:=$15;
    WriteWordMSB(hdr.len,p-8);
    hdr.tei:=ctrlTid;
    hdr.npd:=0;
    hdr.nxt:=0;
    UDPsendPacket(ctrlPip,ctrlAdr,ctrlPrt,buf,p);
    relequish;
    immErr('got delete request!');
    end;
  $15:immErr('got delete response!');
  $02:begin; {echo response}
    lastGot:=currentTime;
    exit;
    end;
  else WriteLn('got unknown message type: '+BStr(hdr.typ));
  end;

End;



Procedure releq2lowerD;
Var
  buf:array[1..4096] of byte;
  hdr:OneGTPpacketHeaderRecord absolute buf;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
Begin;
p:=sizeof(buf);
if UDPreceivePacket(dataPip,a,i,buf,p) then exit;
if (dataPrt<>i) then begin;
  Writeln('got packet from invalid port!');
  exit;
  end;
if not TCPcompareAddress(a,dataAdr) then begin;
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
if (hdr.tei<>1) then begin;
  WriteLn('got invalid tei!');
  exit;
  end;
i:=ReadWordMSB(hdr.len)+8;
if (i>p) then begin;
  WriteLn('got truncated packet!');
  exit;
  end;
o:=sizeof(hdr)+1;
case curMode of
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
{$ifdef debug}
dumpOneBuffer('rx',buf[o],p-o+1);
{$endif}
pipeLineSend(upperPipe,buf[o],p-o+1);
End;



Procedure releq2upper;
Var
  buf:array[1..4096] of byte;
  hdr:OneGTPpacketHeaderRecord absolute buf;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
Begin;
if (getTimePast(lastSent)>10) then begin;
  WriteLn('testing remote...');
  sendEchoRequest(ctrlAdr,ctrlPrt,dataSeq);
  lastSent:=currentTime;
  end;
if (getTimePast(lastGot)>60) then immErr('remote not responding!');
o:=sizeof(hdr);
case curMode of
  1:dec(o,2); {ppp}
  2,3:dec(o,1); {ipvX}
  end;
p:=sizeof(buf);
pipeLineRecv(upperPipe,buf[o+1],p);
if (p<1) then begin;
  pipeLineStats(upperPipe,o,i,i);
  if (o<>0) then exit;
  p:=sizeof(hdr);
  putOneTLVdata(buf,p,$14,#0); {nsapi}
  putOneTLVdata(buf,p,$13,#255); {teardown}
  hdr.flg:=$32;
  hdr.typ:=$14;
  WriteWordMSB(hdr.len,p-8);
  hdr.tei:=ctrlTid;
  WriteWordMSB(hdr.seq,ctrlSeq);
  hdr.npd:=0;
  hdr.nxt:=0;
  UDPsendPacket(ctrlPip,ctrlAdr,ctrlPrt,buf,p);
  relequish;
  immErr('upper logged out!');
  end;
inc(p,o);
hdr.flg:=$32;
hdr.typ:=$ff;
WriteWordMSB(hdr.len,p-8);
hdr.tei:=dataTid;
WriteWordMSB(hdr.seq,dataSeq);
hdr.npd:=0;
hdr.nxt:=0;
UDPsendPacket(dataPip,dataAdr,dataPrt,buf,p);
inc(dataSeq);
{$ifdef debug}
dumpOneBuffer('tx',buf,p);
{$endif}
End;





Label f1,f2,f3;
Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn(proggyName+', done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');
if (paramCount<5) then immErr('using: gtp.code <imsi> <isdn> <apn> <mode:ppp/ipv4/ipv6> <host>');
curMode:=0;
a:=kicsi(paramStr(4));
if (a='ppp') then curMode:=1;
if (a='ipv4') then curMode:=2;
if (a='ipv6') then curMode:=3;
if (curMode<1) then immErr('invalid mode!');
if string2ipAddr(paramStr(5),ctrlAdr) then immErr('invalid address!');
move(ctrlAdr,dataAdr,sizeof(dataAdr));

ctrlPrt:=2123;
if UDPlistenOnPort(ctrlPip,65536,locAddr,ctrlPrt) then immErr('failed to open listening!');
WriteLn('listening on '+ipAddr2string(locAddr)+' '+BStr(ctrlPrt)+'...');
WriteLn('will send to '+ipAddr2string(ctrlAdr)+' '+BStr(ctrlPrt)+'...');

dataPrt:=2152;
if UDPlistenOnPort(dataPip,65536,locAddr,dataPrt) then immErr('failed to open listening!');
WriteLn('listening on '+ipAddr2string(locAddr)+' '+BStr(dataPrt)+'...');
WriteLn('will send to '+ipAddr2string(dataAdr)+' '+BStr(dataPrt)+'...');

upperPipe:=0;
ctrlSeq:=1;
dataSeq:=0;
lastSent:=-99999;
lastGot:=-99999;
f1:
relequish;
timer2start;
releq2lowerC;
if (getTimePast(lastSent)>5) then begin;
  WriteLn('testing remote...');
  sendEchoRequest(ctrlAdr,ctrlPrt,ctrlSeq);
  lastSent:=currentTime;
  inc(ctrlSeq);
  end;
if (ctrlSeq>10) then immErr('remote not responding!');
if (lastGot<0) then goto f1;

lastSent:=-99999;
gotAddr:='';
f2:
relequish;
timer2start;
releq2lowerC;
if (getTimePast(lastSent)>5) then begin;
  WriteLn('creating context...');
  sendContextRequest;
  lastSent:=currentTime;
  inc(ctrlSeq);
  end;
if (ctrlSeq>20) then immErr('remote not responding!');
if (gotAddr='') then goto f2;

WriteLn('address='+ipAddr2string(gotAddr[2]));
WaitForUpperLayer;

f3:
relequish;
timer2start;
releq2lowerC;
releq2lowerD;
releq2upper;
goto f3;
END.