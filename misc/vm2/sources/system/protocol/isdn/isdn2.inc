Procedure layer1releq;
Label f1,f2;
Var
  buf:array[1..4*1024] of byte;
  con:OneConnectionRecord;
  i,o,p:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  msg:LongInt;
  cref,src,trg:String;
  num,chid,bear:LongInt;

Function genTest(send:Boolean):Boolean;
Label f1;
Var i:LongInt;
Begin;
genTest:=True;
if (num<1) then goto f1;
if (bear>=0) then begin;
  if (ConnectionDat^[num].bear shr 20<>bear shr 20) then goto f1;
  ConnectionDat^[num].bear:=bear;
  end;
if (chid>=0) then if (chid<>ConnectionDat^[num].chn) then goto f1;
genTest:=False;
exit;
f1:
i:=findOneChannelD(chid);
if (i>0) then ConnectionDat^[i].stat:=666;
if send then layer2sendGeneric(cref,$4d,101);
End;


Begin;
if (getTimePast(layer1lasTime)<5) then goto f1;
layer1lasTime:=currentTime;
inc(layer1tryDone);
if (layer1tryDone>100+16*2) then immErr('remote possibly dead!');
if (layer1packNum<1) then begin;
  if (layer1tryDone<15) then begin; layer1sendRR(true);goto f1; end;
  layer1sendSABME(true);
  goto f1;
  end;
a:=layer1packDat[layer1packSnd];
buf[1]:=controlChannel;
buf[2]:=pollFinalXor;
buf[3]:=termEndptId;
buf[4]:=(layer1seqSend+1) shl 1;
buf[5]:=(layer1seqRecv shl 1) or 1;
move(ab[1],buf[6],ab0);
pipeLineSend(layer1devPipe,buf,ab0+5);
{$ifdef debug1}dumpOnePacket('sent i frame',ab[1],ab0);{$endif}

f1:
o:=sizeof(buf);
pipeLineRecv(layer1devPipe,buf,o);
if (o<1) then begin;
  pipeLineStats(layer1devPipe,o,i,i);
  if (o=0) then immErr('lower level closed connection!');
  exit;
  end;
if (buf[1]<>controlChannel) then begin;
  p:=buf[1];
  if (p<1) or (p>ConnectionNum) then begin;
    WriteLn('got data for not existent channel: '+BStr(p));
    goto f1;
    end;
  case ConnectionDat^[p].stat of
    3:pipeLineSend(ConnectionDat^[p].pipeP,buf[2],o-1);
    1:WriteLn('got data for disabled channel: '+BStr(p));
    else WriteLn('got data for not up channel: '+BStr(p));
    end;
  goto f1;
  end;
if (o<4) then begin;
  WriteLn('got too short packet!');
  goto f1;
  end;
i:=buf[4];
if (i and 1=0) then if (buf[2] and 2<>pollFinalXor) then begin;
  if (buf[5] and 1<>0) then layer1sendRR(false);
  if layer1gotAck(buf[5]) then WriteLn('got i with bad rx sequence number!');
  if (layer1seqRecv and $7f<>buf[4] shr 1) then begin;
    WriteLn('got i with bad tx sequence number!');
    goto f1;
    end;
  layer1tryDone:=0;
  inc(layer1seqRecv);
  goto f2;
  end;
o:=0;
case i of
  $01:{$ifdef debug1}WriteLn('got rr...'){$endif};
  $05:{$ifdef debug1}WriteLn('got rnr...'){$endif};
  $09:{$ifdef debug1}WriteLn('got rej...'){$endif};
  else o:=1;
  end;
if (o=0) then begin;
  if not layer1gotAck(buf[5]) then goto f1;
  WriteLn('got rr with bad sequence number!');
  goto f1;
  end;
if (buf[2] and 2<>pollFinalXor) then inc(i,$100);
case i and $1ef of
  $16f:begin;
    {$ifdef debug1}WriteLn('got sabme...');{$endif}
    layer1sendSABME(layer1tryDone<90);
    layer1sendUA;
    goto f1;
    end;
  $063:begin;
    {$ifdef debug1}WriteLn('got ua...');{$endif}
    layer1tryDone:=0;
    end;
  else begin;
    WriteLn('got unknown frame: $'+BStr(i shr 8)+byte2hextype(i));
    end;
  end;
if (i and $110=$110) then layer1sendRR(false);
goto f1;
f2:
dec(o,5);
if (o<2) then begin;
  WriteLn('got too short i frame!');
  goto f1;
  end;
move(buf[6],buf,o);
{$ifdef debug1}dumpOnePacket('got i frame',buf,o);{$endif}
if (buf[1]<>8) then begin;
  writeLn('got invalid i protocol!');
  goto f1;
  end;
ab0:=buf[2] and $f;
move(buf[3],ab[1],ab0);
cref:=a;
p:=ab0+3;
msg:=buf[p] and $7f;
if (p>o) then begin;
  WriteLn('got truncated i packet!');
  goto f1;
  end;
{$ifdef debug2}WriteLn('got i message: type='+getMessageTypeNames(msg)+'; callref='+convertToHexstring(cref));{$endif}
bear:=-2;
chid:=-2;
src:='';
trg:='';
while (p<o) do begin;
  inc(p);
  i:=buf[p];
  if (i and $80<>0) then ab0:=0 else begin;
    inc(p);
    move(buf[p],a,sizeof(a));
    inc(p,ab0);
    end;
  {$ifdef debug2}WriteLn('  option: type='+getOptionTypeNames(i)+'; data='+convertToHexstring(a));{$endif}
  case i of
    $04:bear:=decodeBearerCapa(a);
    $18:chid:=decodeChannelId(a);
    $6c:src:=copy(a,2,666);
    $6d:src:=src+':'+copy(a,2,666);
    $70:trg:=copy(a,2,666);
    $71:trg:=trg+':'+copy(a,2,666);
    end;
  end;
{$ifdef debug2}WriteLn('src='+src+'; trg='+trg+'; ch='+BStr(chid)+' bear='+BStr(bear shr 20)+'@'+BStr(bear and $fffff));{$endif}
num:=findOneCallerId(cref);
case msg of
  $05:begin; {setup}
    if (num>0) then begin;
      if genTest(true) then goto f1;
      con:=ConnectionDat^[num];
      if (con.stat=4) then layer2sendCallProc(con) else layer2sendConnect(con);
      goto f1;
      end;
    num:=findOneChannelD(chid);
    if (num<1) then begin;
      layer2sendGeneric(cref,$5a,34);
      goto f1;
      end;
    if clearOneConnectRec(ConnectionDat^[num]) then begin;
      layer2sendGeneric(cref,$5a,42);
      goto f1;
      end;
    if (bear<0) then begin;
      layer2sendGeneric(cref,$5a,65);
      goto f1;
      end;
    if (bear<>$10fa00) then begin;
      layer2sendGeneric(cref,$5a,57);
      goto f1;
      end;
    con:=ConnectionDat^[num];
    con.bear:=bear;
    con.cid:=cref;
    kicserel(' ','',src);
    kicserel(' ','',trg);
    con.srcA:=src;
    con.trgA:=trg;
    con.mode:=3;
    con.stat:=4;
    o:=0;
    for i:=ConnectionNum downto 1 do begin;
      if (ConnectionDat^[i].stat<>0) then continue;
      if (ConnectionDat^[i].cmnd='') then continue;
      o:=i;
      end;
    if (o<1) then begin;
      WriteLn('out of incoming connection slots!');
      layer2sendGeneric(cref,$5a,47);
      goto f1;
      end;
    exchangeStrings(ConnectionDat^[o].cmnd,con.cmnd);
    ConnectionDat^[num]:=con;
    layer2sendCallProc(con);
    goto f1;
    end;
  $0d:genTest(true); {setup ack}
  $01,$02,$03:genTest(true); {alerting, call proceeding, progress}
  $07:begin; {connect}
    if genTest(true) then goto f1;
    layer2sendGeneric(cref,$0f,-1);
    setup4communication(ConnectionDat^[num]);
    end;
  $0f:begin; {connect ack}
    if genTest(true) then goto f1;
    setup4communication(ConnectionDat^[num]);
    end;
  $45,$4d,$5a:begin; {disconnect, release, release done}
    if (msg<>$5a) then layer2sendGeneric(cref,$5a,31);
    if not genTest(false) then begin;
      ConnectionDat^[num].stat:=666;
      ConnectionDat^[num].cid:='';
      end;
    i:=findOneChannelD(chid);
    if (i>0) then ConnectionDat^[i].stat:=666;
    end;
  $26:begin; {resume}
    if genTest(true) then goto f1;
    layer2sendGeneric(cref,$2e,-1);
    end;
  $25:begin; {suspend}
    if genTest(true) then goto f1;
    layer2sendGeneric(cref,$21,85);
    end;
  $2d:begin; {suspend ack}
    if genTest(true) then goto f1;
    layer2sendGeneric(cref,$26,-1);
    end;
  $46:begin; {restart}
    layer2sendGeneric(cref,$4e,-1);
    layer1sendSABME(false);
    end;
  $4e:layer1sendSABME(false); {restart ack}
  $75:begin; {status enquery}
    if genTest(true) then goto f1;
    layer2sendGeneric(cref,$4e,$7d);
    end;
  $7b,$6e:genTest(true); {information, notify}
  $2e,$22:genTest(true); {resume ack, resume rej}
  $21:genTest(true); {suspend rej}
  $7d,$20:genTest(true); {status, user info}
  $60,$79:; {segment, congestion control}
  else begin;
    layer2sendGeneric(cref,$4d,111);
    WriteLn('got invalid message type');
    end;
  end;


goto f1;
End;
