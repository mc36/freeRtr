Function releq2user(var con:OneConnectionRecord):Boolean;
Label f1,f2,f3;
Var
  buf:array[1..16*1024] of byte;
  bufI:array[1..1] of Integer absolute buf;
  i,o,p,q:LongInt;
  a,b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;

Function doRec:Boolean;
Begin;
doRec:=True;
if (con.rcvS<bytesPerPack) then exit;
q:=0;
for p:=1 to bytesPerPack do begin;
  o:=alaw2pcmConverter(con.rcvD[p]);
  for i:=1 to bufferSampleSize do begin;
    inc(q);
    bufI[q]:=o;
    end;
  end;
deleteFromBuffer(con.rcvD,con.rcvS,bytesPerPack);
pipeLineSend(con.pipe,buf,q shl 1);
doRec:=False;
End;

Procedure doPly;
Begin;
move(buf[2],buf,sizeof(buf));
deleteFromBuffer(con.sndD,con.sndS,bytesPerPack-oneBufferMaxSize+con.sndS);
p:=0;
for q:=1 to bytesPerPack do begin;
  o:=0;
  for i:=1 to bufferSampleSize do begin;
    inc(p);
    inc(o,bufI[p]);
    end;
  o:=o div bufferSampleSize;
  inc(con.sndS);
  con.sndD[con.sndS]:=pcm2alawConverter(o);
  end;
End;

Function wtPly:Boolean;
Begin;
wtPly:=(oneBufferMaxSize-con.sndS<bytesPerPack);
End;

Begin;
releq2user:=False;
if (con.strm=0) then goto f3;
if (abs(currentTime-con.sndT)>=ticksPerPack) then begin;
  if (con.sndS<bytesPerPack) then i:=con.sndS else i:=bytesPerPack;
  buf[1]:=con.rtpT;
  move(con.sndD,buf[2],i);
  deleteFromBuffer(con.sndD,con.sndS,i);
  if (i>0) then pipeLineSend(con.strm,buf,i+1);
  if (con.sndP mod 5=0) then con.sndT:=-99999 else con.sndT:=currentTime;
  inc(con.sndP);
  end;
o:=sizeof(buf);
pipeLineRecv(con.strm,buf,o);
if (o<2) then begin;
  pipeLineStats(con.strm,o,i,i);
  if (o<>0) then goto f3;
  WriteLn('rtp closed from '+ipAddr2string(con.addr)+' '+BStr(con.port));
  pipeLineClose(con.strm);
  con.strm:=0;
  con.stat:=5;
  exit;
  end;
dec(o);
if (buf[1]<>con.rtpT) then begin;
  WriteLn('got unknown rtp type from '+ipAddr2string(con.addr)+' '+BStr(con.port));
  goto f3;
  end;
if (o>oneBufferMaxSize) then o:=oneBufferMaxSize;
deleteFromBuffer(con.rcvD,con.rcvS,o-oneBufferMaxSize+con.rcvS);
move(buf[2],con.rcvD[con.rcvS+1],o);
inc(con.rcvS,o);
con.time:=currentTime;
f3:
if (getTimePast(con.time)>2*60) then begin;
  WriteLn('timeout with '+ipAddr2string(con.addr)+' '+BStr(con.port));
  con.stat:=5;
  end;
case con.stat of
  0:begin; {serve as sound device}
    o:=sizeof(buf);
    pipeLineRecv(con.pipe,buf,o);
    if (o<1) then begin;
      pipeLineStats(con.pipe,o,i,i);
      if (o<>0) then exit;
      WriteLn('upper closed with '+ipAddr2string(con.addr)+' '+BStr(con.port));
      con.stat:=5;
      exit;
      end;
    con.time:=currentTime;
    case buf[1] of
      6:begin; {play sample}
        doPly;
        if wtPly then begin; con.stat:=10;exit; end;
        i:=$01010101;
        pipeLineSend(con.pipe,i,1);
        end;
      7:begin; {record sample}
        if doRec then con.stat:=9;
        end;
      8:begin; {play and record sample}
        doPly;
        if wtPly then begin; con.stat:=11;exit; end;
        if doRec then con.stat:=11;
        end;
      1:begin; {get card identification}
        a:=proggyName+' with '+ipAddr2string(con.addr)+' '+BStr(con.port)+#13#10;
        pipeLineSend(con.pipe,a[1],length(a));
        end;
      2:begin; {get mixer names}
        a:=#0#0#0#0#0#0#0#0'no available volumebars'#0;
        pipeLineSend(con.pipe,a[1],length(a));
        end;
      3:begin; {get mixer values}
        a:=#0#0#0#0;
        pipeLineSend(con.pipe,a[1],length(a));
        end;
      4:begin; {set mixer values}
        a:=#0;
        pipeLineSend(con.pipe,a[1],length(a));
        end;
      5:begin; {get buffer info}
        i:=bytesPerPack*bufferSampleSize*2;
        pipeLineSend(con.pipe,i,sizeof(i));
        end;
      9:begin; {dial a number}
        ab0:=o-1;
        move(buf[2],ab[1],ab0);
        kicserel(' ','',a);
        kicserel('<','',a);
        kicserel('>','',a);
        kicserel('"','',a);
        kicserel('`','',a);
        kicserel('''','',a);
        if (con.strm<>0) then begin;
          i:=0;
          pipeLineSend(con.pipe,i,1);
          exit;
          end;
        if (pos('@',a)<1) then a:=a+'@'+generateServerAddr;
        con.rtpA:=serverAddr;
        con.rtpP:=serverPort;
        if RTPopenConnection(con.strm,32768,con.rtpA,con.rtpP) then begin; con.stat:=9999;exit; end;
        con.Scid:='call'+BStr(currentTime)+'-'+BStr(con.pipe)+'@'+ipAddr2string(localAddr);
        con.Ssrc:='"'+myNumber+'" <sip:'+myNumber+'@'+generateMyAddr+'>;tag=call'+BStr(currentTime);
        con.Strg:='<sip:'+a+'>';
        con.Svia:='SIP/2.0/UDP '+generateMyAddr;
        con.Sseq:=BStr(currentTime)+' INVITE';
        con.addr:=serverAddr;
        con.port:=serverPort;
        WriteLn('sending invite to '+ipAddr2string(con.addr)+' '+BStr(con.port));
        sendOneSIPmessage('INVITE sip:'+getPhoneNumber(con.Strg)+' SIP/2.0',#13,generateSDPdata(con),con);
        con.rtry:=0;
        con.time:=currentTime;
        con.stat:=7;
        end;
      10:begin; {hangup call}
        i:=$01010101;
        pipeLineSend(con.pipe,i,1);
        if (con.strm=0) then exit;
        WriteLn('upper disconnecting with '+ipAddr2string(con.addr)+' '+BStr(con.port));
        con.stat:=5;
        end;
      else begin;
        WriteLn('upper fool with '+ipAddr2string(con.addr)+' '+BStr(con.port));
        con.stat:=5;
        end;
      end;
    end;
  9:begin; {wait for recording buffer}
    if doRec then exit;
    con.stat:=0;
    end;
  10:begin; {wait for playback buffer}
    if wtPly then exit;
    i:=$01010101;
    pipeLineSend(con.pipe,i,1);
    con.stat:=0;
    end;
  11:begin; {wait for play and record buffer}
    if wtPly then exit;
    con.stat:=0;
    if doRec then con.stat:=9;
    end;
  1:begin; {wait for execute}
    if (startingProc<>0) then exit;
    a:=inCallPar;
    kicserel('`',getPhoneNumber(con.Ssrc),a);
    kicserel('''',getPhoneNumber(con.Strg),a);
    {$ifdef debug}
    WriteLn('starting '+inCallPrg+' '+a+'...');
    {$endif}
    if (xExecBgnd(inCallPrg,a,startingProc)<>0) then startingProc:=0;
    WriteLn('sending ringing to '+ipAddr2string(con.addr)+' '+BStr(con.port));
    sendOneSIPmessage('SIP/2.0 180 ringing',#13,'',con);
    con.time:=currentTime;
    con.stat:=2;
    end;
  2:begin; {wait for logged in}
    if (startingProc<>0) then exit;
    f1:
    WriteLn('sending notfound to '+ipAddr2string(con.addr)+' '+BStr(con.port));
    sendOneSIPmessage('SIP/2.0 404 not found','','',con);
    con.stat:=999;
    releq2user:=True;
    end;
  3:begin; {just logged in}
    if RTPopenConnection(con.strm,32768,con.rtpA,con.rtpP) then goto f1;
    WriteLn('sending ok to '+ipAddr2string(con.addr)+' '+BStr(con.port));
    sendOneSIPmessage('SIP/2.0 200 ok',#13,generateSDPdata(con),con);
    con.rtry:=0;
    con.time:=currentTime;
    con.stat:=4;
    end;
  4:begin; {waiting for ack}
    if (getTimePast(con.time)<5) then exit;
    inc(con.rtry);
    if (con.rtry>12) then goto f1;
    WriteLn('resending ok to '+ipAddr2string(con.addr)+' '+BStr(con.port));
    sendOneSIPmessage('SIP/2.0 200 ok',#13,generateSDPdata(con),con);
    con.time:=currentTime;
    end;
  5:begin; {begin disconnect}
    f2:
    if (con.strm=0) then begin; con.stat:=999;exit; end;
    WriteLn('sending bye to '+ipAddr2string(con.addr)+' '+BStr(con.port));
    if (con.clld=1) then begin;
      a:=con.Strg;
      con.Strg:=con.Ssrc;
      con.Ssrc:=a;
      end;
    con.Sseq:=BStr(currentTime)+' BYE';
    sendOneSIPmessage('BYE sip:'+getPhoneNumber(con.Strg)+' SIP/2.0','','',con);
    con.rtry:=0;
    con.time:=currentTime;
    con.stat:=6;
    end;
  6:begin; {wait for ok}
    if (getTimePast(con.time)<5) then exit;
    inc(con.rtry);
    if (con.rtry>12) then con.stat:=999;
    WriteLn('resending bye to '+ipAddr2string(con.addr)+' '+BStr(con.port));
    sendOneSIPmessage('BYE sip:'+getPhoneNumber(con.Strg)+' SIP/2.0','','',con);
    con.time:=currentTime;
    end;
  7:begin; {wait for ring}
    pipeLineStats(con.pipe,o,i,i);
    if (o=0) then begin; con.stat:=5;exit; end;
    if (getTimePast(con.time)<5) then exit;
    inc(con.rtry);
    if (con.rtry>12) then goto f2;
    WriteLn('resending invite to '+ipAddr2string(con.addr)+' '+BStr(con.port));
    sendOneSIPmessage('INVITE sip:'+getPhoneNumber(con.Strg)+' SIP/2.0',#13,generateSDPdata(con),con);
    con.time:=currentTime;
    end;
  8:begin; {wait for ok}
    pipeLineStats(con.pipe,o,i,i);
    if (o=0) then begin; con.stat:=5;exit; end;
    end;
  else releq2user:=True;
  end;
End;
