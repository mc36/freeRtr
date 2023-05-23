Procedure releq2upper;
Label f1;
Var
  buf:array[1..32*1024] of byte;
  bufI:array[1..1] of Integer absolute buf;
  i,o,p,q:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;

Function doRec:Boolean;
Begin;
doRec:=True;
if (recvSiz<bytesPerPack) then exit;
q:=0;
for p:=1 to bytesPerPack do begin;
  o:=alaw2pcmConverter(recvBuf[p]);
  for i:=1 to bufferSampleSize do begin;
    inc(q);
    bufI[q]:=o;
    end;
  end;
deleteFromBuffer(recvBuf,recvSiz,bytesPerPack);
pipeLineSend(upperPipe,buf,q shl 1);
doRec:=False;
End;

Procedure doPly;
Begin;
move(buf[2],buf,sizeof(buf));
deleteFromBuffer(sendBuf,sendSiz,bytesPerPack-sizeof(sendBuf)+sendSiz);
p:=0;
for q:=1 to bytesPerPack do begin;
  o:=0;
  for i:=1 to bufferSampleSize do begin;
    inc(p);
    inc(o,bufI[p]);
    end;
  o:=o div bufferSampleSize;
  inc(sendSiz);
  sendBuf[sendSiz]:=pcm2alawConverter(o);
  end;
End;

Function wtPly:Boolean;
Begin;
wtPly:=(sizeof(sendBuf)-sendSiz<bytesPerPack);
End;


Begin;
if (upperStat=7) and (callProcNum<>-1) then
 if not BugOS_ProcessExists(callProcNum) then upperStat:=1;
if (upperPipe=0) then begin;
  if (upperStat=1) then upperStat:=0;
  if (pipeLineGetIncoming(upperPipe)<>0) then exit;
  pipeLineStats(upperPipe,o,i,i);
  if (upperStat=7) and (callProcNum<>o) then begin;
    WriteLn('not call handler refused to log in!');
    pipeLineClose(upperPipe);
    upperPipe:=0;
    exit;
    end;
  callProcNum:=o;
  WriteLn('upper logged in!');
  voicePipe:=0;
  if (upperStat<>7) then begin; upperStat:=0;exit; end;
  voiceAddr:=localAddr;
  voicePort:=localPort;
  voicePipe:=0;
  voiceMode:=0;
  sentPck:=0;
  sentOct:=0;
  rcvdPck:=0;
  rcvdOct:=0;
  skinnySendSoftKey(callIdentify,11);
  inc(upperStat);
  exit;
  end;
case upperStat of
  0:; {command}
  4:begin; {wait for recording}
    if doRec then goto f1;
    upperStat:=0;
    exit;
    end;
  5:begin; {wait for playing}
    if wtPly then goto f1;
    i:=$01010101;
    pipeLineSend(upperPipe,i,1);
    upperStat:=0;
    exit;
    end;
  6:begin; {wait for recording and playing}
    if wtPly then goto f1;
    upperStat:=0;
    if doRec then upperStat:=4;
    exit;
    end;
  1:begin; {disconnect}
    WriteLn('disconnecting...');
    i:=$01010101;
    pipeLineSend(upperPipe,i,1);
    pipeLineClose(voicePipe);
    pipeLineClose(upperPipe);
    voicePipe:=0;
    upperPipe:=0;
    voiceMode:=0;
    skinnySendSoftKey(callIdentify,9);
    upperStat:=0;
    exit;
    end;
  2:goto f1; {wait for dialtone}
  3:begin; {wait for connected}
    if (voiceMode<>3) then goto f1;
    i:=$01010101;
    pipeLineSend(upperPipe,i,1);
    upperStat:=0;
    exit;
    end;
  7:goto f1; {wait for call handler to log in}
  8:begin; {wait for connected}
    if (voiceMode<>3) then goto f1;
    upperStat:=0;
    exit;
    end;
  else begin;
    WriteLn('bad state!');
    upperStat:=1;
    exit;
    end;
  end;
p:=sizeof(buf);
pipeLineRecv(upperPipe,buf,p);
if (p<1) then begin;
  f1:
  pipeLineStats(upperPipe,o,i,i);
  if (o<>0) then exit;
  WriteLn('upper logged out; state='+BStr(upperStat));
  upperStat:=1;
  exit;
  end;
case buf[1] of
  6:begin; {play sample}
    doPly;
    if wtPly then begin; upperStat:=5;exit; end;
    i:=$01010101;
    pipeLineSend(upperPipe,i,1);
    end;
  7:begin; {record sample}
    if doRec then upperStat:=4;
    end;
  8:begin; {play and record sample}
    doPly;
    if wtPly then begin; upperStat:=6;exit; end;
    if doRec then upperStat:=6;
    end;
  1:begin; {get card identification}
    a:='skinny with '+ipAddr2string(serverAddr)+' '+BStr(serverPort)+#13#10;
    pipeLineSend(upperPipe,a[1],length(a));
    end;
  2:begin; {get mixer names}
    a:=#0#0#0#0#0#0#0#0'no available volumebars'#0;
    pipeLineSend(upperPipe,a[1],length(a));
    end;
  3:begin; {get mixer values}
    a:=#0#0#0#0;
    pipeLineSend(upperPipe,a[1],length(a));
    end;
  4:begin; {set mixer values}
    a:=#0;
    pipeLineSend(upperPipe,a[1],length(a));
    end;
  5:begin; {get buffer info}
    i:=bytesPerPack*bufferSampleSize*2;
    pipeLineSend(upperPipe,i,sizeof(i));
    end;
  9:begin; {dial a number}
    ab0:=p-1;
    move(buf[2],ab[1],ab0);
    WriteLn('upper dials '+a+'...');
    if (voicePipe<>0) then begin;
      i:=$01010101;
      pipeLineSend(upperPipe,i,1);
      exit;
      end;
    callPeerNum:=a;
    skinnySendStimulus;
    voiceAddr:=localAddr;
    voicePort:=localPort;
    voicePipe:=0;
    voiceMode:=0;
    sentPck:=0;
    sentOct:=0;
    rcvdPck:=0;
    rcvdOct:=0;
    upperStat:=2;
    end;
  10:begin; {hangup call}
    WriteLn('upper hanged up!');
    upperStat:=1;
    end;
  else begin;
    WriteLn('bad command from upper!');
    upperStat:=1;
    end;
  end;
End;
