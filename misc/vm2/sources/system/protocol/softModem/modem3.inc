Procedure upperReleq;
Label f1;
Var
  buf:array[1..256] of byte;
  bufD:array[1..1] of LongInt absolute buf;
  bufD1:LongInt absolute buf;
  i,o:LongInt;
Begin;
if (upPipeData=0) then begin;
  if (pipeLineGetIncoming(i)<>0) then exit;
  upPipeCtrl:=i;
  i:=1;
  pipeLineSend(upPipeCtrl,i,sizeof(i));
  for i:=1 to 32 do relequish;
  i:=sizeof(o);
  pipeLineRecv(upPipeCtrl,o,i);
  if (i<>sizeof(o)) then begin;
    f1:
    pipeLineClose(upPipeCtrl);
    pipeLineClose(upPipeData);
    upPipeCtrl:=0;
    upPipeData:=0;
    exit;
    end;
  i:=sizeof(o);
  pipeLineRecv(upPipeCtrl,o,i);
  if (i<>sizeof(o)) then goto f1;
  if (pipeLineGetIncoming(i)<>0) then goto f1;
  upPipeData:=i;
  WriteLn('upper logged in!');
  exit;
  end;
if (modemRxSiz>0) then if (pipeLineSend(upPipeData,modemRxDat[1],modemRxSiz)=0) then modemRxSiz:=0;
o:=sizeof(modemTxDat)-modemTxSiz;
if (o>64) then begin;
  pipeLineRecv(upPipeData,modemTxDat[modemTxSiz+1],o);
  inc(modemTxSiz,o);
  end;
if (o<1) then begin;
  pipeLineStats(upPipeData,o,i,i);
  if (o=0) then begin; WriteLn('upper exited!');goto f1; end;
  end;
o:=sizeof(buf);
pipeLineRecv(upPipeCtrl,buf,o);
if (o<sizeof(bufD1)) then exit;
case bufD1 of
  00:begin; {read line status counters}
    bufD[2]:=decodOvr; {overrun errors}
    bufD[3]:=decodPar; {parity errors}
    bufD[4]:=decodFrm; {framing errors}
    bufD[5]:=0;        {break detects}
    bufD[6]:=0;        {current line status}
    decodOvr:=0;
    decodPar:=0;
    decodFrm:=0;
    i:=6*sizeof(bufD1);
    end;
  01:begin; {read modem status counters}
    bufD[2]:=0;          {cts changes}
    bufD[3]:=0;          {dsr changes}
    bufD[4]:=0;          {ring indicator changes}
    bufD[5]:=modemCDchg; {data carrier detect changes}
    if (modemConnected=1) or (modemConnected<-2) or modemCDforced then i:=8 else i:=0;
    bufD[6]:=i or 3;     {current modem status}
    i:=6*sizeof(bufD1);
    end;
  02:begin; {read modem control status}
    bufD[2]:=3; {current modem control}
    i:=2*sizeof(bufD1);
    end;
  03:begin; {set modem control value}
    if (bufD[2] and $01=0) then if not modemDTRignor then modemDisconnect;
    i:=1*sizeof(bufD1);
    end;
  04:begin; {read line status}
    bufD[2]:=modemBitPerSec; {line speed (bit/sec)}
    bufD[3]:=0; {line speed high dword}
    bufD[4]:=modemDataBit; {byte length in bits}
    bufD[5]:=modemParity; {parity}
    bufD[6]:=0; {stop bits}
    bufD[7]:=0; {send break}
    i:=7*sizeof(bufD1);
    end;
  05:begin; {write line status}
    modemBitPerSec:=bufD[2];
    modemDataBit:=bufD[4];
    modemParity:=bufD[5];
    i:=1*sizeof(bufD1);
    end;
  06:begin; {read flow control}
    bufD[2]:=1; {used flow control}
    i:=2*sizeof(bufD1);
    end;
  07:begin; {write flow control}
    i:=1*sizeof(bufD1);
    end;
  08:begin; {driver buffer status}
    bufD[2]:=modemRxSiz; {bytes waiting in rx buffer}
    bufD[3]:=modemTxSiz; {bytes waiting in tx buffer}
    i:=3*sizeof(bufD1);
    end;
  09:begin; {clear driver rx buffer}
    modemFlushRxBuf;
    i:=1*sizeof(bufD1);
    end;
  10:begin; {clear driver tx buffer}
    modemTxSiz:=0;
    i:=1*sizeof(bufD1);
    end;
  11:begin; {clear driver rx and tx buffers}
    modemFlushRxBuf;
    modemTxSiz:=0;
    i:=1*sizeof(bufD1);
    end;
  else begin; WriteLn('bad command from upper!');goto f1; end;
  end;
pipeLineSend(upPipeCtrl,bufD,i);
End;



Procedure doVoiceTx(silence:Boolean);
Label f1,f2;
Var
  i,o,p:LongInt;
Begin;
if silence then begin;
  for i:=1 to modemTxSiz do if modemVtxDLEd then begin;
    if (modemTxDat[i]<>16) then goto f2;
    modemVtxDLEd:=false;
    continue;
    end else if (modemTxDat[i]=16) then modemVtxDLEd:=true;
  f1:
  fillchar(encodBuf[encodSiz+1],soundSize*4,0);
  inc(encodSiz,soundSize);
  modemTxSiz:=0;
  exit;
  end;
if (modemTxSiz<1) then begin;
  modemAdd2rx(#16'u');
  goto f1;
  end;
for p:=1 to modemTxSiz do begin;
  o:=modemTxDat[p];
  if modemVtxDLEd then begin;
    modemVtxDLEd:=false;
    if (o<>16) then goto f2;
    end else if (o=16) then begin; modemVtxDLEd:=true;continue; end;
  o:=(o-$80) shl 8;
  for i:=1 to 11 do begin; inc(encodSiz);encodBuf[encodSiz]:=o; end;
  end;
modemTxSiz:=0;
exit;
f2:
modemAdd2rx(#16#3);
modemConnected:=0;
modemAddResp(0);
End;

Procedure doVoiceRx(mode:LongInt); {bit0=dtmf, bit1=data}
Var
  a:String;
  i,o,p:LongInt;
Begin;
i:=(decodSiz-decodPos) div 11;
if (i<1) then exit;
if (i>sizeof(modemRxDat)-modemRxSiz) then begin; decodPos:=decodSiz;exit; end;
if (mode and 1<>0) then begin;
  i:=decodPos;
  a:=detectDTMFcodes;
  if (a<>'') then modemAdd2rx(#16+a);
  if (decodPos=i) then exit;
  decodPos:=i;
  end;
if (mode and 2<>0) then begin;
  p:=decodPos;
  while (p<decodSiz) do begin;
    o:=0;
    for i:=1 to 11 do begin; inc(p);inc(o,decodBuf[p]); end;
    o:=(((o div 11) shr 8)+$80) and $ff;
    if (o=16) then begin; inc(modemRxSiz);modemRxDat[modemRxSiz]:=o; end;
    inc(modemRxSiz);
    modemRxDat[modemRxSiz]:=o;
    end;
  end;
decodPos:=decodSiz;
End;

Procedure doVoiceHandling;
Begin;
case -modemConnected of
  5:doVoiceTx(false); {rx+tx}
  4:doVoiceTx(false); {tx}
  3:doVoiceTx(true); {rx}
  1:begin; {dial}
    generateDTMFtone(kicsi(copy(modemCommand,1,1)));
    if (modemCommand<>'') then begin;
      modemCommand:=copy(modemCommand,2,666);
      exit;
      end;
    if not movemVoipMode then begin;
      modemConnected:=2;
      exit;
      end;
    modemConnected:=0;
    modemAddResp(0);
    soundFlush;
    end;
  2:begin; {tone}
    generateAnyTone(BVal(modemCommand));
    if (modemCommand<>'') then begin; modemCommand:='';exit; end;
    modemConnected:=0;
    modemAddResp(0);
    soundFlush;
    end;
  else modemDisconnect;
  end;
End;



Procedure connectReleq;
Label f1,f2;
Var
  buf:array[1..32*1024] of byte;
  modCmdS:byte absolute modemCommand;
  a,b:String;
  i,o,p:LongInt;
Begin;
if (modemConnected<>0) then begin; p:=0;goto f1; end;
if (modemTxSiz<1) then exit;
p:=0;
while (p<modemTxSiz) do begin;
  inc(p);
  i:=modemTxDat[p];
  if (i in [$41..$5a]) then inc(i,$20);
  case modCmdS of
    0:if (i<>$61) then continue;
    1:if (i<>$74) then continue;
    end;
  if (i=8) then begin;
    if (modCmdS<=2) then continue;
    dec(modCmdS);
    if modemCmdEcho then modemAdd2rx(#8' '#8);
    continue;
    end;
  if (i=13) then break;
  if (i<32) then continue;
  if modemCmdEcho then modemAdd2rx(chr(i));
  modemCommand:=modemCommand+chr(i);
  end;
i:=modemTxDat[p];
modemDelTxed(p);
if (i<>13) then exit;
if (modCmdS<2) then exit;
if modemCmdEcho then begin; modemAdd2rx(#13#10); end;
a:=copy(modemCommand,3,666);
modemCommand:='';
WriteLn('command: at'+a);
modemGotCommand(a);
exit;
f1:
while (encodSiz<soundSize shr 1) do begin;
  if (modemConnected<0) then begin; doVoiceHandling;continue; end;
  if (modemTxSiz<1) then begin; addOneBit(0);continue; end;
  if (modemConnected>1) then begin; modemDisconnect;exit; end;
  addOneByte(modemTxDat[1]);
  modemDelTxed(1);
  end;
o:=sizeof(buf);
pipeLineRecv(soundPipe,buf,o);
if (o<1) then begin;
  if (p>1) then WriteLn('overflow in sound pipeline!');
  pipeLineStats(soundPipe,o,i,i);
  if (o=0) then immErr('sound device closed connection!');
  if (modemConnected>0) then goto f2;
  case -modemConnected of
    5:doVoiceRx(3); {rx+tx}
    4:doVoiceRx(1); {tx}
    3:doVoiceRx(3); {rx}
    1:doVoiceRx(0); {dial}
    2:doVoiceRx(0); {tone}
    else modemDisconnect;
    end;
  exit;
  end;
i:=decodOvr;
putToDecodeBuf(buf,o);
if (decodOvr<>i) then WriteLn('overflow in record buffer!');
buf[1]:=8;
o:=getFromEncodeBuf(buf[2],soundSize);
pipeLineSend(soundPipe,buf,soundSize+1);
inc(p);
goto f1;
f2:
i:=getOneByte;
if (i>=0) then begin;
  if (modemConnected=1) then modemAdd2rx(chr(i));
  goto f2;
  end;
if (decodCar>0) then begin;
  modemNoCarrPack:=0;
  if (modemConnected=1) then exit;
  inc(modemConnected);
  if (modemConnected<2205000 div soundSize) then exit;
  modemConnected:=1;
  WriteLn('carrier detected!');
  inc(modemCDchg);
  modemAddResp(1);
  exit;
  end;
inc(modemNoCarrPack);
if (modemNoCarrPack<22050000 div soundSize) then exit;
WriteLn('carrier lost!');
modemDisconnect;
End;
