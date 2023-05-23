{$heap 255k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$sysinc filesys.inc}
{$include \sources\internet\kernel\utils\timer2.inc}
{$include \sources\internet\server\sip\g711alaw.inc}

Const
  bufferSampleSize=11;
  bytesPerPack=200;
Var
  devPipe:LongInt;
  addrSiz:LongInt;
  txSeq:LongInt;
  txPak:LongInt;
  txSiz:LongInt;
  txTim:LongInt;
  rxSiz:LongInt;
  rxTim:LongInt;
  txBuf:array[1..1024*32] of byte;
  rxBuf:array[1..1024*32] of byte;
  myName:String;
  subCh:LongInt;
  upperPipe:LongInt;
  upperStat:LongInt;    {1-rx, 2-tx}
  procNam:String;
  procPar:String;
  ticksPerPack:LongInt;



Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;

Procedure dumpPacket(var buffer;siz:LongInt;act:String);
Var
  buf:array[1..1] of byte absolute buffer;
  i:LongInt;
Begin;
Write(act+' '+BStr(siz)+':');
for i:=1 to siz do write(' '+byte2hextype(buf[i]));
WriteLn('');
End;

Procedure openDevice(a:String);
Var
  buf:array[1..1024] of byte;
  i,o,p:LongInt;
Begin;
WriteLn('process: '+a);
o:=BugOS_findProcNam(a);
if (o=0) then immErr('process not found!');
WriteLn('process#: '+BStr(o));
i:=pipeLineCreate(devPipe,o,65536,true);
if (i<>0) then immErr('unabled to create pipeline!');
WriteLn('pipeline#: '+BStr(devPipe));
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(devPipe,buf,i)<>0) then i:=0;
if (i<1) then immErr('initial packet not received!');
move(buf[1],p,sizeof(p));
WriteLn('address size: '+BStr(p));
addrSiz:=p;
move(buf[5],i,sizeof(i));
WriteLn('packet size: '+BStr(i));
o:=17;
Write('station address: ');
for i:=1 to p do begin;
  write(byte2hextype(buf[o])+'-');
  inc(o);
  end;
WriteLn(#8' ');
Write('broadcast address: ');
for i:=1 to p do begin;
  write(byte2hextype(buf[o])+'-');
  inc(o);
  end;
WriteLn(#8' ');
a:='';
while (buf[o]<>0) do begin;
  a:=a+chr(buf[o]);
  inc(o);
  end;
writeln('device name: "'+a+'"');
myName:='vofr on '+a;
End;



Procedure releq2dev;
Label f1;
Var
  buf:array[1..2048] of byte;
  i,o,p:LongInt;
Begin;
if (abs(currentTime-txTim)>=ticksPerPack) then begin;
  if (txSiz<1) then goto f1;
  txSeq:=((txSeq+$60) and $e0) or $10;
  move(txBuf,buf[addrSiz+3],bytesPerPack);
  fillchar(buf,addrSiz,255);
  buf[addrSiz+1]:=subCh;
  buf[addrSiz+2]:=txSeq;
  pipeLineSend(devPipe,buf,addrSiz+bytesPerPack+2);
  dec(txSiz,bytesPerPack);
  if (txSiz<1) then txSiz:=0;
  move(txBuf[bytesPerPack+1],txBuf,txSiz);
  txTim:=currentTime;
  inc(txPak);
  if (txPak mod 5=0) then txTim:=-99999;
  end;
f1:
p:=sizeof(buf);
pipeLineRecv(devPipe,buf,p);
if (p<1) then begin;
  pipeLineStats(devPipe,o,i,i);
  if (o=0) then immErr('device closed pipeline!');
  exit;
  end;
o:=addrSiz+1;
if (buf[o]<>subCh) then begin;
  WriteLn('got for unknown subchannel!');
  goto f1;
  end;
inc(o);
dec(p,o);
if (rxSiz+p>sizeof(rxBuf)) then goto f1;
move(buf[o+1],rxBuf[rxSiz+1],p);
inc(rxSiz,p);
rxTim:=currentTime;
if (upperPipe<>0) then goto f1;
if (getTimePast(txSeq)<60) then goto f1;
WriteLn('stream detected, starting process...');
txSeq:=currentTime;
if (xExecBgnd(procNam,procPar,i)<>0) then i:=0;
goto f1;
End;



Procedure releq2upper;
Var
  buf:array[1..1024*32] of byte;
  bufI:array[1..1] of Integer absolute buf;
  i,o,p,q:LongInt;
  a:String;

Function doRec:Boolean;
Begin;
doRec:=True;
if (rxSiz<bytesPerPack) then exit;
q:=0;
for p:=1 to bytesPerPack do begin;
  o:=alaw2pcmConverter(rxBuf[p]);
  for i:=1 to bufferSampleSize do begin;
    inc(q);
    bufI[q]:=o;
    end;
  end;
dec(rxSiz,bytesPerPack);
move(rxBuf[bytesPerPack+1],rxBuf,rxSiz);
pipeLineSend(upperPipe,buf,q shl 1);
doRec:=False;
End;

Procedure doPly;
Begin;
if (txSiz+bytesPerPack>sizeof(txBuf)) then exit;
move(buf[2],buf,sizeof(buf));
p:=0;
for q:=1 to bytesPerPack do begin;
  o:=0;
  for i:=1 to bufferSampleSize do begin;
    inc(p);
    inc(o,bufI[p]);
    end;
  o:=o div bufferSampleSize;
  inc(txSiz);
  txBuf[txSiz]:=pcm2alawConverter(o);
  end;
End;

Function wtPly:Boolean;
Begin;
wtPly:=(txSiz>sizeof(txBuf) shr 1);
End;


Begin;
while (pipeLineGetIncoming(p)=0) do begin;
  if (upperPipe<>0) then begin;
    pipeLineClose(p);
    continue;
    end;
  upperPipe:=p;
  upperStat:=0;
  rxTim:=currentTime;
  txTim:=-99999;
  txSeq:=0;
  txPak:=0;
  WriteLn('upper logged in!');
  end;
if (upperPipe=0) then exit;
if (getTimePast(rxTim)>60) then begin;
  WriteLn('stream went away!');
  pipeLineClose(upperPipe);
  upperStat:=0;
  end;
if (upperStat<>0) then begin;
  if wtPly then exit;
  if (upperStat=1) then begin;
    i:=$01010101;
    pipeLineSend(upperPipe,i,1);
    upperStat:=0;
    exit;
    end;
  if doRec then exit;
  upperStat:=0;
  end;
p:=sizeof(buf);
pipeLineRecv(upperPipe,buf,p);
if (p<1) then begin;
  pipeLineStats(upperPipe,o,i,i);
  if (o<>0) then exit;
  WriteLn('upper logged out!');
  pipeLineClose(upperPipe);
  upperPipe:=0;
  exit;
  end;

case buf[1] of
  6:begin; {play sample}
    doPly;
    if wtPly then begin; upperStat:=1;exit; end;
    i:=$01010101;
    pipeLineSend(upperPipe,i,1);
    end;
  7:begin; {record sample}
    if doRec then upperStat:=2;
    end;
  8:begin; {play and record sample}
    doPly;
    if wtPly then upperStat:=1;
    if doRec then inc(upperStat,2);
    end;
  1:begin; {get card identification}
    a:=myName;
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
    i:=$01010101;
    pipeLineSend(upperPipe,i,1);
    end;
  10:begin; {hangup call}
    i:=$01010101;
    pipeLineSend(upperPipe,i,1);
    end;
  else begin;
    WriteLn('upper fool!');
    pipeLineClose(upperPipe);
    end;
  end;


End;




Label f1;
Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('voice over frameRelay v1.0, done by Mc at '#%date' '#%time'.');
generateAlawTable;
a:=paramStr(1);
if (a='') then immErr('using: vofr.code <process> <subchannel> <process> [parameter]');
subCh:=BVal(paramStr(2));
if (subCh=0) then subCh:=6;
procNam:=paramStr(3);
procPar:='';
for i:=4 to paramCount do procPar:=procPar+' '+paramStr(i);
procPar:=copy(procPar,2,666);
BugOS_MyProcessInfo(o,i,i);
kicserel('"',BStr(o),procPar);
openDevice(a);
txSeq:=0;
rxSiz:=0;
txSiz:=0;
upperPipe:=0;
timer2start;
ticksPerPack:=ticksPerSec div 32;
if (ticksPerPack<1) then ticksPerPack:=1;

pipeLineBegListen;
BugOS_SignDaemoning;

f1:
relequish;
timer2start;
releq2dev;
releq2upper;
goto f1;
END.