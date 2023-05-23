{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc memory.inc}

{$include async.inc}

Var
  devPipe,upPipeCtrl,upPipeData:LongInt;
  addrSiz,packSiz:LongInt;
  myName:String;

Procedure immErr(a:String);
Begin;
Writeln(a);
Halt(1);
End;



Procedure doUpper;
Label f1;
Var
  buf:array[1..512] of byte;
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

if (decodeBufS>0) then begin;
  if (pipeLineSend(upPipeData,decodeBufD,decodeBufS)=0) then decodeBufS:=0;
  end;
o:=(sizeof(encodeBufD)-encodeBufS) shr 1;
if (o>sizeof(buf)) then o:=sizeof(buf);
if (o>64) then pipeLineRecv(upPipeData,buf,o) else o:=0;
if (o>0) then begin;
  encodeBytes(buf,o);
  encodeFlush;
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
    bufD[2]:=0;        {overrun errors}
    bufD[3]:=decodePrty; {parity errors}
    bufD[4]:=decodeFrmg; {framing errors}
    bufD[5]:=0;        {break detects}
    bufD[6]:=0;        {current line status}
    decodePrty:=0;
    decodeFrmg:=0;
    i:=6*sizeof(bufD1);
    end;
  01:begin; {read modem status counters}
    bufD[2]:=0;          {cts changes}
    bufD[3]:=0;          {dsr changes}
    bufD[4]:=0;          {ring indicator changes}
    bufD[5]:=0;          {data carrier detect changes}
    bufD[6]:=11;         {current modem status}
    i:=6*sizeof(bufD1);
    end;
  02:begin; {read modem control status}
    bufD[2]:=3; {current modem control}
    i:=2*sizeof(bufD1);
    end;
  03:begin; {set modem control value}
    i:=1*sizeof(bufD1);
    end;
  04:begin; {read line status}
    bufD[2]:=19200; {line speed (bit/sec)}
    bufD[3]:=0; {line speed high dword}
    bufD[4]:=dataBits; {byte length in bits}
    bufD[5]:=parityBit; {parity}
    bufD[6]:=0; {stop bits}
    bufD[7]:=0; {send break}
    i:=7*sizeof(bufD1);
    end;
  05:begin; {write line status}
    dataBits:=bufD[4];
    parityBit:=bufD[5];
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
    bufD[2]:=decodeBufS; {bytes waiting in rx buffer}
    bufD[3]:=encodeBufS; {bytes waiting in tx buffer}
    i:=3*sizeof(bufD1);
    end;
  09:begin; {clear driver rx buffer}
    decodeBufS:=0;
    i:=1*sizeof(bufD1);
    end;
  10:begin; {clear driver tx buffer}
    encodeFlush;
    encodeBufS:=0;
    encodeFlush;
    i:=1*sizeof(bufD1);
    end;
  11:begin; {clear driver rx and tx buffers}
    encodeFlush;
    encodeBufS:=0;
    encodeFlush;
    decodeBufS:=0;
    i:=1*sizeof(bufD1);
    end;
  else begin; WriteLn('bad command from upper!');goto f1; end;
  end;
pipeLineSend(upPipeCtrl,bufD,i);
End;


Procedure doLower;
Var
  buf:array[1..512] of byte;
  i,o,p:LongInt;
Begin;
o:=(sizeof(decodeBufD)-decodeBufS) shr 1;
if (o>sizeof(buf)) then o:=sizeof(buf);
if (o>64) then pipeLineRecv(devPipe,buf,o) else o:=0;
if (o>addrSiz) then begin;
  decodeBytes(buf[addrSiz+1],o-addrSiz);
  end;
if (encodeBufS<1) then exit;
i:=encodeBufS+addrSiz;
if (i>sizeof(buf)) then i:=sizeof(buf);
dec(i,addrSiz);
move(encodeBufD,buf[addrSiz+1],i);
fillchar(buf,addrSiz,$ff);
if (pipeLineSend(devPipe,buf,i+addrSiz)<>0) then exit;
dec(encodeBufS,i);
move(encodeBufD[i+1],encodeBufD,encodeBufS);
End;



Label f1;
Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('async v1.0, done by Mc at '#%date' '#%time'.');
a:=paramStr(1);
devPipe:=BVal(a);
if (devPipe=0) then devPipe:=BugOS_findProcNam(a);
if (devPipe<1) then immErr('using: async.code <process>');

WriteLn('opening link...');
if (pipeLineCreate(devPipe,devPipe,65536,true)<>0) then immErr('error creating connection!');
for i:=1 to 16 do relequish;
i:=255;
if (pipeLineRecv(devPipe,a[1],i)<>0) then immErr('failed to receive initial data!');
a[0]:=chr(i);
move(a[1],addrSiz,sizeof(addrSiz));
move(a[5],packSiz,sizeof(packSiz));
a:=copy(a,17+addrSiz+addrSiz,255);
myName:='async on '+copy(a,1,pos(#0,a)-1);
WriteLn('my name: '+myName);
WriteLn('address size: '+BStr(addrSiz));
WriteLn('packet size: '+BStr(packSiz));
BugOS_SignDaemoning;
pipeLineBegListen;

dataBits:=8;
parityBit:=0;
encodeBitD:=0;
encodeBitS:=0;
encodeBufS:=0;
decodeBitD:=0;
decodeBitS:=0;
decodeBufS:=0;
decodePrty:=0;
decodeFrmg:=0;
upPipeCtrl:=0;
upPipeData:=0;

f1:
relequish;
doLower;
doUpper;
goto f1;
END.