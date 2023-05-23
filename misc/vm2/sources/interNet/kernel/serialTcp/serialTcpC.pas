{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc hex.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Var
  tcpPipe,upPipeCtrl,upPipeData:LongInt;
  decodeBufD:array[1..1024] of byte;
  decodeBufS:LongInt;
  encodeBufD:array[1..1024] of byte;
  encodeBufS:LongInt;


Procedure doUpper;
Label f1;
Var
  buf:array[1..512] of byte;
  bufD:array[1..1] of LongInt absolute buf;
  bufD1:LongInt absolute buf;
  i,o:LongInt;
Begin;
if (upPipeData=0) then begin;
  decodeBufS:=0;
  encodeBufS:=0;
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
o:=sizeof(encodeBufD)-encodeBufS;
if (o>sizeof(buf)) then o:=sizeof(buf);
if (o>64) then pipeLineRecv(upPipeData,encodeBufD[encodeBufS+1],o) else o:=0;
inc(encodeBufS,o);
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
    bufD[3]:=0;        {parity errors}
    bufD[4]:=0;        {framing errors}
    bufD[5]:=0;        {break detects}
    bufD[6]:=0;        {current line status}
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
    bufD[4]:=8; {byte length in bits}
    bufD[5]:=0; {parity}
    bufD[6]:=0; {stop bits}
    bufD[7]:=0; {send break}
    i:=7*sizeof(bufD1);
    end;
  05:begin; {write line status}
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
    encodeBufS:=0;
    i:=1*sizeof(bufD1);
    end;
  11:begin; {clear driver rx and tx buffers}
    encodeBufS:=0;
    decodeBufS:=0;
    i:=1*sizeof(bufD1);
    end;
  else begin; WriteLn('bad command from upper!');goto f1; end;
  end;
pipeLineSend(upPipeCtrl,bufD,i);
End;


Procedure doLower;
Var
  i,o,p:LongInt;
Begin;
if (encodeBufS>0) then begin;
  if (pipeLineSend(tcpPipe,encodeBufD,encodeBufS)=0) then encodeBufS:=0;
  end;
o:=sizeof(decodeBufD)-decodeBufS;
if (o>64) then pipeLineRecv(tcpPipe,decodeBufD[decodeBufS+1],o) else o:=0;
inc(decodeBufS,o);
if (o>0) then exit;
pipeLineStats(tcpPipe,o,i,i);
if (o=0) then immErr('tcp connection closed!');
End;


Label f1;
Var
  i,o:LongInt;
  a:String;
BEGIN;
WriteLn('serial client v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');
if (paramCount<2) then immErr('using: sertcp.code <host> <port> [send] [send]...');
i:=BVal(ParamStr(2));
string2ipAddr(ParamStr(1),a);

Write('connecting to '+ipAddr2string(a)+' '+BStr(i)+'...');
TCPbeginConnect(tcpPipe,65536,a,i);
while TCPlookConnected(tcpPipe,a,i,o) do begin;
  if (tcpPipe=0) then immErr(' failed!');
  relequish;
  end;
WriteLn(' ok!');
WriteLn('local side is '+ipAddr2string(a)+' '+BStr(i)+'...');

for i:=3 to paramCount do begin;
  a:=paramStr(i)+#13;
  pipeLineSend(tcpPipe,a[1],length(a));
  end;
BugOS_SignDaemoning;
pipeLineBegListen;

upPipeCtrl:=0;
upPipeData:=0;
decodeBufS:=0;
encodeBufS:=0;

f1:
relequish;
doLower;
doUpper;
goto f1;
END.