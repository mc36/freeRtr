{{$define debug1}
{{$define debug2}
{{$define debug3}
{$heap 127k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc memory.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

{$include \sources\internet\kernel\utils\timer2.inc}
{$include \sources\system\serial\serial.inc}
{$include \sources\system\serial\fcs16.inc}
{$include irda.inc}
{$include irlmp.inc}
{$include irlap.inc}

Function getCurrentHostname:String;
Var t:xtText;
Begin;
if (xtOpen(t,'c:\system\localHost.text',true)=0) then begin;
  getCurrentHostname:=xtReadLn(t,666);
  xtClose(t);
  end else getCurrentHostname:='localHost';
End;


Label f1,f2,f3,f4;
Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('irda driver v1.0, done by Mc at '#%date' '#%time'.');
CreateFCS16Table;
Randomize;
a:=paramStr(1);
serialProc:=BVal(a);
if (serialProc=0) then serialProc:=BugOS_findProcNam(a);
serialPort:=BVal(paramStr(2));
if (serialPort<1) then immErr('using: irda.code <process> <port> [speed]');
SerialOpen;
localName:=getCurrentHostname;
localSpeed:=BVal(paramStr(3));
if (localSpeed<1) then localSpeed:=9600;
ConnectionNum:=0;

pipeLineBegListen;
BugOS_SignDaemoning;

f1:
WriteLn('waiting for device...');
upperClearStates;
localID:=random($7fffffff) or 1;
lastSent:=-99999;
packetInputSiz:=0;
packetInputEsc:=false;
packetOutputSiz:=0;
remoteID:=0;
remoteName:='';
remoteCapa:='';
remoteSpeed:=0;
remoteAddr:=-1;
queueBufferSiz:=0;
fastRounds:=0;
f2:
relequish;
timer2start;
if (getTimePast(lastSent)>2) then begin;
  putOneXIDpack(1);
  lastSent:=currentTime;
  end;
flushOutBuf;
doReceivingWork;
if (remoteID=0) then goto f2;

WriteLn(remoteName+' ('+remoteCapa+') in range...');
remoteAddr:=(random(120)+1) shl 1;
lastSent:=-99999;
dataSize:=128;
windowSize:=1;
preambSize:=0;
o:=0;

f3:
relequish;
timer2start;
if (getTimePast(lastSent)>0) then begin;
  inc(o);
  if (o>3) then goto f1;
  putOneSNRMpack;
  lastSent:=currentTime;
  end;
flushOutBuf;
doReceivingWork;
if (remoteID=0) then goto f1;
if (remoteSpeed<1) then goto f3;

{$ifdef debug2}WriteLn('speed='+BStr(remoteSpeed)+' block='+BStr(dataSize)+' window='+BStr(windowSize)+' preamble='+BStr(preambSize));{$endif}
inc(preambSize);
queueBufferSiz:=0;
remoteHas:=0;
needStop:=0;
lastSent:=currentTime;
retryCount:=0;
packetTx:=0;
packetRx:=0;
upperClearStates;
WriteLn('connected...');

f4:
if (remoteID=0) then goto f1;
relequish;
timer2start;
doReceivingWork;
doTransmitWork;
doUpperWork;

goto f4;
END.