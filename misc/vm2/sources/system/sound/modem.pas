{$heap 127k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc controlChars.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc filesys.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Procedure clrEol;
Begin;
Write(#13'                                                                    '#13);
End;

{$include \sources\internet\kernel\utils\timer2.inc}
{$include \sources\system\serial\serial.inc}
{$include \sources\system\serial\modem.inc}
{$include \sources\system\serial\voice.inc}


Const
  proggyName='voiceModem driver v1.0, done by Mc at '#%date' '#%time'.';
  bufSamSiz=11;
  bufModSiz=512;
  bufIntSiz=bufModSiz*bufSamSiz;

Label f1,f2;
Var
  pipe:LongInt;
  buf:array[1..1024*16] of byte;
  bufI:array[1..1] of Integer absolute buf;
  devMod:Longint;       {device mode: 0=idle, 1=play, 2=rec, 3=playrec}
  devLst:LongInt;       {last time stream used}
  devStp:Boolean;       {should stop transfer}
  devNam:String;        {name of device}
  i,o,p,q:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;


Procedure displayStatus(a:String);
Begin;
textColor($0f);
Write(a);
textColor($07);
WriteLn('');
End;


Procedure devStop;
Begin;
if (devMod=0) then exit;
displayStatus('stopping transfer...');
devLst:=currentTime;
devStp:=true;
if (devMod=1) then begin;
  VoiceDeviceSawSTP:=true;
  PlayModemFin;
  exit;
  end;
CancelRecModem;
End;

Procedure devReleq(dontStop:Boolean);
Begin;
relequish;
timer2start;
if (devMod=0) then exit;
if dontStop then devLst:=currentTime;
if (VoiceDeviceRXsiz>1536) then deleteFromBuffer(VoiceDeviceRXbuf,VoiceDeviceRXsiz,512);
if (getTimePast(devLst)>3) then devStop;
PlayModemDo;
RecordModemDo(devStp);
if not VoiceDeviceSawSTP then exit;
if ModemReadCommand(a,10) then immErr('failed to stop transfer!');
timer2start;
VoiceDeviceSawSTP:=false;
devLst:=currentTime;
devStp:=false;
devMod:=0;
End;

Procedure devChg(mod:LongInt);
Begin;
devLst:=currentTime;
if (mod=devMod) then exit;
devStop;
while (devMod<>0) do devReleq(false);
devStp:=false;
displayStatus('starting transfer...');
case mod of
  0:exit;
  1:if PlayModemBeg then immErr('failed to start recording!');
  2:if RecordModemBeg then immErr('failed to start playback!');
  3:if RecPlayModemBeg then immErr('failed to start duplex transfer!');
  else immErr('invalid mode requested!');
  end;
devMod:=mod;
timer2start;
devLst:=currentTime;
End;



Procedure devPly;
Begin;
while (VoiceDeviceTXsiz<>0) do devReleq(true);
move(buf[2],buf,bufIntSiz shl 1);
o:=0;
for i:=1 to bufIntSiz do begin;
  inc(o,bufI[i]);
  if (i mod bufSamSiz<>0) then continue;
  inc(VoiceDeviceTXsiz);
  VoiceDeviceTXbuf[VoiceDeviceTXsiz]:=((o div bufSamSiz) shr 8)+$7f;
  o:=0;
  end;
End;

Procedure devRec;
Begin;
while (VoiceDeviceRXsiz<bufModSiz) do devReleq(true);
q:=0;
for p:=1 to bufModSiz do begin;
  o:=(VoiceDeviceRXbuf[p]-$7f) shl 8;
  for i:=1 to bufSamSiz do begin;
    inc(q);
    bufI[q]:=o;
    end;
  end;
deleteFromBuffer(VoiceDeviceRXbuf,VoiceDeviceRXsiz,bufModSiz);
pipeLineSend(pipe,buf,bufIntSiz shl 1);
End;


BEGIN;
WriteLn(proggyName);
if (paramCount<3) then immErr('using: modem.code <process> <port> <modem>');
a:=paramStr(1);
serialProc:=BVal(a);
if (serialProc=0) then serialProc:=BugOS_findProcNam(a);
serialPort:=BVal(paramStr(2));
VoiceCommandSet:=BVal(paramStr(3));
ModemShow:=true;
devNam:=proggyName+#13#10'serial driver='+a+'; port#='+BStr(serialPort)+'.'#13#10;

displayStatus('testing modem...');
SerialClose;
for i:=1 to 32 do relequish;
SerialOpen;
for i:=1 to 32 do relequish;
ModemDoCmd('AT',a);
if (ModemTestResponse(a)<>0) then immErr('not found!');

pipeLineBegListen;
BugOS_SignDaemoning;
pipe:=0;
devStp:=false;
devMod:=0;

f1:
devReleq(false);
if (pipe=0) then begin;
  if (pipeLineGetIncoming(p)<>0) then goto f1;
  pipe:=p;
  displayStatus('upper logged in...');
  goto f1;
  end;

i:=sizeof(buf);
if (pipeLineRecv(pipe,buf,i)<>0) then i:=0;
if (i<1) then begin;
  pipeLineStats(pipe,o,i,i);
  if (o<>0) then goto f1;
  f2:
  displayStatus('upper logged out...');
  pipeLineClose(pipe);
  pipe:=0;
  goto f1;
  end;
case buf[1] of
  6:begin; {play sample}
    devChg(1);
    devPly;
    i:=$01010101;
    pipeLineSend(pipe,i,1);
    end;
  7:begin; {record sample}
    devChg(2);
    devRec;
    end;
  8:begin; {play and record sample}
    devChg(3);
    devPly;
    devRec;
    end;
  1:begin; {get card identification}
    pipeLineSend(pipe,devNam[1],length(devNam));
    end;
  2:begin; {get mixer names}
    a:=#0#0#0#0#0#0#0#0'no available volumebars'#0;
    pipeLineSend(pipe,a[1],length(a));
    end;
  3:begin; {get mixer values}
    a:=#0#0#0#0;
    pipeLineSend(pipe,a[1],length(a));
    end;
  4:begin; {set mixer values}
    a:=#0;
    pipeLineSend(pipe,a[1],length(a));
    end;
  5:begin; {get buffer info}
    i:=bufIntSiz shl 1;
    pipeLineSend(pipe,i,sizeof(i));
    end;
  9:begin; {dial a number}
    devChg(0);
    ab0:=i-1;
    move(buf[2],ab[1],ab0);
    ModemSendCommand('ATD'+a+';');
    if ModemWaitConnected(a,o) then i:=0 else i:=$01010101;
    pipeLineSend(pipe,i,1);
    end;
  10:begin; {hangup call}
    devChg(0);
    if ModemDisconnectNow('ATH',10) then i:=0 else i:=$01010101;
    pipeLineSend(pipe,i,1);
    end;
  else begin;
    pipeLineClose(pipe);
    pipe:=0;
    end;
  end;

goto f1;
END.