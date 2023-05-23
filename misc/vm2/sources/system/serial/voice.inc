Var
  VoiceCommandSet:Byte; {0-none, 1-rockwell, 2-zoltrix}
  VoiceDeviceSel:Byte; {same as #vls}
  VoiceDeviceTXbuf:array[1..1024] of byte;
  VoiceDeviceRXbuf:array[1..2048] of byte;
  VoiceDeviceTXsiz:LongInt;
  VoiceDeviceRXsiz:LongInt;
  VoiceDeviceSawDLE:Boolean;
  VoiceDeviceSawSTP:Boolean;
  ModemLastReadLn:String;
  ModemDtmfBusy:Boolean;
  ModemDtmfDialTone:Boolean;
  ModemDtmfAnswerFax:Boolean;
  ModemDtmfCallingFax:Boolean;
  ModemDtmfAnswerData:Boolean;
  ModemDtmfCallingData:Boolean;
  ModemDtmfSilence:Boolean;
  ModemDtmfQuiet:Boolean;



Function ProcessShieldedChar(b:Byte):String;
Var
  c:Char absolute b;
  s:String;
Begin;
case c of
  '0'..'9','*','#','A'..'D':begin;
    ModemEscaped:=copy(ModemEscaped+c,length(ModemEscaped)-250,255);
    s:=c;
    end;
  'a':begin; ModemDtmfAnswerFax:=True;s:='FaxAnswer'; end;
  'b':begin; ModemDtmfBusy:=True;s:='Busy'; end;
  'c':begin; ModemDtmfCallingFax:=True;s:='FaxCall'; end;
  'd':begin; ModemDtmfDialTone:=True;s:='DialTone'; end;
  'e':begin; ModemDtmfCallingData:=True;s:='DataCall'; end;
  'f':begin; ModemDtmfAnswerData:=True;s:='DataAnswer'; end;
  'h':begin; s:='PhoneHangUp'; end;
  't':begin; s:='PhonePickUp'; end;
  'o':begin; s:='OverRun'; end;
  'u':begin; s:='UnderRun'; end;
  's':begin; ModemDtmfSilence:=True;s:='Silence'; end;
  'q':begin; ModemDtmfQuiet:=True;s:='Quite'; end;
  else s:='???:'+BStr(b);
  end;
ProcessShieldedChar:=s;
End;

Procedure RotateOldShielded;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  i:LongInt;
Begin;
a:=ModemEscaped;
ModemEscaped:='';
for i:=1 to ab[0] do ProcessShieldedChar(ab[i]);
End;

Procedure deleteFromBuffer(var buffer;var siz:LongInt;len:LongInt);
Var buf:array[1..1] of byte absolute buffer;
Begin;
if (siz<0) then siz:=0;
if (len>siz) then len:=siz;
if (len<1) then exit;
dec(siz,len);
move(buf[len+1],buf,siz);
End;

Procedure ClearStateModem;
Begin;
VoiceDeviceSawDLE:=False;
VoiceDeviceSawSTP:=False;
fillchar(VoiceDeviceTXbuf,sizeof(VoiceDeviceTXbuf),0);
fillchar(VoiceDeviceRXbuf,sizeof(VoiceDeviceRXbuf),0);
VoiceDeviceTXsiz:=0;
VoiceDeviceRXsiz:=0;
End;

Procedure ClearDetectModem;
Begin;
ModemEscaped:='';
ModemDtmfBusy:=False;
ModemDtmfDialTone:=False;
ModemDtmfAnswerFax:=False;
ModemDtmfCallingFax:=False;
ModemDtmfAnswerData:=False;
ModemDtmfCallingData:=False;
ModemDtmfSilence:=False;
ModemDtmfQuiet:=False;
while keypressed do readkey;
End;

Function ReadUpKeysIfWere:Boolean;
Label f1;
Var
  i:Word;
  c:Char;
Begin;
ReadUpKeysIfWere:=False;
f1:
if not keypressed then Exit;
i:=ReadKey;
if (i and $fe00<>0) then goto f1;
c:=upCase(chr(i));
if not (c in ['0'..'9','*','#','A'..'D']) then goto f1;
ProcessShieldedChar(i);
ReadUpKeysIfWere:=True;
goto f1;
End;



Function ReSetModem:Boolean;
Var a:String;
Begin;
ReSetModem:=True;
if ModemDoCmd('ATH',a) then Exit;
if ModemDoCmd('ATZ',a) then Exit;
if (ModemTestResponse(a)<>0) then Exit;
ModemFlushReceiver;
case VoiceCommandSet of
  0:;
  1:begin;
    if ModemDoCmd('AT#CLS=8',a) then Exit;
    if (ModemTestResponse(a)<>0) then Exit;
    if ModemDoCmd('AT#VTD=3f,3f,3f',a) then Exit;
    if ModemDoCmd('AT#VSD=1',a) then Exit;
    if ModemDoCmd('AT#VSS=1',a) then Exit;
    if ModemDoCmd('AT#VSP=10',a) then Exit;
    if ModemDoCmd('AT#VLS='+BStr(VoiceDeviceSel),a) then Exit;
    end;
  2:begin;
    if ModemDoCmd('AT+FCLASS=8',a) then Exit;
    if (ModemTestResponse(a)<>0) then Exit;
    if ModemDoCmd('AT+VSD=5,10',a) then Exit;
    if ModemDoCmd('AT+VTD=10',a) then Exit;
    if ModemDoCmd('AT+VSM=130',a) then Exit;
    if ModemDoCmd('AT+VLS='+BStr(VoiceDeviceSel),a) then Exit;
    end;
  else Exit;
  end;
RotateOldShielded;
ReSetModem:=False;
End;

Function DataModeModem:Boolean;
Var a:String;
Begin;
DataModeModem:=True;
case VoiceCommandSet of
  0:if ModemDoCmd('AT',a) then Exit;
  1:if ModemDoCmd('AT#CLS=0',a) then Exit;
  2:if ModemDoCmd('AT+FCLASS=0',a) then Exit;
  else Exit;
  end;
if (ModemTestResponse(a)<>0) then Exit;
RotateOldShielded;
DataModeModem:=False;
End;



Function BeepModem(s:String;i:LongInt):Boolean;
Begin;
BeepModem:=True;
if (s='') or (i<1) then Exit;
s:=nagy(s);
if (s[0]=#1) and (s[1] in ['0'..'9','*','#','A'..'D']) then begin;
  s:='{'+s+','+BStr(i)+'}';
  end else begin;
  s:='['+s+','+s+','+BStr(i)+']';
  end;
case VoiceCommandSet of
  0:;
  1:ModemDoCmd('AT#VTS='+s,s);
  2:ModemDoCmd('at+VTS='+s,s);
  else exit;
  end;
RotateOldShielded;
BeepModem:=False;
End;



Procedure CancelRecModem;
procedure x(b:Byte);begin;pipeLineSend(serialData,b,sizeof(b));end;
procedure y(b:Byte);begin;x(controlCharacterDLE);x(b);
end;
Begin;
y(controlCharacterESC);
y(controlCharacterCAN);
y($21);
y(controlCharacterCR);
y($5e);
y(controlCharacterETX);
y(controlCharacterDC4);
End;

Function RecordModemBeg:Boolean;
Var s:String;
Begin;
RecordModemBeg:=True;
ClearStateModem;
RotateOldShielded;
case VoiceCommandSet of
  1:ModemDoCmd('AT#VRX',s);
  2:ModemDoCmd('AT+VRX',s);
  else s:='';
  end;
if (ModemTestResponse(s)<>2) then exit;
RecordModemBeg:=False;
End;

Function RecordModemDo(stopping:Boolean):String;
Label vege;

Procedure putChar(b:Byte);
Begin;
inc(VoiceDeviceRXsiz);
VoiceDeviceRXbuf[VoiceDeviceRXsiz]:=b;
End;

Var
  buf:array[1..512] of Byte;
  s:String;
  i,o:LongInt;
  b:byte;
  c:char absolute b;
Begin;
RecordModemDo:='';
s:='';
if VoiceDeviceSawSTP then exit;
if (VoiceDeviceRXsiz>sizeof(VoiceDeviceRXbuf)-sizeof(buf)) then exit;
if stopping then begin;
  if VoiceDeviceSawDLE then o:=1 else o:=4;
  end else o:=sizeof(buf);
pipeLineRecv(serialData,buf,o);
if (o<1) then begin;
  pipeLineStats(serialData,o,i,i);
  if (o=0) then immErr('serial driver closed connection!');
  exit;
  end;
for i:=1 to o do begin;
  b:=buf[i];
  if not VoiceDeviceSawDLE then begin;
    if (b<>controlCharacterDLE) then begin; putChar(b);continue; end;
    VoiceDeviceSawDLE:=true;
    continue;
    end;
  VoiceDeviceSawDLE:=false;
  if (b=controlCharacterDLE) then begin;
    putChar(controlCharacterDLE);
    continue;
    end;
  if (b=controlCharacterSUB) then begin;
    putChar(controlCharacterDLE);
    putChar(controlCharacterDLE);
    continue;
    end;
  if (b in [controlCharacterETX,controlCharacterCAN]) then begin;
    VoiceDeviceSawSTP:=true;
    break;
    end;
  s:=s+c;
  end;
RecordModemDo:=s;
End;




Function PlayModemBeg:Boolean;
Var s:String;
Begin;
PlayModemBeg:=True;
ClearStateModem;
RotateOldShielded;
case VoiceCommandSet of
  1:ModemDoCmd('AT#VTX',s);
  2:ModemDoCmd('AT+VTX',s);
  else s:='';
  end;
if (ModemTestResponse(s)<>2) then exit;
PlayModemBeg:=False;
End;

Procedure PlayModemFin;
procedure x(b:Byte);begin;pipeLineSend(serialData,b,sizeof(b));end;
Begin;
x(controlCharacterDLE);
x(controlCharacterETX);
End;

Procedure PlayModemCan;
procedure x(b:Byte);begin;pipeLineSend(serialData,b,sizeof(b));end;
Begin;
x(controlCharacterDLE);
x(controlCharacterCAN);
End;

Procedure PlayModemDo;
Var
  buf:array[1..1024] of byte;
  siz,max:LongInt;
  i,o,p:LongInt;

Procedure putChar(c:Byte);
Begin;
inc(siz);
buf[siz]:=c;
End;

Begin;
pipeLineStats(serialData,i,i,max);
if (max<128) then exit;
if (VoiceDeviceTXsiz<1) then exit;
siz:=0;
p:=0;
for o:=1 to VoiceDeviceTXsiz do begin;
  if (siz>max-8) then break;
  i:=VoiceDeviceTXbuf[o];
  inc(p);
  if (i=controlCharacterDLE) then putChar(controlCharacterDLE);
  putChar(i);
  end;
deleteFromBuffer(VoiceDeviceTXbuf,VoiceDeviceTXsiz,p);
if (pipeLineSend(serialData,buf,siz)=0) then exit;
pipeLineStats(serialData,o,i,i);
if (o=0) then immErr('serial driver closed connection!');
End;


Function RecPlayModemBeg:Boolean;
Var
  s:String;
  i:LongInt;
Begin;
RecPlayModemBeg:=True;
ClearStateModem;
RotateOldShielded;
case VoiceCommandSet of
  1:ModemDoCmd('AT#VTR',s);
  2:ModemDoCmd('AT+VTR',s);
  else s:='';
  end;
if (ModemTestResponse(s)<>2) then exit;
for i:=1 to 16 do pipeLineSend(serialData,VoiceDeviceTXbuf,sizeof(VoiceDeviceTXbuf));
RecPlayModemBeg:=False;
End;




Function RecordModem(s:String;tim:LongInt;keys:String):Boolean;
Label f1,f2;
const max=1024;
Var
  filH:xFile;
  filP:LongInt;
  beg:LongInt;
  stp:Boolean;
  i,o:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
RecordModem:=True;
keys:=nagy(keys);
xCreate(s);
if (xOpen(filH,s,xGenFilMod_rw)<>0) then Exit;
if RecordModemBeg then begin; xClose(filH);exit; end;
filP:=0;
stp:=False;
s:='';
timer2start;
beg:=CurrentTime;
f1:
relequish;
timer2start;
if (GetTimePast(beg)>tim) then begin;
  CancelRecModem;
  beg:=CurrentTime;
  tim:=5;
  stp:=True;
  end;
if ReadUpKeysIfWere then tim:=0;
a:=RecordModemDo(stp);
for i:=1 to ab0 do begin;
  s:=s+' <'+ProcessShieldedChar(ab[i])+'>';
  s:=copy(s,length(s)-50,255);
  if (pos(chr(ab[i]),keys)<>0) then tim:=0;
  end;
if (VoiceDeviceRXsiz>=max) then begin;
  xBlockWrite(filH,VoiceDeviceRXbuf,max);
  deleteFromBuffer(VoiceDeviceRXbuf,VoiceDeviceRXsiz,max);
  inc(filP,max);
  end;
Write(#13+BStr(filP+VoiceDeviceRXsiz)+'; '+s+#13);
if VoiceDeviceSawSTP then goto f2;
goto f1;
f2:
xBlockWrite(filH,VoiceDeviceRXbuf,VoiceDeviceRXsiz);
VoiceDeviceRXsiz:=0;
xTruncate(filH);
xClose(filH);
clrEol;
if ModemReadCommand(s,10) then exit;
RecordModem:=False;
End;



Function PlayModem(s:String;keys:String):Boolean;
Label f1,f2;
Var
  filH:xFile;
  filS:LongInt;
  filP:LongInt;
  i,o:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
PlayModem:=True;
keys:=nagy(keys);
if (xOpen(filH,s,xGenFilMod_r)<>0) then Exit;
filS:=xFileSize(filH);
filP:=0;
if PlayModemBeg then begin; xClose(filH);exit; end;
s:='';
f1:
relequish;
timer2start;
if ReadUpKeysIfWere then begin;
  PlayModemCan;
  goto f2;
  end;
if (VoiceDeviceTXsiz<1) then begin;
  VoiceDeviceTXsiz:=filS-filP;
  if (VoiceDeviceTXsiz<1) then begin; PlayModemFin;goto f2; end;
  if (VoiceDeviceTXsiz>sizeof(VoiceDeviceTXbuf)) then VoiceDeviceTXsiz:=sizeof(VoiceDeviceTXbuf);
  xBlockRead(filH,VoiceDeviceTXbuf,VoiceDeviceTXsiz);
  inc(filP,VoiceDeviceTXsiz);
  end;
PlayModemDo;
VoiceDeviceRXsiz:=0;
a:=RecordModemDo(false);
for i:=1 to ab0 do begin;
  s:=s+' <'+ProcessShieldedChar(ab[i])+'>';
  s:=copy(s,length(s)-50,255);
  if (pos(chr(ab[i]),keys)=0) then continue;
  PlayModemCan;
  goto f2;
  end;
Write(#13+BStr(filP)+'; '+s+#13);
goto f1;
f2:
xClose(filH);
clrEol;
if ModemReadCommand(s,10) then exit;
PlayModem:=False;
End;


Function RecPlayModem(p,r:String;tim:LongInt;keys:String):Boolean;
Label f1,f2;
Const max=1024;
Var
  filHp,filHr:xFile;
  filPp,filPr:LongInt;
  filSp:LongInt;
  stp:Boolean;
  beg:LongInt;
  i,o:LongInt;
  a,s:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
RecPlayModem:=True;
keys:=nagy(keys);
if (xOpen(filHp,p,xGenFilMod_r)<>0) then Exit;
filSp:=xFileSize(filHp);
filPp:=0;
xCreate(r);
if (xOpen(filHr,r,xGenFilMod_rw)<>0) then Exit;
filPr:=0;
if RecPlayModemBeg then begin; xClose(filHr);xClose(filHp);exit; end;
stp:=False;
s:='';
timer2start;
beg:=CurrentTime;
f1:
relequish;
timer2start;
if (GetTimePast(beg)>tim) then begin;
  CancelRecModem;
  beg:=CurrentTime;
  tim:=5;
  stp:=True;
  end;
if ReadUpKeysIfWere then tim:=0;
if (VoiceDeviceTXsiz<1) then begin;
  VoiceDeviceTXsiz:=filSp-filPp;
  if (VoiceDeviceTXsiz<1) then VoiceDeviceTXsiz:=0;
  if (VoiceDeviceTXsiz>sizeof(VoiceDeviceTXbuf)) then VoiceDeviceTXsiz:=sizeof(VoiceDeviceTXbuf);
  if (VoiceDeviceTXsiz>0) then xBlockRead(filHp,VoiceDeviceTXbuf,VoiceDeviceTXsiz);
  inc(filPp,VoiceDeviceTXsiz);
  end;
PlayModemDo;
a:=RecordModemDo(stp);
for i:=1 to ab0 do begin;
  s:=s+' <'+ProcessShieldedChar(ab[i])+'>';
  s:=copy(s,length(s)-50,255);
  if (pos(chr(ab[i]),keys)<>0) then tim:=0;
  end;
if (VoiceDeviceRXsiz>=max) then begin;
  xBlockWrite(filHr,VoiceDeviceRXbuf,max);
  deleteFromBuffer(VoiceDeviceRXbuf,VoiceDeviceRXsiz,max);
  inc(filPr,max);
  end;
Write(#13+'r='BStr(filPr+VoiceDeviceRXsiz)+' p='+BStr(filPp)+'; '+s+#13);
if VoiceDeviceSawSTP then goto f2;
goto f1;
f2:
xBlockWrite(filHr,VoiceDeviceRXbuf,VoiceDeviceRXsiz);
VoiceDeviceRXsiz:=0;
xTruncate(filHr);
xClose(filHr);
xClose(filHp);
clrEol;
if ModemReadCommand(a,10) then exit;
RecPlayModem:=False;
End;


Function ReadLnModem(tim,max:LongInt;keys:String):Boolean;
Label f1,f2;
Var
  beg:LongInt;
  a,s:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  stp:Boolean;
  i,o:LongInt;
Begin;
ReadLnModem:=True;
keys:=nagy(keys);
ModemLastReadLn:='';
if RecordModemBeg then exit;
stp:=False;
s:='';
timer2start;
beg:=CurrentTime;
f1:
relequish;
timer2start;
ReadUpKeysIfWere;
if not stp then while (ModemEscaped<>'') do begin;
  a:=copy(ModemEscaped,1,1);
  ModemEscaped:=copy(ModemEscaped,2,255);
  if (pos(a,keys)<>0) then begin; stp:=True;tim:=0;goto f1; end;
  ModemLastReadLn:=copy(ModemLastReadLn+a,length(ModemLastReadLn)-250,255);
  if (length(ModemLastReadLn)<max) then continue;
  stp:=True;
  tim:=0;
  goto f1;
  end;
timer2start;
if (GetTimePast(beg)>tim) then begin;
  CancelRecModem;
  beg:=CurrentTime;
  tim:=5;
  stp:=True;
  end;
VoiceDeviceRXsiz:=0;
a:=RecordModemDo(stp);
for i:=1 to ab0 do begin;
  s:=s+' <'+ProcessShieldedChar(ab[i])+'>';
  s:=copy(s,length(s)-50,255);
  if (pos(chr(ab[i]),keys)<>0) then tim:=0;
  end;
Write(#13+'read: '+s+#13);
if VoiceDeviceSawSTP then goto f2;
goto f1;
f2:
clrEol;
if ModemReadCommand(s,10) then exit;
ReadLnModem:=False;
End;



Function Wait4ringModem:Boolean;
Label f1;
Var
  a:String;
  i:longInt;
Begin;
Wait4ringModem:=True;
f1:
if not ModemReadCommand(a,1) then begin;
  if (ModemTestResponse(a)<>6) then goto f1;
  RotateOldShielded;
  Wait4ringModem:=False;
  Exit;
  end;
while keypressed do case ReadKey of
  $8004:begin; {enter} Wait4ringModem:=False;Exit; end;
  $8005:Exit; {escape}
  end;
relequish;
goto f1;
End;
