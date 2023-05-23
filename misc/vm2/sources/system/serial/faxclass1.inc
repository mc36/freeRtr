Type
  FaxClass1oneFrameRecord=record
    s:LongInt;
    d:array[1..1024] of byte;
    end;
Var
  FaxClass1sawFrames:LongInt; {1-dis, 2-dcs, 4-cfr, 8-ftt 16-mcf, 32-rtp, 64-mps, 128-eop, 256-dcn}
  FaxClass1speed:LongInt;  {$ff-speed, $100-v27, $200-v29, $400-v17, $800-v33}
  FaxClass1firstBit:Byte;


Function FaxClass1modulationCode(short:Byte):LongInt;
Var i:LongInt;
Begin;
case FaxClass1speed and $ff of
  0:i:=24; {2400}
  1:i:=48; {4800}
  2:if (FaxClass1speed and $400<>0) then i:=73+short else i:=72; {7200}
  3:if (FaxClass1speed and $400<>0) then i:=97+short else i:=96; {9600}
  4:i:=121+short; {12000}
  5:i:=145+short; {14400}
  else i:=-1;
  end;
FaxClass1modulationCode:=i;
End;

Function FaxClass1trainingSize:LongInt;
Var i:LongInt;
Begin;
i:=FaxSpeeds[FaxClass1speed and $ff] shr 3;
inc(i,i shr 1);
FaxClass1trainingSize:=i;
End;

Procedure FaxClass1framePutByte(var d:FaxClass1oneFrameRecord;c:byte);
Begin;
inc(d.s);
d.d[d.s]:=c;
End;

Procedure FaxClass1createID(var d:FaxClass1oneFrameRecord;typ:byte);
Const max=20;
Var
  a:String;
  i:LongInt;
Begin;
a:=LocalIdentifier;
while (length(a)<max) do a:=' '+a;
fillchar(d,sizeof(d),0);
FaxClass1framePutByte(d,typ or FaxClass1firstBit);
for i:=max downto 1 do FaxClass1framePutByte(d,ord(a[i]));
End;

Procedure FaxClass1createCapa(var d:FaxClass1oneFrameRecord;dis:Boolean);
Var i,o:LongInt;
Begin;
fillchar(d,sizeof(d),0);
if dis then i:=$80 else i:=$82;
FaxClass1framePutByte(d,i or FaxClass1firstBit);
FaxClass1framePutByte(d,$00);
if dis then begin;
  o:=0;
  if (FaxSpeedLimit>0) then o:=o or 2; {v27ter}
  if (FaxSpeedLimit>1) then o:=o or 1; {v29}
  if (FaxSpeedLimit>3) then o:=o or 8; {v17}
  end else begin;
  o:=0;
  case FaxClass1speed and $ff of
    0:o:=0; {2400}
    1:o:=2; {4800}
    2:if (FaxClass1speed and $400<>0) then o:=11 else o:=3; {7200}
    3:if (FaxClass1speed and $400<>0) then o:=9 else o:=1; {9600}
    4:o:=10; {12000}
    5:o:=8; {14400}
    end;
  end;
FaxClass1framePutByte(d,(o shl 2) or 2);
FaxClass1framePutByte(d,$08);
End;

Procedure FaxClass1createStat(var d:FaxClass1oneFrameRecord;typ:Byte);
Begin;
fillchar(d,sizeof(d),0);
FaxClass1framePutByte(d,typ or FaxClass1firstBit);
End;



Procedure FaxClass1displayFrame(var d:FaxClass1oneFrameRecord;rx:Boolean);
Label f1;
Var
  a,b:String;
  i:LongInt;
  t:Byte;

Procedure readIdentifier;
Var i:LongInt;
Begin;
a:='';
for i:=21 downto 2 do a:=a+chr(d.d[i]);
a:='"'+xLevesz(a)+'"';
End;

Procedure readCapability(dis:Boolean);
Label f1;
Begin;
a:='';
if dis then begin;
  if (d.d[2] and $20<>0) then a:=a+', v80';
  a:=a+', ecm:';
  if (d.d[2] and $40<>0) then a:=a+'256' else a:=a+'64';
  if (d.d[3] and $01<>0) then a:=a+', polling';
  end;
t:=(d.d[3] shr 2) and $f;
if dis then begin;
  i:=$ff;
  b:='';
  if (t=0) then begin; b:=b+'+v27terFB';i:=i or $000; end;
  if (t and 2<>0) then begin; b:=b+'+v27ter';i:=i or $100; end;
  if (t and 1<>0) then begin; b:=b+'+v29';i:=i or $200; end;
  if (t and 8<>0) then begin; b:=b+'+v17';i:=i or $400; end;
  if (t and 4<>0) then begin; b:=b+'+v33';i:=i or $800; end;
  b:=copy(b,2,255);
  if rx then FaxClass1speed:=i;
  end else begin;
  i:=$ff;
  b:='?';
  case t of
    0:begin; b:='v27ter';i:=$100; end; {2400}
    2:begin; b:='v27ter';i:=$101; end; {4800}
    3:begin; b:='v29';i:=$202; end; {7200}
    1:begin; b:='v29';i:=$203; end; {9600}
    11:begin; b:='v17';i:=$402; end; {7200}
    9:begin; b:='v17';i:=$403; end; {9600}
    10:begin; b:='v17';i:=$404; end; {12000}
    8:begin; b:='v17';i:=$405; end; {14400}
    end;
  t:=i and $ff;
  if (t>MaxFaxSpeeds) then b:='?/'+b else b:=BStr(FaxSpeeds[t])+'/'+b;
  if rx then FaxClass1speed:=i;
  end;
a:=a+', prot:'+b;
if (d.d[3] and $40=0) then b:='98' else b:='196';
a:=a+', '+b+' lpi';
if (d.d[3] and $40=0) then b:='1' else b:='2';
a:=a+', '+b+'d';
case d.d[4] and 3 of
  0:b:='215';
  2:b:='303';
  1:b:='255';
  3:b:='?';
  end;
a:=a+', '+b+' mm';
case (d.d[4] shr 2) and 3 of
  0:b:='297';
  2:b:='any';
  1:b:='364';
  3:b:='?';
  end;
a:=a+', '+b+' mm';
case (d.d[4] shr 4) and 7 of
  0:b:='20';
  4:b:='40';
  2:b:='10';
  1:b:='5';
  6:b:='10';
  3:b:='20';
  5:b:='40';
  7:b:='0';
  end;
a:=a+', '+b+' ms/ln';
if (d.d[4] and $80=0) then goto f1;
if (d.d[5] and $02<>0) then a:=a+', unc';
if (d.d[5] and $04<>0) then a:=a+', ecm';
if (d.d[5] and $40<>0) then a:=a+', t6';
if (d.d[5] and $80=0) then goto f1;
if (d.d[6] and $80=0) then goto f1;
if (d.d[7] and $01<>0) then a:=a+', 15 ln/mm';
if (d.d[7] and $02<>0) then a:=a+', 300 dpi';
if (d.d[7] and $04<>0) then a:=a+', 400 dpi';
if (d.d[7] and $08<>0) then a:=a+', inch' else a:=a+', mm';
if (d.d[7] and $40<>0) then a:=a+', selpoll';
if (d.d[7] and $80=0) then goto f1;
if (d.d[8] and $01<>0) then a:=a+', subaddr';
if (d.d[8] and $02<>0) then a:=a+', passwd';
if (d.d[8] and $04<>0) then a:=a+', rdy2btf';
if (d.d[8] and $10<>0) then a:=a+', btf';
if (d.d[8] and $20<>0) then a:=a+', dtm';
if (d.d[8] and $40<>0) then a:=a+', edi';
if (d.d[8] and $80=0) then goto f1;
if (d.d[9] and $01<>0) then a:=a+', btm';
if (d.d[9] and $04<>0) then a:=a+', rdy2mix';
if (d.d[9] and $08<>0) then a:=a+', chr';
if (d.d[9] and $20<>0) then a:=a+', t4';
if (d.d[9] and $80=0) then goto f1;
if (d.d[10] and $01<>0) then a:=a+', t505';
if (d.d[10] and $02<>0) then a:=a+', dig';
if (d.d[10] and $04<>0) then a:=a+', full' else a:=a+', half';
if (d.d[10] and $08<>0) then a:=a+', jpeg';
if (d.d[10] and $10<>0) then a:=a+', colr';
if (d.d[10] and $40<>0) then a:=a+', 12bit';
if (d.d[10] and $80=0) then goto f1;
if (d.d[11] and $01<>0) then a:=a+', sbsmpl';
if (d.d[11] and $02<>0) then a:=a+', illm';
if (d.d[11] and $04<>0) then a:=a+', gamt';
if (d.d[11] and $08<>0) then a:=a+', 279mm';
if (d.d[11] and $10<>0) then a:=a+', 355mm';
if (d.d[11] and $20<>0) then a:=a+', t85bs';
if (d.d[11] and $40<>0) then a:=a+', t85l0';
if (d.d[11] and $80=0) then goto f1;

f1:
a:=copy(a,3,255);
End;

Begin;
t:=d.d[1];

{Initial Identification}
if (t=$80) then begin;
  readCapability(true);
  write2logP('Digital ID Signal: '+a);
  if not rx then goto f1;
  FaxClass1firstBit:=1;
  FaxClass1sawFrames:=FaxClass1sawFrames or 1;
  goto f1;
  end;
if (t=$40) then begin;
  readIdentifier;
  write2logP('Called Susbcriber ID: '+a);
  goto f1;
  end;
if (t=$20) then begin;
  write2logP('Non Standard Features');
  goto f1;
  end;
if (t and $f=$0) then begin;
  write2logP('unknown Initial Identification!');
  goto f1;
  end;

{Command To Send}
if (t=$81) then begin;
  readCapability(true);
  write2logP('Digital Transmit Command');
  goto f1;
  end;
if (t=$41) then begin;
  readIdentifier;
  write2logP('Calling Subscriber ID: '+a);
  goto f1;
  end;
if (t=$21) then begin;
  write2logP('Non Standard Command');
  goto f1;
  end;
if (t=$c1) then begin;
  readIdentifier;
  write2logP('Password: '+a);
  goto f1;
  end;
if (t=$a1) then begin;
  write2logP('Selective Polling');
  goto f1;
  end;
if (t and $f=$1) then begin;
  write2logP('unknown Command To Send!');
  goto f1;
  end;

{Command To Receive}
if (t and $fe=$82) then begin;
  readCapability(false);
  write2logP('Digital Command Signal: '+a);
  if not rx then goto f1;
  FaxClass1firstBit:=0;
  FaxClass1sawFrames:=FaxClass1sawFrames or 2;
  goto f1;
  end;
if (t and $fe=$42) then begin;
  readIdentifier;
  write2logP('Transmitting Subscriber ID: '+a);
  goto f1;
  end;
if (t and $fe=$22) then begin;
  write2logP('Non Standard Setup');
  goto f1;
  end;
if (t and $fe=$c2) then begin;
  readIdentifier;
  write2logP('Subaddress: '+a);
  goto f1;
  end;
if (t and $fe=$a2) then begin;
  readIdentifier;
  write2logP('Password: '+a);
  goto f1;
  end;
if (t and $e=$2) then begin;
  write2logP('unknown Command To Receive!');
  goto f1;
  end;

{Pre Message Response}
if (t and $fe=$84) then begin;
  write2logP('Confirmation to Receive');
  if not rx then goto f1;
  FaxClass1sawFrames:=FaxClass1sawFrames or 4;
  goto f1;
  end;
if (t and $fe=$44) then begin;
  write2logP('Failure To Train');
  if not rx then goto f1;
  FaxClass1sawFrames:=FaxClass1sawFrames or 8;
  goto f1;
  end;
if (t and $e=$4) then begin;
  write2logP('unknown Pre Message Response!');
  goto f1;
  end;

{Post Message Command}
if (t and $fe=$8e) then begin;
  write2logP('End Of Message');
  if not rx then goto f1;
  FaxClass1sawFrames:=FaxClass1sawFrames or 128;
  goto f1;
  end;
if (t and $fe=$4e) then begin;
  write2logP('Multipage Signal');
  if not rx then goto f1;
  FaxClass1sawFrames:=FaxClass1sawFrames or 64;
  goto f1;
  end;
if (t and $fe=$2e) then begin;
  write2logP('End Of Procedure');
  if not rx then goto f1;
  FaxClass1sawFrames:=FaxClass1sawFrames or 128;
  goto f1;
  end;
if (t and $fe=$9e) then begin;
  write2logP('Procedure Interrupt - End Of Message');
  if not rx then goto f1;
  FaxClass1sawFrames:=FaxClass1sawFrames or 128;
  goto f1;
  end;
if (t and $fe=$5e) then begin;
  write2logP('Procedure Interrupt - Multipage Signal');
  if not rx then goto f1;
  FaxClass1sawFrames:=FaxClass1sawFrames or 128;
  goto f1;
  end;
if (t and $fe=$3e) then begin;
  write2logP('Procedure Interrupt - End Of Procedure');
  if not rx then goto f1;
  FaxClass1sawFrames:=FaxClass1sawFrames or 128;
  goto f1;
  end;
if (t and $e=$e) then begin;
  write2logP('unknown Post Message Command!');
  goto f1;
  end;

{Post Message Response}
if (t and $fe=$8c) then begin;
  write2logP('Message Confirmation');
  if not rx then goto f1;
  FaxClass1sawFrames:=FaxClass1sawFrames or 16;
  goto f1;
  end;
if (t and $fe=$cc) then begin;
  write2logP('Retrain Positive');
  if not rx then goto f1;
  FaxClass1sawFrames:=FaxClass1sawFrames or 32;
  goto f1;
  end;
if (t and $fe=$4c) then begin;
  write2logP('Retrain Negative');
  if not rx then goto f1;
  FaxClass1sawFrames:=FaxClass1sawFrames or 32;
  goto f1;
  end;
if (t and $fe=$ac) then begin;
  write2logP('Procedure Interrupt - Positive');
  goto f1;
  end;
if (t and $fe=$2c) then begin;
  write2logP('Procedure Interrupt - Negative');
  goto f1;
  end;
if (t and $fe=$fc) then begin;
  write2logP('File Diagnostic Message');
  goto f1;
  end;
if (t and $e=$c) then begin;
  write2logP('unknown Post Message Command!');
  goto f1;
  end;

{Other Line Control}
if (t and $fe=$fa) then begin;
  write2logP('Disconnect Now');
  if not rx then goto f1;
  FaxClass1sawFrames:=FaxClass1sawFrames or 256;
  goto f1;
  end;
if (t and $fe=$1a) then begin;
  write2logP('Command Repeat');
  goto f1;
  end;
if (t and $e=$a) then begin;
  write2logP('unknown Other Line Control!');
  goto f1;
  end;


write2logP('invalid frame type!');
f1:
End;



Procedure FaxClass1frameSend(var d:FaxClass1oneFrameRecord;final:Boolean);
Var
  a:String;
  i:LongInt;
  b:Byte;
Begin;
a:='';
for i:=1 to d.s do a:=a+' '+byte2hextype(d.d[i]);
write2logP('hdlcTx: FLG CTR'+a+' FCS16');
TransmitOneChar($ff);
if final then TransmitOneChar($13) else TransmitOneChar($03);
for i:=1 to d.s do begin;
  if (i mod 64=0) then HandleXonXoff(false);
  b:=d.d[i];
  if (b<>controlCharacterDLE) then begin;
    TransmitOneChar(b);
    continue;
    end;
  TransmitOneChar(controlCharacterDLE);
  TransmitOneChar(controlCharacterDLE);
  end;
HandleXonXoff(false);
TransmitOneChar(controlCharacterDLE);
TransmitOneChar(controlCharacterETX);
FaxClass1displayFrame(d,false);
End;

Function FaxClass1frameRecv(retry:LongInt):Boolean;
Label f1,f2;
Var
  d:FaxClass1oneFrameRecord;
  b:Byte;
  a:String;
  i:LongInt;
Begin;
FaxClass1frameRecv:=True;
FaxClass1sawFrames:=0;
f1:
fillChar(d,sizeof(d),0);
timer2start;
i:=CurrentTime;
while (1=1) do begin;
  if (d.s>sizeof(d.d)) then begin;
    write2logP('too big hdlc frame!');
    exit;
    end;
  relequish;
  timer2start;
  if (GetTimePast(i)>60) then begin;
    write2logP('timeout while receiving hdlc frame!');
    exit;
    end;
  if not TestReceivedOne then continue;
  b:=ReceiveOneChar;
  if (b<>controlCharacterDLE) then begin;
    FaxClass1framePutByte(d,b);
    continue;
    end;
  b:=ReceiveOneChar;
  if (b=controlCharacterDLE) then begin;
    FaxClass1framePutByte(d,controlCharacterDLE);
    continue;
    end;
  if (b=controlCharacterSUB) then begin;
    FaxClass1framePutByte(d,controlCharacterDLE);
    FaxClass1framePutByte(d,controlCharacterDLE);
    continue;
    end;
  if (b=controlCharacterETX) then break;
  end;
a:='';
for i:=1 to d.s do a:=a+' '+byte2hextype(d.d[i]);
write2logP('hdlcRx:'+a);
if (d.s<5) then begin;
  write2logP('got a too short frame!');
  goto f2;
  end;
if (CalcFCS16(d.d,d.s-2)<>ReadWordLSB(d.d[d.s-1])) then begin;
  write2logP('got fcs corrupted frame!');
  goto f2;
  end;
if (d.d[1]<>$ff) then begin;
  write2logP('got invalid address field!');
  goto f2;
  end;
if (d.d[2] and $ef<>$03) then begin;
  write2logP('got invalid control field!');
  goto f2;
  end;
b:=d.d[2] and $10;
dec(d.s,4);
move(d.d[3],d.d,d.s);
FaxClass1displayFrame(d,true);
if ModemReadCommand(a,60) then begin;
  write2logP('modem failed to respond!');
  exit;
  end;
i:=ModemTestResponse(a);
if (i=2) then goto f1;
if (i<>0) then begin;
  f2:
  dec(retry);
  if (retry<0) then exit;
  ModemSendCommand('AT+FRH=3');
  if ModemReadCommand(a,60) then begin;
    write2logP('modem failed to respond!');
    exit;
    end;
  if (ModemTestResponse(a)<>2) then goto f2;
  goto f1;
  end;
if (b=0) then begin;
  inc(retry);
  goto f2;
  end;
FaxClass1frameRecv:=False;
End;



Function FaxClass1init:Boolean;
Var a:String;
Begin;
FaxClass1init:=True;
if ModemDoCmd('AT+FCLASS=1',a) then exit;
if (ModemTestResponse(a)<>0) then exit;
FaxClass1init:=False;
End;



Function FaxClass1recv:Boolean;
Label f1,f2,f3,f4;
Var
  currSpeed:LongInt;
  d:FaxClass1oneFrameRecord;
  a:String;
  i,o,p:LongInt;

Procedure trainUp(i:Byte);
Begin;
inc(p,8);
inc(o,bitCountTable[i]);
End;

Begin;
FaxClass1recv:=True;
FaxClass1firstBit:=0;
write2logS('connecting...');
ModemSendCommand('ATA');
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>2) then exit;
p:=10;
f1:
FaxClass1createID(d,$40);
FaxClass1frameSend(d,false);
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>2) then exit;
FaxClass1createCapa(d,true);
FaxClass1frameSend(d,true);
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>0) then exit;
ModemSendCommand('AT+FRH=3');
if not ModemReadCommand(a,4) then goto f3;
TransmitOneChar(controlCharacterCR);
if ModemReadCommand(a,60) then exit;
ModemSendCommand('AT+FTH=3');
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>2) then exit;
dec(p);
if (p>0) then goto f1;
write2logP('retry counter expired...');
FaxClass1firstBit:=1;
FaxClass1createStat(d,$fa); {DCN}
FaxClass1frameSend(d,true);
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>0) then exit;
exit;

f2:
ModemSendCommand('AT+FRH=3');
if ModemReadCommand(a,60) then exit;
f3:
if (ModemTestResponse(a)<>2) then exit;
if FaxClass1frameRecv(10) then exit;
if (FaxClass1sawFrames and 3<>2) then begin;
  write2logP('did not got DCS frame!');
  Exit;
  end;
currSpeed:=FaxClass1speed;

write2logS('channel training...');
write2logP('receiving '+BStr(FaxClass1trainingSize shl 3)+' zero bits...');
ModemSendCommand('AT+FRM='+BStr(FaxClass1modulationCode(0)));
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>2) then exit;
p:=0;o:=0;
while (1=1) do begin;
  i:=ReceiveOneChar;
  if (i<>controlCharacterDLE) then begin;
    trainUp(i);
    continue;
    end;
  i:=ReceiveOneChar;
  if (i=controlCharacterETX) then break;
  if (i=controlCharacterCAN) then break;
  if (i=controlCharacterDLE) then begin;
    trainUp(controlCharacterDLE);
    continue;
    end;
  if (i=controlCharacterSUB) then begin;
    trainUp(controlCharacterDLE);
    trainUp(controlCharacterDLE);
    continue;
    end;
  end;
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>5) then exit;
i:=(o*100) div p;
write2logP('received '+BStr(p)+' bits, '+BStr(o)+' was not zero: '+BStr(i)+'% bad...');
if (i>10) then begin;
  ModemSendCommand('AT+FTH=3');
  if ModemReadCommand(a,60) then exit;
  if (ModemTestResponse(a)<>2) then exit;
  FaxClass1createStat(d,$44); {FTT}
  FaxClass1frameSend(d,true);
  if ModemReadCommand(a,60) then exit;
  if (ModemTestResponse(a)<>0) then exit;
  goto f2;
  end;
ModemSendCommand('AT+FTH=3');
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>2) then exit;
FaxClass1createStat(d,$84); {CFR}
FaxClass1frameSend(d,true);
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>0) then exit;

f4:
FaxClass1speed:=currSpeed;
ModemSendCommand('AT+FRM='+BStr(FaxClass1modulationCode(1)));
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>2) then exit;
i:=ReceiveUpPage(false,false);
write2logS('page #'+BStr(filPag)+': '+BStr(i)+' bytes received...');
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>5) then exit;
ModemSendCommand('AT+FRH=3');
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>2) then exit;
if FaxClass1frameRecv(10) then exit;
ModemSendCommand('AT+FTH=3');
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>2) then exit;
FaxClass1createStat(d,$8c); {MCF}
FaxClass1frameSend(d,true);
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>0) then exit;
if (FaxClass1sawFrames and 64<>0) then goto f4;
if (FaxClass1sawFrames and 128=0) then begin;
  write2logP('did not got MPS nor EO? frame!');
  Exit;
  end;
write2logS('finishing session...');
ModemSendCommand('AT+FRH=3');
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>2) then exit;
if FaxClass1frameRecv(10) then exit;
if (FaxClass1sawFrames and 256=0) then begin;
  write2logP('did not got DCN frame!');
  Exit;
  end;
FaxClass1recv:=False;
End;



Function FaxClass1send:Boolean;
Label f1,f2,f3;
Var
  RemoteCapa:Longint;
  currSpeed:LongInt;
  d:FaxClass1oneFrameRecord;
  a:String;
  i,o,p:LongInt;
Begin;
FaxClass1send:=True;
FaxClass1firstBit:=0;
write2logS('connecting...');
ModemSendCommand('ATD'+phoneNumber);
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>2) then exit;
if FaxClass1frameRecv(10) then exit;
if (FaxClass1sawFrames and 3<>1) then begin;
  write2logP('did not got DIS frame!');
  Exit;
  end;
RemoteCapa:=FaxClass1speed;
currSpeed:=-1;

f1:
dec(currSpeed);
if (currSpeed<0) then begin;
  currSpeed:=0;
  if (RemoteCapa and $100<>0) then currSpeed:=1;
  if (RemoteCapa and $200<>0) then currSpeed:=3;
  if (RemoteCapa and $400<>0) then currSpeed:=5;
  end;
FaxClass1speed:=currSpeed;
ModemSendCommand('AT+FTH=3');
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>2) then exit;
FaxClass1createID(d,$42);
FaxClass1frameSend(d,false);
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>2) then exit;
FaxClass1createCapa(d,false);
FaxClass1frameSend(d,true);
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>0) then exit;
ModemSendCommand('AT+FTS=8');
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>0) then exit;

write2logS('channel training...');
p:=FaxClass1trainingSize;
write2logP('sending '+BStr(p shl 3)+' zero bits...');
ModemSendCommand('AT+FTM='+BStr(FaxClass1modulationCode(0)));
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>2) then exit;
for i:=1 to p do begin;
  if (i mod 64=0) then HandleXonXoff(false);
  TransmitOneChar(0);
  end;
HandleXonXoff(false);
TransmitOneChar(controlCharacterDLE);
TransmitOneChar(controlCharacterETX);
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>0) then exit;
ModemSendCommand('AT+FRH=3');
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>2) then exit;
if FaxClass1frameRecv(10) then exit;
if (FaxClass1sawFrames and 8<>0) then goto f1;
if (FaxClass1sawFrames and 4=0) then begin;
  write2logP('did not got CFR nor FTT frame!');
  Exit;
  end;

f2:
FaxClass1speed:=currSpeed;
ModemSendCommand('AT+FTM='+BStr(FaxClass1modulationCode(1)));
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>2) then exit;
i:=SendOutPage(false,false);
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>0) then exit;
write2logS('page #'+BStr(filPag)+': '+BStr(i)+' bytes sent...');
if not isAnotherNextPage then goto f3;
ModemSendCommand('AT+FTH=3');
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>2) then exit;
FaxClass1createStat(d,$4e); {MPS}
FaxClass1frameSend(d,true);
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>0) then exit;
ModemSendCommand('AT+FRH=3');
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>2) then exit;
if FaxClass1frameRecv(10) then exit;
if (FaxClass1sawFrames and 32<>0) then begin; currSpeed:=-1;goto f1; end;
if (FaxClass1sawFrames and 16=0) then begin;
  write2logP('did not got MCF nor RT? frame!');
  Exit;
  end;
goto f2;
f3:
write2logS('finishing session...');
ModemSendCommand('AT+FTH=3');
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>2) then exit;
FaxClass1createStat(d,$2e); {EOP}
FaxClass1frameSend(d,true);
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>0) then exit;
ModemSendCommand('AT+FRH=3');
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>2) then exit;
if FaxClass1frameRecv(10) then exit;
ModemSendCommand('AT+FTH=3');
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>2) then exit;
FaxClass1createStat(d,$fa); {DCN}
FaxClass1frameSend(d,true);
if ModemReadCommand(a,60) then exit;
if (ModemTestResponse(a)<>0) then exit;
FaxClass1send:=False;
End;
