Function getMessageTypeNames(i:LongInt):String;
Var a:String;
Begin;
case i of
  $00:a:='national message';
  $01:a:='alerting';
  $02:a:='call proceeding';
  $07:a:='connect';
  $0f:a:='connect ack';
  $03:a:='progress';
  $05:a:='setup';
  $0d:a:='setup ack';
  $26:a:='resume';
  $2e:a:='resume ack';
  $22:a:='resume rej';
  $25:a:='suspend';
  $2d:a:='suspend ack';
  $21:a:='suspend rej';
  $20:a:='user info';
  $45:a:='disconnect';
  $4d:a:='release';
  $5a:a:='release done';
  $46:a:='restart';
  $4e:a:='restart ack';
  $60:a:='segment';
  $79:a:='congestion control';
  $7b:a:='information';
  $6e:a:='notify';
  $7d:a:='status';
  $75:a:='status enquery';
  $00..$1f:a:='unknown call estab';
  $20..$3f:a:='unknown call info';
  $40..$5f:a:='unknown call clear';
  $60..$7f:a:='unknown misc';
  else a:='unknown';
  end;
getMessageTypeNames:=byte2hextype(i)+'='+a;
End;

Function getOptionTypeNames(i:LongInt):String;
Var a:String;
Begin;
case i of
  $80..$8f:a:='reserved';
  $90..$9f:a:='shift';
  $a0:a:='more data';
  $a1:a:='sending complete';
  $b0..$bf:a:='congestion level';
  $d0..$df:a:='repeat indicator';
  $00:a:='segmented message';
  $04:a:='bearer capability';
  $08:a:='cause code';
  $10:a:='call identity';
  $14:a:='call state';
  $18:a:='channel id';
  $1e:a:='progress';
  $20:a:='network specific';
  $27:a:='notification';
  $28:a:='display';
  $29:a:='date/time';
  $2c:a:='keypad facility';
  $34:a:='signal';
  $40:a:='information rate';
  $42:a:='end2end delay';
  $43:a:='delay select';
  $44:a:='layer1 parameters';
  $45:a:='layer1 window size';
  $46:a:='packet size';
  $47:a:='user group';
  $4a:a:='reverse charge';
  $6c:a:='calling num';
  $6d:a:='calling sub';
  $70:a:='called num';
  $71:a:='called sub';
  $74:a:='redirect num';
  $78:a:='net select';
  $79:a:='restart';
  $7c:a:='layer1 compatible';
  $7d:a:='layer2 compatible';
  $7e:a:='user-user';
  $7f:a:='extension';
  else a:='unknown';
  end;
getOptionTypeNames:=byte2hextype(i)+'='+a;
End;




Function layer1addPack(a:String):Boolean;
Begin;
layer1addPack:=true;
if (layer1packNum>=maxLayer1packs) then exit;
layer1packDat[layer1packWrt]:=a;
if (layer1packDat[layer1packWrt]<>a) then begin;
  WriteLn('upper wanted to send too big packet!');
  exit;
  end;
layer1addPack:=false;
layer1packWrt:=(layer1packWrt mod maxLayer1packs)+1;
inc(layer1packNum);
if (layer1packNum>1) then exit;
layer1lasTime:=-9999;
dec(layer1tryDone);
End;



Procedure layer1sendSABME(send:Boolean);
Var
  buf:array[1..16] of byte;
  i:LongInt;
Begin;
layer1packSnd:=5;
layer1packWrt:=layer1packSnd;
layer1packNum:=0;
layer1seqSend:=-1;
layer1seqRecv:=0;
layer1lasTime:=currentTime;
if (layer1tryDone<90) then layer1tryDone:=99;
for i:=1 to ConnectionNum do begin;
  if (ConnectionDat^[i].mode in [1,2]) then continue;
  ConnectionDat^[i].stat:=999;
  ConnectionDat^[i].mode:=0;
  end;
if not send then exit;
{$ifdef debug1}WriteLn('sending sabme...');{$endif}
buf[1]:=controlChannel;
buf[2]:=pollFinalXor;
buf[3]:=termEndptId;
buf[4]:=$7f;
pipeLineSend(layer1devPipe,buf,4);
End;

Procedure layer1sendUA;
Var buf:array[1..16] of byte;
Begin;
{$ifdef debug1}WriteLn('sending ua...');{$endif}
buf[1]:=controlChannel;
buf[2]:=pollFinalXor xor 2;
buf[3]:=termEndptId;
buf[4]:=$73;
pipeLineSend(layer1devPipe,buf,4);
End;

Procedure layer1sendRR(cmd:Boolean);
Var buf:array[1..16] of byte;
Begin;
{$ifdef debug1}WriteLn('sending rr...');{$endif}
buf[1]:=controlChannel;
if cmd then buf[2]:=pollFinalXor else buf[2]:=pollFinalXor xor 2;
buf[3]:=termEndptId;
buf[4]:=$01;
buf[5]:=(layer1seqRecv shl 1) or 1;
pipeLineSend(layer1devPipe,buf,5);
End;

Function layer1gotAck(got:LongInt):Boolean;
Var i:LongInt;
Begin;
layer1gotAck:=true;
if (layer1packNum<1) then i:=1 else i:=2;
if (got shr 1<>(layer1seqSend+i) and $7f) then exit;
layer1gotAck:=false;
layer1tryDone:=0;
if (layer1packNum<1) then exit;
dec(layer1packNum);
inc(layer1seqSend);
layer1packSnd:=(layer1packSnd mod maxLayer1packs)+1;
if (layer1packNum<1) then exit;
layer1lasTime:=-9999;
dec(layer1tryDone);
End;



Function decodeChannelId(a:String):LongInt;
Label f1;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;

function get:LongInt;
label f1;
begin;
inc(p);
get:=ab[p] and $7f;
f1:
if (p>ab0) then exit;
if (ab[p] and $80<>0) then exit;
inc(p);
goto f1;
end;

Begin;
decodeChannelId:=-1;
ab0:=ab0 and $7f;
for i:=ab0+1 to 255 do ab[i]:=0;
p:=0;
i:=get;
if (i and $40<>0) then get;
if (i and 4<>0) then exit;
o:=i and 3;
if (o=0) then begin;
  if (i and 8<>0) then exit;
  o:=3;
  end;
if (o=3) then begin; o:=-1;goto f1; end;
if (i and $20=0) then begin; decodeChannelId:=o;exit; end;
if (o<>1) then exit;
i:=get;
if (i and $60<>0) then exit;
if (i and $f<>3) then exit;
if (i and $10=0) then begin; decodeChannelId:=get;exit; end;
a:=copy(a,p+1,4);
while (ab0<4) do a:=#0+a;
o:=ReadLongMSB(ab[1]);
f1:
p:=-1;
for i:=0 to ConnectionNum-1 do begin;
  if (ConnectionDat^[i+1].stat<>0) then continue;
  if ((1 shl ConnectionDat^[i+1].chn) and o=0) then continue;
  p:=i;
  end;
decodeChannelId:=p;
End;

Function decodeBearerCapa(a:String):LongInt;
{hex:0msssss; m:1=data,2=term,3=voice; sssss=bps}
Label f1;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;

function get:LongInt;
begin;
inc(p);
get:=ab[p];
end;

Begin;
decodeBearerCapa:=-1;
ab0:=ab0 and $7f;
for i:=ab0+1 to 255 do ab[i]:=0;
p:=0;
i:=get;
if (i and $60<>0) then exit;
i:=i and $1f;
case i of
  8:o:=1;
  0:o:=3;
  else exit;
  end;
i:=get;
if (i and $60<>0) then exit;
if (i and $1f<>$10) then exit;
i:=get;
if (i and $60<>$20) then begin; i:=0;goto f1; end;
case i and $1f of
  1:if (o<>1) then exit;
  3:if (o<>3) then exit;
  else exit;
  end;
if (i and $80<>0) then begin; i:=0;goto f1; end;
i:=get;
if (i and $40<>0) then o:=2;
i:=v110speedValueTab[i and $1f];
if (i<1) then exit;
f1:
if (i=0) then i:=64000;
decodeBearerCapa:=(o shl 20) or i;
End;

Function encodeChannelId(c:LongInt):String;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
if (ConnectionNum<=2) then begin;
  ab0:=1;
  ab[1]:=(c and 3) or $88;
  encodeChannelId:=a;
  exit;
  end;
ab0:=3;
ab[1]:=$a9;
ab[2]:=$83;
ab[3]:=c or $80;
encodeChannelId:=a;
End;

Function encodeBearerCapa(c:LongInt):String;
Label f1;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
Begin;
encodeBearerCapa:='';
o:=c shr 20;
if (o>2) then i:=0 else i:=8;
c:=c and $fffff;
ab0:=2;
ab[1]:=i or $80;
ab[2]:=$90;
if (o=1) and (c=64000) then goto f1;
if (o=3) then i:=$83 else i:=1;
inc(ab0);
ab[ab0]:=i or $20;
case o of
  1,2:;
  3:goto f1;
  else exit;
  end;
p:=-1;
for i:=0 to 31 do if (v110speedValueTab[i]=c) then p:=i;
if (p<0) then exit;
if (o=2) then p:=p or $40;
inc(ab0);
ab[ab0]:=p or $80;
f1:
encodeBearerCapa:=a;
End;

Function encodePhoneNum(a:String):String;
Begin;
encodePhoneNum:=#129+a;
End;




Procedure layer2msgBegin(var m:String;cref:String;t:LongInt);
Const max=15;
Var
  ab:array[0..1] of byte absolute cref;
  ab0:byte absolute cref;
Begin;
{$ifdef debug2}WriteLn('creating i message: type='+getMessageTypeNames(t)+'; callref='+convertToHexstring(cref));{$endif}
if (ab0>max) then ab0:=max;
ab[1]:=ab[1] xor $80;
m:=#8+chr(ab0)+cref+chr(t and $7f);
End;

Procedure layer2msgOption(var m:String;t:LongInt;c:String);
Begin;
{$ifdef debug2}WriteLn('  option: type='+getOptionTypeNames(t)+'; data='+convertToHexstring(c));{$endif}
if (t and $80<>0) then m:=m+chr(t) else m:=m+chr(t)+chr(length(c))+c;
End;

Procedure layer2msgCause(var m:String;t:LongInt);
Begin;
if (t<0) then exit;
layer2msgOption(m,8,#$80+chr(t or $80));
End;

Procedure layer2sendGeneric(cref:String;t,c:LongInt);
Var a:String;
Begin;
layer2msgBegin(a,cref,t);
layer2msgCause(a,c);
layer1addPack(a);
End;

Procedure layer2sendCallProc(var con:OneConnectionRecord);
Var a:String;
Begin;
layer2msgBegin(a,con.cid,$02);
layer2msgOption(a,$18,encodeChannelId(con.chn));
layer1addPack(a);
End;

Procedure layer2sendConnect(var con:OneConnectionRecord);
Var a:String;
Begin;
layer2msgBegin(a,con.cid,$07);
layer2msgOption(a,$18,encodeChannelId(con.chn));
layer1addPack(a);
End;

Procedure layer2sendSetup(var con:OneConnectionRecord);
Var a:String;
Begin;
layer2msgBegin(a,con.cid,$05);
layer2msgOption(a,$04,encodeBearerCapa(con.bear));
layer2msgOption(a,$18,encodeChannelId(con.chn));
if (con.srcA<>'') then layer2msgOption(a,$6c,encodePhoneNum(con.srcA));
layer2msgOption(a,$70,encodePhoneNum(con.trgA));
layer1addPack(a);
End;




Function clearOneConnectRec(var con:OneConnectionRecord):Boolean;
Var
  a:String;
  i,o:LongInt;
Begin;
clearOneConnectRec:=False;
pipeLineClose(con.pipeP);
if (con.cid<>'') then begin;
  layer2sendGeneric(con.cid,$4d,127);
  clearOneConnectRec:=True;
  end;
a:=con.cmnd;
i:=con.mode;
o:=con.chn;
fillchar(con,sizeof(con),0);
con.chn:=o;
if (i>2) then i:=0;
con.cmnd:=a;
con.mode:=i;
con.stat:=i;
con.time:=currentTime;
End;

Procedure setup4communication(var con:OneConnectionRecord);
Var i:LongInt;
Begin;
case con.mode of
  3:i:=3;
  4:i:=3;
  5:i:=666;
  else i:=666;
  end;
if (con.stat=5) then con.stat:=i;
if (con.stat<>i) then con.stat:=666;
End;
