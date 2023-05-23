{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$sysinc hex.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Var
  addrSiz:LongInt;
  packSiz:LongInt;
  localAddr:array[1..32] of byte;
  broadAddr:array[1..32] of byte;
  remotAddr:array[1..32] of byte;
  deviceName:String;
  lastSent:LongInt;
  lastGot:LongInt;
  lastStat:LongInt;
  lastData:String;


Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;

Function conv2hex(i:LongInt):String;
Begin;
conv2hex:=byte2hextype(i shr 24)+byte2hextype(i shr 16)+byte2hextype(i shr 8)+byte2hextype(i);
End;

Function convAddr(var data):String;
Var
  d:array[1..1] of byte absolute data;
  i:LongInt;
  a:String;
Begin;
a:='';
for i:=1 to addrSiz do a:=a+'-'+byte2hextype(d[i]);
convAddr:=copy(a,2,666);
End;

Function getOneTLV(var buffer;var ps:LongInt):String;
Var
  buf:array[1..1] of byte absolute buffer;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
getOneTLV:='';
i:=buf[ps+2];
if (i<2) then exit;
ab[1]:=buf[ps+1];
ab0:=i-1;
move(buf[ps+3],ab[2],ab0-1);
inc(ps,ab0+1);
getOneTLV:=a;
End;

Procedure putOneTLV(var buffer;var ps:LongInt;a:String;i:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
inc(ps);buf[ps]:=i;
inc(ps);buf[ps]:=ab0+2;
move(ab[1],buf[ps+1],ab0);
inc(ps,ab0);
End;

Label f1;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  etherPipe:LongInt;
  i,o,p,q:LongInt;
  buf:array[1..1024*8] of byte;
BEGIN;
WriteLn('oam v1.0, done by Mc at '#%date' '#%time'.');

a:=ParamStr(1);
if (a='') then immErr('using: oam.code <process>');
WriteLn('process: '+a);
o:=BugOS_findProcNam(a);
if (o=0) then immErr('process not found!');
WriteLn('process#: '+BStr(o));
i:=pipeLineCreate(etherPipe,o,65536,true);
if (i<>0) then immErr('unabled to create pipeline!');
WriteLn('pipeline#: '+BStr(etherPipe));
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(etherPipe,buf,i)<>0) then i:=0;
if (i<1) then immErr('initial packet not received!');
move(buf[1],addrSiz,sizeof(addrSiz));
move(buf[5],packSiz,sizeof(packSiz));
o:=17;
move(buf[o],localAddr,sizeof(localAddr));inc(o,addrSiz);
move(buf[o],broadAddr,sizeof(broadAddr));inc(o,addrSiz);
deviceName:='';
while (buf[o]<>0) do begin;
  deviceName:=deviceName+chr(buf[o]);
  inc(o);
  end;
WriteLn('address size: '+BStr(addrSiz));
WriteLn('packet size: '+BStr(packSiz));
WriteLn('station address: '+convAddr(localAddr));
WriteLn('broadcast address: '+convAddr(broadAddr));
WriteLn('device name: "'+deviceName+'"');

BugOS_SignDaemoning;
timer2start;

lastSent:=-99999;
lastGot:=-99999;
lastStat:=0;
lastData:='';

f1:
if (lastGot>0) then if (getTimePast(lastGot)>5) then begin;
  WriteLn(convAddr(remotAddr)+' neighbor down!');
  lastGot:=-99999;
  lastStat:=0;
  lastData:='';
  end;
if (getTimePast(lastSent)>=1) then begin;
  lastSent:=currentTime;
  a:=#$01#$80#$c2#$00#$00#$02#$88#$09#$03;
  move(ab[1],buf,ab0);
  p:=ab0;
  if (lastGot<0) then i:=$08 else i:=((lastStat and $18) shl 2) or $10;
  WriteWordMSB(buf[p+1],i);inc(p,2);
  inc(p);buf[p]:=0;
  putOneTLV(buf,p,#$01#$00#$00#$00#$09#$05#$dc#$00#$00#$00#$00#$00#$00#$00,1);
  if (lastGot>=0) then putOneTLV(buf,p,lastData,2);
  pipeLineSend(etherPipe,buf,p);
  end;
o:=sizeof(buf);
if (pipeLineRecv(etherPipe,buf,o)<>0) then o:=0;
if (o<1) then begin;
  relequish;
  timer2start;
  goto f1;
  end;
i:=ReadWordMSB(buf[addrSiz+1]);
if (i<>$8809) then begin;
  WriteLn('got packet with unknown type!');
  goto f1;
  end;
if (buf[addrSiz+3]<>3) then begin;
  WriteLn('got packet with unknown subtype!');
  goto f1;
  end;
lastStat:=ReadWordMSB(buf[addrSiz+4]);
p:=addrSiz+6;
lastData:='';
while (p<o) do begin;
  a:=getOneTLV(buf,p);
  if (ab0<1) then break;
  if (ab[1]=1) then lastData:=copy(a,2,666);
  end;
if (lastData='') then begin;
  WriteLn('got packet without local info tlv!');
  goto f1;
  end;
move(buf,remotAddr,addrSiz);
if (lastGot<0) then WriteLn(convAddr(remotAddr)+' neighbor up!');
lastGot:=currentTime;
goto f1;
END.