{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc hex.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}

Type
  wccpHereAmMessageRecord=record
    typ:LongInt;                        {type}
    ver:LongInt;                        {version}
    rev:LongInt;                        {hash revision}
    hsh:array[1..32] of byte;           {hash info}
    hsi:LongInt;                        {hash valid}
    mid:LongInt;                        {received id}
    end;
  wccpOneEntryDataRecord=record
    adr:LongInt;                        {ip address}
    rev:LongInt;                        {hash revision}
    hsh:array[1..32] of byte;           {hash info}
    hsi:LongInt;                        {hash valid}
    end;
  wccpSeeYouMessageRecord=record
    typ:LongInt;                        {type}
    ver:LongInt;                        {version}
    chg:LongInt;                        {change number}
    mid:LongInt;                        {received id}
    num:LongInt;                        {number of caches}
    dat:array[1..1] of wccpOneEntryDataRecord;
    end;
  wccpAssignMessageRecord=record
    typ:LongInt;                        {type}
    mid:LongInt;                        {received id}
    num:LongInt;                        {number of caches}
    dat:array[0..0] of LongInt;
    end;
Var
  lowerPipe:LongInt;
  upperPipe:LongInt;
  devcePipe:LongInt;
  devceName:String;
  addrSize:LongInt;
  loclAddr:OneTCPaddressRecord;
  loclPort:LongInt;
  peerAddr:OneTCPaddressRecord;
  peerPort:LongInt;
  peerEcho:Boolean;
  lastHere:LongInt;
  lastIsee:LongInt;
  lastAsgn:LongInt;
  lastHash:array[1..32] of byte;
  lastHshS:LongInt;
  lastHshV:LongInt;
  lastMsgI:LongInt;
  cacheDat:array[1..512] of LongInt;
  cacheNum:LongInt;


Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;


Function decodeOneMessageType(i:LongInt):String;
Var a:String;
Begin;
case i of
  7:a:='hereIam';
  8:a:='seeYou';
  9:a:='assignBucket';
  else a:='unknown='+BStr(i);
  end;
decodeOneMessageType:=a;
End;


Procedure openDevProc(a:String);
Var
  buf:array[1..1024] of byte;
  i,o:LongInt;
Begin;
WriteLn('process: '+a);
o:=BugOS_findProcNam(a);
if (o=0) then immErr('process not found!');
WriteLn('process#: '+BStr(o));
i:=pipeLineCreate(devcePipe,o,65536,true);
if (i<>0) then immErr('unabled to create pipeline!');
WriteLn('pipeline#: '+BStr(devcePipe));
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(devcePipe,buf,i)<>0) then i:=0;
if (i<1) then immErr('initial packet not received!');
move(buf[1],addrSize,sizeof(addrSize));
o:=(2*addrSize)+17;
devceName:='';
while (buf[o]<>0) do begin;
  devceName:=devceName+chr(buf[o]);
  inc(o);
  end;
WriteLn('address size: '+BStr(addrSize));
WriteLn('device name: "'+devceName+'"');
End;


Procedure releq2upper;
Var
  buf:array[1..2048] of byte;
  i,o,p,q:LongInt;
  a:String;
Begin;
while (pipeLineGetIncoming(p)=0) do begin;
  pipeLineStats(p,q,i,i);
  if (upperPipe<>0) then begin; pipeLineClose(p);break; end;
  BugOS_ProcessName(q,buf,i,i,o);
  if (o and $40=0) then begin; pipeLineClose(p);break; end;
  upperPipe:=p;
  i:=2;
  move(i,buf[1],sizeof(i));
  i:=1500;
  move(i,buf[5],sizeof(i));
  i:=0;
  move(i,buf[9],sizeof(i));
  move(i,buf[13],sizeof(i));
  i:=17;
  buf[i+0]:=11;buf[i+1]:=22;inc(i,2);
  buf[i+0]:=255;buf[i+1]:=255;inc(i,2);
  a:='wccp with '+ipAddr2string(peerAddr)+' '+BStr(peerPort);
  move(a[1],buf[i],sizeof(a));
  inc(i,length(a));
  buf[i]:=0;
  pipeLineSend(upperPipe,buf,i);
  WriteLn('upper logged in!');
  end;
if (upperPipe=0) then exit;
o:=sizeof(buf);
if (pipeLineRecv(upperPipe,buf[addrSize+1],o)<>0) then o:=0;
if (o<1) then begin;
  pipeLineStats(upperPipe,o,i,i);
  if (o<>0) then exit;
  WriteLn('upper logged out!');
  pipeLineClose(upperPipe);
  upperPipe:=0;
  exit;
  end;
WriteWordMSB(buf[addrSize+1],$883e);
pipeLineSend(devcePipe,buf,o+addrSize);
End;


Procedure releq2lower;
Var
  buf:array[1..2048] of byte;
  i,o:LongInt;
  a:String;
Begin;
o:=sizeof(buf);
if (pipeLineRecv(devcePipe,buf,o)<>0) then o:=0;
if (o<1) then begin;
  pipeLineStats(devcePipe,o,i,i);
  if (o<>0) then exit;
  immErr('lower closed data connection!');
  end;
i:=ReadWordMSB(buf[addrSize+1]);
if (i<>$883e) then begin;
  WriteLn('got packet with wrong type!');
  exit;
  end;
if peerEcho then pipeLineSend(devcePipe,buf,o);
if (upperPipe<>0) then pipeLineSend(upperPipe,buf[addrSize+1],o-addrSize);
End;


Procedure releq2control;
Label f1;
Var
  buf:array[1..2048] of byte;
  msgH:wccpHereAmMessageRecord absolute buf;
  msgS:wccpSeeYouMessageRecord absolute buf;
  msgA:wccpAssignMessageRecord absolute buf;
  i,o,p:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
if (getTimePast(lastIsee)>300) then immErr('remote not responding!');
if (getTimePast(lastHere)>10) then begin;
  lastHere:=currentTime;
  WriteLongMSB(msgH.typ,7);
  WriteLongMSB(msgH.ver,4);
  WriteLongMSB(msgH.rev,lastHshV);
  move(lastHash,msgH.hsh,sizeof(lastHash));
  WriteLongMSB(msgH.hsi,lastHshS);
  WriteLongMSB(msgH.mid,lastMsgI);
  UDPsendPacket(lowerPipe,peerAddr,peerPort,buf,sizeof(msgH));
  end;
o:=sizeof(buf);
if UDPreceivePacket(lowerPipe,a,i,buf,o) then begin;
  if (lowerPipe<>0) then exit;
  immErr('lower closed control connection!');
  end;
if (i<>peerPort) then begin;
  f1:
  WriteLn('got packet from '+ipAddr2string(a)+' '+BStr(i)+'...');
  exit;
  end;
if not TCPcompareAddress(a,peerAddr) then goto f1;
if (o<sizeof(msgS)-sizeof(msgS.dat)) then begin;
  WriteLn('got too small packet!');
  exit;
  end;
if (ReadLongMSB(msgS.ver)<>4) then begin;
  WriteLn('got invalid version!');
  exit;
  end;
if (ReadLongMSB(msgS.typ)<>8) then begin;
  WriteLn('got invalid type!');
  exit;
  end;
p:=ReadLongMSB(msgS.num);
if (o<sizeof(msgS)+(p-1)*sizeof(msgS.dat)) then begin;
  WriteLn('got truncated packet!');
  exit;
  end;
lastMsgI:=ReadLongMSB(msgS.mid);
lastIsee:=currentTime;
cacheNum:=0;
for o:=1 to p do begin;
  a:=IPv4addressPrefix;
  move(msgS.dat[o].adr,ab[ab0+1],sizeof(i));
  inc(cacheNum);
  cacheDat[cacheNum]:=msgS.dat[o].adr;
  if not TCPcompareAddress(ab[1],loclAddr) then continue;
  dec(cacheNum);
  lastHshV:=ReadLongMSB(msgS.dat[o].rev);
  move(msgS.dat[o].hsh,lastHash,sizeof(lastHash));
  lastHshS:=ReadLongMSB(msgS.dat[o].hsi);
  end;
if (cacheNum>=p) then begin;
  WriteLn('myself not found on list of caches!');
  exit;
  end;
if (getTimePast(lastAsgn)<120) then exit;
lastAsgn:=currentTime;
WriteLongMSB(msgA.typ,9);
WriteLongMSB(msgA.mid,lastMsgI);
WriteLongMSB(msgA.num,cacheNum+1);
move(loclAddr,a[1],sizeof(loclAddr));
ab0:=sizeof(loclAddr);
move(ab[length(IPv4addressPrefix)+1],msgA.dat,sizeof(msgA.dat));
o:=sizeof(msgA);
i:=cacheNum*sizeof(i);
move(cacheDat,buf[o+1],i);
inc(o,i);
fillchar(buf[o+1],$100,0);
inc(o,$100);
UDPsendPacket(lowerPipe,peerAddr,peerPort,buf,o);
End;




Label f1;
Var
  i,o:LongInt;
  a:String;
BEGIN;
WriteLn('wccp1 client v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');

if (paramCount<>4) then immErr('using: wccp.code <gre> <addr> <port> <resend>');

if string2ipAddr(paramStr(2),peerAddr) then immErr('bad address!');
peerPort:=BVal(paramStr(3));
if (peerPort=0) then peerPort:=2048;
peerEcho:=(BVal(paramStr(4))<>0);

loclPort:=peerPort;
if UDPlistenOnPort(lowerPipe,65536,loclAddr,loclPort) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(loclAddr)+' '+BStr(loclPort)+'...');
WriteLn('will send to '+ipAddr2string(peerAddr)+' '+BStr(peerPort)+'...');
if peerEcho then a:='will' else a:='won''t';
WriteLn(a+' echo back packets.');

openDevProc(ParamStr(1));
pipeLineBegListen;
timer2start;
upperPipe:=0;
lastHere:=-999999;
lastIsee:=currentTime;
lastAsgn:=-999999;
fillchar(lastHash,sizeof(lastHash),0);
lastHshS:=$80000000;
lastHshV:=0;
lastMsgI:=0;

f1:
relequish;
timer2start;
releq2control;
releq2upper;
releq2lower;
goto f1;
END.