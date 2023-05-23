{$heap 15k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Type
  OneCiscoHdlcPacketRecord=record
    adr:Word;
    typ:Word;
    cmd:longInt;
    sndSeq:LongInt;
    rcvSeq:LongInt;
    rel:Word;
    tim:LongInt;
    end;
Var
  devPipe:LongInt;
  uppPipe:LongInt;
  lastSent:LongInt;
  lastRecv:LongInt;
  seqSent:LongInt;
  seqRecv:LongInt;
  seqTime:LongInt;
  myName:String;



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
myName:='ciscoHDLC on '+a;
End;



Procedure doKeepalive;
Var
  dat:OneCiscoHdlcPacketRecord;
Begin;
if (getTimePast(lastRecv)>16*60) then immErr('remote possibly dead!');
if (getTimePast(lastSent)<10) then exit;
WriteLn('sending keepalive; mySeq='+BStr(seqSent)+' sawSeq='+BStr(seqRecv));
seqTime:=currentTime*(1000 div ticksPerSec);
WriteWordMSB(dat.adr,$8f00);
WriteWordMSB(dat.typ,$8035);
WriteLongMSB(dat.cmd,2);
WriteLongMSB(dat.sndSeq,seqSent);
WriteLongMSB(dat.rcvSeq,seqRecv);
WriteWordMSB(dat.rel,$ffff);
WriteLongMSB(dat.tim,seqTime);
pipeLineSend(devPipe,dat,sizeof(dat));
inc(seqSent);
lastSent:=currentTime;
End;



Function doLower:Boolean;
Var
  buf:array[1..4*1024] of byte;
  dat:OneCiscoHdlcPacketRecord absolute buf;
  i,o,p:LongInt;
Begin;
doLower:=False;
p:=sizeof(buf);
if (pipeLineRecv(devPipe,buf,p)<>0) then p:=0;
if (p<1) then begin;
  pipeLineStats(devPipe,p,i,o);
  if (p=0) then immErr('lower level closed connection!');
  exit;
  end;
doLower:=True;
if (ReadWordMSB(dat.typ)=$8035) then begin;
  if (ReadLongMSB(dat.cmd)<>2) then exit;
  seqRecv:=ReadLongMSB(dat.sndSeq);
  i:=ReadLongMSB(dat.rcvSeq);
  if (ReadLongMSB(dat.tim)=seqTime) then exit;
  lastRecv:=currentTime;
  WriteLn('got keepalive; peerSeq='+BStr(seqRecv)+' sawSeq='+BStr(i));
  exit;
  end;
pipeLineSend(uppPipe,buf,p);
End;



Function doUpper:Boolean;
Var
  buf:array[1..4*1024] of byte;
  dat:OneCiscoHdlcPacketRecord absolute buf;
  i,o,p:LongInt;
Begin;
doUpper:=False;
p:=sizeof(buf);
if (pipeLineRecv(uppPipe,buf,p)<>0) then p:=0;
if (p<1) then begin;
  pipeLineStats(uppPipe,o,i,i);
  if (o=0) then immErr('upper level closed connection!');
  exit;
  end;
WriteWordMSB(dat.adr,$f00);
pipeLineSend(devPipe,buf,p);
doUpper:=True;
End;




Procedure WaitForUpperLayer;
Var
  i,o,p:LongInt;
  a:String;
Begin;
Write('waiting for upper level...');
BugOS_SignDaemoning;
pipeLineBegListen;
while (pipeLineGetIncoming(uppPipe)<>0) do relequish;
pipeLineEndListen;
BugOS_MyProcessInfo(i,o,p);
i:=i xor p xor o;
p:=((i shr 16) xor i) and $ffff;
a:='12341234'#0#0#0#0#0#0#0#0+chr(p shr 8)+chr(p)+#255#255+myName+#0;
i:=2;move(i,a[1],sizeof(i));
i:=1500;move(i,a[5],sizeof(i));
pipeLineSend(uppPipe,a[1],length(a));
WriteLn(' done!');
End;



Label f1;
BEGIN;
WriteLn('ciscoHdlc v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<>1) then immErr('using: ciscoHdlc.code <process>');

seqSent:=0;
seqRecv:=0;
timer2start;
lastSent:=0;
lastRecv:=currentTime;
seqTime:=0;
uppPipe:=0;
openDevice(ParamStr(1));
WaitForUpperLayer;

f1:
relequish;
timer2start;
doKeepalive;
while doLower do;
while doUpper do;
goto f1;
END.