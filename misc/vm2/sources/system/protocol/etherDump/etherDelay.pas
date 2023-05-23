{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Type
  OnePacketRecord=record
    tim:LongInt;
    siz:LongInt;
    dat:array[1..$600] of byte;
    end;
  OnePortRecord=record
    timer:LongInt;
    count:LongInt;
    delay:LongInt;
    bndwd:LongInt;
    pckRx:LongInt;
    pckTx:LongInt;
    adrSiz:LongInt;
    adrDat:array[1..32] of byte;
    pckDat:array[1..1] of OnePacketRecord;
    end;
Var
  port1pipe:LongInt;
  port2pipe:LongInt;
  ConnectionDat:^array[1..1] of byte;
  port2begin:LongInt;
  packetNum:LongInt;
  packetDrp:LongInt;

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;

Function ResizeMem(n:LongInt):Boolean;
Var p:Pointer;
Begin;
ResizeMem:=True;
port2begin:=(n-1)*sizeof(OnePacketRecord)+sizeof(OnePortRecord)+1;
if (ExtendedMemoryResize(p,port2begin*2+1024)<port2begin*2) then exit;
ConnectionDat:=p^;
ResizeMem:=False;
End;


Function convEtherAddr(size:LongInt;var data):String;
Var
  buf:array[1..1] of byte absolute data;
  i:LongInt;
  a:String;
Begin;
a:='';
for i:=1 to size do a:=a+'-'+byte2hextype(buf[i]);
convEtherAddr:=copy(a,2,255);
End;

Function EthernetOpen(a:String):LongInt;
Var
  buf:array[1..4*1024] of byte;
  ethPipe,ethProc:LongInt;
  i,o,p:LongInt;
Begin;
WriteLn('  process: '+a);
ethProc:=BugOS_findProcNam(a);
if (ethProc=0) then immErr('process not found!');
if (pipeLineCreate(ethPipe,ethProc,65536,true)<>0) then immErr('unabled to create pipeline!');
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(ethPipe,buf,i)<>0) then i:=0;
if (i<1) then immErr('initial packet not received!');
move(buf[1],p,sizeof(p));
move(buf[5],i,sizeof(i));
move(buf[9],i,sizeof(i));
move(buf[13],i,sizeof(i));
o:=17;
WriteLn('  unicast: '+convEtherAddr(p,buf[o]));
inc(o,p);
WriteLn('  broadcast: '+convEtherAddr(p,buf[o]));
inc(o,p);
a:='';
while (buf[o]<>0) do begin;
  a:=a+chr(buf[o]);
  inc(o);
  end;
writeLn('  device: "'+a+'"');
EthernetOpen:=ethPipe;
End;

Procedure initOnePort(var buffer;a,b,c:String);
Var
  d:OnePortRecord absolute buffer;
  i,o:LongInt;
Begin;
fillchar(d,sizeof(d),0);
d.timer:=CurrentTime;
i:=BVal(a);
if (i<1) then i:=1;
d.bndwd:=(i*1024) div ticksPerSec;
i:=pos('.',b);
if (i<1) then i:=666;
o:=BVal(copy(b,1,i-1))*ticksPerSec;
b:=copy(b+'00',i+1,2);
i:=(BVal(b)*ticksPerSec) div 100;
d.delay:=o+i;
d.adrSiz:=0;
kicserel('.','',c);
kicserel('-','',c);
while (c<>'') do begin;
  a:=copy(c,1,2);
  c:=copy(c,3,666);
  i:=BVal('$'+a);
  if (i=0) then if (a<>'00') then continue;
  inc(d.adrSiz);
  d.adrDat[d.adrSiz]:=i;
  end;
WriteLn('  tx bandwidth: '+BStr(d.bndwd*ticksPerSec)+' bytes/sec');
i:=d.delay div ticksPerSec;
o:=((d.delay mod ticksPerSec)*100) div ticksPerSec;
WriteLn('  tx delay: '+BStr(i)+'.'+BStr(o)+' sec');
WriteLn('  tx address: '+convEtherAddr(d.adrSiz,d.adrDat));
End;



Procedure doWorkRx(pipe:LongInt;var buffer);
Label f1;
Var
  d:OnePortRecord absolute buffer;
  buf:array[1..4096] of byte;
  i,o,p:LongInt;
Begin;
f1:
p:=sizeof(buf);
pipeLineRecv(pipe,buf,p);
if (p<1) then exit;
i:=(d.pckRx mod packetNum)+1;
if (i=d.pckTx) then begin;
  inc(packetDrp);
  goto f1;
  end;
d.pckRx:=i;
d.pckDat[i].tim:=currentTime;
d.pckDat[i].siz:=p;
move(buf,d.pckDat[i].dat,p);
goto f1;
End;



Procedure doWorkTx(pipe:LongInt;var buffer);
Label f1;
Var
  d:OnePortRecord absolute buffer;
  i,o,p:LongInt;
Begin;
if (d.timer<>currentTime) then begin;
  d.timer:=currentTime;
  d.count:=0;
  end;
f1:
if (d.count>d.bndwd) then exit;
if (d.pckRx=d.pckTx) then exit;
i:=(d.pckTx mod packetNum)+1;
o:=currentTime-d.pckDat[i].tim;
if (o<0) then o:=6666666;
if (o<d.delay) then exit;
d.pckTx:=i;
inc(d.count,d.pckDat[i].siz);
move(d.adrDat,d.pckDat[i].dat,d.adrSiz);
pipeLineSend(pipe,d.pckDat[i].dat,d.pckDat[i].siz);
goto f1;
End;




Label f1;
Var
  i,o:LongInt;
  a:String;
BEGIN;
WriteLn('ethernet delay v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<3) then immErr('using: etherdelay.code <packs> <port1> <port2> <bw1> <bw2> <del1> <del2> [mac1] [mac2]');
packetNum:=BVal(paramStr(1));
if (packetNum<64) then packetNum:=64;
if ResizeMem(packetNum) then immErr('error allocating memory!');
timer2start;
WriteLn('port1:');
port1pipe:=EthernetOpen(paramStr(2));
initOnePort(ConnectionDat^[1],paramStr(4),paramStr(6),paramStr(8));
WriteLn('port2:');
port2pipe:=EthernetOpen(paramStr(3));
initOnePort(ConnectionDat^[port2begin],paramStr(5),paramStr(7),paramStr(9));

WriteLn('doing work...');
BugOS_SignDaemoning;
packetDrp:=0;

f1:
relequish;
timer2start;
doWorkRx(port2pipe,ConnectionDat^[1]);
doWorkRx(port1pipe,ConnectionDat^[port2begin]);
doWorkTx(port1pipe,ConnectionDat^[1]);
doWorkTx(port2pipe,ConnectionDat^[port2begin]);
if (packetDrp<1) then goto f1;
WriteLn(BStr(packetDrp)+' packets dropped!');
packetDrp:=0;
goto f1;
END.