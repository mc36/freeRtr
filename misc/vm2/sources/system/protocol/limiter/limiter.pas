{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$sysinc hex.inc}
{$sysinc alap.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$include \sources\internet\kernel\utils\timer2.inc}


{$include memory.inc}
{$include config.inc}




Procedure clearCounts(var d:oneDescriptorRecord);
Begin;
inc(d.txC,d.txL);
if (d.txC>d.txL) then d.txC:=d.txL;
inc(d.rxC,d.rxL);
if (d.rxC>d.rxL) then d.rxC:=d.rxL;
End;



Label f1,f2;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p,q:LongInt;
  buf:array[1..1024*8] of byte;
  lastTime:LongInt;
BEGIN;
WriteLn('limiter v1.0, done by Mc at '#%date' '#%time'.');

timer2start;
if (paramCount<2) then immErr('using: limiter.code <process> <config>');
ReadUpConfig(paramStr(2));

a:=ParamStr(1);
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
move(buf[1],i,sizeof(i));
if (addrSiz<>i) then immErr('address size mismatch!');
move(buf[5],packSiz,sizeof(packSiz));
move(buf[9],ioBase,sizeof(ioBase));
move(buf[13],memBase,sizeof(memBase));
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
WriteLn(BStr(userNum)+' users read.');

pipeLineBegListen;
BugOS_SignDaemoning;

f1:
if (q=0) then relequish;
timer2start;
if (getTimePast(lastTime)<>0) then begin;
  for i:=1 to userNum do clearCounts(userDat[i]);
  lastTime:=currentTime;
  end;
while (pipeLineGetIncoming(p)=0) do begin;
  pipeLineStats(p,q,i,i);
  if (uppPipe<>0) then begin; pipeLineClose(p);break; end;
  BugOS_ProcessName(q,buf,i,i,o);
  if (o and $40=0) then begin; pipeLineClose(p);break; end;
  uppPipe:=p;
  move(addrSiz,buf[1],sizeof(addrSiz));
  i:=packSiz-4;
  move(i,buf[5],sizeof(i));
  move(ioBase,buf[9],sizeof(ioBase));
  move(memBase,buf[13],sizeof(memBase));
  i:=17;
  move(localAddr,buf[i],addrSiz);inc(i,addrSiz);
  move(broadAddr,buf[i],addrSiz);inc(i,addrSiz);
  a:='limiter on '+deviceName;
  move(a[1],buf[i],sizeof(a));
  inc(i,length(a));
  buf[i]:=0;
  pipeLineSend(uppPipe,buf,i);
  WriteLn('upper logged in!');
  end;
q:=0;
o:=sizeof(buf);
if (pipeLineRecv(devPipe,buf,o)<>0) then o:=0;
if (o>0) then begin;
  q:=1;
  ab0:=addrSiz;
  move(buf,ab[1],addrSiz);
  p:=QuickFind(1,userNum,a);
  if (p<1) then p:=1;
  if (userDat[p].rxC<0) then goto f2;
  dec(userDat[p].rxC,o);
  pipeLineSend(uppPipe,buf,o);
  end;
f2:
o:=sizeof(buf);
if (pipeLineRecv(uppPipe,buf,o)<>0) then o:=0;
if (o<1) then begin;
  if (uppPipe=0) then goto f1;
  pipeLineStats(uppPipe,o,i,i);
  if (o<>0) then goto f1;
  pipeLineClose(uppPipe);
  uppPipe:=0;
  WriteLn('upper logged out!');
  goto f1;
  end;
q:=1;
ab0:=addrSiz;
move(buf,ab[1],addrSiz);
p:=QuickFind(1,userNum,a);
if (p<1) then p:=1;
if (userDat[p].txC<0) then goto f1;
dec(userDat[p].txC,o);
pipeLineSend(devPipe,buf,o);
goto f1;
END.