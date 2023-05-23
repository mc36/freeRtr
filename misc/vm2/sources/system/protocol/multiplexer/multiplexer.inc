Procedure openInterface(a:String);
Var
  buf:array[1..32*1024] of byte;
  i,o:LongInt;
Begin;
WriteLn('process: '+a);
etherPipe:=BVal(a);
if (etherPipe=0) then etherPipe:=BugOS_findProcNam(a);
if (etherPipe=0) then immErr('interface not found!');
if (pipeLineCreate(etherPipe,etherPipe,65536,true)<>0) then immErr('unabled to create pipeline!');
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(etherPipe,buf,i)<>0) then i:=0;
if (i<1) then immErr('initial packet not received!');
move(buf[1],addrSize,sizeof(addrSize));
move(buf[5],packSize,sizeof(packSize));
o:=17;
Write('station address: ');
move(buf[o],localAddr,addrSize);
for i:=1 to addrSize do begin;
  write(byte2hextype(buf[o])+'-');
  inc(o);
  end;
WriteLn(#8' ');
Write('broadcast address: ');
move(buf[o],broadAddr,addrSize);
for i:=1 to addrSize do begin;
  write(byte2hextype(buf[o])+'-');
  inc(o);
  end;
WriteLn(#8' ');
deviceName:='';
while (buf[o]<>0) do begin;
  deviceName:=deviceName+chr(buf[o]);
  inc(o);
  end;
writeln('device name: "'+deviceName+'"');
End;




Procedure servLower;
Label f1;
Var
  buf:array[1..32*1024] of byte;
  i,o,p:LongInt;
Begin;
p:=sizeof(buf);
if (pipeLineRecv(etherPipe,buf,p)<>0) then p:=0;
if (p<1) then begin;
  pipeLineStats(etherPipe,o,i,i);
  if (o<>0) then exit;
  immErr('lower closed connection!');
  exit;
  end;
o:=ReadWordMSB(buf[addrSize+1]);
for i:=3 to ConnectionNum do begin;
  if (o<ConnectionDat^[i].protB) then continue;
  if (o>ConnectionDat^[i].protE) then continue;
  if (ConnectionDat^[i].pipe=0) then continue;
  pipeLineSend(ConnectionDat^[i].pipe,buf,p);
  goto f1;
  end;
WriteLn('got invalid protocol: $'+byte2hextype(o shr 8)+byte2hextype(o));
f1:
if (ConnectionDat^[1].pipe=0) then exit;
pipeLineSend(ConnectionDat^[1].pipe,buf,p);
End;




Procedure servUpper;
Var
  buf:array[1..32*1024] of byte;
  i,o,p:LongInt;
Begin;
for i:=1 to ConnectionNum do begin;
  if (ConnectionDat^[i].pipe=0) then continue;
  p:=sizeof(buf);
  if (pipeLineRecv(ConnectionDat^[i].pipe,buf,p)<>0) then p:=0;
  if (p>0) then begin;
    pipeLineSend(etherPipe,buf,p);
    if (ConnectionDat^[2].pipe=0) then continue;
    pipeLineSend(ConnectionDat^[2].pipe,buf,p);
    continue;
    end;
  pipeLineStats(ConnectionDat^[i].pipe,p,o,o);
  if (p<>0) then continue;
  pipeLineClose(ConnectionDat^[i].pipe);
  ConnectionDat^[i].pipe:=0;
  WriteLn(ConnectionDat^[i].name+' logged out!');
  end;
End;




Procedure servLogin;
Label f1,f2;
Var
  buf:array[1..32*1024] of byte;
  a:String;
  i,o,p:LongInt;

Procedure addBin(var data;siz:LongInt);
Begin;
move(data,buf[o+1],siz);
inc(o,siz);
End;

Begin;
f1:
if (pipeLineGetIncoming(p)<>0) then p:=0;
if (p=0) then exit;
pipeLineStats(p,o,i,i);
if (o=0) then goto f2;
BugOS_ProcessName(o,buf,i,i,o);
if (o and $40=0) then goto f2;
move(buf,a,sizeof(a));
o:=666;
for i:=1 to length(a) do if (a[i]='\') then o:=i;
a:=copy(a,o+1,255);
if (a='') then goto f2;
a:=kicsi(a);
o:=0;
for i:=1 to ConnectionNum do begin;
  if (ConnectionDat^[i].pipe<>0) then continue;
  if (ConnectionDat^[i].name<>a) then continue;
  ConnectionDat^[i].pipe:=p;
  inc(o);
  end;
if (o<1) then goto f2;
WriteLn(a+' logged in!');
o:=0;
addBin(addrSize,sizeof(addrSize));
addBin(packSize,sizeof(packSize));
i:=0;
addBin(i,sizeof(i));
addBin(i,sizeof(i));
addBin(localAddr,addrSize);
addBin(broadAddr,addrSize);
a:='multiplexer on ';
addBin(a[1],length(a));
addBin(deviceName[1],length(deviceName));
i:=0;
addBin(i,1);
pipeLineSend(p,buf,o);
goto f1;
f2:
pipeLineClose(p);
goto f1;
End;
