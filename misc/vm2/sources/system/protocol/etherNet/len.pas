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
  deviceName:String;
  ioBase:LongInt;
  memBase:LongInt;


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



Label f1;
Var
  a:String;
  etherPipe:LongInt;
  upperPipe:LongInt;
  i,o,p,q:LongInt;
  buf:array[1..1024*8] of byte;
BEGIN;
WriteLn('len v1.0, done by Mc at '#%date' '#%time'.');

a:=ParamStr(1);
if (a='') then immErr('using: len.code <process>');
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
WriteLn('io base: '+conv2hex(ioBase));
WriteLn('mem base: '+conv2hex(memBase));
WriteLn('station address: '+convAddr(localAddr));
WriteLn('broadcast address: '+convAddr(broadAddr));
WriteLn('device name: "'+deviceName+'"');

pipeLineBegListen;
BugOS_SignDaemoning;

f1:
o:=sizeof(buf);
if (pipeLineRecv(etherPipe,buf,o)=0) then begin;
  p:=readWordMSB(buf[addrSiz+1]);
  if (p>o-addrSiz-2) then begin;
    WriteLn('got invalid length ('+BStr(p)+') from '+convAddr(buf));
    goto f1;
    end;
  move(buf[addrSiz+3],buf[addrSiz+1],p);
  pipeLineSend(upperPipe,buf,p+addrSiz);
  goto f1;
  end;
relequish;
if (upperPipe=0) then begin;
  if (pipeLineGetIncoming(p)<>0) then goto f1;
  pipeLineStats(p,q,i,i);
  BugOS_ProcessName(q,buf,i,i,o);
  if (o and $40=0) then begin; pipeLineClose(p);goto f1; end;
  upperPipe:=p;
  move(addrSiz,buf[1],sizeof(addrSiz));
  i:=packSiz-2;
  move(i,buf[5],sizeof(i));
  move(ioBase,buf[9],sizeof(ioBase));
  move(memBase,buf[13],sizeof(memBase));
  i:=17;
  move(localAddr,buf[i],addrSiz);inc(i,addrSiz);
  move(broadAddr,buf[i],addrSiz);inc(i,addrSiz);
  a:='len on '+deviceName;
  move(a[1],buf[i],sizeof(a));
  inc(i,length(a));
  buf[i]:=0;
  pipeLineSend(upperPipe,buf,i);
  WriteLn('upper logged in!');
  goto f1;
  end;
p:=sizeof(buf);
if (pipeLineRecv(upperPipe,buf,p)<>0) then p:=0;
if (p<1) then begin;
  pipeLineStats(upperPipe,o,i,i);
  if (o<>0) then goto f1;
  pipeLineClose(upperPipe);
  upperPipe:=0;
  WriteLn('upper logged out!');
  goto f1;
  end;
move(buf[addrSiz+1],buf[addrSiz+3],p);
writeWordMSB(buf[addrSiz+1],p-addrSiz);
pipeLineSend(etherPipe,buf,p+2);
goto f1;
END.