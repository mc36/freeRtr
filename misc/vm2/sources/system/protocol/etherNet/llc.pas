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
  llcId:LongInt;
  snapId:LongInt;
  i,o,p,q:LongInt;
  buf:array[1..1024*8] of byte;
BEGIN;
WriteLn('llc v1.0, done by Mc at '#%date' '#%time'.');

snapId:=BVal(paramStr(2));
llcId:=BVal(paramStr(3));
if (snapId<0) then begin;
  llcId:=llcId and $ff;
  end else begin;
  snapId:=snapId and $ffffff;
  llcId:=$aa;
  end;

a:=ParamStr(1);
if (a='') then immErr('using: llc.code <process> [snap-id] [llc-id]...');
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
a:=conv2hex(snapId);
if (snapId<0) then a:='disabled';
WriteLn('snap id: '+a);
WriteLn('llc id: '+conv2hex(llcId));
upperPipe:=0;

pipeLineBegListen;
BugOS_SignDaemoning;

f1:
o:=sizeof(buf);
if (pipeLineRecv(etherPipe,buf,o)=0) then begin;
  p:=readWordMSB(buf[addrSiz+1])+addrSiz+2;
  if (p>o) then begin;
    WriteLn('got invalid length ('+BStr(p)+') from '+convAddr(buf));
    goto f1;
    end;
  i:=buf[addrSiz+3];
  if (i<>llcId) then begin;
    WriteLn('got invalid dsap ($'+byte2hextype(i)+') from '+convAddr(buf));
    goto f1;
    end;
  i:=buf[addrSiz+5];
  if (i<>$03) then begin;
    WriteLn('got invalid control ($'+byte2hextype(i)+') from '+convAddr(buf));
    goto f1;
    end;
  if (snapId<0) then begin;
    move(buf[addrSiz+6],buf[addrSiz+1],p);
    pipeLineSend(upperPipe,buf,p-5);
    goto f1;
    end;
  i:=readLongMSB(buf[addrSiz+5]) and $ffffff;
  if (i<>snapId) then begin;
    WriteLn('got invalid snap ($'+conv2hex(i)+') from '+convAddr(buf));
    goto f1;
    end;
  move(buf[addrSiz+9],buf[addrSiz+1],p);
  pipeLineSend(upperPipe,buf,p-8);
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
  if (snapId<0) then i:=packSiz-5 else i:=packSiz-8;
  move(i,buf[5],sizeof(i));
  move(ioBase,buf[9],sizeof(ioBase));
  move(memBase,buf[13],sizeof(memBase));
  i:=17;
  move(localAddr,buf[i],addrSiz);inc(i,addrSiz);
  move(broadAddr,buf[i],addrSiz);inc(i,addrSiz);
  a:='llc on '+deviceName;
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
if (snapId<0) then begin;
  move(buf[addrSiz+1],buf[addrSiz+6],p);
  inc(p,5);
  end else begin;
  move(buf[addrSiz+1],buf[addrSiz+9],p);
  writeLongMSB(buf[addrSiz+5],snapId);
  inc(p,8);
  end;
buf[addrSiz+3]:=llcId;
buf[addrSiz+4]:=llcId;
buf[addrSiz+5]:=$03;
writeWordMSB(buf[addrSiz+1],p-addrSiz-2);
pipeLineSend(etherPipe,buf,p);
goto f1;
END.