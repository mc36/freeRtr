{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$sysinc filesys.inc}
{$sysinc hex.inc}
{$sysinc crypto.inc}
{$sysinc random.inc}
{$include \sources\system\login\login.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Type
  OneConnectionRecord=record
    mac:array[1..32] of byte;
    chl:String[47];
    num:LongInt;
    sta:LongInt;        {1-id, 2-chal, 3-success, 4-fail, 100-access}
    tim:LongInt;
    try:LongInt;
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  addrSiz:LongInt;
  packSiz:LongInt;
  localAddr:array[1..32] of byte;
  broadAddr:array[1..32] of byte;
  myAddr:array[1..32] of byte;
  deviceName:String;
  ioBase:LongInt;
  memBase:LongInt;
  md5algoNum:LongInt;
  pipe,upp:LongInt;
  unicast:Boolean;


Function CheckOneUser(usr:String;var pwd:String):Boolean;
Label err;
Var
  d:OneLoginUserDataRecord;
  f:xFile;
  i,o:LongInt;
Begin;
CheckOneUser:=True;
usr:=kicsi(usr);
fillchar(f,sizeof(f),0);
if (xOpen(f,LoginDatabaseFilename,xGenFilMod_r)<>0) then goto err;
o:=xFileSize(f) div sizeof(d);
for i:=1 to o do begin;
  if (xBlockRead(f,d,sizeof(d))<>0) then goto err;
  if (kicsi(d.userName)<>usr) then continue;
  xClose(f);
  if (d.flags and LoginFlags_accessEth=0) then exit;
  pwd:=d.password;
  CheckOneUser:=False;
  exit;
  end;
err:
fillchar(d,sizeof(d),0);
xClose(f);
End;

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;

Function ResizeMem(n:LongInt):Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
ResizeMem:=True;
i:=n*sizeof(OneConnectionRecord);
if (ExtendedMemoryResize(p,i)<i) then exit;
ConnectionNum:=n;
ConnectionDat:=p^;
ResizeMem:=False;
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

Function compareAddrs(var addr1,addr2):Boolean;
Var
  a1:array[1..1] of byte absolute addr1;
  a2:array[1..1] of byte absolute addr2;
  i:LongInt;
Begin;
compareAddrs:=False;
for i:=addrSiz downto 1 do if (a1[i]<>a2[i]) then exit;
compareAddrs:=True;
End;

Function FindOneAddr(var addr):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do if compareAddrs(addr,ConnectionDat^[i].mac) then goto f1;
i:=0;
f1:
FindOneAddr:=i;
End;



Function relequish2conn(var con:OneConnectionRecord):Boolean;
Label f1;

Procedure sendEAPOLmsg(hdr,dat:String);
Var
  buf:array[1..512] of byte;
  i:LongInt;
Begin;
hdr:=#$88#$8e#$01+hdr;
i:=addrSiz;
if unicast then move(con.mac,buf,addrSiz) else move(myAddr,buf,addrSiz);
move(hdr[1],buf[i+1],sizeof(hdr));
inc(i,length(hdr));
WriteWordMSB(buf[i+1],length(dat));
inc(i,2);
move(dat[1],buf[i+1],sizeof(dat));
inc(i,length(dat));
pipeLineSend(pipe,buf,i);
End;

Var
  a:String;
  i:LongInt;
  ab:array[0..128] of byte absolute a;
  ab0:byte absolute a;
Begin;
relequish2conn:=False;
if (con.sta=100) then exit;
if (getTimePast(con.tim)<30) then exit;
con.tim:=currentTime;
inc(con.try);
if (con.try>5) then goto f1;
con.num:=random($100);
case con.sta of
  1:begin;
    WriteLn('sending identity request to '+convAddr(con.mac));
    sendEAPOLmsg(#0,#1+chr(con.num)+#0#5#1);
    end;
  2:begin;
    WriteLn('sending md5challenge request to '+convAddr(con.mac));
    ab0:=32;
    for i:=1 to ab0 do ab[i]:=Random($100);
    con.chl:=a;
    a:=#4+chr(ab0)+a;
    sendEAPOLmsg(#0,#1+chr(con.num)+#0+chr(ab0+4)+a);
    end;
  3:begin;
    WriteLn('sending success to '+convAddr(con.mac));
    sendEAPOLmsg(#0,#3+chr(con.num)+#0#4);
    con.sta:=100;
    end;
  4:begin;
    WriteLn('sending failure to '+convAddr(con.mac));
    sendEAPOLmsg(#0,#4+chr(con.num)+#0#4);
    con.sta:=2;
    end;
  100:;
  else begin;
    f1:
    relequish2conn:=True;
    end;
  end;
End;



Label f1,f2,f3,f4;
Var
  a,b,c:String;
  i,o,p:LongInt;
  con:OneConnectionRecord;
  buf:array[1..1024*8] of byte;
  ab:array[0..128] of byte absolute a;
  ab0:byte absolute a;
BEGIN;
WriteLn('eapol server v1.0, done by Mc at '#%date' '#%time'.');

if CryptoStartActions then immErr('failed to find crypto process!');
CryptoGetHasherList(buf,i);
md5algoNum:=CryptoFindOneAlgo(buf,i,'md5');
if (md5algoNum<1) then immErr('failed to find md5 algorithm!');
Randomize;

a:=ParamStr(1);
if (a='') then immErr('using: eapol.code <process> [1=unicast/0=multicast]');
WriteLn('process: '+a);
o:=BugOS_findProcNam(a);
if (o=0) then immErr('process not found!');
WriteLn('process#: '+BStr(o));
i:=pipeLineCreate(pipe,o,65536,true);
if (i<>0) then immErr('unabled to create pipeline!');
WriteLn('pipeline#: '+BStr(pipe));
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(pipe,buf,i)<>0) then i:=0;
if (i<1) then immErr('initial packet not received!');
move(buf[1],addrSiz,sizeof(addrSiz));
move(buf[5],packSiz,sizeof(packSiz));
move(buf[9],ioBase,sizeof(ioBase));
move(buf[13],memBase,sizeof(memBase));
o:=17;
move(buf[o],localAddr,sizeof(localAddr));inc(o,addrSiz);
move(buf[o],broadAddr,sizeof(broadAddr));inc(o,addrSiz);
deviceName:='eapol on ';
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
unicast:=(BVal(paramStr(2))=1);

myAddr[1]:=$01;
myAddr[2]:=$80;
myAddr[3]:=$c2;
myAddr[4]:=$00;
myAddr[5]:=$00;
myAddr[6]:=$03;
upp:=0;
ResizeMem(0);
pipeLineBegListen;
BugOS_SignDaemoning;

f1:
relequish;
timer2start;
for i:=ConnectionNum downto 1 do if relequish2conn(ConnectionDat^[i]) then begin;
  ConnectionDat^[i]:=ConnectionDat^[ConnectionNum];
  ResizeMem(ConnectionNum-1);
  end;
if (upp<>0) then goto f3;
if (pipeLineGetIncoming(p)<>0) then goto f2;
pipeLineStats(p,o,i,i);
BugOS_ProcessName(o,buf,i,i,o);
if (o and $40=0) then begin; pipeLineClose(p);goto f2; end;
upp:=p;
move(addrSiz,buf[1],sizeof(addrSiz));
move(packSiz,buf[5],sizeof(packSiz));
move(ioBase,buf[9],sizeof(ioBase));
move(memBase,buf[13],sizeof(memBase));
i:=17;
move(localAddr,buf[i],addrSiz);inc(i,addrSiz);
move(broadAddr,buf[i],addrSiz);inc(i,addrSiz);
move(deviceName[1],buf[i],sizeof(deviceName));
inc(i,length(deviceName));
buf[i]:=0;
pipeLineSend(upp,buf,i);
WriteLn('upper logged in!');
goto f2;
f3:
i:=sizeof(buf);
if (pipeLineRecv(upp,buf,i)<>0) then i:=0;
if (i>0) then begin;
  pipeLineSend(pipe,buf,i);
  goto f3;
  end;
pipeLineStats(pipe,p,o,i);
if (p<>0) then goto f2;
pipeLineClose(upp);
upp:=0;
WriteLn('upper logged out!');
f2:
i:=sizeof(buf);
if (pipeLineRecv(pipe,buf,i)<>0) then i:=0;
if (i<1) then goto f1;
if (ReadWordMSB(buf[addrSiz+1])<>$888e) then begin;
  o:=FindOneAddr(buf);
  if (o<>0) then begin;
    if (ConnectionDat^[o].sta<>3) then goto f2;
    pipeLineSend(upp,buf,i);
    goto f2;
    end;
  if ResizeMem(ConnectionNum+1) then goto f2;
  fillchar(con,sizeof(con),0);
  con.sta:=1;
  con.tim:=-99999;
  move(buf,con.mac,addrSiz);
  ConnectionDat^[ConnectionNum]:=con;
  goto f2;
  end;
i:=buf[addrSiz+3];
if (i<>1) then begin;
  WriteLn('got unknown version ('+BStr(i)+') packet from '+convAddr(buf));
  goto f2;
  end;
i:=buf[addrSiz+4];
case i of
  0:;
  2:begin;
    WriteLn('got logout from '+convAddr(buf));
    o:=FindOneAddr(buf);
    if (o=0) then goto f2;
    ConnectionDat^[o]:=ConnectionDat^[ConnectionNum];
    ResizeMem(ConnectionNum-1);
    goto f2;
    end;
  1:begin;
    WriteLn('got login from '+convAddr(buf));
    o:=FindOneAddr(buf);
    if (o=0) then begin;
      if ResizeMem(ConnectionNum+1) then goto f2;
      o:=ConnectionNum;
      end;
    fillchar(con,sizeof(con),0);
    con.sta:=1;
    con.tim:=-99999;
    move(buf,con.mac,addrSiz);
    ConnectionDat^[o]:=con;
    goto f2;
    end;
  else begin;
    WriteLn('got unknown eapol type ('+BStr(i)+') from '+convAddr(buf));
    goto f2;
    end;
  end;
WriteLn('got eap from '+convAddr(buf));
i:=ReadWordMSB(buf[addrSiz+5]);
if (i>255) then i:=255;
ab0:=i;
move(buf[addrSiz+7],ab[1],ab0);
i:=ReadWordMSB(ab[3]);
if (i>255) then i:=255;
ab0:=i;
case ab[1] of
  1:begin;
    WriteLn('  request');
    goto f2;
    end;
  2:;
  3:begin;
    WriteLn('  success');
    goto f2;
    end;
  4:begin;
    WriteLn('  failure');
    goto f2;
    end;
  else begin;
    WriteLn('  unknown: '+BStr(ab[1]));
    goto f2;
    end;
  end;
p:=FindOneAddr(buf);
if (p=0) then begin;
  if ResizeMem(ConnectionNum+1) then goto f2;
  p:=ConnectionNum;
  fillchar(con,sizeof(con),0);
  con.sta:=1;
  con.tim:=-99999;
  move(buf,con.mac,addrSiz);
  ConnectionDat^[p]:=con;
  end else con:=ConnectionDat^[p];
con.tim:=-99999;
case ab[5] of
  1:begin;
    WriteLn('  identity: '+copy(a,6,666));
    con.sta:=2;
    goto f4;
    end;
  2:begin;
    WriteLn('  notify: '+copy(a,6,666));
    goto f2;
    end;
  3:begin;
    WriteLn('  nak');
    goto f2;
    end;
  4:;
  else begin;
    WriteLn('  unknown: '+BStr(ab[5]));
    goto f2;
    end;
  end;
WriteLn('  md5challenge');
if (ab[2]<>con.num) then begin;
  WriteLn('got unknown id!');
  goto f2;
  end;
move(ab[6],b,sizeof(b));
a:=copy(a,length(b)+7,666);
con.sta:=4;
if CheckOneUser(a,c) then goto f4;
a:=chr(con.num)+c+con.chl;
i:=ab0;
CryptoImmHasher(md5algoNum,'','',ab[1],i);
ab0:=i;
if (a=b) then con.sta:=3;
f4:
ConnectionDat^[p]:=con;
goto f2;
END.