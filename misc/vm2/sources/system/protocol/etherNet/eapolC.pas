{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$sysinc hex.inc}
{$sysinc crypto.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Var
  addrSiz:LongInt;
  packSiz:LongInt;
  localAddr:array[1..32] of byte;
  broadAddr:array[1..32] of byte;
  deviceName:String;
  ioBase:LongInt;
  memBase:LongInt;
  md5algoNum:LongInt;
  userName:String;
  passWord:String;
  unicast:Boolean;


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


Function putEAPOLmsg(var buffer;hdr,dat:String):LongInt;
Var
  buf:array[1..1] of byte absolute buffer;
  i:LongInt;
Begin;
hdr:=#$88#$8e#$01+hdr;
i:=addrSiz;
move(broadAddr,buf,addrSiz);
move(hdr[1],buf[i+1],sizeof(hdr));
inc(i,length(hdr));
WriteWordMSB(buf[i+1],length(dat));
inc(i,2);
move(dat[1],buf[i+1],sizeof(dat));
inc(i,length(dat));
putEAPOLmsg:=i;
End;




Label f1;
Var
  a:String;
  ab0:byte absolute a;
  ab:array[0..1] of byte absolute a;
  pipe,time,retry:LongInt;
  i,o,p:LongInt;
  buf:array[1..1024*8] of byte;
BEGIN;
WriteLn('eapol client v1.0, done by Mc at '#%date' '#%time'.');

if CryptoStartActions then immErr('failed to find crypto process!');
CryptoGetHasherList(buf,i);
md5algoNum:=CryptoFindOneAlgo(buf,i,'md5');
if (md5algoNum<1) then immErr('failed to find md5 algorithm!');

userName:=paramStr(2);
passWord:=paramStr(3);
if (paramCount<1) then immErr('using: eapol.code <process> [[1=uni/0=multi]<user> <password>]');

a:=ParamStr(1);
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
unicast:=false;
a:=copy(userName,1,1);
if (a>='0') and (a<='1') then begin;
  userName:=copy(userName,2,666);
  unicast:=(BVal(a)=1);
  end;

timer2start;
time:=-99999;
retry:=0;
broadAddr[1]:=$01;
broadAddr[2]:=$80;
broadAddr[3]:=$c2;
broadAddr[4]:=$00;
broadAddr[5]:=$00;
broadAddr[6]:=$03;

f1:
relequish;
timer2start;
if (retry>16) then immErr('retry counter expired!');
i:=sizeof(buf);
if (pipeLineRecv(pipe,buf,i)<>0) then i:=0;
if (i<1) then begin;
  if (getTimePast(time)<45) then goto f1;
  time:=currentTime;
  WriteLn('sending notification to '+convAddr(broadAddr));
  inc(retry);
  i:=putEAPOLmsg(buf,#2,'');
  pipeLineSend(pipe,buf,i);
  if (userName='') then goto f1;
  i:=putEAPOLmsg(buf,#1,'');
  pipeLineSend(pipe,buf,i);
  goto f1;
  end;
i:=ReadWordMSB(buf[addrSiz+1]);
if (i<>$888e) then begin;
  WriteLn('got unknown type (0x'+byte2hextype(i shr 8)+byte2hextype(i)+') packet from '+convAddr(buf));
  goto f1;
  end;
retry:=0;
i:=buf[addrSiz+3];
if (i<>1) then begin;
  WriteLn('got unknown version ('+BStr(i)+') packet from '+convAddr(buf));
  goto f1;
  end;
i:=buf[addrSiz+4];
if (i<>0) then begin;
  WriteLn('got unknown eapol type ('+BStr(i)+') from '+convAddr(buf));
  goto f1;
  end;
WriteLn('got packet from '+convAddr(buf));
if unicast then move(buf,broadAddr,sizeof(broadAddr));
if (userName='') then halt(0);
i:=ReadWordMSB(buf[addrSiz+5]);
if (i>255) then i:=255;
ab0:=i;
move(buf[addrSiz+7],ab[1],ab0);

i:=ReadWordMSB(ab[3]);
if (i>255) then i:=255;
ab0:=i;
case ab[1] of
  1:;
  2:begin;
    WriteLn('  response');
    goto f1;
    end;
  3:begin;
    WriteLn('  success');
    halt(0);
    end;
  4:begin;
    WriteLn('  failure');
    goto f1;
    end;
  else begin;
    WriteLn('  unknown: '+BStr(ab[1]));
    goto f1;
    end;
  end;

case ab[5] of
  1:begin;
    WriteLn('  identity: '+copy(a,6,666));
    i:=putEAPOLmsg(buf,#0,#2+chr(ab[2])+#0+chr(length(userName)+5)+#1+userName);
    pipeLineSend(pipe,buf,i);
    end;
  2:begin;
    WriteLn('  notify: '+copy(a,6,666));
    i:=putEAPOLmsg(buf,#0,#2+chr(ab[2])+#0#5#2);
    pipeLineSend(pipe,buf,i);
    end;
  3:begin;
    WriteLn('  nak');
    end;
  4:begin;
    WriteLn('  md5challenge');
    o:=ab[2];
    move(ab[6],a,sizeof(a));
    a:=chr(o)+passWord+a;
    i:=length(a);
    CryptoImmHasher(md5algoNum,'','',a[1],i);
    ab0:=i;
    a:=chr(ab0)+a+userName;
    i:=putEAPOLmsg(buf,#0,#2+chr(o)+#0+chr(ab0+5)+#4+a);
    pipeLineSend(pipe,buf,i);
    end;
  else begin;
    WriteLn('  unknown: '+BStr(ab[5]));
    i:=putEAPOLmsg(buf,#0,#2+chr(ab[2])+#0#6#3#4);
    pipeLineSend(pipe,buf,i);
    end;
  end;

goto f1;
END.