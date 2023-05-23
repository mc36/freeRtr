{{$define debug}
{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc param.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}
{$include cstp.inc}

Const proggyName='cstp client v1.0';
Var
  lowerPipe:LongInt;
  upperPipe:LongInt;
  hostName:String;
  prAdr:OneTCPaddressRecord;
  prPrt:LongInt;
  gotAdr:String;
  bufSize:LongInt;


Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;



Procedure startOneConnect(url,usr,pwd:String);
Label f1;
Var
  hst,loc,ctx,ses,adr:String;
  i,o,p:LongInt;
  a,b:String;

Function xLevesz(a:String):String;
Begin;
a:=' '+a+' ';
kicserel('  ',' ',a);
xLevesz:=copy(a,2,length(a)-2);
End;

Function getChar:LongInt;
Label f1;
Var
  b:Byte;
  i,o:LongInt;
Begin;
getChar:=-1;
f1:
i:=sizeof(b);
pipeLineRecv(lowerPipe,b,i);
if (i=sizeof(b)) then begin; getChar:=b;exit; end;
pipeLineStats(lowerPipe,o,i,i);
if (o=0) then immErr('remote closed connection!');
relequish;
goto f1;
End;

Function getLine:String;
Label f1,f2;
Var
  a:STring;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
getLine:='';
ab0:=0;
f1:
i:=getChar;
if (i=13) then goto f1;
if (i=10) then goto f2;
inc(ab0);
ab[ab0]:=i;
goto f1;
f2:
getLine:=a;
{$ifdef debug}writeln('<-- "'+a+'"');{$endif}
End;

Procedure putLine(a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
{$ifdef debug}writeln('--> "'+a+'"');{$endif}
pipeLineSend(lowerPipe,ab[1],ab0);
a:=#13#10;
pipeLineSend(lowerPipe,ab[1],ab0);
End;


Function getReply:LongInt;
Label f1,f2;
Var
  i,o,p:LongInt;
  a,b:String;
Begin;
a:=getLine;
i:=pos(' ',a);
a:=copy(a,i+1,666);
getReply:=bval(copy(a,1,pos(' ',a)-1));
p:=0;
loc:='';
ctx:='';
ses:='';
adr:='';
f1:
b:=getLine;
if (b='') then goto f2;
i:=pos(':',b);
a:=kicsi(xlevesz(copy(b,1,i-1)));
b:=xlevesz(copy(b,i+1,666));
if (a='location') then begin;
  i:=pos('://',b);
  if (i<1) then begin; loc:=b;goto f1; end;
  b:=copy(b,i+3,666);
  i:=pos('/',b);
  loc:=copy(b,i,666);
  goto f1;
  end;
if (a='content-length') then begin;
  p:=BVal(b);
  goto f1;
  end;
if (a='set-cookie') then begin;
  i:=pos(';',b);
  if (i<1) then i:=666;
  b:=copy(b,1,i-1);
  i:=pos('=',b);
  a:=kicsi(copy(b,1,i-1));
  b:=copy(b,i+1,666);
  if (a='webvpn') then ses:=b;
  if (a='webvpncontext') then ctx:=b;
  end;
if (a='x-cstp-address') then begin;
  adr:=b;
  goto f1;
  end;
goto f1;
f2:
while (p>0) do begin;
  getChar;
  dec(p);
  end;
End;

Begin;
i:=pos('://',url);
if (i<1) then i:=-666;
a:=kicsi(copy(url,1,i-1));
url:=copy(url,i+3,666);
p:=443;
if (a='http') then p:=80;
if (a='https') then p:=443;
if (copy(url,1,1)='[') then begin;
  i:=pos(']',url);
  b:=copy(url,2,i-2);
  url:=copy(url,i+1,666);
  end else b:='';
i:=pos('/',url);
if (i<1) then i:=666;
a:=copy(url,i,666);
url:=copy(url,1,i-1);
i:=pos(':',url);
if (i<1) then i:=666;
o:=bval(copy(url,i+1,666));
if (o<>0) then p:=o;
hst:=b+copy(url,1,i-1);
if string2ipAddr(hst,b) then immErr('bad ip address!');
url:=a;
move(b,prAdr,sizeof(prAdr));
prPrt:=p;
Write('connecting '+ipAddr2string(b)+' '+BStr(p)+'...');
TCPbeginConnect(lowerPipe,65536,b,p);
while TCPlookConnected(lowerPipe,a,i,o) do begin;
  relequish;
  if (lowerPipe<>0) then continue;
  immErr(' failed!');
  end;
WriteLn(' ok!');
hostName:=ipAddr2string(a);
WriteLn('local side is '+hostName+' '+BStr(i)+'...');
WriteLn('waiting to get secured...');
repeat
  a:=getLine;
  until (a='secure');
WriteLn('getting context id...');
f1:
putLine('GET '+url+' HTTP/1.1');
putLine('Host: '+hst);
putLine('User-Agent: '+proggyName);
putLine('Accept: */*');
putLine('Accept-Language: en-us,en;q=0.5');
putLine('Accept-Encoding: deflate');
putLine('Accept-Charset: *');
putLine('Keep-Alive: 300');
putLine('Connection: Keep-Alive');
putLine('');
getReply;
if (loc<>'') then begin; url:=loc;goto f1; end;
if (ctx='') then immErr('error getting context id!');
WriteLn('getting session id...');
a:='username='+usr+'&password='+pwd+'&Login=Login&next=';
putLine('POST '+url+' HTTP/1.1');
putLine('Host: '+hst);
putLine('User-Agent: '+proggyName);
putLine('Accept: */*');
putLine('Accept-Language: en-us,en;q=0.5');
putLine('Accept-Encoding: deflate');
putLine('Accept-Charset: *');
putLine('Keep-Alive: 300');
putLine('Connection: keep-alive');
putLine('Cookie: webvpncontext='+ctx);
putLine('Content-Type: application/x-www-form-urlencoded');
putLine('Content-Length: '+BStr(length(a)+2));
putLine('');
putLine(a);
getReply;
if (ses='') then immErr('error getting session id!');
WriteLn('getting ip address...');
putLine('CONNECT /CSCOSSLC/tunnel HTTP/1.1');
putLine('Host: '+hst);
putLine('User-Agent: '+proggyName);
putLine('X-CSTP-Version: 1');
putLine('X-CSTP-Hostname: '+hostName);
putLine('X-CSTP-Accept-Encoding: deflate');
putLine('Cookie: webvpn='+ses);
putLine('');
getReply;
if (adr='') then immErr('error getting ip address!');
WriteLn('address='+adr);
gotAdr:=adr;
End;



Procedure WaitForUpperLayer;
Var
  i,o,p:LongInt;
  a:String;
Begin;
Write('waiting for upper level...');
BugOS_SignDaemoning;
pipeLineBegListen;
while (pipeLineGetIncoming(upperPipe)<>0) do relequish;
pipeLineEndListen;
a:='cstp with '+ipAddr2string(prAdr)+' '+BStr(prPrt);
BugOS_MyProcessInfo(i,o,p);
i:=i xor p xor o;
p:=((i shr 24) xor (i shr 16) xor (i shr 8) xor i) and $ff;
a:='12341234'#0#0#0#0#0#0#0#0+chr(p)+#255+a+#0;
i:=1;move(i,a[1],sizeof(i));
i:=1400;move(i,a[5],sizeof(i));
pipeLineSend(upperPipe,a[1],length(a));
WriteLn(' done!');
End;


Procedure releq2upper;
Var
  buf:array[1..4096] of byte;
  hdr:onePacketHeader absolute buf;
  i,o:LongInt;
Begin;
o:=sizeof(buf);
pipeLineRecv(upperPipe,buf,o);
if (o<1) then begin;
  pipeLineStats(upperPipe,o,i,i);
  if (o=0) then immErr('upper closed connection!');
  exit;
  end;
move(buf,buf[sizeof(hdr)],o);
dec(o);
WriteLongMSB(hdr.id,packetMagicCookie);
WriteWordMSB(hdr.len,o);
hdr.typ:=0;
pipeLineSend(lowerPipe,buf,o+sizeof(hdr));
End;


Procedure releq2lower;
Var
  buf:array[1..4096] of byte;
  hdr:onePacketHeader absolute buf;
  i,o,p:LongInt;
Begin;
pipeLineStats(lowerPipe,p,i,o);
if (p=0) then immErr('lower closed connection!');
if (bufSize>0) then begin;
  if (i<bufSize) then exit;
  o:=bufSize;
  pipeLineRecv(lowerPipe,buf[2],o);
  if (o<>bufSize) then immErr('bad number of bytes received!');
  buf[1]:=$11;
  pipeLineSend(upperPipe,buf,o+1);
  bufSize:=0;
  exit;
  end;
if (i<sizeof(hdr)) then exit;
o:=sizeof(hdr);
pipeLineRecv(lowerPipe,buf,o);
if (o<>sizeof(hdr)) then immErr('bad number of bytes received!');
if (ReadLongMSB(hdr.id)<>packetMagicCookie) then immErr('got bad cookie!');
bufSize:=ReadWordMSB(hdr.len);
End;


Label f1;
BEGIN;
WriteLn(proggyName+', done by Mc at '#%date' '#%time'.');
if TLSfindProcess then immErr('failed to find tls process!');
if (paramCount<3) then immErr('using: cstp.code <url> <user> <pass>');

startOneConnect(paramStr(1),paramStr(2),paramStr(3));
WaitForUpperLayer;
bufSize:=0;

f1:
relequish;
releq2lower;
releq2upper;
goto f1;
END.