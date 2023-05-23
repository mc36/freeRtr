{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc param.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$sysinc inet_dns.inc}
{$include \sources\internet\kernel\utils\timer2.inc}
{$include base64.inc}

Const proggyName='http client v1.0';

Var
  pipe:LongInt;
  connMode:byte; {1=close, 2=keepalive}
  restMode:LongInt;
  retrMode:Byte; {1=get, 2=put, 3=post}
  autoRetry:Boolean;
  proxy:String;
  remoteAddr:OneTCPaddressRecord;
  remotePort:LongInt;
  inpDat:array[1..4*1024] of byte;
  inpSiz:LongInt;
  inpPos:LongInt;
  outDat:array[1..2*1024] of byte;
  outSiz:LongInt;
  browser:String;
  authUsr:String;
  authPwd:String;
  secure:Boolean;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function GetNextWord(Var a:String):String;
Var i:Word;
Begin;
i:=pos(' ',a);
if (i<1) then i:=666;
GetNextWord:=copy(a,1,i-1);
a:=copy(a,i+1,255);
End;

Function ReadLine:String;
Label f0,f1;
Var
  a:String;
  w:Word;
Begin;
f0:
a:='';
Write(#13'http>');
f1:
w:=ReadKey;
if (w and $fe00=0) then begin;{simple key}
  w:=w and $ff;
  if (w in [0,255,13,10,8,9]) then w:=ord(' ');
  if (length(a)>250) then goto f1;
  a:=a+chr(w);
  write(chr(w));
  goto f1;
  end;
case w of
  $8001:begin;{redraw}
    clrscr;
    goto f0;
    end;
  $8003:begin;{backspace}
    if (a='') then goto f1;
    Write(#8' '#8);
    a:=copy(a,1,length(a)-1);
    goto f1;
    end;
  $8004:begin;{enter}
    WriteLn('');
    ReadLine:=a;
    exit;
    end;
  $8005:begin;{escape}
    WriteLn('');
    goto f0;
    end;
  end;
goto f1;
End;

Function xLevesz(a:String):String;
Begin;
Kicserel(#9,' ',a);
Kicserel(#255,' ',a);
Kicserel(#0,' ',a);
a:=' '+a+' ';
Kicserel('  ',' ',a);
xLevesz:=copy(a,2,length(a)-2);
End;




Procedure receive2buffer;
Label f1;
Var i,o:LongInt;
Begin;
if (inpPos<inpSiz) then exit;
inpPos:=0;
f1:
inpSiz:=sizeof(inpDat);
if (pipeLineRecv(pipe,inpDat,inpSiz)<>0) then inpSiz:=0;
if (inpSiz>0) then exit;
pipeLineStats(pipe,o,i,i);
if (o<>0) then begin;
  relequish;
  goto f1;
  end;
pipeLineClose(pipe);
pipe:=0;
End;

Function getResponse:String;
Label f1;
Var
  i:LongInt;
  a,b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
ab0:=0;
b:=a;
Write('<-- ');
f1:
inc(inpPos);
if (inpPos>inpSiz) then begin;
  receive2buffer;
  if (pipe<>0) then goto f1;
  WriteLn('');
  getResponse:='';
  exit;
  end;
i:=inpDat[inpPos];
if (i=13) then goto f1;
if (i<>10) then begin;
  inc(ab0);
  ab[ab0]:=i;
  if (ab0<255) then goto f1;
  Write(a);
  if (b='') then b:=a;
  ab0:=0;
  goto f1;
  end;
WriteLn(a);
getResponse:=b+a;
End;


Procedure sendBuffer;
Label f1,f2;
Var i,o:LongInt;
Begin;
if (outSiz<1) then goto f2;
f1:
if (pipeLineSend(pipe,outDat,outSiz)=0) then goto f2;
relequish;
pipeLineStats(pipe,o,i,i);
if (o=0) then exit;
goto f1;
f2:
outSiz:=0;
End;

Procedure addBuffer(a:string);
Begin;
move(a[1],outDat[outSiz+1],length(a));
inc(outSiz,length(a));
Write(a);
End;

Procedure BeginLine;
Begin;
Write('--> ');
End;

Procedure sendFile(var f:xFile;status:Boolean);
Label f1,f2;
Var
  i,q,o,p,s:LongInt;
  buf:array[1..4*1024] of byte;
Begin;
p:=xFilePos(f);
s:=xFileSize(f);
if status then WriteLn('going to send '+BStr(s)+' bytes...');
f1:
if status then write(#13+BStr(p)+#13);
q:=s-p;
if (q>sizeof(buf)) then q:=sizeof(buf);
if (q<1) then begin;
  if status then WriteLn(BStr(p)+' bytes sent');
  xClose(f);
  exit;
  end;
xBlockRead(f,buf,q);
f2:
if (pipeLineSend(pipe,buf,q)=0) then begin;
  inc(p,q);
  goto f1;
  end;
relequish;
pipeLineStats(pipe,o,i,i);
if (o<>0) then goto f2;
pipeLineClose(pipe);
pipe:=0;
xClose(f);
if status then WriteLn('');
End;


Function DecodeRange(var fpos,fsiz:LongInt;b:String):Boolean;
Const reqBeg='bytes ';
Var
  a:String;
  i:LongInt;
Begin;
DecodeRange:=True;
b:=kicsi(b);
if (copy(b,1,length(reqBeg))=reqBeg) then b:=copy(b,length(reqBeg)+1,255);
i:=pos('-',b);
a:=copy(b,1,i-1);
b:=copy(b,i+1,255);
i:=BVal(a);
if (BStr(i)<>a) then exit;
fpos:=i;
i:=pos('/',b);
a:=copy(b,1,i-1);
b:=copy(b,i+1,255);
i:=BVal(a);
if (BStr(i)<>a) then exit;
inc(i);
if (BStr(i)<>b) then exit;
fsiz:=i;
DecodeRange:=False;
End;



Function doConnecting(a:String):Boolean;
Label ok;
Var
  b:String;
  i,o,p:LongInt;
Begin;
doConnecting:=true;
WriteLn('going to connect to '+a+'...');
if (proxy<>'') then a:=proxy;
if (copy(a,1,1)='[') then begin;
  i:=pos(']',a);
  b:=copy(a,i+1,255);
  a:=copy(a,2,i-2);
  end else begin;
  b:=a;
  a:='';
  end;
i:=pos(':',b);
if (i<1) then i:=$666;
a:=a+copy(b,1,i-1);
p:=BVal(copy(b,i+1,255));
if (p=0) then begin;
  p:=80;
  if (proxy<>'') then p:=8080;
  if secure then p:=443;
  end;
Write('resolving '+a+'...');
DNSresolvePut(1,a);
while (1=1) do begin;
  i:=DNSresolveGet(a,b);
  if (i=0) then begin; relequish;continue; end;
  if (i and $80=0) then break;
  WriteLn(' failed!');
  exit;
  end;
WriteLn(' ok!');
if (not TCPcompareAddress(b,remoteAddr)) or (remotePort<>p) then begin;
  pipeLineClose(pipe);
  pipe:=0;
  end;
if (pipe<>0) then begin;
  WriteLn('connection still active...');
  goto ok;
  end;
inpPos:=0;
inpSiz:=0;
Write('connecting to '+ipAddr2string(b)+' '+BStr(p)+'...');
remotePort:=p;
move(b,remoteAddr,sizeof(remoteAddr));
TCPbeginConnect(pipe,65536,b,p);
while TCPlookConnected(pipe,a,i,o) do begin;
  relequish;
  if (pipe<>0) then continue;
  WriteLn(' failed!');
  exit;
  end;
WriteLn(' ok!');
WriteLn('local side is '+ipAddr2string(a)+' '+BStr(i)+'...');
if not secure then goto ok;
while (1=1) do begin;
  a:=getResponse;
  if (a='secure') then break;
  if (pipe<>0) then continue;
  WriteLn(' failed!');
  exit;
  end;
ok:
doConnecting:=false;
End;




Procedure doRequest(url,trg,ctx:String;urlFile:Boolean);
Label kezd,vege,j1,j2;
Var
  retrSize:LongInt;
  f1,f2,f3:xFile;
  i,o,p:LongInt;
  a,b,c:String;
  ctxb:String;
Begin;
retrSize:=-1;
kezd:
outSiz:=0;
fillChar(f1,sizeof(f1),0);
fillChar(f2,sizeof(f2),0);
fillChar(f3,sizeof(f3),0);
if urlFile then begin;
  if (xOpen(f1,url,xGenFilMod_r)<>0) then begin;
    writeln('error opening '+url+'!');
    goto vege;
    end;
  i:=xFileSize(f1);
  if (i>255) then i:=255;
  xBlockRead(f1,b[1],i);
  b[0]:=chr(i);
  xSeek(f1,0);
  end else begin;
  b:=url;
  end;
i:=pos('://',b);
if (i<1) then begin;
  WriteLn('invalid url!');
  goto vege;
  end;
a:=copy(b,i+3,255);
a:=copy(a,1,pos('/',a)-1);
c:=a;
if doConnecting(a) then goto vege;
xCreate(trg);
if (xOpen(f2,trg,xGenFilMod_rw)<>0) then begin;
  writeln('error opening '+trg+'!');
  goto vege;
  end;
xSeek(f2,restMode);
if (ctx<>'') then begin;
  if (xOpen(f3,ctx,xGenFilMod_r)<>0) then begin;
    writeln('error opening '+ctx+'!');
    goto vege;
    end;
  i:=xFileSize(f3);
  if (i>255) then i:=255;
  xBlockRead(f3,a[1],i);
  a[0]:=chr(i);
  i:=pos(#13,a);
  if (i<3) then begin;
    WriteLn('invalid content!');
    goto vege;
    end;
  ctxb:=copy(a,3,i-3);
  xSeek(f3,0);
  end else ctxb:='';
case retrMode of
  1:a:='GET ';
  2:a:='PUT ';
  3:a:='POST ';
  else begin;
    WriteLn('invalid method!');
    goto vege;
    end;
  end;
BeginLine;addBuffer(a);
if urlFile then begin;
  sendBuffer;
  write('...location...');
  sendFile(f1,false);
  xClose(f1);
  end else begin;
  addBuffer(url);
  end;
addBuffer(' HTTP/1.0'#13#10);
BeginLine;addBuffer('Host: ');
addBuffer(c);
addBuffer(#13#10);
BeginLine;addBuffer('Connection: Keep-Alive'#13#10);
BeginLine;addBuffer('User-Agent: ');
a:=proggyName;
kicserel(' v','/',a);
kicserel(' ','_',a);
addBuffer(a);
if (browser<>'') then addBuffer(' '+browser);
addBuffer(#13#10);
BeginLine;addBuffer('Accept-Language: en, *'#13#10);
BeginLine;addBuffer('Accept-Charset: us-ascii, iso-8859-2, *'#13#10);
BeginLine;addBuffer('Accept: text/html, text/plain, */*'#13#10);
if (restMode<>0) then begin;
  BeginLine;addBuffer('Range: bytes=');
  addBuffer(BStr(restMode)+'-');
  addBuffer(#13#10);
  end;
a:=authUsr+':'+authPwd;
if (a<>':') then begin;
  BeginLine;addBuffer('Authorization: Basic ');
  addBuffer(encodeBase64(a));
  addBuffer(#13#10);
  end;
if (ctxb<>'') then begin;
  BeginLine;addBuffer('Content-Type: multipart/form-data; boundary=');
  addBuffer(ctxb);
  addBuffer(#13#10);
  BeginLine;addBuffer('Content-Length: '+BStr(xFileSize(f3))+#13#10);
  end;
BeginLine;addBuffer(#13#10);
sendBuffer;
if (ctxb<>'') then begin;
  sendFile(f3,true);
  xClose(f3);
  end;
b:=getResponse;
a:=GetNextWord(b);
WriteLn('status: '+b);
restMode:=0;
retrSize:=-1;
connMode:=1;
c:='';
j1:
b:=getResponse;
if (pipe=0) then goto vege;
if (b='') then begin;
  WriteLn('content type is: '+c+'...');
  WriteLn('going to receive '+BStr(retrSize)+' bytes starting from '+BStr(restMode)+'...');
  xSeek(f2,restMode);
  goto j2;
  end;
i:=pos(':',b);
a:=kicsi(xLevesz(copy(b,1,i-1)));
b:=xLevesz(copy(b,i+1,255));
if (a='content-length') then begin;
  retrSize:=BVal(b);
  goto j1;
  end;
if (a='connection') then begin;
  connMode:=1;
  if (pos('keep-alive',kicsi(b))<>0) then connMode:=2;
  goto j1;
  end;
if (a='content-range') then begin;
  DecodeRange(restMode,retrSize,b);
  goto j1;
  end;
if (a='content-type') then begin;
  c:=b;
  goto j1;
  end;

goto j1;
j2:
if (retrSize<>-1) then if (restMode>=retrSize) then begin;
  WriteLn(BStr(restMode)+' bytes received successfully!');
  goto vege;
  end;
receive2buffer;
if (inpPos<inpSiz) then begin;
  i:=inpSiz-inpPos;
  if (retrSize<>-1) then begin;
    o:=retrSize-restMode;
    if (i>o) then i:=o;
    end;
  xBlockWrite(f2,inpDat[inpPos+1],i);
  inc(inpPos,i);
  inc(restMode,i);
  write(#13+BStr(restMode)+#13);
  goto j2;
  end;
if (pipe<>0) then goto j2;
if (retrSize=-1) then begin;
  retrSize:=restMode;
  goto j2
  end;
WriteLn('site disconnected before end!');
vege:
xClose(f1);
xClose(f2);
xClose(f3);
if autoRetry then if (restMode<retrSize) then begin;
  WriteLn('restarting automatically...');
  pipeLineClose(pipe);
  pipe:=0;
  goto kezd;
  end;
restMode:=0;
autoRetry:=false;
authUsr:='';
authPwd:='';
End;



Procedure doCommand(b:String);
Var
  i,o,p:LongInt;
  a,c:String;
Begin;
a:=kicsi(GetNextWord(b));
if (pipe<>0) then begin;
  pipeLineStats(pipe,o,i,i);
  if (o=0) then begin;
    pipeLineClose(pipe);
    pipe:=0;
    end;
  end;
if (a='quit') then halt(0);
if (a='') then exit;
if (a='help') or (a='?') then begin;
  WriteLn(' commands');
  WriteLn('~~~~~~~~~~');
  WriteLn('connect <name> [port]');
  WriteLn('disconnect');
  WriteLn('quit');
  WriteLn('secure');
  WriteLn('clear');
  WriteLn('proxy <host> [port]');
  WriteLn('user <username>');
  WriteLn('pass <password>');
  WriteLn('browser <browser id>');
  WriteLn('method <get/put/post>');
  WriteLn('restart <offset>');
  WriteLn('automode <automatic-restart-enable>');
  WriteLn('get <url> [file-contains-contents]');
  WriteLn('get2 <file-contains-url> <local> [file-contains-contents]');
  WriteLn('get3 <local> <url>');
  exit;
  end;
if (a='disconnect') then begin;
  pipeLineClose(pipe);
  pipe:=0;
  exit;
  end;
if (a='connect') then begin;
  pipeLineClose(pipe);
  pipe:=0;
  a:=GetNextWord(b);
  doConnecting('['+a+']:'+b);
  exit;
  end;
if (a='browser') then begin;
  browser:=b;
  exit;
  end;
if (a='user') then begin;
  authUsr:=b;
  exit;
  end;
if (a='pass') then begin;
  authPwd:=b;
  exit;
  end;
if (a='proxy') then begin;
  a:=GetNextWord(b);
  if (a='') then proxy:='' else proxy:='['+a+']:'+b;
  exit;
  end;
if (a='method') then begin;
  a:=kicsi(b);
  i:=0;
  if (a='get') then i:=1;
  if (a='put') then i:=2;
  if (a='post') then i:=3;
  if (i<1) then WriteLn('invalid method: '+b) else retrMode:=i;
  exit;
  end;
if (a='restart') then begin;
  restMode:=BVal(b);
  if (restMode<0) then restMode:=0;
  WriteLn('will try to restart at '+BStr(restMode)+'...');
  exit;
  end;
if (a='automode') then begin;
  autoRetry:=(BVal(b)=1);
  if autoRetry then a:='' else a:='not ';
  WriteLn('will '+a+'try to restart automatically...');
  exit;
  end;
if (a='get') then begin;
  a:=GetNextWord(b);
  c:=a;
  kicserel('/','\',c);
  c:=xFileName(c,2)+xFileName(c,3);
  doRequest(a,c,b,false);
  exit;
  end;
if (a='get2') then begin;
  a:=GetNextWord(b);
  c:=GetNextWord(b);
  doRequest(a,c,b,true);
  exit;
  end;
if (a='get3') then begin;
  a:=GetNextWord(b);
  doRequest(b,a,'',false);
  exit;
  end;
if (a='secure') then begin;
  pipeLineClose(pipe);
  pipe:=0;
  if TLSfindProcess then immErr('failed to find tls process!');
  secure:=true;
  exit;
  end;
if (a='clear') then begin;
  pipeLineClose(pipe);
  pipe:=0;
  if TCPfindProcess then immErr('failed to find tcp process!');
  secure:=false;
  exit;
  end;


WriteLn('unknown command: '+a+' '+b);
End;





Label f1;
Var
  a:String;
  t:xtText;
BEGIN;
WriteLn(proggyName+', done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');
if DNSstartResolver then immErr('failed to find dns process!');

pipe:=0;
connMode:=0;
restMode:=0;
retrMode:=1;
inpSiz:=0;
inpPos:=0;
outSiz:=0;
autoRetry:=false;
browser:='[no browser]';
authUsr:='';
authPwd:='';
proxy:='';
secure:=false;

a:=GetAllParameters;
if (a='') then goto f1;
if (xtOpen(t,a,true)<>0) then goto f1;
while not xtEOF(t) do begin;
  a:=xtReadLn(t,255);
  doCommand(a);
  end;
xtClose(t);

f1:
WriteLn('');
a:=ReadLine;
doCommand(a);
goto f1;
END.