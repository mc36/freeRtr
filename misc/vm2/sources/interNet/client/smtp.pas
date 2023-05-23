{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$sysinc inet_dns.inc}
{$include base64.inc}

Const
  MessageBodyFile='.msg';
  MessageHeadFile='.hdr';
  MessageRprtFile='.err';

Var
  pipe:LongInt;
  inpDat:array[1..1024] of byte;
  inpSiz:LongInt;
  inpPos:LongInt;
  outDat:array[1..1024] of byte;
  outSiz:LongInt;

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

function getNextLine(var t:xtText):string;
var
  i:longint;
  a:String;
begin;
a:=xtReadLn(t,255);
i:=pos(';',a);
if (i>0) then a:=copy(a,1,i-1);
a:=' '+a+' ';
kicserel('  ',' ',a);
a:=copy(a,2,length(a)-2);
getNextLine:=a;
end;




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
Label f1,f2;
Var
  i:LongInt;
  a:String;
Begin;
f1:
a:='';
Write('<-- ');
f2:
inc(inpPos);
if (inpPos>inpSiz) then begin;
  receive2buffer;
  if (pipe<>0) then goto f2;
  WriteLn('');
  getResponse:='';
  exit;
  end;
i:=inpDat[inpPos];
if (i=13) then goto f2;
if (i<>10) then begin;
  a:=a+chr(i);
  goto f2;
  end;
WriteLn(a);
getResponse:=a;
i:=BVal(copy(a,1,3));
if (i<100) then goto f1;
if (copy(a,4,1)<>' ') then goto f1;
End;

Function analResponse(var a:String):Boolean;
Begin;
analResponse:=(kicsi(copy(a,1,1))<>'2');
End;

Procedure sendCommand(a:String);
Label f1,f2;
Var i,o:LongInt;
Begin;
Write('--> ');
a:=a+#13#10;
f1:
if (pipeLineSend(pipe,a[1],length(a))<>0) then begin;
  relequish;
  pipeLineStats(pipe,o,i,i);
  if (o<>0) then goto f1;
  WriteLn('');
  exit;
  end;
Write(a);
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
End;


Function FindNextFile(var a:string;b:string):Boolean;
Label f1,f2;
Var
  sr:xDirEntryRec;
  f,ff:xFile;
Begin;
FindNextFile:=False;
if (xDirOpen(f,b)<>0) then begin;
  f2:
  xDirClose(f);
  exit;
  end;
f1:
if (xDirRead(f,sr)<>0) then goto f2;
if (sr.name='') then goto f2;
a:=xFileName(sr.name,2);
if (xOpen(ff,b+a+MessageBodyFile,xGenFilMod_r)<>0) then goto f1;
xClose(ff);
if (xOpen(ff,b+a+MessageHeadFile,xGenFilMod_r)<>0) then goto f1;
xClose(ff);
if (xOpen(ff,b+a+MessageRprtFile,xGenFilMod_r)=0) then begin;
  xClose(ff);
  goto f1;
  end;
xDirClose(f);
FindNextFile:=True;
End;


Label f1;
Var
  serverName:String;
  serverAddr:OneTCPaddressRecord;
  serverPort:LongInt;
  serverUser:String;
  serverPass:String;
  sourceDirectory:String;
  t:xtText;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('smtp client v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');
if DNSstartResolver then immErr('failed to find dns process!');

a:=paramStr(1);
if (a='') then immErr('using: smtp.code <config>');
if (xtOpen(t,a,true)<>0) then immErr('error opening config file!');
serverName:=getNextLine(t);
serverPort:=BVal(getNextLine(t));
sourceDirectory:=getNextLine(t);
serverUser:=getNextLine(t);
serverPass:=getNextLine(t);
xtClose(t);

if (serverPort=0) then serverPort:=25;
if (copy(sourceDirectory,length(sourceDirectory),255)<>'\') then sourceDirectory:=sourceDirectory+'\';

Write('resolving '+serverName+'...');
DNSresolvePut(1,serverName);
while (1=1) do begin;
  i:=DNSresolveGet(a,b);
  if (i=0) then begin; relequish;continue; end;
  if (i and $80<>0) then immErr(' failed!');
  break;
  end;
WriteLn(' ok!');
move(b,serverAddr,sizeof(serverAddr));
Write('connecting to '+ipAddr2string(serverAddr)+' '+BStr(serverPort)+'...');
TCPbeginConnect(pipe,65536,serverAddr,serverPort);
while TCPlookConnected(pipe,a,i,o) do begin;
  relequish;
  if (pipe<>0) then continue;
  immErr(' failed!');
  end;
WriteLn(' ok!');
WriteLn('local side is '+ipAddr2string(a)+' '+BStr(i)+'...');
inpPos:=0;
inpSiz:=0;
a:=getResponse;
if analResponse(a) then immErr('invalid response!');

if (xtOpen(t,'c:\system\localHost.text',true)<>0) then immErr('error opening localhost.text');
b:=xtReadLn(t,255);
xtClose(t);
sendCommand('EHLO '+b);
a:=getResponse;
if analResponse(a) then begin;
  sendCommand('HELO '+b);
  a:=getResponse;
  if analResponse(a) then immErr('invalid response!');
  end;

if (serverUser<>'') then begin;
  sendCommand('AUTH PLAIN');
  a:=getResponse;
  if (copy(a,1,1)<>'3') then immErr('invalid response!');
  sendCommand(encodeBase64(#0+serverUser+#0+serverPass));
  a:=getResponse;
  if analResponse(a) then immErr('invalid response!');
  end;

sendCommand('HELP');
a:=getResponse;

while FindNextFile(b,sourceDirectory) do begin;
  WriteLn('going to send '+b+'...');
  sendCommand('RSET');
  a:=getResponse;
  xCreate(sourceDirectory+b+MessageRprtFile);
  if (xtOpen(t,sourceDirectory+b+MessageHeadFile,true)<>0) then continue;
  a:=xtReadLn(t,255);
  sendCommand('MAIL FROM:<'+a+'>');
  a:=getResponse;
  if analResponse(a) then goto f1;
  while not xtEOF(t) do begin;
    a:=xtReadLn(t,255);
    sendCommand('RCPT TO:<'+a+'>');
    a:=getResponse;
    if analResponse(a) then goto f1;
    end;
  xtClose(t);
  sendCommand('DATA');
  a:=getResponse;
  if (copy(a,1,1)<>'3') then continue;
  outSiz:=0;
  if (xtOpen(t,sourceDirectory+b+MessageBodyFile,true)<>0) then continue;
  while not xtEOF(t) do begin;
    while not xtEOL(t) do begin;
      a:=xtRead(t,192);
      addBuffer(a);
      sendBuffer;
      end;
    a:=xtReadLn(t,255)+#13#10;
    addBuffer(a);
    a:=xtRead(t,16);
    if (copy(a,1,1)='.') then a:='.'+a;
    addBuffer(a);
    Write(#13+BStr(xtGetPos(t))+#13);
    end;
  WriteLn(#13+BStr(xtFileSize(t))+' bytes sent');
  xtClose(t);
  addBuffer(#13#10'.'#13#10);
  sendBuffer;
  a:=getResponse;
  if analResponse(a) then goto f1;
  xErase(sourceDirectory+b+MessageBodyFile);
  xErase(sourceDirectory+b+MessageHeadFile);
  xErase(sourceDirectory+b+MessageRprtFile);
  continue;
  f1:
  xtClose(t);
  end;

sendCommand('QUIT');
a:=getResponse;
if analResponse(a) then immErr('invalid response!');
WriteLn('successful!');
END.