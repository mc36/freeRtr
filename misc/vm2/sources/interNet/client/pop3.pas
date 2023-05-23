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
{$sysinc crypto.inc}
{$sysinc random.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$sysinc inet_dns.inc}

Const
  MessageBodyFile='.msg';

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

Function binary2digest(a:String):String;
Var
  aa:array[0..1] of byte absolute a;
  i:LongInt;
  b:String;
Begin;
b:='';
for i:=1 to aa[0] do b:=b+byte2hextype(aa[i]);
binary2digest:=kicsi(b);
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

Function generateRandomName(pat,ext:String):String;
Label f1;
Var a:String;
Begin;
f1:
a:=BStr(random($7fffffff))+'.'+BStr(random($7fffffff))+'.'+BStr(random($7fffffff))+ext;
if (xCreate(pat+a)<>0) then goto f1;
generateRandomName:=a;
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
  a:String;
Begin;
a:='';
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
  a:=a+chr(i);
  goto f1;
  end;
WriteLn(a);
getResponse:=a;
End;

Function analResponse(var a:String):Boolean;
Begin;
analResponse:=(kicsi(copy(a,1,3))<>'+ok');
End;

Procedure sendCommand(a:String;hide:Boolean);
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
if hide then a:=copy(a,1,pos(' ',a))+'*****'#13#10;
Write(a);
End;

Function getMultilineResp:String;
Label f1;
Var a:String;
Begin;
a:=getResponse;
getMultilineResp:=a;
if analResponse(a) then exit;
f1:
if (pipe=0) then exit;
a:=getResponse;
if (a='.') then exit;
goto f1;
End;

Procedure getMessageBody(var f:xFile);
Label f1;
Var
  ring:String;
  rang:array[0..15] of byte absolute ring;
  bytes:LongInt;
  i:LongInt;
Begin;
ring:='123'#13#10;
bytes:=0;
f1:
inc(inpPos);
if (inpPos>inpSiz) then begin;
  receive2buffer;
  if (pipe<>0) then goto f1;
  Write(#13'                  '#13);
  exit;
  end;
i:=inpDat[inpPos];
if (outSiz>=sizeof(outDat)) then begin;
  Write(#13+BStr(bytes)+#13);
  xBlockWrite(f,outDat,outSiz);
  outSiz:=0;
  end;
move(rang[2],rang[1],sizeof(rang));
rang[5]:=i;
if (i=$2e) then if (rang[4] in [10,13]) then goto f1;
inc(bytes);
inc(outSiz);
outDat[outSiz]:=i;
if (i<>10) then goto f1;
if (ring<>#13#10'.'#13#10) then goto f1;
xBlockWrite(f,outDat,outSiz);
xSeek(f,xFilePos(f)-2);
WriteLn(#13+BStr(bytes)+' bytes received');
End;




Var
  md5algorithm:LongInt;
  serverName:String;
  serverAddr:OneTCPaddressRecord;
  serverPort:LongInt;
  username:String;
  password:String;
  leaveMessages:Boolean;
  authenticMode:Byte;
  targetDirectory:String;
  t:xtText;
  f:xFile;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('pop3 client v1.0, done by Mc at '#%date' '#%time'.');
Randomize;
if TCPfindProcess then immErr('failed to find tcp process!');
if DNSstartResolver then immErr('failed to find dns process!');
if CryptoStartActions then immErr('failed to find crypto process!');

CryptoGetHasherList(inpDat,p);
md5algorithm:=CryptoFindOneAlgo(inpDat,p,'md5');
if (md5algorithm<1) then immErr('failed to find md5 algorithm!');

a:=paramStr(1);
if (a='') then immErr('using: pop3.code <config>');
if (xtOpen(t,a,true)<>0) then immErr('error opening config file!');
serverName:=getNextLine(t);
serverPort:=BVal(getNextLine(t));
username:=getNextLine(t);
password:=getNextLine(t);
leaveMessages:=(BVal(getNextLine(t))<>1);
authenticMode:=BVal(getNextLine(t));
targetDirectory:=getNextLine(t);
xtClose(t);

if (serverPort=0) then serverPort:=110;
if (copy(targetDirectory,length(targetDirectory),255)<>'\') then targetDirectory:=targetDirectory+'\';

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
b:=getResponse;
if analResponse(b) then immErr('invalid response!');
case authenticMode of
  1:begin; {user/pass}
    sendCommand('USER '+username,false);
    a:=getResponse;
    if analResponse(a) then immErr('invalid response!');
    sendCommand('PASS '+password,true);
    a:=getResponse;
    if analResponse(a) then immErr('invalid response!');
    end;
  2:begin; {apop}
    i:=pos('<',b);
    if (i<1) then immErr('invalid greeting message!');
    b:=copy(b,i,255);
    i:=pos('>',b);
    if (i<1) then immErr('invalid greeting message!');
    b:=copy(b,1,i);
    WriteLn('got cookie: '+b);
    a:=b+password;
    i:=length(a);
    CryptoImmHasher(md5algorithm,'','',a[1],i);
    a[0]:=chr(i);
    a:=binary2digest(a);
    sendCommand('APOP '+username+' '+a,false);
    a:=getResponse;
    if analResponse(a) then immErr('invalid response!');
    end;
  else immErr('unknown authentication mode in config file!');
  end;
WriteLn('authenticated successfully!');

sendCommand('STAT',false);
a:=getResponse;
if analResponse(a) then immErr('invalid response!');
GetNextWord(a);
p:=BVal(GetNextWord(a));
o:=BVal(GetNextWord(a));
WriteLn('server has '+alakit(p)+' messages for us allocating '+alakit(o)+' bytes.');
sendCommand('CAPA',false);
getMultilineResp;
sendCommand('HELP',false);
getMultilineResp;
sendCommand('UIDL',false);
getMultilineResp;
sendCommand('LIST',false);
getMultilineResp;

for o:=1 to p do begin;
  a:=generateRandomName(targetDirectory,MessageBodyFile);
  WriteLn('receiving message #'+BStr(o)+' of '+BStr(p)+' to '+a+'...');
  if (xOpen(f,targetDirectory+a,xGenFilMod_rw)<>0) then continue;
  sendCommand('RETR '+BStr(o),false);
  a:=getResponse;
  if analResponse(a) then immErr('invalid response!');
  outSiz:=0;
  getMessageBody(f);
  xTruncate(f);
  xClose(f);
  if leaveMessages then continue;
  sendCommand('DELE '+BStr(o),false);
  a:=getResponse;
  if analResponse(a) then immErr('invalid response!');
  end;

sendCommand('QUIT',false);
a:=getResponse;
if analResponse(a) then immErr('invalid response!');
WriteLn('successful!');
END.