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


Var
  convMode:byte; {1=asc, 2=bin}
  remoteAddr:String;
  remotePort:LongInt;
  parameter:String;

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
Write(#13'gopher>');
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


Procedure getFile(var f:xFile;url:String);
Label f1,f2;
Var
  a,b:String;
  pipe:LongInt;
  i,o,p,s:LongInt;
  buf:array[1..4*1024] of byte;
  crlf:LongInt;

Procedure doConvAsc;
Var i,o,p:LongInt;
Begin;
p:=0;
for o:=1 to s do begin;
  i:=buf[o];
  case i of
    13:crlf:=1;
    10:if (crlf=1) then crlf:=2 else crlf:=0;
    46:if (crlf=2) then begin; crlf:=0;continue; end else crlf:=0;
    else crlf:=0;
    end;
  inc(p);
  buf[p]:=i;
  end;
s:=p;
End;


Begin;
a:=remoteAddr;
p:=remotePort;
if (p=0) then p:=70;
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
Write('connecting to '+ipAddr2string(b)+' '+BStr(p)+'...');
TCPbeginConnect(pipe,4096,b,p);
while TCPlookConnected(pipe,a,i,o) do begin;
  relequish;
  if (pipe<>0) then continue;
  WriteLn(' failed!');
  exit;
  end;
WriteLn(' ok!');
WriteLn('local side is '+ipAddr2string(a)+' '+BStr(i)+'...');
WriteLn('sending request...');
p:=length(url);
move(url[1],buf,p);
buf[p+1]:=9;
i:=length(parameter);
move(parameter[1],buf[p+2],i);
if (i>0) then inc(p,i+1);
inc(p);
buf[p]:=13;
buf[p]:=10;
pipeLineSend(pipe,buf,p);
WriteLn('receiving data...');
p:=0;
crlf:=0;
f1:
s:=sizeof(buf);
if (pipeLineRecv(pipe,buf,s)<>0) then s:=0;
if (s>0) then begin;
  Write(#13+BStr(p)+#13);
  if (convMode=1) then doConvAsc;
  xBlockWrite(f,buf,s);
  inc(p,s);
  goto f1;
  end;
pipeLineStats(pipe,o,i,i);
if (o<>0) then begin;
  relequish;
  goto f1;
  end;
pipeLineClose(pipe);
pipe:=0;
xTruncate(f);
WriteLn(BStr(p)+' bytes received!');
parameter:='';
End;






Procedure doCommand(b:String);
Label f1;
Var
  f:xFile;
  i,o,p:LongInt;
  a:String;
Begin;
a:=kicsi(GetNextWord(b));
if (a='quit') then halt(0);
if (a='') then exit;
if (a='connect') then begin;
  remoteAddr:=GetNextWord(b);
  remotePort:=BVal(GetNextWord(b));
  exit;
  end;
if (a='help') or (a='?') then begin;
  WriteLn(' commands');
  WriteLn('~~~~~~~~~~');
  WriteLn('connect <name> [port]');
  WriteLn('quit');
  WriteLn('ascii');
  WriteLn('binary');
  WriteLn('get <remote> [local]');
  WriteLn('get2 <local> <remote>');
  WriteLn('param <parameter>');
  exit;
  end;
if (a='ascii') then begin;
  convMode:=1;
  WriteLn('ascii mode selected...');
  exit;
  end;
if (a='binary') then begin;
  convMode:=2;
  WriteLn('binary mode selected...');
  exit;
  end;
if (a='param') then begin;
  parameter:=b;
  WriteLn('parameter set...');
  exit;
  end;
if (a='get') then begin;
  a:=GetNextWord(b);
  if (b='') then begin;
    b:=a;kicserel('/','\',b);
    b:=xFileName(b,2)+xFileName(b,3);
    end;
  xCreate(b);
  if (xOpen(f,b,xGenFilMod_rw)<>0) then begin;
    writeln('error opening '+b+'!');
    exit;
    end;
  getFile(f,a);
  xClose(f);
  exit;
  end;
if (a='get2') then begin;
  a:=GetNextWord(b);
  xCreate(a);
  if (xOpen(f,a,xGenFilMod_rw)<>0) then begin;
    writeln('error opening '+a+'!');
    exit;
    end;
  getFile(f,b);
  xClose(f);
  exit;
  end;

WriteLn('unknown command: '+a+' '+b);
End;






Label f1;
Var
  a:String;
  t:xtText;
BEGIN;
WriteLn('gopher client v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');
if DNSstartResolver then immErr('failed to find dns process!');

convMode:=2;
remoteAddr:='';
remotePort:=0;
parameter:='';

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