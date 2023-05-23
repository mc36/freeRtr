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
  remoteAddr:String;
  remotePort:LongInt;
  device:String;
  jobName:String;
  deleteAfter:Boolean;
  pipe:LongInt;

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
Write(#13'lpd>');
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


Function doConnecting:Boolean;
Label f1;
Var
  a,b:String;
  i,o,p:LongInt;
Begin;
doConnecting:=true;
a:=remoteAddr;
p:=remotePort;
if (p=0) then p:=515;
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
doConnecting:=false;
End;

Procedure disconnect;
Begin;
pipeLineClose(pipe);
pipe:=0;
End;

Procedure sendText(a:String);
Begin;
pipeLineSend(pipe,a[1],length(a));
End;

Function waitForAck:Boolean;
Label f1,f2;
Var
  i,o:LongInt;
  b:Byte;
Begin;
Write('waiting for acknowledge...');
waitForAck:=true;
f1:
i:=sizeof(b);
if (pipeLineRecv(pipe,b,i)<>0) then i:=0;
if (i=sizeof(b)) then begin;
  if (b<>0) then goto f2;
  waitForAck:=false;
  WriteLn(' ok!');
  exit;
  end;
pipeLineStats(pipe,o,i,i);
if (o=0) then goto f2;
relequish;
goto f1;
f2:
WriteLn(' failed!');
disconnect;
End;

Function sendFile(typ:LongInt;fil,dat:String):Boolean;
Label vege;
Var
  f:xFile;
  i,o,p,s:LongInt;
  buf:array[1..1024] of byte;
Begin;
sendFile:=True;
if (xOpen(f,fil,xGenFilMod_r)<>0) then begin;
  WriteLn('error opening file!');
  goto vege;
  end;
sendText(chr(typ));
s:=xFileSize(f);
sendText(BStr(s)+' ');
sendText(dat);
sendText(jobName);
sendText(#10);
if waitForAck then goto vege;
WriteLn('sending '+BStr(s)+' bytes...');
p:=0;
while (p<s) do begin;
  o:=s-p;
  if (o>sizeof(buf)) then o:=sizeof(buf);
  xBlockRead(f,buf,o);
  while (pipeLineSend(pipe,buf,o)<>0) do begin;
    relequish;
    pipeLineStats(pipe,o,i,i);
    if (o=0) then goto vege;
    end;
  inc(p,o);
  Write(BStr(p)+#13);
  end;
xClose(f);
WriteLn('sending end of file...');
sendText(#0);
if waitForAck then goto vege;
sendFile:=False;
exit;
vege:
xClose(f);
disconnect;
End;




Procedure submitJob(hed,dat:String);
Label f1;
Begin;
if doConnecting then exit;
WriteLn('sending "printer job"...');
sendText(#2);
sendText(device);
sendText(#10);
if waitForAck then exit;
WriteLn('sending "control file"...');
if sendFile(2,hed,'cfA') then exit;
WriteLn('sending "data file"...');
if sendFile(3,dat,'dfA') then exit;
WriteLn('successfully finished!');
disconnect;
if not deleteAfter then exit;
xErase(hed);
xErase(dat);
End;



Procedure readStatus(typ:LongInt;dat:String);
Label f1;
Var
  i,o:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
if doConnecting then exit;
WriteLn('sending request...');
sendText(chr(typ));
sendText(device);
sendText(dat);
sendText(#10);
WriteLn('receiving data...');
f1:
i:=64;
if (pipeLineRecv(pipe,a[1],i)<>0) then i:=0;
if (i>0) then begin;
  ab0:=i;
  for i:=ab0 downto 1 do if (ab[i]=10) then a:=copy(a,1,i-1)+#13+copy(a,i,666);
  write(a);
  goto f1;
  end;
pipeLineStats(pipe,o,i,i);
if (o<>0) then begin;
  relequish;
  goto f1;
  end;
disconnect;
WriteLn('');
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
  WriteLn('device <name>');
  WriteLn('job <name>');
  WriteLn('delete <1/0>');
  WriteLn('put <head> <data>');
  WriteLn('status <list>');
  WriteLn('statverb <list>');
  WriteLn('remove <agent> <list>');
  WriteLn('continue');
  exit;
  end;
if (a='device') then begin;
  device:=b;
  exit;
  end;
if (a='job') then begin;
  jobName:=b;
  exit;
  end;
if (a='delete') then begin;
  deleteAfter:=(BVal(b)=1);
  exit;
  end;
if (a='continue') then begin;
  readStatus(1,b);
  exit;
  end;
if (a='put') then begin;
  a:=GetNextWord(b);
  submitJob(a,b);
  exit;
  end;
if (a='status') then begin;
  readStatus(3,' '+b);
  exit;
  end;
if (a='statverb') then begin;
  readStatus(4,' '+b);
  exit;
  end;
if (a='remove') then begin;
  readStatus(5,' '+b);
  exit;
  end;


WriteLn('unknown command: '+a+' '+b);
End;






Label f1;
Var
  a:String;
  t:xtText;
BEGIN;
WriteLn('line printer client v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');
if DNSstartResolver then immErr('failed to find dns process!');

remoteAddr:='';
remotePort:=0;
device:='';
jobName:='123localhost';
deleteAfter:=false;
pipe:=0;

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