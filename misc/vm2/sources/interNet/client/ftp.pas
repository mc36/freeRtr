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
  pipe:LongInt;
  data:LongInt;
  bind:LongInt;
  ip6:boolean;
  xferMode:byte; {1=port, 2-pasv}
  convMode:byte; {1=asc, 2=bin}
  restMode:LongInt;
  inpDat:array[1..1024] of byte;
  inpSiz:LongInt;
  inpPos:LongInt;
  remoteAddr:OneTCPaddressRecord;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function GetNextWord(Var a:String;s:String):String;
Var i:Word;
Begin;
i:=pos(s,a);
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
Write(#13'ftp>');
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

Function analResponse1(var a:String):Boolean;
Begin;
analResponse1:=(kicsi(copy(a,1,1))<>'1');
End;

Function analResponse2(var a:String):Boolean;
Begin;
analResponse2:=(kicsi(copy(a,1,1))<>'2');
End;

Function analResponse3(var a:String):Boolean;
Begin;
analResponse3:=(kicsi(copy(a,1,1))<>'3');
End;

Procedure sendString(a:String);
Label f1,f2;
Var i,o:LongInt;
Begin;
Write('--> ');
f1:
if (pipeLineSend(pipe,a[1],length(a))<>0) then begin;
  relequish;
  pipeLineStats(pipe,o,i,i);
  if (o<>0) then goto f1;
  exit;
  end;
Write(a);
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



Procedure sendFile(var pipe:LongInt;var f:xFile;status:Boolean);
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

Procedure getFile(var pipe:LongInt;var f:xFile;s:LongInt;status:Boolean);
Label f1,f2;
Var
  i,o,p:LongInt;
  buf:array[1..4*1024] of byte;
Begin;
p:=xFilePos(f);
if status then WriteLn('going to receive '+BStr(s)+' bytes...');
f1:
i:=sizeof(buf);
if (pipeLineRecv(pipe,buf,i)<>0) then i:=0;
if (i>0) then begin;
  if status then Write(#13+BStr(p)+#13);
  xBlockWrite(f,buf,i);
  inc(p,i);
  goto f1;
  end;
pipeLineStats(pipe,o,i,i);
if (o<>0) then begin;
  relequish;
  goto f1;
  end;
pipeLineClose(pipe);
pipe:=0;
xClose(f);
if status then WriteLn(BStr(p)+' bytes received!');
End;



Procedure cancelBinding;
Begin;
pipeLineClose(bind);
while (pipeLineGetIncoming(bind)=0) do pipeLineClose(bind);
pipeLineEndListen;
End;

Function beginXfer1:Boolean;
Label err;
Var
  a,b:String;
  i,o:LongInt;
  bb:array[1..1] of byte absolute b;
Begin;
beginXfer1:=True;
bind:=0;
data:=0;
case xferMode of
  1:begin; {port}
    pipeLineBegListen;
    TCPlistenOnPort(bind,65536,b,o);
    o:=o and $ffff;
    if ip6 then begin;
      a:='EPRT |2|'+ipAddr2string(b)+'|'+BStr(o)+'|';
      end else begin;
      a:='PORT '+BStr(bb[13])+','+BStr(bb[14])+','+BStr(bb[15])+','+BStr(bb[16])+','+BStr(o shr 8)+','+BStr(o and $ff);
      end;
    sendCommand(a,false);
    a:=getResponse;
    if analResponse2(a) then goto err;
    end;
  2:begin; {pasv}
    if ip6 then a:='EPSV 2' else a:='PASV';
    sendCommand(a,false);
    a:=getResponse;
    if analResponse2(a) then goto err;
    i:=pos('(',a);
    if (i<1) then goto err;
    a:=copy(a,i+1,255);
    i:=pos(')',a);
    if (i<1) then goto err;
    a:=copy(a,1,i-1);
    kicserel(' ','',a);
    if ip6 then begin;
      GetNextWord(a,'|');
      GetNextWord(a,'|');
      b:=GetNextWord(a,'|');
      if (b='') then begin;
        move(remoteAddr,b,sizeof(remoteAddr));
        end else begin;
        if string2ipAddr(b,b) then goto err;
        end;
      o:=BVal(GetNextWord(a,'|'));
      end else begin;
      b:=IPv4addressPrefix;
      move(b[1],b,128);
      for i:=13 to 16 do bb[i]:=BVal(GetNextWord(a,','));
      i:=BVal(GetNextWord(a,','));
      o:=BVal(GetNextWord(a,','));
      o:=(i shl 8) or o;
      end;
    WriteLn('connecting to '+ipAddr2string(b)+' '+BStr(o)+'...');
    TCPbeginConnect(data,65536,b,o);
    end;
  else goto err;
  end;
case convMode of
  1:sendCommand('TYPE A',false); {ascii}
  2:sendCommand('TYPE I',false); {binary}
  else goto err;
  end;
getResponse;
sendCommand('MODE S',false);getResponse;
sendCommand('STRU F',false);getResponse;
beginXfer1:=False;
exit;
err:
WriteLn('failed to estabilish data connection!');
pipeLineClose(data);
cancelBinding;
End;

Function beginXfer2(var size:LongInt):Boolean;
Label err;
Var
  a,b:String;
  i,o,p:LongInt;
Begin;
beginXfer2:=True;
a:=getResponse;
if analResponse1(a) and analResponse2(a) and analResponse3(a) then goto err;
a:=copy(a,pos('(',a)+1,255);
a:=copy(a,1,pos(')',a)-1);
size:=BVal(GetNextWord(a,' '));
Write('waiting for data connection get ready...');
case xferMode of
  1:begin; {port}
    timer2start;p:=CurrentTime;
    while (pipeLineGetIncoming(data)<>0) do begin;
      if (GetTimePast(p)>5*60) then goto err;
      relequish;
      end;
    while TCPlookConnected(data,a,i,o) do begin;
      relequish;
      if (data=0) then goto err;
      end;
    WriteLn(' ok!');
    WriteLn('remote side is '+ipAddr2string(a)+' '+BStr(i)+'...');
    end;
  2:begin; {pasv}
    while TCPlookConnected(data,a,i,o) do begin;
      relequish;
      if (data=0) then goto err;
      end;
    WriteLn(' ok!');
    WriteLn('local side is '+ipAddr2string(a)+' '+BStr(i)+'...');
    end;
  else goto err;
  end;
cancelBinding;
beginXfer2:=False;
exit;
err:
WriteLn(' failed!');
pipeLineClose(data);
cancelBinding;
End;




Procedure doCommand(b:String);
Label f1;
Var
  f:xFile;
  i,o,p:LongInt;
  a:String;
Begin;
a:=kicsi(GetNextWord(b,' '));
while (inpPos<inpSiz) do getResponse;
if (a='quit') then halt(0);
if (a='') then exit;
if (a='disconnect') then begin;
  sendCommand('QUIT',false);
  a:=getResponse;
  pipeLineClose(pipe);
  pipe:=0;
  exit;
  end;
if (a='connect') then begin;
  pipeLineClose(pipe);
  pipe:=0;
  a:=GetNextWord(b,' ');
  p:=BVal(GetNextWord(b,' '));
  if (p=0) then p:=21;
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
  ip6:=not isAddressIPv4mask(b);
  move(b,remoteAddr,sizeof(remoteAddr));
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
  inpPos:=0;
  inpSiz:=0;
  f1:
  a:=getResponse;
  if analResponse2(a) then begin;
    WriteLn('invalid response!');
    exit;
    end;
  WriteLn('successful!');
  exit;
  end;
if (a='user') then begin;
  sendCommand('USER '+b,false);
  a:=getResponse;
  if analResponse3(a) then begin;
    WriteLn('invalid response!');
    exit;
    end;
  WriteLn('successful!');
  exit;
  end;
if (a='pass') then begin;
  sendCommand('PASS '+b,true);
  goto f1;
  end;
if (a='acct') then begin;
  sendCommand('ACCT '+b,true);
  goto f1;
  end;
if (a='help') or (a='?') then begin;
  WriteLn(' commands');
  WriteLn('~~~~~~~~~~');
  WriteLn('connect <name> [port]');
  WriteLn('disconnect');
  WriteLn('quit');
  WriteLn('user <username>');
  WriteLn('pass <password>');
  WriteLn('acct <account-info>');
  WriteLn('remotehelp');
  WriteLn('status');
  WriteLn('port');
  WriteLn('pasv');
  WriteLn('ascii');
  WriteLn('binary');
  WriteLn('quote <command>');
  WriteLn('cd');
  WriteLn('cd..');
  WriteLn('cd <dir>');
  WriteLn('cd2 <file-contains-path>');
  WriteLn('md <dir>');
  WriteLn('rd <dir>');
  WriteLn('del <file>');
  WriteLn('ren <source> <target>');
  WriteLn('dir [path]');
  WriteLn('dir2 <local> [path]');
  WriteLn('restart <offset>');
  WriteLn('get <remote> [local]');
  WriteLn('get2 <file-contains-name> <local>');
  WriteLn('get3 <local> <remote>');
  WriteLn('put <local> [remote]');
  exit;
  end;
if (a='remotehelp') then begin;
  sendCommand('HELP',false);
  goto f1;
  end;
if (a='status') then begin;
  sendCommand('STAT',false);
  a:=getResponse;
  sendCommand('SYST',false);
  goto f1;
  end;
if (a='cd..') then begin;
  sendCommand('CDUP',false);
  goto f1;
  end;
if (a='quote') then begin;
  sendCommand(b,false);
  goto f1;
  end;
if (a='cd') then begin;
  if (b='') then begin;
    sendCommand('PWD',false);
    goto f1;
    end;
  sendCommand('CWD '+b,false);
  goto f1;
  end;
if (a='cd2') then begin;
  if (xOpen(f,b,xGenFilMod_r)<>0) then begin;
    writeln('error opening '+b+'!');
    exit;
    end;
  sendString('CWD ');
  sendFile(pipe,f,false);
  Write('...anywhere...');
  sendString(#13#10);
  goto f1;
  end;
if (a='md') then begin;
  sendCommand('MKD '+b,false);
  goto f1;
  end;
if (a='rd') then begin;
  sendCommand('RMD '+b,false);
  goto f1;
  end;
if (a='del') then begin;
  sendCommand('DELE '+b,false);
  goto f1;
  end;
if (a='ren') then begin;
  sendCommand('RNFR '+GetNextWord(b,' '),false);
  a:=getResponse;
  sendCommand('RNTO '+b,false);
  goto f1;
  end;
if (a='port') then begin;
  xferMode:=1;
  WriteLn('data connection will opened by server...');
  exit;
  end;
if (a='pasv') then begin;
  xferMode:=2;
  WriteLn('data connection will opened by client...');
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
if (a='restart') then begin;
  restMode:=BVal(b);
  if (restMode<0) then restMode:=0;
  WriteLn('will try to restart at '+BStr(restMode)+'...');
  exit;
  end;
if (a='dir') then begin;
  restMode:=0;
  if beginXfer1 then exit;
  if (b<>'') then b:=' '+b;
  sendCommand('REST 0',false);
  a:=getResponse;
  sendCommand('LIST'+b,false);
  if beginXfer2(i) then exit;
  while (data<>0) do begin;
    i:=128;
    if (pipeLineRecv(data,a[1],i)<>0) then i:=0;
    if (i<1) then begin;
      pipeLineStats(data,o,i,i);
      if (o=0) then break;
      relequish;
      continue;
      end;
    a[0]:=chr(i);
    write(a);
    end;
  pipeLineClose(data);
  goto f1;
  end;
if (a='put') then begin;
  a:=GetNextWord(b,' ');
  if (b='') then b:=xFileName(a,2)+xFileName(a,3);
  if (xOpen(f,a,xGenFilMod_r)<>0) then begin;
    writeln('error opening '+a+'!');
    exit;
    end;
  p:=restMode;
  restMode:=0;
  sendCommand('ALLO '+BStr(xFileSize(f)),false);
  a:=getResponse;
  if beginXfer1 then begin; xClose(f);exit; end;
  sendCommand('REST '+BStr(p),false);
  a:=getResponse;
  if analResponse3(a) then p:=0;
  sendCommand('STOR '+b,false);
  if beginXfer2(i) then begin; xClose(f);exit; end;
  xSeek(f,p);
  sendFile(data,f,true);
  pipeLineClose(data);
  goto f1;
  end;
if (a='get') then begin;
  p:=restMode;
  restMode:=0;
  if beginXfer1 then exit;
  sendCommand('REST '+BStr(p),false);
  a:=getResponse;
  if analResponse3(a) then p:=0;
  a:=GetNextWord(b,' ');
  sendCommand('RETR '+a,false);
  if (b='') then begin;
    b:=a;kicserel('/','\',b);
    b:=xFileName(b,2)+xFileName(b,3);
    end;
  if beginXfer2(o) then exit;
  xCreate(b);
  if (xOpen(f,b,xGenFilMod_rw)<>0) then begin;
    writeln('error opening '+b+'!');
    exit;
    end;
  xSeek(f,p);
  getFile(data,f,o,true);
  goto f1;
  end;
if (a='get2') then begin;
  p:=restMode;
  restMode:=0;
  a:=GetNextWord(b,' ');
  if (xOpen(f,a,xGenFilMod_r)<>0) then begin;
    writeln('error opening '+a+'!');
    exit;
    end;
  if beginXfer1 then begin; xClose(f);exit; end;
  sendCommand('REST '+BStr(p),false);
  a:=getResponse;
  if analResponse3(a) then p:=0;
  sendString('RETR ');
  sendFile(pipe,f,false);
  Write('...anyfile...');
  sendString(#13#10);
  if beginXfer2(o) then exit;
  xCreate(b);
  if (xOpen(f,b,xGenFilMod_rw)<>0) then begin;
    writeln('error opening '+b+'!');
    exit;
    end;
  xSeek(f,p);
  getFile(data,f,o,true);
  goto f1;
  end;
if (a='get3') then begin;
  p:=restMode;
  restMode:=0;
  if beginXfer1 then exit;
  sendCommand('REST '+BStr(p),false);
  a:=getResponse;
  if analResponse3(a) then p:=0;
  a:=GetNextWord(b,' ');
  sendCommand('RETR '+b,false);
  if beginXfer2(o) then exit;
  xCreate(a);
  if (xOpen(f,a,xGenFilMod_rw)<>0) then begin;
    writeln('error opening '+a+'!');
    exit;
    end;
  xSeek(f,p);
  getFile(data,f,o,true);
  goto f1;
  end;
if (a='dir2') then begin;
  restMode:=0;
  if beginXfer1 then exit;
  sendCommand('REST 0',false);
  a:=getResponse;
  a:=GetNextWord(b,' ');
  if (b<>'') then b:=' '+b;
  sendCommand('LIST'+b,false);
  if beginXfer2(o) then exit;
  xCreate(a);
  if (xOpen(f,a,xGenFilMod_rw)<>0) then begin;
    writeln('error opening '+b+'!');
    exit;
    end;
  getFile(data,f,o,true);
  pipeLineClose(data);
  goto f1;
  end;

WriteLn('unknown command: '+a+' '+b);
End;






Label f1;
Var
  a:String;
  t:xtText;
BEGIN;
WriteLn('ftp client v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');
if DNSstartResolver then immErr('failed to find dns process!');

restMode:=0;
xferMode:=2;
convMode:=2;
pipe:=0;
data:=0;
bind:=0;
inpPos:=0;
inpSiz:=0;

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