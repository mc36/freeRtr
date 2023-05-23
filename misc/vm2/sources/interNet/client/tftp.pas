{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc hex.inc}
{$sysinc crt.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc param.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$sysinc inet_dns.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

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
Write(#13'tftp>');
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




Const
  retryCount=8;
  timeoutVal=3;
  blockSizes=512;
Var
  pipe:LongInt;
  mode:LongInt;                 {1=ascii, 2=binary}
  addr:OneTCPaddressRecord;
  port:LongInt;



Function FlushPipe:Boolean;
Label f1;
Var
  buf:array[1..4*1024] of byte;
  a:OneTCPaddressRecord;
  i,p:LongInt;
Begin;
FlushPipe:=True;
if (pipe=0) then begin;
  writeLn('not connected!');
  exit;
  end;
f1:
i:=sizeof(buf);
if not UDPreceivePacket(pipe,a,p,buf,i) then goto f1;
FlushPipe:=False;
End;


Procedure AnalizeErrorPacket(var buffer);
Var
  buf:array[1..1] of byte absolute buffer;
  a:String;
  i:LongInt;
Begin;
i:=ReadWordMSB(buf[3]);
case i of
  0:a:='not defined, see error message';
  1:a:='file not found';
  2:a:='access violation';
  3:a:='disk full or allocation exceeded';
  4:a:='illegal tftp operation';
  5:a:='unknown transfer id';
  6:a:='file already exists';
  7:a:='no such user';
  else a:='unknown';
  end;
WriteLn('got error packet!');
WriteLn('code: '+BStr(i)+' ('+a+')');
move(buf[5],a[1],sizeof(a));
a[0]:=#255;
a:=copy(a,1,pos(#0,a)-1);
WriteLn('message: '+a);
End;






Function RecvFile(var f:xFile;b:String):Boolean;
Label f1,f2,f3,f4;
Var
  retry,time,size:LongInt;
  sequ,port2:LongInt;
  buf:array[1..4*1024] of byte;
  a:OneTCPaddressRecord;
  i,o,p:LongInt;
  c:String;
Begin;
RecvFile:=True;
if FlushPipe then exit;
sequ:=1;
size:=0;
retry:=retryCount;
time:=-99999;
f1:
p:=sizeof(buf);
if not UDPreceivePacket(pipe,a,port2,buf,p) then begin;
  if not TCPcompareAddress(addr,a) then goto f1;
  i:=ReadWordMSB(buf[1]);
  case i of
    3:goto f3;
    5:AnalizeErrorPacket(buf);
    else WriteLn('got invalid opcode: '+BStr(i));
    end;
  exit;
  end;
relequish;
timer2start;
if (GetTimePast(time)<timeoutVal) then goto f1;
dec(retry);
if (retry<0) then exit;
WriteLn('sending read request to '+b+'...');
WriteWordMSB(buf[1],1);
move(b[1],buf[3],length(b));
i:=length(b)+2;
case mode of
  1:c:=#0'netascii'#0;
  2:c:=#0'octet'#0;
  else exit;
  end;
move(c[1],buf[i+1],length(c));
inc(i,length(c));
UDPsendPacket(pipe,addr,port,buf,i);
time:=currentTime;
goto f1;
f2:
p:=sizeof(buf);
if UDPreceivePacket(pipe,a,i,buf,p) then begin;
  relequish;
  timer2start;
  if (GetTimePast(time)<timeoutVal) then goto f2;
  WriteLn(' timeout!');
  dec(retry);
  if (retry<0) then exit;
  p:=blockSizes;
  goto f4;
  end;
if (i<>port2) then goto f2;
if not TCPcompareAddress(addr,a) then goto f2;
i:=ReadWordMSB(buf[1]);
case i of
  3:goto f3;
  5:AnalizeErrorPacket(buf);
  else WriteLn('got invalid opcode: '+BStr(i));
  end;
goto f2;
f3:
i:=ReadWordMSB(buf[3]);
if (i<>sequ and $ffff) then begin;
  WriteLn(' got invalid sequence!');
  goto f2;
  end;
dec(p,4);
xBlockWrite(f,buf[5],p);
inc(sequ);
retry:=retryCount;
inc(size,p);
f4:
Write(#13+BStr(size));
WriteWordMSB(buf[1],4);
WriteWordMSB(buf[3],sequ-1);
UDPsendPacket(pipe,addr,port2,buf,4);
time:=currentTime;
if (p>=blockSizes) then goto f2;
WriteLn(#13+BStr(size));
RecvFile:=False;
End;






Function SendFile(var f:xFile;b:String):Boolean;
Label f1,f2,f3,f4,f5;
Var
  retry,time,posi,size:LongInt;
  sequ,port2:LongInt;
  txBuf:array[1..4*1024] of byte;
  txSiz:LongInt;
  buf:array[1..4*1024] of byte;
  a:OneTCPaddressRecord;
  i,o,p:LongInt;
  c:String;
Begin;
SendFile:=True;
if FlushPipe then exit;

sequ:=1;
size:=xFileSize(f);
posi:=xFilePos(f);
retry:=retryCount;
time:=-99999;
f1:
p:=sizeof(buf);
if not UDPreceivePacket(pipe,a,port2,buf,p) then begin;
  if not TCPcompareAddress(addr,a) then goto f1;
  i:=ReadWordMSB(buf[1]);
  case i of
    4:goto f5;
    5:AnalizeErrorPacket(buf);
    else WriteLn('got invalid opcode: '+BStr(i));
    end;
  exit;
  end;
relequish;
timer2start;
if (GetTimePast(time)<timeoutVal) then goto f1;
dec(retry);
if (retry<0) then exit;
WriteLn('sending write request to '+b+'...');
WriteWordMSB(buf[1],2);
move(b[1],buf[3],length(b));
i:=length(b)+2;
case mode of
  1:c:=#0'netascii'#0;
  2:c:=#0'octet'#0;
  else exit;
  end;
move(c[1],buf[i+1],length(c));
inc(i,length(c));
UDPsendPacket(pipe,addr,port,buf,i);
time:=currentTime;
goto f1;
f2:
p:=sizeof(buf);
if UDPreceivePacket(pipe,a,i,buf,p) then begin;
  relequish;
  timer2start;
  if (GetTimePast(time)<timeoutVal) then goto f2;
  WriteLn(' timeout!');
  dec(retry);
  if (retry<0) then exit;
  p:=blockSizes;
  goto f4;
  end;
if (i<>port2) then goto f2;
if not TCPcompareAddress(addr,a) then goto f2;
i:=ReadWordMSB(buf[1]);
case i of
  4:goto f3;
  5:AnalizeErrorPacket(buf);
  else WriteLn('got invalid opcode: '+BStr(i));
  end;
goto f2;
f3:
i:=ReadWordMSB(buf[3]);
if (i<>sequ and $ffff) then begin;
  WriteLn(' got invalid sequence!');
  goto f2;
  end;
inc(sequ);
retry:=retryCount;
inc(posi,txSiz);
if (txSiz<blockSizes) then begin;
  WriteLn(#13+BStr(posi));
  SendFile:=False;
  exit;
  end;
f5:
txSiz:=size-posi;
if (txSiz>blockSizes) then txSiz:=blockSizes;
xBlockRead(f,txBuf,txSiz);
f4:
Write(#13+BStr(posi));
WriteWordMSB(buf[1],3);
WriteWordMSB(buf[3],sequ);
move(txBuf,buf[5],txSiz);
UDPsendPacket(pipe,addr,port2,buf,txSiz+4);
time:=currentTime;
goto f2;
End;




Procedure doCommand(b:String);
Var
  f:xFile;
  i,o,p:LongInt;
  a:String;
Begin;
a:=kicsi(GetNextWord(b));
if (a='quit') then halt(0);
if (a='') then exit;
if (a='disconnect') then begin;
  pipeLineClose(pipe);
  pipe:=0;
  exit;
  end;
if (a='connect') then begin;
  pipeLineClose(pipe);
  pipe:=0;
  a:=GetNextWord(b);
  port:=BVal(GetNextWord(b));
  if (port=0) then port:=69;
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
  move(b,addr,sizeof(addr));
  i:=port;
  if UDPlistenOnPort(pipe,4096,b,i) then begin;
    WriteLn('failed to listen on port!');
    pipeLineClose(pipe);
    pipe:=0;
    exit;
    end;
  WriteLn('listening on '+ipAddr2string(b)+' '+BStr(i)+'...');
  WriteLn('will send to '+ipAddr2string(addr)+' '+BStr(port)+'...');
  exit;
  end;
if (a='help') or (a='?') then begin;
  WriteLn(' commands');
  WriteLn('~~~~~~~~~~');
  WriteLn('connect <name> [port]');
  WriteLn('disconnect');
  WriteLn('quit');
  WriteLn('ascii');
  WriteLn('binary');
  WriteLn('get <remote> [local]');
  WriteLn('put <local> [remote]');
  exit;
  end;
if (a='ascii') then begin;
  mode:=1;
  WriteLn('ascii mode selected...');
  exit;
  end;
if (a='binary') then begin;
  mode:=2;
  WriteLn('binary mode selected...');
  exit;
  end;
if (a='put') then begin;
  a:=GetNextWord(b);
  if (b='') then b:=xFileName(a,2)+xFileName(a,3);
  if (xOpen(f,a,xGenFilMod_r)<>0) then begin;
    writeln('error opening '+a+'!');
    exit;
    end;
  if SendFile(f,b) then a:='failed' else a:='successful!';
  xClose(f);
  WriteLn(a);
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
  if RecvFile(f,a) then a:='failed' else a:='successful!';
  xClose(f);
  WriteLn(a);
  exit;
  end;

WriteLn('unknown command: '+a+' '+b);
end;




Label f1;
Var
  a:String;
  t:xtText;
BEGIN;
WriteLn('tftp client v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');
if DNSstartResolver then immErr('failed to find dns process!');

pipe:=0;
mode:=2;

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