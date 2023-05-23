Var
  myPath:String;        {path of myself}
  ConnectConn1:LongInt; {data connection}
  ConnectConn2:LongInt; {control connection}
  ConnectType:LongInt;  {connection type}
  ConnectSndr:LongInt;  {connection sender: 0=raw, 1=udp}
  ConnectHost:String;   {hostname of remote}
  ConnectAddr:OneTCPaddressRecord;      {address of remote}
  ConnectPort:LongInt;  {port on remote}
  ConnectEmuN:String;   {terminal emulator name}
  ConnectEmu1:LongInt;  {terminal emulator pipeline}
  ConnectEmu2:LongInt;  {terminal emulator pipeline}
  ConnectEmuP:LongInt;  {terminal emulator process}
Const escapeKey=$8605;
Var
  TerminalScr:screen_bufferRec;
  TerminalCpE:Boolean;
  TerminalCpF:xFile;




Procedure doFinalClear;
Begin;
scr_load(TerminalScr);
scr_fresh;
TextColor($07);
gotoXY(1,screenSizY);
WriteLn('');
End;


Procedure showError(a:String);
Var x,y:LongInt;
Begin;
scr_load(TerminalScr);
scr_window(x,y,'error',length(a),1,$47,$4c);
scr_write(x,y,$4f,a);
currntPosX:=x;
currntPosY:=y;
scr_fresh;
ReadKey;
End;



Procedure sendOneKey(w:Word);
Begin;
pipeLineSend(ConnectEmu2,w,sizeof(w));
End;




Function openTerminalEmu:Boolean;
Label f1,f2,err;
Var
  i,o,p:LongInt;
Begin;
openTerminalEmu:=True;
pipeLineClose(ConnectEmu1);
pipeLineClose(ConnectEmu2);
BugOS_KillProcess(ConnectEmuP);
if (pipeLineBegListen<>0) then exit;
if (xExecBgnd(ConnectEmuN,'',ConnectEmuP)<>0) then exit;
f1:
relequish;
if not BugOS_ProcessExists(ConnectEmuP) then begin;
  err:
  pipeLineEndListen;
  pipeLineClose(ConnectEmu1);
  pipeLineClose(ConnectEmu2);
  BugOS_KillProcess(ConnectEmuP);
  ConnectEmu1:=0;
  ConnectEmu2:=0;
  ConnectEmuP:=0;
  exit;
  end;
if (pipeLineGetIncoming(ConnectEmu1)<>0) then goto f1;
pipeLineStats(ConnectEmu1,o,i,i);
if (o<>ConnectEmuP) then begin;
  pipeLineClose(ConnectEmu1);
  goto f1;
  end;
i:=screenSizX;pipeLineSend(ConnectEmu1,i,sizeof(i));
i:=screenSizY;pipeLineSend(ConnectEmu1,i,sizeof(i));
f2:
relequish;
if not BugOS_ProcessExists(ConnectEmuP) then goto err;
if (pipeLineGetIncoming(ConnectEmu2)<>0) then goto f2;
pipeLineStats(ConnectEmu2,o,i,i);
if (o<>ConnectEmuP) then begin;
  pipeLineClose(ConnectEmu2);
  goto f2;
  end;
pipeLineEndListen;
for i:=1 to 16 do relequish;
openTerminalEmu:=False;
End;








Function openConnection:Boolean;
Label f0,f1,f2;
Const methodMax=16;
Var
  x,y:LongInt;
  buf:array[1..1024] of byte;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  w:Word;
  i,o,p:LongInt;


Procedure sel(c:LongInt);
Var i:LongInt;
begin;
i:=ConnectType+7;
currntPosX:=((i and 3)*12)+x;
currntPosY:=(i shr 2)+y;
scr_color(currntPosX,currntPosY,c,12);
End;

Procedure stat(a:String);
Const max=38;
Begin;
currntPosY:=y+10;
currntPosX:=length(a)+x+10;
a:=copy(a+dup(max,' '),1,max);
scr_write(x+10,currntPosY,$1c,a);
End;

Function doResolv(allowDNS:Boolean):Boolean;
Label f1,vege;
Var i:LongInt;
Begin;
doResolv:=True;
stat('failed to convert address...');
if not string2ipAddr(ConnectHost,ConnectAddr) then goto vege;
if not allowDNS then exit;
stat('dns process not found!');
if DNSstartResolver then exit;
stat('resolving...');
scr_fresh;
stat('failed to resolve address...');
DNSresolvePut(1,ConnectHost);
f1:
relequish;
i:=DNSresolveGet(ConnectHost,buf);
move(buf,ConnectAddr,sizeof(ConnectAddr));
if (i=0) then goto f1;
if (i and $80<>0) then exit;
vege:
stat('connecting...');
scr_fresh;
stat('failed to connect remote...');
doResolv:=False;
End;

Function waitConnect:Boolean;
Var
  a:String;
  i,o:LongInt;
Begin;
waitConnect:=True;
while TCPlookConnected(connectConn1,a,i,o) do begin;
  if (connectConn1=0) then exit;
  relequish;
  end;
waitConnect:=False;
End;



Begin;
openConnection:=True;
a:=GetMyFullFileName;
x:=0;
for y:=1 to ab0 do if (ab[y]=$5c) then x:=y;
myPath:=copy(a,1,x);
ConnectType:=1;
ConnectEmu1:=0;
ConnectEmu2:=0;
ConnectEmuP:=0;
TerminalCpE:=false;
fillchar(ConnectAddr,sizeof(ConnectAddr),0);
f0:
scr_window(x,y,'connection',48,12,$17,$1b);
scr_write(x,y+1,$1f,'type of connection:');
scr_write(x,y+2,$1a,' tcp         ssh         tls         udp');
scr_write(x,y+3,$1a,' serial      ipx         x25         interface');
scr_write(x,y+4,$1a,' proxy       block pipe  char pipe   clnp');
scr_write(x,y+5,$1a,' rtp         dccp        dtls        ip.proto');
scr_write(x,y+7,$1f,'    host:');
scr_write(x,y+8,$1f,'    port:');
scr_write(x,y+9,$1f,'terminal:');
scr_write(x,y+10,$1f,'  status:');
stat('setup...');
f1:
sel($70);
scr_fresh;
w:=ReadKey;
sel($1a);
case w of
  $8001:begin; scr_clear;goto f0; end;
  $8004:goto f2;{enter}
  $8005:exit;{esc}
  $0020:inc(ConnectType);{space}
  $8003:dec(ConnectType);{backspace}
  $800c:dec(ConnectType,4);{up}
  $800d:inc(ConnectType,4);{down}
  $800e:dec(ConnectType);{left}
  $800f:inc(ConnectType);{right}
  end;
if (ConnectType<1) then ConnectType:=methodMax;
if (ConnectType>methodMax) then ConnectType:=1;
goto f1;
f2:
sel($70);
case ConnectType of
  1:begin; ConnectHost:='localhost';ConnectPort:=23; end;
  2:begin; ConnectHost:='localhost';ConnectPort:=23; end;
  3:begin; ConnectHost:='localhost';ConnectPort:=992; end;
  4:begin; ConnectHost:='localhost';ConnectPort:=23; end;
  5:begin; ConnectHost:='serial.code';ConnectPort:=1; end;
  6:begin; ConnectHost:='0::0';ConnectPort:=1024; end;
  7:begin; ConnectHost:='0::0';ConnectPort:=0; end;
  8:begin; ConnectHost:='hdlc.code';ConnectPort:=0; end;
  9:begin; ConnectHost:='localhost';ConnectPort:=23; end;
  10:begin; ConnectHost:='';ConnectPort:=0; end;
  11:begin; ConnectHost:='';ConnectPort:=0; end;
  12:begin; ConnectHost:='';ConnectPort:=0; end;
  13:begin; ConnectHost:='localhost';ConnectPort:=0; end;
  14:begin; ConnectHost:='localhost';ConnectPort:=23; end;
  15:begin; ConnectHost:='localhost';ConnectPort:=992; end;
  16:begin; ConnectHost:='localhost';ConnectPort:=0; end;
  else exit;
  end;
ConnectEmuN:=myPath+'termIn-ansi.code';
if scr_read(ConnectHost,x+10,y+7,$1a,38) then exit;
a:=BStr(ConnectPort);
if scr_read(a,x+10,y+8,$1a,38) then exit;
ConnectPort:=BVal(a);
if (ConnectPort=0) and (a<>'0') then exit;
if scr_read(ConnectEmuN,x+10,y+9,$1a,38) then exit;

stat('starting terminal emulator...');
scr_fresh;
stat('failed to start terminal emulator!');
if openTerminalEmu then exit;

ConnectSndr:=0;
DNSstartResolver;
case ConnectType of
  1:begin; {tcp}
    stat('tcp process not found!');
    if TCPfindProcess then exit;
    if doResolv(true) then exit;
    TCPbeginConnect(connectConn1,65536,ConnectAddr,ConnectPort);
    if waitConnect then exit;
    end;
  2:begin; {ssh}
    stat('ssh process not found!');
    if SSHfindProcess then exit;
    if doResolv(true) then exit;
    TCPbeginConnect(connectConn1,65536,ConnectAddr,ConnectPort);
    if waitConnect then exit;
    end;
  3:begin; {tls}
    stat('tls process not found!');
    if TLSfindProcess then exit;
    if doResolv(true) then exit;
    TCPbeginConnect(connectConn1,65536,ConnectAddr,ConnectPort);
    if waitConnect then exit;
    end;
  4:begin; {udp}
    stat('tcp process not found!');
    ConnectSndr:=1;
    if TCPfindProcess then exit;
    if doResolv(true) then exit;
    i:=ConnectPort;
    if UDPlistenOnPort(connectConn1,65536,a,i) then exit;
    end;
  5:begin; {serial}
    stat('serial process not found!');
    o:=BugOS_findProcNam(ConnectHost);
    if (o=0) then exit;
    stat('failed to open port!');
    if (pipeLineCreate(connectConn2,o,4096,true)<>0) then exit;
    if (pipeLineCreate(connectConn1,o,65536,false)<>0) then exit;
    i:=ConnectPort-1;
    pipeLineSend(connectConn2,i,sizeof(i));
    pipeLineSend(connectConn2,connectConn1,sizeof(connectConn1));
    for i:=1 to 16 do relequish;
    i:=sizeof(buf);
    if (pipeLineRecv(connectConn2,buf,i)<>0) then i:=0;
    if (i<1) then exit;
    end;
  6:begin; {ipx}
    stat('ipx process not found!');
    ConnectSndr:=1;
    TCPprocessId:=BugOS_findProcNam('ipx.code');
    if (TCPprocessId=0) then exit;
    if doResolv(false) then exit;
    i:=ConnectPort;
    if UDPlistenOnPort(connectConn1,65536,a,i) then exit;
    end;
  7:begin; {x25}
    stat('x25 process not found!');
    TCPprocessId:=BugOS_findProcNam('x25.code');
    if (TCPprocessId=0) then exit;
    if doResolv(false) then exit;
    TCPbeginConnect(connectConn1,65536,ConnectAddr,ConnectPort);
    if waitConnect then exit;
    end;
  8:begin; {interface}
    stat('interface process not found!');
    o:=BugOS_findProcNam(ConnectHost);
    if (o=0) then exit;
    stat('failed to open interface!');
    if (pipeLineCreate(connectConn1,o,65536,true)<>0) then exit;
    for i:=1 to 16 do relequish;
    i:=sizeof(buf);
    if (pipeLineRecv(connectConn1,buf,i)<>0) then i:=0;
    if (i<1) then exit;
    end;
  9:begin; {proxy}
    stat('proxy process not found!');
    TCPprocessId:=BugOS_findProcNam('proxy.code');
    if (TCPprocessId=0) then exit;
    if doResolv(true) then exit;
    TCPbeginConnect(connectConn1,65536,ConnectAddr,ConnectPort);
    if waitConnect then exit;
    end;
  10:begin; {block pipeline}
    stat('process not found!');
    o:=BugOS_findProcNam(ConnectHost);
    if (o=0) then exit;
    stat('failed to open pipeline!');
    if (pipeLineCreate(connectConn1,o,65536,true)<>0) then exit;
    end;
  11:begin; {stream pipeline}
    stat('process not found!');
    o:=BugOS_findProcNam(ConnectHost);
    if (o=0) then exit;
    stat('failed to open pipeline!');
    if (pipeLineCreate(connectConn1,o,65536,false)<>0) then exit;
    end;
  12:begin; {clnp}
    stat('clnp process not found!');
    ConnectSndr:=1;
    TCPprocessId:=BugOS_findProcNam('clnp.code');
    if (TCPprocessId=0) then exit;
    i:=ConnectPort;
    if UDPlistenOnPort(connectConn1,65536,a,i) then exit;
    end;
  13:begin; {rtp}
    stat('rtp process not found!');
    if RTPfindProcess then exit;
    if doResolv(true) then exit;
    RTPbeginConnect(connectConn1,65536,ConnectAddr,ConnectPort);
    if waitConnect then exit;
    end;
  14:begin; {dccp}
    stat('dccp process not found!');
    if DCCPfindProcess then exit;
    if doResolv(true) then exit;
    RTPbeginConnect(connectConn1,65536,ConnectAddr,ConnectPort);
    i:=0;
    pipeLineSend(connectConn1,i,sizeof(i));
    if waitConnect then exit;
    end;
  15:begin; {dtls}
    stat('dtls process not found!');
    if DTLSfindProcess then exit;
    if doResolv(true) then exit;
    RTPbeginConnect(connectConn1,65536,ConnectAddr,ConnectPort);
    if waitConnect then exit;
    end;
  16:begin; {ip protocol}
    stat('tcp process not found!');
    ConnectSndr:=1;
    if TCPfindProcess then exit;
    if doResolv(true) then exit;
    move(ConnectAddr,a,sizeof(ConnectAddr));i:=ConnectPort;
    if ProtoListenOnPort(connectConn1,65536,a,i) then exit;
    end;
  else exit;
  end;
DNSstopResolver;

stat('successful!');
openConnection:=False;
End;



Procedure sendConnection(var buf;siz:LongInt);
Begin;
if (siz<1) then exit;
case ConnectSndr of
  0:begin; {raw}
    pipeLineSend(ConnectConn1,buf,siz);
    end;
  1:begin; {udp}
    UDPsendPacket(ConnectConn1,ConnectAddr,ConnectPort,buf,siz);
    end;
  end;
End;



Procedure recvConnection(var buf;var siz:LongInt);
Var
  a:String;
  i,o:LongInt;
Begin;
case ConnectSndr of
  0:begin; {raw}
    if (pipeLineRecv(ConnectConn1,buf,siz)<>0) then siz:=0;
    end;
  1:begin; {udp}
    if (UDPreceivePacket(ConnectConn1,a,i,buf,siz)<>0) then siz:=0;
    end;
  else siz:=0;
  end;
End;



Function testConnection:LongInt;
Var i,o,p:LongInt;
Begin;
testConnection:=-1;
pipeLineStats(ConnectConn1,i,o,p);
if (i=0) then begin;
  pipeLineClose(ConnectConn1);
  pipeLineClose(ConnectConn2);
  ConnectConn1:=0;
  ConnectConn2:=0;
  exit;
  end;
if (p<1) then p:=0;
testConnection:=p;
End;



Function getConnectionType:String;
Var a:String;
Begin;
case ConnectType of
  1:a:='tcp';
  2:a:='ssh';
  3:a:='tls';
  4:a:='udp';
  5:a:='serial';
  6:a:='ipx';
  7:a:='x25';
  8:a:='interface';
  9:a:='proxy';
  10:a:='block pipeline';
  11:a:='stream pipeline';
  12:a:='clnp';
  13:a:='rtp';
  14:a:='dccp';
  15:a:='dtls';
  16:a:='ip protocol';
  else a:='unknown';
  end;
getConnectionType:=a;
End;
