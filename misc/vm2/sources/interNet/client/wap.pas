{$heap 303k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc param.inc}
{$sysinc datetime.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Const proggyName='wap v1.0';
Var
  pip:LongInt;
  adr:OneTCPaddressRecord;
  prt:LongInt;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;


Function doWTPWSPget(u:String;var buffer):LongInt;
Label f1,f2,f3,f4;
Var
  buf:array[1..4096] of byte;
  ps,siz:longint;
  tid,sid,tim,rtr:longint;


procedure addByte(a:longint);
begin;
inc(siz);
buf[siz]:=a;
end;

procedure addWord(a:longint);
begin;
inc(siz);
buf[siz]:=a shr 8;
inc(siz);
buf[siz]:=a and $ff;
end;

procedure addMint(a:longint);
var
  b:array[1..16] of byte;
  i,o:longint;
begin;
o:=0;
repeat
  inc(o);
  b[o]:=a and $7f;
  a:=a shr 7;
  until (a=0);
for i:=o downto 1 do begin;
  inc(siz);
  buf[siz]:=b[i];
  if (i<>1) then inc(buf[siz],$80);
  end;
end;

function getMint:LongInt;
var i,o:longint;
begin;
o:=0;
repeat
  inc(ps);
  i:=buf[ps];
  o:=(o shl 7) or (i and $7f);
  until (i and $80=0);
getMint:=o;
end;

procedure addStr(a:string);
var
  ab:array[0..255] of byte absolute a;
  i:longint;
begin;
for i:=1 to ab[0] do addByte(ab[i]);
end;


procedure wtpAbort;
var i:longint;
begin;
siz:=0;
addByte($20);
addWord(tid and $7fff);
addByte($08);
end;

procedure wtpInvoke(retrans:boolean;cls:longint);
var i:longint;
begin;
siz:=0;
if retrans then i:=1 else i:=0;
addByte(i or $0a);
addWord(tid and $7fff);
addByte((cls and 3) or $10);
end;

procedure wtpAck(retrans,vrfyok:boolean);
var i:longint;
begin;
siz:=0;
if retrans then i:=1 else i:=0;
if vrfyok then inc(i,4);
addByte(i or $18);
addWord(tid and $7fff);
end;

procedure wspConnect;
begin;
addByte($01);
addByte($10);
addByte($00);
addByte($00);
addByte($04);addByte($80);
addMint($ffff);
addByte($04);addByte($81);
addMint($ffff);
buf[7]:=siz-8;
end;

procedure wspDisconnect;
begin;
addByte($05);
addMint(sid);
end;

procedure wspGet;
begin;
addByte($40);
addByte(length(u));
addStr(u);
addByte($80);addByte($80);
addByte($a9);addStr(proggyName+#0);
end;

function wtpTest:LongInt;
var i:longint;
begin;
wtpTest:=-1;
ps:=0;
if (buf[2]<>(tid shr 8) or $80) then exit;
if (buf[3]<>tid and $ff) then exit;
i:=(buf[1] shr 3) and $f;
ps:=3;
wtpTest:=i;
if (i<>3) then exit;
if (buf[1] and 4=0) then exit;
wtpAck(false,true);
UDPsendPacket(pip,adr,prt,buf,siz);
end;



Var
  i,o,p:LongInt;
  a:string;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
doWTPWSPget:=-1;
timer2start;
tid:=random($fff) and $ff;

writeln('connecting...');
rtr:=0;
f1:
if (rtr>10) then exit;
wtpInvoke((rtr>0),2);
wspConnect;
UDPsendPacket(pip,adr,prt,buf,siz);
inc(rtr);
tim:=currentTime;
f2:
relequish;
timer2start;
if (getTimePast(tim)>5) then goto f1;
siz:=sizeof(buf);
if UDPreceivePacket(pip,a,i,buf,siz) then siz:=0;
if (siz<1) then goto f2;
if (i<>prt) then goto f2;
if not TCPcompareAddress(a,adr) then goto f2;
i:=wtpTest;
if (i=3) then goto f2;
if (i<>2) then goto f2;
inc(ps);
if (buf[ps]=3) then begin;
  inc(ps); {flag}
  inc(ps);p:=buf[ps]; {flags}
  if (p and $80<>0) then inc(ps); {bearer type}
  if (p and $40<>0) then begin; {port}
    prt:=readWordMSB(buf[ps+1]);
    inc(ps,2);
    end;
  case p and $3f of
    4:begin;
      a:=IPv4addressPrefix;
      move(buf[ps+1],ab[ab0+1],p);
      move(ab[1],adr,sizeof(adr));
      end;
    16:move(buf[ps+1],adr,sizeof(adr));
    end;
  inc(ps,p);
  WriteLn('redirected to '+ipAddr2string(adr)+' '+BStr(prt)+'...');
  goto f1;
  end;
if (buf[ps]<>2) then goto f2;
sid:=getMint;

writeln('getting...');
inc(tid);
rtr:=0;
f3:
if (rtr>10) then exit;
dec(tid);
wtpAck(false,false);
inc(tid);
UDPsendPacket(pip,adr,prt,buf,siz);
wtpInvoke((rtr>0),2);
wspGet;
UDPsendPacket(pip,adr,prt,buf,siz);
inc(rtr);
tim:=currentTime;
f4:
relequish;
timer2start;
if (getTimePast(tim)>5) then goto f3;
siz:=sizeof(buf);
if UDPreceivePacket(pip,a,i,buf,siz) then siz:=0;
if (siz<1) then goto f4;
if (i<>prt) then goto f4;
if not TCPcompareAddress(a,adr) then goto f4;
i:=wtpTest;
if (i=3) then goto f4;
if (i<>2) then goto f4;
inc(ps);
if (buf[ps]<>4) then goto f4;
inc(ps);
{if (buf[ps]<>$20) then goto f4;}
i:=getMint;
inc(ps,i);
dec(siz,ps);
if (siz<1) then siz:=0;
move(buf[ps+1],buffer,siz);
doWTPWSPget:=siz;

writeln('disconnecting...');
wtpAck(false,false);
inc(tid);
UDPsendPacket(pip,adr,prt,buf,siz);
wtpInvoke(false,0);
wspDisconnect;
UDPsendPacket(pip,adr,prt,buf,siz);

End;




Var
  b:array[1..1024] of char;
  i,o:longint;
  a:String;
  f:xFile;
BEGIN;
WriteLn(proggyName+', done by Mc at '#%date' '#%time'.');
if (paramCount<3) then immErr('using: wap.code <gw> <port> <url> [file]');
if TCPfindProcess then immErr('failed to find tcp process!');
Randomize;
i:=0;
if UDPlistenOnPort(pip,65536,b,i) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(b)+' '+BStr(i)+' port...');

if string2ipAddr(paramStr(1),adr) then immErr('bad address!');
prt:=BVal(paramStr(2));
if (prt<1) then prt:=9201;
WriteLn('will send to '+ipAddr2string(adr)+' '+BStr(prt)+'...');

a:=paramStr(3);
if (pos('://',a)<1) then a:='http://'+a;
writeLn('will get '+a+'...');
o:=doWTPWSPget(a,b);
if (o<1) then immErr('error!');
writeLn(BStr(o)+' bytes received.');
a:=paramStr(4);
if (a='') then begin;
  for i:=1 to o do write(b[i]);
  halt(0);
  end;
xErase(a);
xCreate(a);
writeLn('writing '+a+'...');
if (xOpen(f,a,xGenFilMod_rw)<>0) then immErr('error opening!');
xBlockWrite(f,b,o);
xClose(f);
writeLn('done!');
END.