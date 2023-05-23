{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc hex.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc memory.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}
{$sysinc crypto.inc}
{$sysinc inet_addr.inc}

{$include \sources\system\login\login.inc}
{$include \sources\internet\kernel\utils\timer2.inc}
{$include ppp.inc}
{$include lcp.inc}
{$include auth.inc}
{$include ipcp6.inc}
{$include ipcp4.inc}


Procedure doUpp(var pck:OnePPPpacketRecord;var s:LongInt);
Label f1,f2,f3;
Var
  pip:LongInt;
  i,o,p:LongInt;
  a,b:String;
Begin;
f1:
if (pipeLineGetIncoming(pip)<>0) then goto f3;
pipeLineStats(pip,p,i,o);
BugOS_ProcessName(p,pck,i,i,o);
if (o and $40=0) then goto f2;
i:=128;
if (pipeLineRecv(pip,b[1],i)<>0) then i:=0;
b[0]:=chr(i);
a:=kicsi(copy(b,1,8));
b:=copy(b,9,255);
case ipVer of
  4:begin;
    if (a='arpadd--') then begin;
      a:='ok';
      goto f2;
      end;
    if (a='arpread-') then begin;
      a:='arpdata'#0#0#0#0;
      goto f2;
      end;
    if (a='param---') then begin;
      b[0]:=#4;
      move(LocIP[13],b[1],4);
      a:='param'+b;
      move(RemIP[13],b[1],4);a:=a+b;
      a:=a+#255#255#255#255;
      move(dns1ip[13],b[1],4);a:=a+b;
      move(dns2ip[13],b[1],4);a:=a+b;
      goto f2;
      end;
    if (a='data----') and (upper=0) then begin;
      a:='data';
      pipeLineSend(pip,a[1],length(a));
      upper:=pip;
      goto f1;
      end;
    end;
  6:begin;
    if (a='adradd--') then begin;
      a:='ok';
      goto f2;
      end;
    if (a='adrread-') then begin;
      a:='adrdata'#0#0#0#0;
      goto f2;
      end;
    if (a='param6--') then begin;
      a:='param6'#0#0#0;
      fillchar(b,sizeof(b),0);
      b[0]:=#16;
      b[1]:=#$fe;
      b[2]:=#$80;
      move(IPCP6ifIdLoc,b[9],sizeof(IPCP6ifIdLoc));a:=a+b;
      move(LocIP[1],b[1],16);a:=a+b;
      move(RemIP[1],b[1],16);a:=a+b;
      a:=a+#255#255#255#255#255#255#255#255#255#255#255#255#255#255#255#255;
      move(dns1ip[1],b[1],16);a:=a+b;
      move(dns2ip[1],b[1],16);a:=a+b;
      goto f2;
      end;
    if (a='data6---') and (upper=0) then begin;
      a:='data6';
      pipeLineSend(pip,a[1],length(a));
      upper:=pip;
      goto f1;
      end;
    end;
  end;

a:='error';
f2:
pipeLineSend(pip,a[1],length(a));
pipeLineClose(pip);
goto f1;
f3:
if (upper=0) then begin; s:=0;exit; end;
s:=sizeof(pck);
if (pipeLineRecv(upper,pck,s)<>0) then s:=0;
if (s>0) then exit;
pipeLineStats(upper,o,i,i);
if (o=0) then immErr('upper level closed connection!');
s:=0;
End;

Procedure recv(var pck:OnePPPpacketRecord;var i:LongInt);
Var o:LongInt;
Begin;
timer2start;
i:=sizeof(pck);
if (pipeLineRecv(pipe,pck,i)<>0) then i:=0;
if (i>0) then exit;
pipeLineStats(pipe,o,i,i);
if (o=0) then immErr('lower level closed connection!');
i:=0;
relequish;
End;



Label lcp,auth,ipcp,main;
Var
  pck:OnePPPpacketRecord;
  a:String;
  i,o,p:LongInt;
BEGIN;
WriteLn('ppp v1.0, done by Mc at '#%date' '#%time'.');
Randomize;
if CryptoStartActions then immErr('failed to find crypto process!');
CryptoGetHasherList(pck,p);
md5algorithm:=CryptoFindOneAlgo(pck,p,'md5');
if (md5algorithm<1) then immErr('failed to find md5 algorithm!');

name:=paramStr(1);
proc:=BVal(name);
if (proc=0) then proc:=BugOS_findProcNam(name);
if (proc<1) then begin;
  WriteLn('using: ppp.code <process> [options]');
  WriteLn('options:');
  WriteLn('  l<localIP>');
  WriteLn('  r<remoteIP>');
  WriteLn('  d<dns1ip>');
  WriteLn('  n<dns2ip>');
  WriteLn('  c<charmap>');
  WriteLn('  u<username>');
  WriteLn('  p<password>');
  WriteLn('  i<ipconfig>');
  WriteLn('  v<ipVersion>');
  WriteLn('  serv');
  Halt(1);
  end;

fillchar(LocIP,sizeof(LocIP),0);
fillchar(RemIP,sizeof(RemIP),0);
fillchar(dns1ip,sizeof(dns1ip),0);
fillchar(dns2ip,sizeof(dns2ip),0);
LocChrMap:=0;
RemChrMap:=$ffffffff;
userName:='';
password:='';
ipConfig:='';
serverMode:=False;
LocMagicNum:=Random($ffffffff);
RemMagicNum:=Random($ffffffff);
ipVer:=4;
for i:=2 to ParamCount do begin;
  a:=paramStr(i);
  if (kicsi(a)='serv') then begin; serverMode:=True;continue; end;
  case LowCase(a[1]) of
    'l':if not string2ipAddr(copy(a,2,255),locIP[1]) then LocIP[0]:=#16;
    'r':if not string2ipAddr(copy(a,2,255),remIP[1]) then remIP[0]:=#16;
    'd':if not string2ipAddr(copy(a,2,255),dns1IP[1]) then dns1IP[0]:=#16;
    'n':if not string2ipAddr(copy(a,2,255),dns2IP[1]) then dns2IP[0]:=#16;
    'c':LocChrMap:=BVal(copy(a,2,255));
    'u':userName:=copy(a,2,255);
    'p':passWord:=copy(a,2,255);
    'i':ipConfig:=copy(a,2,255);
    'v':ipVer:=BVal(copy(a,2,255));
    end;
  end;

if (pipeLineCreate(pipe,proc,65536,true)<>0) then immErr('error creating connection!');
for i:=1 to 16 do relequish;
i:=255;
if (pipeLineRecv(pipe,a[1],i)<>0) then immErr('failed to receive initial data!');
a[0]:=chr(i);
move(a[5],LocRecvUnit,sizeof(LocRecvUnit));
a:=copy(a,19,255);
name:='ppp over '+copy(a,1,pos(#0,a)-1);
WriteLn('name will be: '+name);
RemRecvUnit:=LocRecvUnit;

WriteLn('--- statring negotiation...');
AUTHretryDone:=0;
upper:=0;
pipeLineBegListen;
BugOS_SignDaemoning;

lcp:
LCPstartNow;
while LCPworking do begin;
  doUpp(pck,i);
  recv(pck,i);
  if (i=0) then begin;
    if (GetTimePast(lastSent)<timeoutVal) then continue;
    LCPsendConfReq;
    lastSent:=CurrentTime;
    continue;
    end;
  if (ReadWordMSB(pck.p)<>LCPprotocolNumber) then begin;
    WriteLn('got invalid protocol: '+PPPgetProtocolName(ReadWordMSB(pck.p)));
    continue;
    end;
  if PPPtestPacket(pck,i) then continue;
  LCPgotPacket(pck);
  end;

auth:
AUTHstartNow;
while AUTHworking do begin;
  doUpp(pck,i);
  recv(pck,i);
  if (i=0) then begin;
    if (GetTimePast(lastSent)<timeoutVal) then continue;
    AUTHsendPacket;
    lastSent:=CurrentTime;
    continue;
    end;
  case ReadWordMSB(pck.p) of
    LCPauthPAP,LCPauthCHAP,LCPauthEAP:begin;
      if PPPtestPacket(pck,i) then continue;
      AUTHgotPacket(pck);
      end;
    LCPprotocolNumber:begin;
      if PPPtestPacket(pck,i) then continue;
      LCPgotPacket(pck);
      end;
    else begin;
      WriteLn('got invalid protocol: '+PPPgetProtocolName(ReadWordMSB(pck.p)));
      continue;
      end;
    end;
  end;

ipcp:
IPCP4working:=false;
IPCP6working:=false;
case ipVer of
  4:IPCP4startNow;
  6:IPCP6startNow;
  end;
while IPCP4working or IPCP6working do begin;
  doUpp(pck,i);
  recv(pck,i);
  if (i=0) then begin;
    if (GetTimePast(lastSent)<timeoutVal) then continue;
    case ipVer of
      4:IPCP4sendConfReq;
      6:IPCP6sendConfReq;
      end;
    lastSent:=CurrentTime;
    continue;
    end;
  case ReadWordMSB(pck.p) of
    IPCPV4protocolNumber:if (ipVer=4) then begin;
      if PPPtestPacket(pck,i) then continue;
      IPCP4gotPacket(pck);
      end else WriteLn('got IPCPv4 but not applicable!');
    IPCPV6protocolNumber:if (ipVer=6) then begin;
      if PPPtestPacket(pck,i) then continue;
      IPCP6gotPacket(pck);
      end else WriteLn('got IPCPv6 but not applicable!');
    LCPauthPAP,LCPauthCHAP,LCPauthEAP:begin;
      if PPPtestPacket(pck,i) then continue;
      AUTHgotPacket(pck);
      end;
    LCPprotocolNumber:begin;
      if PPPtestPacket(pck,i) then continue;
      LCPgotPacket(pck);
      end;
    else begin;
      WriteLn('got invalid protocol: '+PPPgetProtocolName(ReadWordMSB(pck.p)));
      continue;
      end;
    end;
  end;

WriteLn('--- all layers up, data follows...');
BugOS_SignDaemoning;
WriteLn('max receive unit: '+BStr(LocRecvUnit));
WriteLn('max transmit unit: '+BStr(RemRecvUnit));
WriteLn('local magic number: '+conv2hex(LocMagicNum,4));
WriteLn('remote magic number: '+conv2hex(RemMagicNum,4));
WriteLn('receive async char map: '+conv2hex(LocChrMap,4));
WriteLn('transmit async char map: '+conv2hex(RemChrMap,4));
WriteLn('local ip address: '+ipAddr2string(LocIP[1]));
WriteLn('remote ip address: '+ipAddr2string(RemIP[1]));
WriteLn('dns1 ip address: '+ipAddr2string(dns1ip[1]));
WriteLn('dns2 ip address: '+ipAddr2string(dns2ip[1]));
WriteLn('---');

buggyTyp:=0;
buggyCnt:=0;
LCPouterEchoes:=0;
lastSent:=-99999;
registerMyself;

main:
doUpp(pck,i);
if (i<>0) then case ipVer of
  4:begin;
    pck.a:=addressField;
    pck.x:=controlField;
    WriteWordMSB(pck.p,IPV4protocolNumber);
    pipeLineSend(pipe,pck,i);
    end;
  6:begin;
    move(pck,o,sizeof(o));
    if (o<>0) then goto main;
    dec(i,16);
    move(pck.d[13],pck.c,i);
    pck.a:=addressField;
    pck.x:=controlField;
    WriteWordMSB(pck.p,IPV6protocolNumber);
    pipeLineSend(pipe,pck,i);
    end;
  end;
recv(pck,i);
if (i=0) then begin;
  if (GetTimePast(lastSent)<timeoutVal*2) then goto main;
  if (LCPouterEchoes>16) then immErr('remote possibly dead!');
  LCPsendEchoReq;
  lastSent:=CurrentTime;
  goto main;
  end;
case ReadWordMSB(pck.p) of
  IPV4protocolNumber:if (ipVer=4) then begin;
    move(RemIP[13],pck,4);
    pipeLineSend(upper,pck,i);
    buggyTyp:=0;
    lastSent:=CurrentTime;
    LCPouterEchoes:=0;
    goto main;
    end else WriteLn('got IPv4 but not applicable!');
  IPV6protocolNumber:if (ipVer=6) then begin;
    move(pck.c,pck.d[13],i);
    inc(i,16);
    move(RemIP[1],pck.c,16);
    o:=0;
    move(o,pck,sizeof(o));
    pipeLineSend(upper,pck,i);
    buggyTyp:=0;
    lastSent:=CurrentTime;
    LCPouterEchoes:=0;
    goto main;
    end else WriteLn('got IPv6 but not applicable!');
  IPCPV4protocolNumber:if (ipVer=4) then begin;
    if PPPtestPacket(pck,i) then goto main;
    if not IPCP4gotPacket(pck) then goto main;
    if (buggyTyp<>3) then begin;
      buggyTyp:=3;
      buggyCnt:=0;
      end else begin;
      inc(buggyCnt);
      if (buggyCnt>5) then goto lcp;
      end;
    goto main;
    end else WriteLn('got IPCPv4 but not applicable!');
  IPCPV6protocolNumber:if (ipVer=6) then begin;
    if PPPtestPacket(pck,i) then goto main;
    if not IPCP6gotPacket(pck) then goto main;
    if (buggyTyp<>3) then begin;
      buggyTyp:=3;
      buggyCnt:=0;
      end else begin;
      inc(buggyCnt);
      if (buggyCnt>5) then goto lcp;
      end;
    goto main;
    end else WriteLn('got IPCPv6 but not applicable!');
  LCPauthPAP,LCPauthCHAP,LCPauthEAP:begin;
    if PPPtestPacket(pck,i) then goto main;
    AUTHgotPacket(pck);
    if (buggyTyp<>2) then begin;
      buggyTyp:=2;
      buggyCnt:=0;
      end else begin;
      inc(buggyCnt);
      if (buggyCnt>5) then goto auth;
      end;
    goto main;
    end;
  LCPprotocolNumber:begin;
    if PPPtestPacket(pck,i) then goto main;
    if not LCPgotPacket(pck) then goto main;
    if (buggyTyp<>1) then begin;
      buggyTyp:=1;
      buggyCnt:=0;
      end else begin;
      inc(buggyCnt);
      if (buggyCnt>5) then goto lcp;
      end;
    goto main;
    end;
  else begin;
    WriteLn('got invalid protocol: '+PPPgetProtocolName(ReadWordMSB(pck.p)));
    LCPsendProtoRej(pck,i);
    goto main;
    end;
  end;

goto main;
END.