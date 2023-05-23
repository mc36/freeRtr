{$undef debug}
{{$define debug}
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
{$sysinc datetime.inc}
{$sysinc random.inc}
{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}

{$include \sources\internet\kernel\utils\timer2.inc}
{$include \sources\system\login\login.inc}
{$include \sources\system\login\authenticator.inc}
{$include memory.inc}
{$include radius1.inc}
{$include radius2.inc}

Var
  trustUserID:Boolean;
  relaxAuthen:Boolean;
  userIDtoGive:LongInt;
  selfIdentifier:String;
  radiusMethod:LongInt;
  transactSeq:LongInt;
  udpPipe:Longint;



Procedure ReadUpConfig(a:String);
Var t:xtText;

function gnl:string;
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
gnl:=a;
end;

Var
  b:String;
  i:LongInt;
Begin;
WriteLn('reading '+a+'...');
if (xtOpen(t,a,true)<>0) then immErr('error opening!');
if string2ipAddr(gnl,authentingAddr) then immErr('wrong ip address!');
authentingPort:=BVal(gnl);
if (authentingPort=0) then authentingPort:=1812;
sharedSecret:=gnl;
radiusMethod:=BVal(gnl);
if (radiusMethod<0) then radiusMethod:=0;
if (radiusMethod>2) then radiusMethod:=2;
trustUserID:=(gnl='1');
a:=gnl;
userIDtoGive:=BVal(a);
if (userIDtoGive=0) and (a<>'0') then userIDtoGive:=-1;
selfIdentifier:=gnl;
relaxAuthen:=(gnl='1');
xtClose(t);
if (selfIdentifier<>'') then exit;
if (xtOpen(t,'c:\system\localHost.text',true)<>0) then exit;
selfIdentifier:=xtReadLn(t,666);
xtClose(t);
End;




Function doOneRequest(user,pass,info:String;service:LongInt;var uid:LongInt):Longint;
Label f1;
Var
  d:OneRadiusPacketRecord;
  retry,time:LongInt;
  i,o,p:LongInt;
  a,b,c:String;
  ab:array[0..128] of byte absolute a;
  ab0:byte absolute a;
  stateMsg:String;
  eapState:LongInt;
  eapID:LongInt;
  eapStr:String;


Procedure genRandom;
Begin;
for i:=1 to sizeof(requestAuth) do requestAuth[i]:=random($100);
transactSeq:=(transactSeq+1) and $ff;
End;

Procedure beginPacket;
Var i:LongInt;
Begin;
repeat
  i:=sizeof(d);
  if (pipeLineRecv(udpPipe,d,i)<>0) then i:=0;
  until (i=0);
fillchar(d,sizeof(d),0);
move(requestAuth,d.auth,sizeof(requestAuth));
if (stateMsg<>'') then putRadiusAttrib(d,24,stateMsg);
d.id:=transactSeq;
d.cod:=1;
ab0:=sizeof(service);
WriteLongMSB(ab[1],service);
putRadiusAttrib(d,5,a);
putRadiusAttrib(d,87,getAuthenticationServiceName(service));
putRadiusAttrib(d,61,#0#0#0#5);
putRadiusAttrib(d,30,getAuthenticationServiceName(service)+' '+ipAddr2string(accountingAddr)+' '+BStr(accountingPort));
putRadiusAttrib(d,31,info);
putRadiusAttrib(d,32,selfIdentifier);
if trustUserID then putRadiusAttrib(d,89,#0);
End;

Procedure sendPacket;
Begin;
putRadiusAttrib(d,80,#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
WriteWordMSB(d.len,d.siz+OneRadiusPacketHeader);
a:=calcRadiusMsgAuth(d);
move(ab[1],d.dat[d.siz-ab0+1],ab0);
WriteLn('sending '+getRadiusPacketCode(d.cod));
{$ifdef debug}dumpOneRadiusPacket(d,false);{$endif}
UDPsendPacket(udpPipe,authentingAddr,authentingPort,d,d.siz+OneRadiusPacketHeader);
timer2start;
time:=currentTime;
End;

Function recvPacket:LongInt;
Label f1,f2,f3;
Var
  usrid:LongInt;
  authed:Boolean;
Begin;
recvPacket:=4;
fillchar(d,sizeof(d),0);
stateMsg:='';
c:='';
f1:
timer2start;
if (getTimePast(time)>=3) then exit;
i:=sizeof(d);
if UDPreceivePacket(udpPipe,d.adr,d.prt,d,i) then i:=0;
if (i<1) then begin;
  relequish;
  goto f1;
  end;
d.siz:=i-OneRadiusPacketHeader;
i:=0;
if (d.prt<>authentingPort) then inc(i);
if not TCPcompareAddress(d.adr,authentingAddr) then inc(i);
if (i<>0) then begin;
  writeLn('got from bad address: '+ipAddr2string(d.adr)+' '+BStr(d.prt));
  goto f1;
  end;
if (d.siz<1) then begin;
  writeLn('got too short packet!');
  goto f1;
  end;
i:=ReadWordMSB(d.len)-i-OneRadiusPacketHeader;
if (i>d.siz) then begin;
  writeLn('got truncated packet!');
  exit;
  end;
if (d.id<>transactSeq) then begin;
  writeLn('got bad id value!');
  goto f1;
  end;
ab0:=sizeof(requestAuth);
move(d.auth,ab[1],ab0);
move(requestAuth,d.auth,sizeof(requestAuth));
if (a<>calcRadiusRespAuth(d)) then begin;
  writeLn('got bad response authenticator!');
  goto f1;
  end;
WriteLn('received '+getRadiusPacketCode(d.cod));
{$ifdef debug}dumpOneRadiusPacket(d,false);{$endif}
usrid:=userIDtoGive;
authed:=relaxAuthen;
f2:
a:=getRadiusAttrib(d);
if (ab0=0) then goto f3;
i:=ab[1];
a:=copy(a,2,666);
case i of
  80:begin;
    authed:=true;
    i:=d.pos-ab0+1;
    fillchar(d.dat[i],ab0,0);
    b:=calcRadiusMsgAuth(d);
    if (a=b) then goto f2;
    WriteLn('got bad message-authenticator!');
    goto f1;
    end;
  79:c:=a;
  24:stateMsg:=a;
  89:begin;
    usrid:=BVal(a);
    if (usrid=0) and (a<>'0') then usrid:=userIDtoGive;
    goto f2;
    end;
  end;
goto f2;
f3:
if not authed then begin;
  WriteLn('message not authenticated!');
  goto f1;
  end;
if (d.cod=2) then i:=0 else i:=3;
if not trustUserID then usrid:=userIDtoGive;
uid:=usrid;
if (i<>0) then uid:=-1;
recvPacket:=i;
End;


Begin;
doOneRequest:=4;
uid:=-1;
genRandom;
eapState:=0;
retry:=0;
stateMsg:='';
f1:
inc(retry);
if (retry>5) then exit;
beginPacket;
case radiusMethod of
  0:begin; {pap}
    putRadiusAttrib(d,1,user);
    a:=pass;
    while (ab0 and $f<>0) do a:=a+#0;
    putRadiusAttrib(d,2,encdecRadiusPasswd(a,false));
    end;
  1:begin; {chap}
    putRadiusAttrib(d,1,user);
    ab0:=32;
    for i:=1 to ab0 do ab[i]:=random($100);
    putRadiusAttrib(d,60,a);
    p:=random($100);
    a:=chr(p)+pass+a;
    i:=ab0;
    CryptoImmHasher(md5algoNum,'','',ab[1],i);
    ab0:=i;
    putRadiusAttrib(d,3,chr(p)+a);
    end;
  2:begin; {eap}
    if (eapState=0) then a:=#2+chr(random($100))+#255#255#1+user else begin;
      a:=chr(eapID)+pass+eapStr;
      i:=ab0;
      CryptoImmHasher(md5algoNum,'','',a[1],i);
      ab0:=i;
      a:=chr(ab0)+a+user;
      a:=#2+chr(eapID)+#255#255#4+a;
      end;
    WriteWordMSB(ab[3],ab0);
    putRadiusAttrib(d,79,a);
    end;
  end;
sendPacket;
i:=recvPacket;
if (i=4) then goto f1;
if (d.cod=11) then begin;
  a:=c;
  if (ab0<6) then goto f1;
  if (ab[1]<>1) then goto f1;
  if (ab[5]<>4) then goto f1;
  i:=ReadWordMSB(ab[3]);
  if (i>ab0) then goto f1;
  ab0:=i;
  eapID:=ab[2];
  move(ab[6],eapStr,sizeof(eapStr));
  eapState:=1;
  retry:=0;
  goto f1;
  end;
doOneRequest:=i;
End;




Label f1,f2,f3;
Var
  req:authenticateRequestRecord;
  rep:authenticateResponseRecord;
  d:OneRadiusPacketRecord;
  a:String;
  i,o,p:LongInt;
  pip:LongInt;
BEGIN;
WriteLn('authen_radius v1.0, done by Mc at '#%date' '#%time'.');
Randomize;
if TCPfindProcess then immErr('failed to find tcp process!');
if pipeLineBegListen then immErr('failed to start listening!');
if CryptoStartActions then immErr('failed to find crypto process!');
CryptoGetHasherList(d,i);
md5algoNum:=CryptoFindOneAlgo(d,i,'md5');
if (md5algoNum<1) then immErr('failed to find md5 algorithm!');
transactSeq:=random($100);

ReadUpConfig(paramStr(1));
i:=0;
if UDPlistenOnPort(udpPipe,4096,accountingAddr,accountingPort) then immErr('failed to listen on port!');
if (selfIdentifier='') then selfIdentifier:=ipAddr2string(accountingAddr)+' '+BStr(accountingPort);
WriteLn('local side is '+ipAddr2string(accountingAddr)+' '+BStr(accountingPort)+' port...');
WriteLn('will query with '+ipAddr2string(authentingAddr)+' '+BStr(authentingPort)+' port...');
BugOS_SignDaemoning;

f1:
if (pipeLineGetIncoming(pip)<>0) then begin;
  relequish;
  goto f1;
  end;
p:=16;
f2:
dec(p);
if (p<0) then begin;
  f3:
  pipeLineClose(pip);
  goto f1;
  end;
o:=sizeof(req);
if (pipeLineRecv(pip,req,o)<>0) then o:=0;
if (o=0) then begin;
  relequish;
  goto f2;
  end;
if (o<>sizeof(req)) then goto f3;
{$ifdef debug}writeln('user='+req.user+' pass='+req.pass+' info='+req.info+' service='+BStr(req.service));{$endif}
rep.stat:=doOneRequest(req.user,req.pass,req.info,req.service,rep.uid);
{$ifdef debug}writeln('result='+BStr(rep.stat)+' uid='+BStr(rep.uid));{$endif}
pipeLineSend(pip,rep,sizeof(rep));
pipeLineClose(pip);
goto f1;
END.