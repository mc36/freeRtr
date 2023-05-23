Function releq2upper:Boolean;
Label f1,f2,f3;
Var
  buf:array[1..4*1024] of byte;
  buf2:array[1..4*1024] of byte;
  ps,siz,siz2:LongInt;
  cmd,cid,src,trg,via,csq,cnt:String;
  adr:OneTCPaddressRecord;
  prt:LongInt;

Function isMore:Boolean;
Begin;
isMore:=(ps<siz);
End;

Function getLine:String;
Label f1,f2;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
ab0:=0;
f1:
inc(ps);
if (ps>siz) then goto f2;
i:=buf[ps];
if (i=32) and (ab[ab0]=32) then goto f1;
if not (i in [13,10]) then begin;
  inc(ab0);
  ab[ab0]:=i;
  goto f1;
  end;
if (buf[ps+1]=23-i) then inc(ps);
if (buf[ps+1]=32) then goto f1;
f2:
getLine:=a;
End;

Procedure beginReply(a:String);
Begin;
siz:=0;
appendOneLine(buf,siz,'SIP/2.0 '+a);
appendOneLine(buf,siz,'Via: 'via+';received='+ipAddr2string(adr));
appendOneLine(buf,siz,'To: '+trg+';tag=reg'+BStr(currentTime));
appendOneLine(buf,siz,'From: '+src);
appendOneLine(buf,siz,'Call-ID: '+cid);
appendOneLine(buf,siz,'CSeq: '+csq);
appendOneLine(buf,siz,'User-Agent: '+ProggyName+' for BugOS');
appendOneLine(buf,siz,'Allow: REGISTER,MESSAGE,INVITE,NOTIFY,ACK,CANCEL,BYE');
appendOneLine(buf,siz,'Date: '+getDateForHeader);
appendOneLine(buf,siz,'Max-Forwards: 70');
appendOneLine(buf,siz,'Content-Length: 0');
End;

Var
  i,o,p:LongInt;
  a,b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
releq2upper:=false;
siz:=sizeof(buf);
if UDPreceivePacket(localPipe,adr,prt,buf,siz) then exit;
if (siz<2) then exit;
releq2upper:=true;
for i:=1 to 5 do buf[siz+i]:=$61;
for i:=1 to siz do if (buf[i] in [0,7,9,255]) then buf[i]:=32;
{$ifdef debug}
dumpOneMessage('got from '+ipAddr2string(adr)+' '+BStr(prt),buf,siz);
{$endif}
ps:=0;
cmd:=getLine;
kicserel('  ',' ',cmd);
cid:='';
src:='';
trg:='';
via:='';
csq:='';
cnt:='';
f1:
b:=getLine;
if (b='') then goto f2;
kicserel('  ',' ',b);
i:=pos(':',b);
a:=kicsi(xLevesz(copy(b,1,i-1)));
b:=xLevesz(copy(b,i+1,666));
if (a='from') then src:=b;
if (a='to') then trg:=b;
if (a='via') then via:=b;
if (a='cseq') then csq:=b;
if (a='call-id') then cid:=b;
if (a='contact') then cnt:=b;
goto f1;
f2:
b:=kicsi(cmd);
a:=kicsi(getWord(b));
if (a='subscribe') then goto f3;
if (a='register') then begin;
  WriteLn('got register from '+ipAddr2string(adr)+' '+BStr(prt));
  o:=findOneUserName(getUserName(src));
  if (o<1) then begin;
    f3:
    WriteLn('sending notallowed to '+ipAddr2string(adr)+' '+BStr(prt));
    beginReply('405 method not allowed');
    appendOneLine(buf,siz,'');
    UDPsendPacket(localPipe,adr,prt,buf,siz);
    {$ifdef debug}
    dumpOneMessage('sent to '+ipAddr2string(adr)+' '+BStr(prt),buf,siz);
    {$endif}
    exit;
    end;
  if (ConnectionDat^[o].stat<2) then begin;
    ConnectionDat^[o].time:=currentTime;
    ConnectionDat^[o].stat:=1;
    ConnectionDat^[o].addr:=adr;
    ConnectionDat^[o].port:=prt;
    end;
  beginReply('200 ok');
  appendOneLine(buf,siz,'Expires: 300');
  if (cnt<>'') then appendOneLine(buf,siz,'Contact: '+cnt);
  appendOneLine(buf,siz,'');
  UDPsendPacket(localPipe,adr,prt,buf,siz);
  {$ifdef debug}
  dumpOneMessage('sent to '+ipAddr2string(adr)+' '+BStr(prt),buf,siz);
  {$endif}
  exit;
  end;
if (copy(a,1,4)<>'sip/') then begin;
  o:=findOneUserName(getUserName(src));
  if (o<1) then goto f3;
  if (ConnectionDat^[o].stat<1) then goto f3;
  ConnectionDat^[o].time:=currentTime;
  if (prt<>ConnectionDat^[o].port) then goto f3;
  if not TCPcompareAddress(adr,ConnectionDat^[o].addr) then goto f3;
  p:=findOneUserName(getUserName(trg));
  if (p<1) then p:=findOneRouting(getPhoneNumber(trg));
  if (p<1) then goto f3;
  if (ConnectionDat^[p].stat<1) then goto f3;
  adr:=ConnectionDat^[p].addr;
  prt:=ConnectionDat^[p].port;
  siz2:=0;
  ps:=0;
  appendOneLine(buf2,siz2,getLine);
  appendOneLine(buf2,siz2,'Via: SIP/2.0/UDP '+generateMyAddr);
  repeat
    a:=getLine;
    appendOneLine(buf2,siz2,a);
    until (a='');
  i:=siz-ps;
  if (i<1) then i:=0;
  move(buf[ps+1],buf2[siz2+1],i);
  inc(siz2,i);
  UDPsendPacket(localPipe,adr,prt,buf2,siz2);
  {$ifdef debug}
  dumpOneMessage('sent to '+ipAddr2string(adr)+' '+BStr(prt),buf2,siz2);
  {$endif}
  exit;
  end;
p:=findOneUserName(getUserName(trg));
if (p<1) then p:=findOneRouting(getPhoneNumber(trg));
if (p<1) then exit;
if (ConnectionDat^[p].stat<1) then exit;
if (prt<>ConnectionDat^[p].port) then exit;
if not TCPcompareAddress(adr,ConnectionDat^[p].addr) then exit;
ConnectionDat^[p].time:=currentTime;
o:=findOneUserName(getUserName(src));
if (o<1) then exit;
if (ConnectionDat^[o].stat<1) then exit;
adr:=ConnectionDat^[o].addr;
prt:=ConnectionDat^[o].port;
siz2:=0;
ps:=0;
appendOneLine(buf2,siz2,getLine);
b:='via: sip/2.0/udp '+generateMyAddr;
repeat
  a:=getLine;
  if (kicsi(a)=b) then continue;
  appendOneLine(buf2,siz2,a);
  until (a='');
i:=siz-ps;
if (i<1) then i:=0;
move(buf[ps+1],buf2[siz2+1],i);
inc(siz2,i);
UDPsendPacket(localPipe,adr,prt,buf2,siz2);
{$ifdef debug}
dumpOneMessage('sent to '+ipAddr2string(adr)+' '+BStr(prt),buf2,siz2);
{$endif}
End;
