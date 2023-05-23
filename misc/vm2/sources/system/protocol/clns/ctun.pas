{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$sysinc hex.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Type
  oneDescriptorRecord=record
    adrD:array[1..128] of byte;
    adrS:LongInt;
    pipe:LongInt;
    proc:LongInt;
    end;
Var
  tunnelDat:array[1..16] of oneDescriptorRecord;
  tunnelNum:LongInt;
  localAdrS:LongInt;
  localAdrD:array[1..128] of byte;
  localPort:LongInt;
  upperPipe:LongInt;


Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;

Function netaddr2str(var buffer;siz:LongInt):String;
Var
  buf:array[1..1] of byte absolute buffer;
  a:String;
  i:LongInt;
Begin;
a:='';
for i:=1 to siz do begin;
  if (i and 1=0) then a:=a+'.';
  a:=a+byte2hextype(buf[i]);
  end;
netaddr2str:=a;
End;

Function findOnePeer(var buffer;siz:LongInt):LongInt;
Label f1;
Var
  buf:array[1..1] of byte absolute buffer;
  i,o:LongInt;
Begin;
for o:=1 to tunnelNum do begin;
  if (tunnelDat[o].adrS<>siz) then continue;
  for i:=1 to siz do if (buf[i]<>tunnelDat[o].adrD[i]) then continue;
  goto f1;
  end;
o:=0;
f1:
findOnePeer:=o;
End;



Label f1;
Var
  a,b:String;
  i,o,p,q:LongInt;
  buf:array[1..1024*8] of byte;
BEGIN;
WriteLn('clnp tunnel v1.0, done by Mc at '#%date' '#%time'.');

localPort:=BVal(paramStr(1));
if (localPort=0) then localPort:=$cc;
fillchar(tunnelDat,sizeof(tunnelDat),0);
tunnelNum:=0;
for p:=2 to paramCount do begin;
  a:=paramStr(p);
  kicserel('.','',a);
  o:=0;
  while (a<>'') do begin;
    inc(o);
    buf[o]:=BVal('$'+copy(a,1,2));
    a:=copy(a,3,666);
    end;
  if (o<2) then continue;
  if (buf[o]=0) then buf[o]:=localPort;
  inc(tunnelNum);
  tunnelDat[tunnelNum].adrS:=o;
  move(buf,tunnelDat[tunnelNum].adrD,o);
  end;
if (tunnelNum<1) then immErr('using: ctun.code <port> <peer> [peer]...');

TCPprocessId:=BugOS_findProcNam('clnp.code');
if (TCPprocessId=0) then immErr('process not found!');
i:=localPort;
if UDPlistenOnPort(upperPipe,65536,buf,i) then immErr('error opening pipeline!');
for i:=1 to 16 do relequish;
i:=sizeof(buf);
pipeLineRecv(upperPipe,buf,i);
if (i<1) then immErr('error reading local address!');
localAdrS:=buf[1];
move(buf[2],localAdrD,localAdrS);
WriteLn('local port: '+BStr(localPort));
WriteLn('local address: '+netaddr2str(localAdrD,localAdrS));
Write('peers:');
for i:=1 to tunnelNum do Write(' '+netaddr2str(tunnelDat[i].adrD,tunnelDat[i].adrS));
WriteLn('');

pipeLineBegListen;
BugOS_SignDaemoning;

f1:
o:=sizeof(buf);
if (pipeLineRecv(upperPipe,buf,o)=0) then begin;
  i:=buf[1];
  p:=findOnePeer(buf[2],i);
  if (p<1) then begin;
    WriteLn('got from '+netaddr2str(buf[2],i));
    goto f1;
    end;
  if (tunnelDat[p].pipe=0) then goto f1;
  pipeLineSend(tunnelDat[p].pipe,buf[i+1],o-i);
  goto f1;
  end;
while (pipeLineGetIncoming(p)=0) do begin;
  pipeLineStats(p,q,i,i);
  BugOS_ProcessName(q,buf,i,i,o);
  if (o and $40=0) then begin; pipeLineClose(p);break; end;
  o:=0;
  for i:=1 to tunnelNum do if (tunnelDat[i].pipe=0) then begin; o:=i;break; end;
  if (o=0) then begin; pipeLineClose(p);break; end;
  tunnelDat[o].pipe:=p;
  tunnelDat[o].proc:=q;
  b:=netaddr2str(tunnelDat[o].adrD,tunnelDat[o].adrS);
  a:='clns tunnel with '+b;
  a:='12341234'#0#0#0#0#0#0#0#0+chr(o)+#255+a+#0;
  i:=1;move(i,a[1],sizeof(i));
  i:=1400;move(i,a[5],sizeof(i));
  pipeLineSend(tunnelDat[o].pipe,a[1],length(a));
  WriteLn('tunnel '+b+' logged in!');
  end;
for q:=1 to tunnelNum do begin;
  if (tunnelDat[q].pipe=0) then continue;
  p:=tunnelDat[q].adrS;
  o:=sizeof(buf);
  if (pipeLineRecv(tunnelDat[q].pipe,buf[p+1],o)<>0) then o:=0;
  if (o<1) then begin;
    pipeLineStats(tunnelDat[q].pipe,o,i,i);
    if (o<>0) then continue;
    pipeLineClose(tunnelDat[q].pipe);
    tunnelDat[q].pipe:=0;
    WriteLn('tunnel '+netaddr2str(tunnelDat[q].adrD,tunnelDat[q].adrS)+' logged out!');
    continue;
    end;
  move(tunnelDat[q].adrD,buf[2],p);
  buf[1]:=p;
  pipeLineSend(upperPipe,buf,o+p);
  end;
relequish;
goto f1;
END.