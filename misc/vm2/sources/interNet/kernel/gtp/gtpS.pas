{$define debug}
{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc random.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Const proggyName='gtp server v1.0';

{$include memory.inc}
{$include config.inc}
{$include gtp.inc}
{$include gtpS.inc}

Procedure gotIncoming(p:LongInt);
Var
  i,o:LongInt;
  a:String;
Begin;
o:=FindConnectByStat(2);
if (o<1) then begin;
  {$ifdef debug}
  WriteLn('refused connection from upper layer...');
  {$endif}
  pipeLineClose(p);
  exit;
  end;
ConnectionDat^[o].pipe:=p;
ConnectionDat^[o].stat:=3;
a:=ipAddr2string(ConnectionDat^[o].ctrl)+' '+BStr(ctrlPrt);
a:='12341234'#0#0#0#0#0#0#0#0+#11#255'gtp with '+a+#0;
i:=1;move(i,a[1],sizeof(i));
i:=1400;move(i,a[5],sizeof(i));
pipeLineSend(p,a[1],length(a));
{$ifdef debug}
WriteLn('accepted connection from upper layer...');
{$endif}
End;



Label f1;
Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn(proggyName+', done by Mc at '#%date' '#%time'.');
Randomize;
if TCPfindProcess then immErr('failed to find tcp process!');

a:=paramStr(1);
if (a='') then immErr('using: gtp.code <config>');
ReadUpConfig(a);

if (ctrlPrt=0) then ctrlPrt:=2123;
if UDPlistenOnPort(ctrlPip,65536,ctrlAdr,ctrlPrt) then immErr('failed to open listening!');
WriteLn('listening on '+ipAddr2string(ctrlAdr)+' '+BStr(ctrlPrt)+'...');

if (dataPrt=0) then dataPrt:=2152;
if UDPlistenOnPort(dataPip,65536,dataAdr,dataPrt) then immErr('failed to open listening!');
WriteLn('listening on '+ipAddr2string(dataAdr)+' '+BStr(dataPrt)+'...');

pipeLineBegListen;
BugOS_SignDaemoning;
nextTeid:=1;

f1:
relequish;
timer2start;
while (pipeLineGetIncoming(o)=0) do gotIncoming(o);
doLowerD;
doLowerC;
for i:=1 to ConnectionNum do if doConn(ConnectionDat^[i]) then clearOneConnectionRecord(ConnectionDat^[i]);
goto f1;
END.
