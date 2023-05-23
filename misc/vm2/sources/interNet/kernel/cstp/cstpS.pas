{{$define debug}
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

Const proggyName='cstp server v1.0';

{$include memory.inc}
{$include config.inc}
{$include cstp.inc}
{$include cstpS.inc}

Procedure gotIncoming(p:LongInt);
Var
  i,o:LongInt;
  a:String;
Begin;
o:=FindConnectByStat(5);
if (o<1) then begin;
  {$ifdef debug}
  WriteLn('refused connection from upper layer...');
  {$endif}
  pipeLineClose(p);
  exit;
  end;
ConnectionDat^[o].pipU:=p;
a:=ipAddr2string(ConnectionDat^[o].addr)+' '+BStr(ConnectionDat^[o].port);
a:='12341234'#0#0#0#0#0#0#0#0+#11#255'cstp with '+a+#0;
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
  i,o,p:LongInt;
  buf:array[1..2*1024] of byte;
  con:OneConnectionRecord;
BEGIN;
WriteLn(proggyName+', done by Mc at '#%date' '#%time'.');
Randomize;
if TLSfindProcess then immErr('failed to find tls process!');

a:=paramStr(1);
if (a='') then immErr('using: cstp.code <config>');
ReadUpConfig(a);

if (servPort=0) then servPort:=443;
i:=servPort;
if TCPlistenOnPort(p,65536,servAddr,i) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(servAddr)+' '+BStr(i)+' port...');
servPort:=i;

pipeLineBegListen;
BugOS_SignDaemoning;
timer2start;

f1:
relequish;
timer2start;

while (pipeLineGetIncoming(p)=0) do begin;
  pipeLineStats(p,o,i,i);
  if (o<>TCPprocessId) then begin;
    gotIncoming(p);
    continue;
    end;
  o:=FindConnectByStat(0);
  if (o<1) then begin;
    a:='HTTP/1.1 404 error'#13#10#13#10'out of connection slots!'#13#10;
    pipeLineSend(p,a[1],length(a));
    pipeLineClose(p);
    goto f1;
    end;
  con:=ConnectionDat^[o];
  con.stat:=1;
  con.pipL:=p;
  con.time:=CurrentTime;
  ConnectionDat^[o]:=con;
  end;
for i:=1 to ConnectionNum do if doConn(ConnectionDat^[i]) then begin;
  con:=ConnectionDat^[i];
  pipeLineClose(con.pipL);
  pipeLineClose(con.pipU);
  con.stat:=0;
  con.pipL:=0;
  con.pipU:=0;
  ConnectionDat^[i]:=con;
  end;

goto f1;
END.