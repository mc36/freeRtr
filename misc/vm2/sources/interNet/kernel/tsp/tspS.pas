{$define debug}
{$undef debug}
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

{$include parseXML.inc}
{$include memory.inc}
{$include config.inc}
{$include tspS.inc}

Procedure gotIncoming(pip:LongInt);
Label f1,f2,f3;
Var
  con:OneConnectionRecord;
  i,o,p:LongInt;
  a,b:String;
  buf:array[1..1024] of byte;
Begin;
if (upperCon=0) then begin;
  WriteLn('got unwanted connection from upper layer!');
  goto f1;
  end;
con:=ConnectionDat^[upperCon];
pipeLineStats(pip,p,i,o);
BugOS_ProcessName(p,buf,i,i,o);
if (o and $40=0) then begin;
  {$ifdef debug}
  WriteLn('refused connection from not privilegized process...');
  {$endif}
  goto f1;
  end;
i:=128;
if (pipeLineRecv(pip,b[1],i)<>0) then i:=0;
b[0]:=chr(i);
a:=kicsi(copy(b,1,8));
b:=copy(b,9,255);
{$ifdef debug}
WriteLn('got request from upper layer: '+a);
{$endif}
case con.perV of
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
      move(ip4addr[13],b[1],4);
      a:='param'+b;
      move(con.peer[13],b[1],4);a:=a+b;
      a:=a+#255#255#255#255;
      goto f2;
      end;
    if (a='data----') then begin;
      a:='data';
      goto f3;
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
      a:='param6'#0#0#0+#$fe#$80#0#0#0#0#0#0#0#0#0#0#0#0#0#0;
      b[0]:=#16;
      move(ip6addr,b[1],16);a:=a+b;
      move(con.peer,b[1],16);a:=a+b;
      a:=a+#255#255#255#255#255#255#255#255#255#255#255#255#255#255#255#255;
      goto f2;
      end;
    if (a='data6---') then begin;
      a:='data6';
      goto f3;
      end;
    end;
  end;

a:='error';
f2:
pipeLineSend(pip,a[1],length(a));
f1:
pipeLineClose(p);
exit;
f3:
pipeLineSend(pip,a[1],length(a));
con.pipe:=pip;
con.timR:=currentTime;
con.timT:=currentTime;
con.stat:=4;
ConnectionDat^[upperCon]:=con;
upperCon:=0;
WriteLn('connection opened with '+ipAddr2string(con.addr)+' '+BStr(con.port));
End;

Label f1;
Var
  a:String;
  i,o:LongInt;
  buf:array[1..2*1024] of byte;
BEGIN;
WriteLn('tsp server v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');

a:=paramStr(1);
if (a='') then immErr('using: tsp.code <config>');
ReadUpConfig(a);

if (servPort=0) then servPort:=3653;
if UDPlistenOnPort(servPipe,65536,servAddr,servPort) then immErr('failed to open listening!');
WriteLn('listening on '+ipAddr2string(servAddr)+' '+BStr(servPort)+'...');

pipeLineBegListen;
BugOS_SignDaemoning;
timer2start;

f1:
o:=sizeof(buf);
if not UDPreceivePacket(servPipe,a,i,buf,o) then begin;
  gotOnePacket(a,i,buf,o);
  goto f1;
  end;
relequishUpper;
for i:=1 to ConnectionNum do relequishConn(ConnectionDat^[i],i);
while (pipeLineGetIncoming(i)=0) do gotIncoming(i);
relequish;
timer2start;
goto f1;
END.