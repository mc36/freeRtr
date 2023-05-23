{$heap 511k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}
{$sysinc crypto.inc}
{$sysinc bignum.inc}
{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}
{$include \sources\internet\kernel\utils\keys.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

{$include asn1hdr.inc}
{$include asn1num.inc}
{$include memory.inc}
{$include digsig.inc}
{$include cert.inc}
{$include tls1.inc}
{$include tls2.inc}
{$include tls5.inc}
{$include config.inc}


Label f1;
Var
  con:OneConnectionRecord;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('tls server v1.0, done by Mc at '#%date' '#%time'.');
Randomize;
if TCPfindProcess then immErr('failed to find tcp process!');
if CryptoStartActions then immErr('failed to find crypto process!');
if BigNumStartActions then immErr('failed to find bignum process!');

ConnectionNum:=0;
lastSent:=0;
if pipeLineBegListen then immErr('failed to start listening!');

a:=ParamStr(1);
if (a='') then immErr('using: tls.code <config>');
ReadUpConfig(a);

BugOS_SignDaemoning;
f1:
relequish;
timer2start;

while (pipeLineGetIncoming(p)=0) do begin;
  if ResizeMem(ConnectionNum+1) then begin;
    a:='server is too busy!'#13#10;
    pipeLineSend(p,a[1],length(a));
    WriteLn('failed to allocate memory!');
    pipeLineClose(p);
    goto f1;
    end;
  fillchar(con,sizeof(con),0);
  pipeLineStats(p,o,i,i);
  con.proc:=o;
  if (o=TCPprocessId) then begin;
    con.stat:=2;
    con.pipeT:=p;
    end else begin;
    con.stat:=3;
    con.pipeL:=p;
    end;
  con.time:=CurrentTime;
  ConnectionDat^[ConnectionNum]:=con;
  end;
for i:=ConnectionNum downto 1 do if doConn(ConnectionDat^[i],con,i) then begin;
  con:=ConnectionDat^[i];
  pipeLineClose(con.pipeT);
  pipeLineClose(con.pipeL);
  a:=con.err;
  if (a<>'') then writeln(a+' ('+ipAddr2string(con.addr)+' '+BStr(con.port)+')');
  if (con.secure=2) and (con.sessID<>'') then begin;
    con.stat:=1;
    con.time:=CurrentTime;
    con.pipeT:=0;
    con.pipeL:=0;
    con.secure:=3;
    ConnectionDat^[i]:=con;
    continue;
    end;
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  end;

goto f1;
END.