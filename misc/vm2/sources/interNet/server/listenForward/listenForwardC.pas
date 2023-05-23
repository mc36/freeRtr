{{$define debug}
{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc hex.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

{$include listenForward.inc}

Label f1;
Var
  con:OneConnectionRecord;
  trgAddr:OneTCPaddressRecord;
  trgPort:LongInt;
  i,o,p,q:LongInt;
  a:String;
BEGIN;
WriteLn('listen forward client v1.0, done by Mc at '#%date' '#%time'.');
TCPfindProcess;
Randomize;

string2ipAddr(ParamStr(1),a);
o:=BVal(ParamStr(2));
string2ipAddr(ParamStr(3),trgAddr);
trgPort:=BVal(ParamStr(4));
if (trgPort=0) then immErr('using: listenForwardC.code <srvAddr> <srvPort> <trgAddr> <trgPort>');

WriteLn('will forward to '+ipAddr2string(trgAddr)+' '+BStr(trgPort)+'...');
Write('connecting to '+ipAddr2string(a)+' '+BStr(o)+'...');
TCPbeginConnect(uplinkPipe,65536,a,o);
while TCPlookConnected(uplinkPipe,a,i,o) do begin;
  relequish;
  if (uplinkPipe=0) then immErr(' failed!');
  end;
WriteLn(' ok!');
WriteLn('local side is '+ipAddr2string(a)+' '+BStr(i)+'...');
BugOS_SignDaemoning;

ConnectionCur:=0;
uplinkHdrSiz:=0;
uplinkLast:=0;
ResizeMem(0);

f1:
relequish;
timer2start;
p:=uplinkDoUsualWork(i,o,a);
if (p>=0) then case i of
  3:begin; {open connection}
    p:=findOneConnect(o);
    if (p>0) then begin;
      WriteLn('duplicated connection id, something wrong!');
      uplinkSendFrame(5,o,0,i);
      goto f1;
      end;
    if ResizeMem(ConnectionNum+1) then begin;
      WriteLn('failed to allocate memory!');
      uplinkSendFrame(5,o,0,i);
      goto f1;
      end;
    fillchar(con,sizeof(con),0);
    con.conn:=o;
    con.stat:=2;
    TCPbeginConnect(con.pipe,65536,trgAddr,trgPort);
    ConnectionDat^[ConnectionNum]:=con;
    end;
  end;
if uplinkBufferFull then goto f1;
releq2conn;
goto f1;
END.