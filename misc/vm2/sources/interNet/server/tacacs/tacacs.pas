{$heap 31k}
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
{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}

{$include \sources\internet\kernel\utils\timer2.inc}
{$include \sources\system\login\login.inc}
{$include \sources\system\login\authenticator.inc}
{$include memory.inc}
{$include tacacs1.inc}
{$include tacacs2.inc}
{$include config.inc}


Label f1;
Var
  a:String;
  i,o,p:LongInt;
  d:OneConnectionRecord;
BEGIN;
WriteLn('tacacs+ server v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');
if pipeLineBegListen then immErr('failed to start listening!');
if CryptoStartActions then immErr('failed to find crypto process!');
CryptoGetHasherList(d,i);
md5algoNum:=CryptoFindOneAlgo(d,i,'md5');
if (md5algoNum<1) then immErr('failed to find md5 algorithm!');

a:=ParamStr(1);
if (a='') then immErr('using: tacacs.code <config>');
ReadUpConfig(a);
logFileDate:=0;
ConnectionNum:=0;

i:=serverPort;
if (i=0) then i:=49;
if TCPlistenOnPort(p,65536,serverAddr,i) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(serverAddr)+' '+BStr(i)+' port...');
serverPort:=i;

BugOS_SignDaemoning;
f1:
relequish;
timer2start;
while (pipeLineGetIncoming(p)=0) do begin;
  if ResizeMem(ConnectionNum+1) then begin;
    WriteLn('failed to allocate memory!');
    pipeLineClose(p);
    goto f1;
    end;
  fillchar(d,sizeof(d),0);
  d.stat:=1;
  d.pipe:=p;
  d.time:=CurrentTime;
  ConnectionDat^[ConnectionNum]:=d;
  end;
for i:=ConnectionNum downto 1 do if doConn(ConnectionDat^[i]) then begin;
  d:=ConnectionDat^[i];
  pipeLineClose(d.pipe);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(d));
  ResizeMem(ConnectionNum-1);
  end;

goto f1;
END.