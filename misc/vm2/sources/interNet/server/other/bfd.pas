{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc param.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;


Type
  oneBFDpacketType=record
    vers:Byte;                  {version/diag}
    flag:Byte;                  {flags}
    mult:Byte;                  {multiplier}
    leng:Byte;                  {length of message}
    myDs:LongInt;               {my discriminator}
    rmDs:LongInt;               {remote discriminator}
    mnTx:LongInt;               {min tx}
    mnRx:LongInt;               {min rx}
    ecRx:LongInt;               {echo rx}
    end;

Label f1;
Var
  pipeUdp,portUdp:LongInt;
  addrUdp:OneTCPaddressRecord;
  lastGot,lastSent,state,discr:LongInt;
  buf:array[1..1024] of byte;
  bfd:oneBFDpacketType absolute buf;
  i,o,p:LongInt;
  a:String;

Procedure sendOne(flg:LongInt);
Begin;
fillchar(bfd,sizeof(bfd),0);
bfd.vers:=$20;
bfd.flag:=flg or state;
bfd.mult:=3;
bfd.leng:=sizeof(bfd);
bfd.myDs:=$02020202;
bfd.rmDs:=discr;
WriteLongMSB(bfd.mnTx,1000000);
WriteLongMSB(bfd.mnRx,1000000);
WriteLongMSB(bfd.ecRx,50000);
lastSent:=currentTime;
UDPsendPacket(pipeUdp,addrUdp,portUdp,bfd,sizeof(bfd));
End;

BEGIN;
WriteLn('bfd v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');

a:=paramStr(1);
if (a='') then immErr('using: bfd.code <host> [port]');
if string2ipAddr(a,addrUdp) then immErr('invalid ip address!');
portUdp:=BVal(paramStr(2));
if (portUdp=0) then portUdp:=3784;
if UDPlistenOnPort(pipeUdp,4096,a,portUdp) then immErr('failed to listen on port!');
WriteLn('local side is '+ipAddr2string(a)+' '+BStr(portUdp)+' port...');

WriteLn('sending to '+ipAddr2string(addrUdp)+' '+BStr(portUdp)+'...');

BugOS_SignDaemoning;
lastSent:=-99999;
lastGot:=-99999;
discr:=0;
state:=$40;

f1:
relequish;
timer2start;
if (lastGot>=0) then if (getTimePast(lastGot)>0) then begin;
  WriteLn('neighbor down!');
  lastGot:=-99999;
  state:=$40;
  discr:=0;
  end;
if (currentTime<>lastSent) then sendOne($20);
p:=sizeof(buf);
if UDPreceivePacket(pipeUdp,a,o,buf,p) then p:=0;
if (p<1) then goto f1;
if (p<>sizeof(bfd)) then goto f1;
if not TCPcompareAddress(a,addrUdp) then goto f1;
discr:=bfd.myDs;
if (lastGot<0) then WriteLn('neighbor up!');
if (bfd.flag shr 6 in [2,3]) then state:=$c0 else state:=$80;
if (bfd.flag and $20<>0) then sendOne($10);
lastGot:=currentTime;
goto f1;
END.