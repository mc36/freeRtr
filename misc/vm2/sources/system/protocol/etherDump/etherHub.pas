{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}

Type OneEtherAddrRec=array[1..6] of byte;

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;

Function convEtherAddr(var d:OneEtherAddrRec):String;
Var
  i:byte;
  a:String;
Begin;
a:='';
for i:=1 to sizeof(d) do a:=a+'-'+byte2hextype(d[i]);
convEtherAddr:=copy(a,2,255);
End;

Function EthernetOpen(a:String):LongInt;
Var
  buf:array[1..4*1024] of byte;
  ethPipe,ethProc:LongInt;
  d:OneEtherAddrRec;
  i,o:LongInt;
Begin;
WriteLn('  process: '+a);
ethProc:=BugOS_findProcNam(a);
if (ethProc=0) then immErr('process not found!');
if (pipeLineCreate(ethPipe,ethProc,65536,true)<>0) then immErr('unabled to create pipeline!');
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(ethPipe,buf,i)<>0) then i:=0;
if (i<1) then immErr('initial packet not received!');
move(buf[1],i,sizeof(i));
{if (i<>sizeof(OneEtherAddrRec)) then immErr('invalid address length!');}
move(buf[5],i,sizeof(i));
move(buf[9],i,sizeof(i));
move(buf[13],i,sizeof(i));
o:=17;
move(buf[o],d,sizeof(d));inc(o,sizeof(d));
WriteLn('  unicast: '+convEtherAddr(d));
move(buf[o],d,sizeof(d));inc(o,sizeof(d));
WriteLn('  broadcast: '+convEtherAddr(d));
a:='';
while (buf[o]<>0) do begin;
  a:=a+chr(buf[o]);
  inc(o);
  end;
writeLn('  device: "'+a+'"');
EthernetOpen:=ethPipe;
End;



Label f1;
Const portMax=16;
Var
  addrLen:LongInt;
  portDat:array[1..portMax] of LongInt;
  portNum:LongInt;
  buf:array[1..4*1024] of byte;
  i,o,p,q:LongInt;
  a:String;
BEGIN;
WriteLn('ethernet hub v1.0, done by Mc at '#%date' '#%time'.');
addrLen:=BVal(paramStr(1));
portNum:=paramCount-1;
if (portNum>portMax) then portNum:=portMax;
if (portNum<2) then immErr('using: etherHub.code <addrlen> <port1> <port2> [port3]...');
for i:=1 to portNum do begin;
  WriteLn('port #'+BStr(i)+':');
  portDat[i]:=EthernetOpen(paramStr(i+1));
  end;
WriteLn('address length: '+BStr(addrLen));

WriteLn('doing work...');
BugOS_SignDaemoning;

f1:
q:=0;
for p:=1 to portNum do begin;
  o:=sizeof(buf);
  if (pipeLineRecv(portDat[p],buf,o)<>0) then o:=0;
  if (o<1) then begin;
    pipeLineStats(portDat[p],o,i,i);
    if (o=0) then immErr('port #'+BStr(p)+' closed connection!');
    continue;
    end;
  fillchar(buf,addrLen,$ff);
  for i:=1 to portNum do begin;
    if (i=p) then continue;
    pipeLineSend(portDat[i],buf,o);
    end;
  inc(q);
  end;
if (q<1) then relequish;
goto f1;
END.