Type
  OnePacketRecord=record
    s:LongInt;
    c:LongInt;
    d:array[1..4*1024] of byte;
    end;
  OneQuestionRecord=record
    typ:LongInt;
    nam:String;
    end;
  OneResponseRecord=record
    typ:LongInt;
    nam:String;
    dat:String;
    end;
Type
  OneConnectionRecord=record
    quest:String;               {the question}
    reply:String;               {the response}
    stat:LongInt;               {status: 1-wait4quest, 2-nam2adr, 3-adr2nam, 4-nam2mx}
                                {100-cacheADDR, 101-cacheMX}
    time:LongInt;               {time of last action}
    retry:Byte;                 {retry counter}
    server:Byte;                {server number}
    pipe:LongInt;               {time of last action}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  ConnectionAct:LongInt;
  ConnectionNxt:LongInt;
  NameServerList:String;
  lastSent:LongInt;
  lastCleanup:LongInt;
  serverPort:LongInt;
  serverAddr:OneTCPaddressRecord;
  listenPipeUdp:LongInt;
  useIPv6addr:Boolean;
Const
  remotePort=53;

Function ResizeMem(n:LongInt):Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
ResizeMem:=True;
i:=n*sizeof(OneConnectionRecord);
if (ExtendedMemoryResize(p,i)<i) then exit;
ConnectionNum:=n;
ConnectionDat:=p^;
ResizeMem:=False;
End;

Procedure DumpOnePacket(a:String;var d:OnePacketRecord);
Var i:LongInt;
Begin;
Write(a+':');
for i:=1 to d.s do Write(' '+byte2hextype(d.d[i]));
WriteLn('');
End;


Procedure ReadUpNameServerList;
Const min=16;
Var
  a:String;
  ip:OneTCPaddressRecord;
  i:LongInt;
Begin;
NameServerList:='';
a[0]:=chr(sizeof(ip));
for i:=2 to paramCount do begin;
  if string2ipAddr(paramStr(i),ip) then continue;
  move(ip,a[1],sizeof(ip));
  NameServerList:=NameServerList+a;
  end;
ConnectionAct:=BVal(ParamStr(1));
if (ConnectionAct<min) then ConnectionAct:=min;
End;


Function FindInCache4name2addr(nam:string;var rep:OneResponseRecord):Boolean;
Var i:LongInt;
Begin;
rep.nam:=nam;
if not string2ipAddr(nam,rep.dat) then begin;
  FindInCache4name2addr:=True;
  exit;
  end;
nam:=kicsi(nam);
for i:=1 to ConnectionAct do begin;
  if (ConnectionDat^[i].stat<>100) then continue;
  if (ConnectionDat^[i].quest<>nam) then continue;
  move(ConnectionDat^[i].reply,rep.dat,sizeof(rep.dat));
  FindInCache4name2addr:=True;
  exit;
  end;
FindInCache4name2addr:=False;
End;

Function FindInCache4name2mail(nam:string;var rep:OneResponseRecord):Boolean;
Var i:LongInt;
Begin;
rep.nam:=nam;
nam:=kicsi(nam);
for i:=1 to ConnectionAct do begin;
  if (ConnectionDat^[i].stat<>101) then continue;
  if (ConnectionDat^[i].quest<>nam) then continue;
  move(ConnectionDat^[i].reply,rep.dat,sizeof(rep.dat));
  FindInCache4name2mail:=True;
  exit;
  end;
FindInCache4name2mail:=False;
End;

Function FindInCache4addr2name(var adr;var rep:OneResponseRecord):Boolean;
Var i:LongInt;
Begin;
move(adr,rep.dat,sizeof(rep.dat));
for i:=1 to ConnectionAct do begin;
  if (ConnectionDat^[i].stat<>100) then continue;
  if not TCPcompareAddress(ConnectionDat^[i].reply,adr) then continue;
  rep.nam:=ConnectionDat^[i].quest;
  FindInCache4addr2name:=True;
  exit;
  end;
FindInCache4addr2name:=False;
End;
