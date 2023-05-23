Type
  OnePacketRecord=record
    s:LongInt;
    c:LongInt;
    d:array[1..4*1024] of byte;
    end;
  OneConnectionRecord=record
    pipe:LongInt;               {pipeline id of control connection}
    time:LongInt;               {time of last action}
    adr:OneTCPaddressRecord;    {remote address}
    prt:LongInt;                {remote port}
    stat:LongInt;               {status: 0-disconn, 1-init, 2-waitSize, 3-waitPacket}
                                {4-waitResA, 5-waitResPTR, 6-waitResMX, 7-zoneTx, 8-zoneRx}
                                {100-udpWaitResA, 101-udpWaitResPTR, 102-udpWaitResMX}
                                {200-connect, 201-soaSize, 202-soaData, 203-axfrSize}
                                {204-axfrData}
    quest:String;               {the question}
    origQ:String;               {original question}
    origI:LongInt;              {original identificatino}
    origF:LongInt;              {proginal flags value}
    fileH:xFile;                {file handle}
    fileHH:xFile;               {file handle}
    fileP:LongInt;              {file position}
    fileS:LongInt;              {file size}
    zoneBeg:LongInt;            {beginning zone}
    zoneCur:LongInt;            {current zone}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  lastSent:LongInt;
  serverPort:LongInt;
  serverAddr:OneTCPaddressRecord;
  listenPipeTcp:LongInt;
  listenPipeUdp:LongInt;
  TempPath:String;
  ZonesNum:LongInt;
  FrwrdReq:Boolean;
  fershIntrvl:LongInt;
  LastZoneTry:LongInt;
  LastZoneNum:LongInt;


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

Function xLevesz(a:String;comment:Boolean):String;
Var i:byte;
Begin;
i:=pos(';',a);
if comment then if (i<>0) then a:=copy(a,1,i-1);
Kicserel(#0,' ',a);
Kicserel(#255,' ',a);
Kicserel(#9,' ',a);
a:=' '+a+' ';
Kicserel('  ',' ',a);
a:=copy(a,2,length(a)-2);
xLevesz:=a;
End;

Function GetNextWord(Var a:String):String;
Var i:Word;
Begin;
i:=pos(' ',a);
if (i<1) then i:=666;
GetNextWord:=copy(a,1,i-1);
a:=copy(a,i+1,255);
End;

Function GetNextString(Var a:String):String;
Label f1,f2;
Var c,s:String;
Begin;
if (copy(a,1,1)<>'"') then begin;
  GetNextString:=GetNextWord(a);
  Exit;
  end;
a:=copy(a,2,255);
s:='';
f1:
c:=copy(a,1,1);
a:=copy(a,2,255);
if (c='') then goto f2;
if (c='"') then goto f2;
if (c<>'\') then begin; s:=s+c;goto f1; end;
c:=copy(a,1,1);
a:=copy(a,2,255);
s:=s+c;
goto f1;
f2:
while (copy(a,1,1)=' ') do a:=copy(a,2,255);
GetNextString:=s;
End;

Function GetCurrentDate:String;
Function x(i:LongInt):string;var a:string;Begin;a:=BStr(i);while (length(a)<2) do a:='0'+a;x:=a;End;
Var a1,a2,a3:Word;
Begin;
xGetDate(a1,a2,a3);
GetCurrentDate:=x(a1)+x(a2)+x(a3);
End;

Function GetCurrentTime:String;
Function x(i:LongInt):string;var a:string;Begin;a:=BStr(i);while (length(a)<2) do a:='0'+a;x:=a;End;
Var a1,a2,a3:Word;
Begin;
xGetTime(a1,a2,a3);
GetCurrentTime:=x(a1)+x(a2)+x(a3);
End;


Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;
