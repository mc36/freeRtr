Const
  inputBufMax=64;
  relayListMax=128;
  usersListMax=512;
Type
  OneOutgoingRecord=record
    pipe:LongInt;               {pipeline id of control connection}
    time:LongInt;               {time of last action}
    nam:String;                 {name of remote}
    adr:OneTCPaddressRecord;    {remote address}
    prt:LongInt;                {remote port}
    stat:LongInt;               {status: 0-free, 1-nextRcpt, 2-resolveMX,
                                 3-nextServer, 4-resolveA, 5-connecting,
                                 6-welcome, 7-heloResp, 8-mailResp, 9-rcptResp
                                 10-dataResp, 11-send, 12-ack, 13-quit
                                 14-authCmd, 15-authData}
    cmd:String;                 {command line}
    rsp:LongInt;                {response number}
    rxb:String;                 {receiver buffer}
    fileH:xtText;               {file handle}
    fileN:String;               {file name}
    msgPos:LongInt;             {response message position}
    ibufD:array[1..inputBufMax] of byte;        {input buffer data}
    ibufS:LongInt;              {input buffer size}
    ibufP:LongInt;              {input buffer position}
    currRcpt:LongInt;           {next recipient number}
    source:String;              {source email address}
    target:String;              {target email address}
    servers:String;             {list of remaining servers}
    retry:LongInt;              {retry counter}
    sawNL:Boolean;              {saw new line characters}
    end;
Type
  OneConnectionRecord=record
    pipe:LongInt;               {pipeline id of control connection}
    time:LongInt;               {time of last action}
    adr:OneTCPaddressRecord;    {remote address}
    prt:LongInt;                {remote port}
    stat:LongInt;               {status: 0-disconn, 1-init, 2-cmd, 3-rx, 4-auth}
    remote:String;              {remote server name}
    cmd:String;                 {command line}
    rxb:String;                 {receiver buffer}
    fileH:xFile;                {file handle}
    fileN:String;               {file name}
    ibufD:array[1..inputBufMax] of byte;        {input buffer data}
    ibufS:LongInt;              {input buffer size}
    ibufP:LongInt;              {input buffer position}
    relay:Boolean;              {relay for this client}
    sawNL:Boolean;              {saw new line characters}
    end;
Var
  CurrOutgoing:OneOutgoingRecord;
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  relayListDat:array[1..relayListMax] of record
    adr:OneTCPaddressRecord;
    msk:OneTCPaddressRecord;
    end;
  relayListNum:LongInt;
  relaySelfAddr:Boolean;
  usersListDat:array[1..usersListMax] of record
    emil:String;
    path:String;
    uid:LongInt;
    end;
  usersListNum:LongInt;
  serverPort,serverSPort:LongInt;
  serverAddr:OneTCPaddressRecord;
  lastSent:LongInt;
  domainName:String;
  timeZone:String;
  tempPath:String;
  relayServer:String;
  relayUserPass:String;
Const
  LocalMessageFile='.msg';
  LocalForwardFile='forward';
  LocalAutoreplyFile='autoreply';
  MsgdatExt='.m';
  LocalExt='.l';
  RemoteExt='.r';


Function conv2hex(i:LongInt):String;
Begin;
conv2hex:=byte2hextype(i shr 24)+byte2hextype(i shr 16)+byte2hextype(i shr 8)+byte2hextype(i);
End;

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

Function getWord(var a:String):String;
Var i:LongInt;
Begin;
i:=pos(' ',a);
if (i<1) then i:=666;
getWord:=copy(a,1,i-1);
a:=copy(a,i+1,255);
End;

Function GetDateForHeader:String;
Const MonthNames:array[1..12] of String[3]=('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
Function x(i:LongInt):string;var a:string;Begin;a:=BStr(i);while (length(a)<2) do a:='0'+a;x:=a;End;
Var
  a1,a2,a3:Word;
  a:String;
Begin;
xGetDate(a1,a2,a3);
a:=x(a3)+' '+MonthNames[a2]+' '+x(a1);
xGetTime(a1,a2,a3);
GetDateForHeader:=a+' '+x(a1)+':'+x(a2)+':'+x(a3)+' '+TimeZone;
End;

Function GetDateForFiles:String;
Function x(i:LongInt):string;var a:string;Begin;a:=BStr(i);while (length(a)<2) do a:='0'+a;x:=a;End;
Var
  a1,a2,a3:Word;
  a:String;
Begin;
xGetDate(a1,a2,a3);
a:=x(a1)+x(a2)+x(a3);
xGetTime(a1,a2,a3);
GetDateForFiles:=a+x(a1)+x(a2)+x(a3);
End;

Function TestAddrMask(a1,a2,a3:OneTCPaddressRecord):Boolean;
Var i:LongInt;
Begin;
TestAddrMask:=False;
for i:=1 to sizeof(a1) do if (a1[i] and a3[i]<>a2[i] and a3[i]) then exit;
TestAddrMask:=True;
End;

Function FindEmailAddr(a:String):LongInt;

Function QuickReadOne(n:LongInt):String;
Begin;
QuickReadOne:=usersListDat[n].emil;
End;

Procedure QuickSwapOne(n1,n2:LongInt);Begin;End;

{$sysinc quicksrt.inc}

Begin;
FindEmailAddr:=QuickFind(1,usersListNum,kicsi(a));
End;

Function EmailAddrGetDomain(b:String):String;
Var i,p:LongInt;
Begin;
p:=0;
for i:=1 to length(b) do if (b[i]='@') then p:=i+1;
EmailAddrGetDomain:=copy(b,p,255);
End;

Procedure EmailAddrDeQuote(var a:String);
Begin;
if (copy(a,1,1)+copy(a,length(a),255)='<>') then a:=copy(a,2,length(a)-2);
Kicserel('\','',a);
End;

Procedure EmailAddrEnQuote(var a:String);

Procedure x(var a:string);
Var i:LongInt;
Begin;
i:=0;
Kicserel('@',#13,a);inc(i,Kicserel(#13,'\@',a));
Kicserel('<',#13,a);inc(i,Kicserel(#13,'\<',a));
Kicserel('>',#13,a);inc(i,Kicserel(#13,'\>',a));
if (i>0) then a:='"'+a+'"';
End;

Var
  i,p:LongInt;
  b:String;
Begin;
if (a='') then exit;
p:=0;
for i:=1 to length(a) do if (a[i]='@') then p:=i;
if (p=0) then exit;
b:=copy(a,p+1,255);
a:=copy(a,1,p-1);
x(a);
x(b);
a:=a+'@'+b;
End;

Function EmailAddrTestSyntax(a:String):Boolean;
Var i:LongInt;
Begin;
EmailAddrTestSyntax:=True;
i:=pos('@',a);
if (i<=1) then exit;
if (i>=length(a)) then exit;
EmailAddrTestSyntax:=False;
End;

Procedure CreateFile(pat,ext:String;var nam:String);
Label f1;
Begin;
f1:
nam:=BStr(random($7fffffff))+'.'+BStr(random($7fffffff))+'.'+BStr(random($7fffffff));
if (xCreate(pat+nam+ext)<>0) then goto f1;
End;

Function getPasswordOfUser(a:String):String;
Var
  t:xtText;
  i,o:LongInt;
Begin;
getPasswordOfUser:='';
i:=FindEmailAddr(a);
if (i<1) then exit;
if (xtOpen(t,usersListDat[i].path+'password',true)<>0) then exit;
getPasswordOfUser:=xtReadLn(t,666);
xtClose(t);
End;
