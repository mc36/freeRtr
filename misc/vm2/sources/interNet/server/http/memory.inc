Const
  mimeTypesMax=128;
  hostslistMax=512;
  inputBufMax=64;
Type
  OneConnectionRecord=record
    pipe:LongInt;               {pipeline id of control connection}
    time:LongInt;               {time of last action}
    process:LongInt;            {process executed for serving}
    request:String;             {request file created}
    adr:OneTCPaddressRecord;    {remote address}
    prt:LongInt;                {remote port}
    stat:LongInt;               {status: 0-disconn, 1=init, 2-cmd, 3-url, 4-prot}
                                {5-lines, 6-begin, 7-tx, 8-execWait, 9-exec, 10-param}
                                {11-header, 12-data}
    fileH:xFile;                {file handle}
    fileP:LongInt;              {file position}
    fileS:LongInt;              {file size}
    ibufD:array[1..inputBufMax] of byte;        {input buffer data}
    ibufS:LongInt;              {input buffer size}
    ibufP:LongInt;              {input buffer position}
    quoB:array[0..3] of byte;   {quote buffer}
    quoS:byte;                  {quote size}
    connTp:Byte;                {connection type: 0-close, 1-keepAlive}
    range:Boolean;              {is the range parameter valid}
    rangeBeg:LongInt;           {range beginning}
    rangeEnd:LongInt;           {range ending}
    bond:String;                {boundary separator}
    authed:String;              {authentication info}
    url:String;                 {requested filename}
    host:String;                {requested hostname}
    hostN:LongInt;              {requested host number}
    hostU:LongInt;              {requested host user}
    clnt:String;                {requester client}
    line:String;                {current line}
    ntry:String;                {entry name}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  MimeTypesDat:array[1..mimeTypesMax] of record
    ext:string[7];
    typ:string[63];
    end;
  MimeTypesNum:LongInt;
  hostsListDat:array[1..hostslistMax] of record
    nam:String;
    pat:String;
    userId:LongInt;
    dirList:Boolean;
    readFile:Boolean;
    execute:Boolean;
    authentic:Boolean;
    end;
  hostsListNum:LongInt;
  serverPortP:LongInt;
  serverPortS:LongInt;
  serverAddr:OneTCPaddressRecord;
  serverName:String;
  lastSent:LongInt;
  TempPath:String;
  limitCgis:LongInt;
  TimeOutCgi:LongInt;
  limitHost:LongInt;
Const
  RequestExt='.q';
  ResponseExt='.r';



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

Function CountAddress(adr:OneTCPaddressRecord):LongInt;
Var i,o:LongInt;
Begin;
o:=0;
for i:=1 to ConnectionNum do if TCPcompareAddress(adr,ConnectionDat^[i].adr) then inc(o);
CountAddress:=o;
End;

Function getWord(var a:String):String;
Var i:LongInt;
Begin;
i:=pos(' ',a);
if (i<1) then i:=666;
getWord:=copy(a,1,i-1);
a:=copy(a,i+1,255);
End;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;


Function FindHostName(a:String):LongInt;

Function QuickReadOne(n:LongInt):String;
Begin;
QuickReadOne:=hostsListDat[n].nam;
End;

Procedure QuickSwapOne(n1,n2:LongInt);Begin;End;

{$sysinc quicksrt.inc}

Var b:String;
Begin;
if not string2ipAddr(a,b) then a:=ipAddr2string(b);
FindHostName:=QuickFind(1,hostsListNum,kicsi(a));
End;

Function FindExtension(a:String):LongInt;

Function QuickReadOne(n:LongInt):String;
Begin;
QuickReadOne:=MimeTypesDat[n].ext;
End;

Procedure QuickSwapOne(n1,n2:LongInt);Begin;End;

{$sysinc quicksrt.inc}

Begin;
FindExtension:=QuickFind(1,MimeTypesNum,kicsi(a));
End;
