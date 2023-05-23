Const
  inputBufMax=64;
Type
  OneConnectionRecord=record
    pipe:LongInt;               {pipeline id of control connection}
    data:LongInt;               {pipeline id of data connection}
    time:LongInt;               {time of last action}
    adr:OneTCPaddressRecord;    {remote address}
    prt:LongInt;                {remote port}
    stat:LongInt;               {status: 0-disconn, 1-init, 2-auth, 3-cmd, 4-rx}
                                {5-tx, 6-list, 7-nlst, 8-connect, 9-mlsd, 10-wait}
    uid:LongInt;                {user id}
    fileH:xFile;                {file handle}
    fileP:LongInt;              {file position}
    fileS:LongInt;              {file size}
    path:String;                {current path}
    ibufD:array[1..inputBufMax] of byte;        {input buffer data}
    ibufS:LongInt;              {input buffer size}
    ibufP:LongInt;              {input buffer position}
    cmd:String;                 {command buffer}
    dataT:LongInt;              {type of data: 1-user, 2-rnfr}
    dataS:String;               {data string}
    rest:LongInt;               {restart pointer}
    start:Boolean;              {ready to start transfer}
    mlst:Byte;                  {mlst flags}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  portCmd:LongInt;
  portDat:LongInt;
  addrCmd:OneTCPaddressRecord;
  addrDat:OneTCPaddressRecord;
  CurrentYear:LongInt;
  lastSent:LongInt;
  defaultPath:String;
Const
  MonthNames:array[1..12] of String[3]=('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
  factNames_Size=$01;
  factNames_Modify=$02;
  factNames_Create=$04;
  factNames_Type=$08;
  factNames_Perm=$10;



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

Function FindOneAddress(adr:OneTCPaddressRecord;except:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do if TCPcompareAddress(adr,ConnectionDat^[i].adr) then if (i<>except) then goto f1;
i:=0;
f1:
FindOneAddress:=i;
End;

Function getWord(var a:String):String;
Var i:LongInt;
Begin;
i:=pos(' ',a);
if (i<1) then i:=666;
getWord:=copy(a,1,i-1);
a:=copy(a,i+1,255);
End;

Procedure GetCurrentYear;
Var a,b,c:Word;
Begin;
xGetDate(a,b,c);
CurrentYear:=a;
End;
