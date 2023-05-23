Const
  inputBufMax=64;
  usersListMax=512;
Type
  OneConnectionRecord=record
    pipe:LongInt;               {pipeline id of control connection}
    time:LongInt;               {time of last action}
    adr:OneTCPaddressRecord;    {remote address}
    prt:LongInt;                {remote port}
    stat:LongInt;               {status: 0-disconn, 1-init, 2-auth, 3-cmd, 4-uidl, 5-list, 6-retr}
    usrNum:LongInt;             {user number; 0=bad}
    fileH:xtText;               {file handle}
    fileD:xFile;                {file handle}
    fileP:LongInt;              {file position}
    fileS:LongInt;              {file size}
    ibufD:array[1..inputBufMax] of byte;        {input buffer data}
    ibufS:LongInt;              {input buffer size}
    ibufP:LongInt;              {input buffer position}
    cmd:String;                 {command buffer}
    sawNL:Boolean;              {saw new line}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  usersListDat:array[1..usersListMax] of record
    name:String;
    path:String;
    uid:LongInt;
    end;
  usersListNum:LongInt;
  serverPort,serverSPort:LongInt;
  serverAddr:OneTCPaddressRecord;
  lastSent:LongInt;
  md5algorithm:LongInt;
Const
  LocalMessageFile='.msg';
  LocalPasswordFile='password';
  LocalDeletedFile='deleted';


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

Function FindUserName(a:String):LongInt;

Function QuickReadOne(n:LongInt):String;
Begin;
QuickReadOne:=usersListDat[n].name;
End;

Procedure QuickSwapOne(n1,n2:LongInt);Begin;End;

{$sysinc quicksrt.inc}

Begin;
FindUserName:=QuickFind(1,usersListNum,kicsi(a));
End;
