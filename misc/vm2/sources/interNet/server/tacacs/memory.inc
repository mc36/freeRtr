Type
  OneTacacsHeaderRecord=record
    ver:Byte;                   {version number}
    typ:Byte;                   {type of data: 1-authen, 2-author, 3-acct}
    seq:Byte;                   {sequence number: increase by 1}
    flg:Byte;                   {flags: 1-cleartext, 4-multiplexing}
    ses:LongInt;                {session id}
    siz:LongInt;                {length of payload}
    end;
  OneTacacsAuthenReqRecord=record
    act:Byte;                   {action: 1-login, 2-chpass, 3-sendpass, 4-sendauth}
    prv:Byte;                   {privilege level}
    typ:Byte;                   {authentication type: 1-ascii, 2-pap, 3-chap, 4-arap, 5-mschap}
    svc:Byte;                   {service: 0-none, 1-login, 2-enable, 3-ppp, 4-arap, 5-pt, 6-rcmd, 7-x25, 8-nasi, 9-proxy}
    len_usr:Byte;               {length of user name}
    len_prt:Byte;               {length of port name}
    len_adr:Byte;               {length of address}
    len_dat:Byte;               {length of data}
    end;
  OneTacacsContinueRecord=record
    len_usr:Word;               {length of user data}
    len_dat:Word;               {length of data}
    flg:Byte;                   {flags: 1-abort}
    end;
  OneTacacsAuthenRepRecord=record
    stat:Byte;                  {status: 1-pass, 2-fail, 3-getdata, 4-getuser, 5-getpass, 6-restart, 7-error, 33-follow}
    flag:Byte;                  {flags: 1-noecho}
    len_msg:Word;               {length of message}
    len_dat:Word;               {length of data}
    end;
  OneTacacsAuthorReqRecord=record
    amth:Byte;                  {authentication method used}
    priv:Byte;                  {privilege level}
    atyp:Byte;                  {authentication type used}
    asvc:Byte;                  {authentication service used}
    len_usr:Byte;               {length of user name}
    len_prt:Byte;               {length of port name}
    len_adr:Byte;               {length of address}
    argCnt:Byte;                {arguments}
    end;
  OneTacacsAuthorRepRecord=record
    stat:Byte;                  {status: 1-passadd, 2-passrep, 16-fail, 17-error, 33-follow}
    argCnt:Byte;                {arguments}
    len_msg:Word;               {length of message}
    len_dat:Word;               {length of data}
    end;
  OneTacacsAccountReqRecord=record
    flag:Byte;                  {flags: 1-none, 2-start, 4-stop, 8-watchdog}
    amth:Byte;                  {authentication method used}
    priv:Byte;                  {privilege level}
    atyp:Byte;                  {authentication type used}
    asvc:Byte;                  {authentication service used}
    len_usr:Byte;               {length of user name}
    len_prt:Byte;               {length of port name}
    len_adr:Byte;               {length of address}
    argCnt:Byte;                {arguments}
    end;
  OneTacacsAccountRepRecord=record
    len_msg:Word;               {length of message}
    len_dat:Word;               {length of data}
    stat:Byte;                  {status: 1-success, 2-error, 33-follow}
    end;
Type
  OneConnectionRecord=record
    pipe:LongInt;               {pipeline id of control connection}
    time:LongInt;               {time of last action}
    adr:OneTCPaddressRecord;    {remote address}
    prt:LongInt;                {remote port}
    stat:LongInt;               {status: 0-disconn, 1-init, 2-head, 3-data}
    head:OneTacacsHeaderRecord; {header data}
    size:LongInt;               {size of packet to receive}
    cmdA:String;                {received data}
    cmdB:String;                {received data}
    cmdT:LongInt;               {type of data: 1-username}
    end;
Var
  logFileName:String;
  logFileDate:LongInt;
  logFileHndr:xtText;
  logFileLock:Boolean;
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  serverAddr:OneTCPaddressRecord;
  serverPort:LongInt;
  md5algoNum:LongInt;
  sharedSecret:String;
  userPrompt:String;
  passPrompt:String;
  succPrompt:String;
  failPrompt:String;


Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
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

Function getCurrentDateBin:LongInt;
Var a,b,c:Word;
Begin;
xGetDate(a,b,c);
getCurrentDateBin:=(c shl 16) or (b shl 8) or c;
End;

Function padUpWithZeros(i:LongInt):String;
Var a:String;
Begin;
a:=BStr(i);
while (length(a)<2) do a:='0'+a;
padUpWithZeros:=a;
End;

Function getCurrentDateStr:String;
Var a,b,c:Word;
Begin;
xGetDate(a,b,c);
getCurrentDateStr:=padUpWithZeros(a)+'-'+padUpWithZeros(b)+'-'+padUpWithZeros(c);
End;

Function getCurrentTimeStr:String;
Var a,b,c:Word;
Begin;
xGetTime(a,b,c);
getCurrentTimeStr:=padUpWithZeros(a)+':'+padUpWithZeros(b)+':'+padUpWithZeros(c);
End;


Procedure append2log(act,usr,frm,dat:String);
Var
  i:LongInt;
  a:String;
Begin;
i:=getCurrentDateBin;
if (i<>logFileDate) then begin;
  logFileDate:=i;
  xtClose(logFileHndr);
  a:=logFileName;
  i:=pos('%',a);
  if (i>0) then a:=copy(a,1,i-1)+getCurrentDateStr+copy(a,i+1,666);
  xCreate(a);
  if (xtOpen(logFileHndr,a,false)<>0) then immErr('error opening '+a+'!');
  end;
xtWrite(logFileHndr,getCurrentTimeStr+' '+act);
xtWrite(logFileHndr,' '+usr);
xtWrite(logFileHndr,' '+frm);
xtWriteLn(logFileHndr,' '+dat);
if logFileLock then exit;
xtClose(logFileHndr);
dec(logFileDate);
End;



Function CheckOneUser(usr:String;var pwd:String):Boolean;
Label err;
Var
  d:OneLoginUserDataRecord;
  f:xFile;
  i,o:LongInt;
Begin;
CheckOneUser:=True;
usr:=kicsi(usr);
fillchar(f,sizeof(f),0);
if (xOpen(f,LoginDatabaseFilename,xGenFilMod_r)<>0) then goto err;
o:=xFileSize(f) div sizeof(d);
for i:=1 to o do begin;
  if (xBlockRead(f,d,sizeof(d))<>0) then goto err;
  if (kicsi(d.userName)<>usr) then continue;
  xClose(f);
  if (d.flags and LoginFlags_accessTac=0) then exit;
  pwd:=d.password;
  CheckOneUser:=False;
  exit;
  end;
err:
fillchar(d,sizeof(d),0);
xClose(f);
End;
