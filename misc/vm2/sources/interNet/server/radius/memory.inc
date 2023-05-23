Type
  OneRadiusPacketRecord=record
    cod:Byte;
    id:Byte;
    len:Word;
    auth:array[1..16] of byte;
    dat:array[1..2*1024] of byte;
    siz:LongInt;
    pos:LongInt;
    adr:OneTCPaddressRecord;
    prt:LongInt;
    end;
Const OneRadiusPacketHeader=20;
Var
  RandomDat:Array[0..255] of String[47];
  RandomNum:LongInt;
  requestAuth:Array[1..16] of byte;
  sharedSecret:String;
  logFileName:String;
  logFileDate:LongInt;
  logFileHndr:xtText;
  logFileLock:Boolean;
  md5algoNum:LongInt;
  authentingAddr:OneTCPaddressRecord;
  authentingPort:LongInt;
  authentingPipe:LongInt;
  accountingAddr:OneTCPaddressRecord;
  accountingPort:LongInt;
  accountingPipe:LongInt;
  attribs2addr:OneRadiusPacketRecord;



Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
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
{$ifdef debug}WriteLn('log: '+act+' '+usr+' '+frm+' '+dat);{$endif}
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



Function CheckOneUser(usr:String;var pwd:String;var uid:LongInt):Boolean;
Label err;
Var
  d:OneLoginUserDataRecord;
  f:xFile;
  i,o:LongInt;
Begin;
CheckOneUser:=True;
uid:=-1;
usr:=kicsi(usr);
fillchar(f,sizeof(f),0);
if (xOpen(f,LoginDatabaseFilename,xGenFilMod_r)<>0) then goto err;
o:=xFileSize(f) div sizeof(d);
for i:=1 to o do begin;
  if (xBlockRead(f,d,sizeof(d))<>0) then goto err;
  if (kicsi(d.userName)<>usr) then continue;
  xClose(f);
  if (d.flags and LoginFlags_accessRad=0) then exit;
  pwd:=d.password;
  uid:=d.userID;
  CheckOneUser:=False;
  exit;
  end;
err:
fillchar(d,sizeof(d),0);
xClose(f);
End;
