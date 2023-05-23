Const domainListMax=128;
Type
  OneConnectionRecord=record
    stat:LongInt;               {status: 0=disconn, 1=init, 2=head, 3=data}
    pipe:LongInt;               {pipeline id}
    time:LongInt;               {time of last action}
    addr:OneTCPaddressRecord;   {client address}
    port:LongInt;               {client port}
    buf:array[1..128] of byte;  {buffer for header}
    size:LongInt;               {size of data}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  domainListDat:array[1..domainListMax] of record
    perm:Boolean;
    name:String;
    end;
  domainListNum:LongInt;
  serverPort:LongInt;
  serverAddr:OneTCPaddressRecord;
  blockUrlBeg:String;
  blockUrlEnd:String;
  logFileName:String;
  logFileDate:LongInt;
  logFileHndr:xtText;
  logFileLock:Boolean;



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

Function getWord(var a:String):String;
Var i:LongInt;
Begin;
i:=pos(' ',a);
if (i<1) then i:=666;
getWord:=copy(a,1,i-1);
a:=copy(a,i+1,255);
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


Procedure append2log(act,url:String);
Var
  i:LongInt;
  a:String;
Begin;
WriteLn(act+' '+url);
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
xtWriteLn(logFileHndr,' '+url);
if logFileLock then exit;
xtClose(logFileHndr);
dec(logFileDate);
End;



Function testOneUrl(a:String):LongInt; {1=permit, 0=deny}
Label f1;
Var i:LongInt;
Begin;
for i:=1 to domainListNum do if FileMaskingTestOne(a,domainListDat[i].name) then begin;
  if domainListDat[i].perm then testOneUrl:=1;
  exit;
  end;
f1:
testOneUrl:=0;
End;
