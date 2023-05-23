Type
  OneConnectionRecord=record
    pipe:LongInt;               {pipeline id of control connection}
    time:LongInt;               {time of last action}
    adr:OneTCPaddressRecord;    {remote address}
    prt:LongInt;                {remote port}
    stat:LongInt;               {status: 0=disconn, 1=init, 2=cmd, 3=param}
                                {4=request, 5=binary, 6=text, 7=dir, 8=gopher}
    file:xFile;                 {file handler}
    text:xtText;                {file handler}
    pos:LongInt;                {position in file}
    siz:LongInt;                {size of file}
    cmd:String;                 {command data}
    prm:String;                 {parameter data}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
Var
  serverPort:LongInt;
  serverAddr:OneTCPaddressRecord;
  serverPath:String;
  userID:LongInt;
  textExt:String;
  gophExt:String;



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


Function xLevesz(a:String):String;
Begin;
a:=' '+a+' ';
kicserel('  ',' ',a);
xLevesz:=copy(a,2,length(a)-2);
End;

Function repairPath(a:String):String;
Begin;
if (copy(a,length(a),255)<>'\') then a:=a+'\';
repairPath:=a;
End;


Procedure ReadUpConfig(a:String);
Var t:xtText;

function gnl:string;
var
  i:longint;
  a:String;
begin;
a:=xtReadLn(t,255);
i:=pos(';',a);
if (i>0) then a:=copy(a,1,i-1);
gnl:=xLevesz(a);
end;

function ext(a:string):String;
begin;
a:=' '+kicsi(a)+' ';
kicserel(' ',#13,a);
ext:=a;
end;

Begin;
if (xtOpen(t,a,true)<>0) then immErr('error opening config!');
serverPort:=BVal(gnl);
serverPath:=repairPath(gnl);
userID:=BVal(gnl);
gophExt:=ext(gnl);
textExt:=ext(gnl);
xtClose(t);
End;
