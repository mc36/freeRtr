Const
  allowdHostMax=512;
  printSpoolMax=64;
  dataFileExt='.data';
  headFileExt='.head';
Type
  OneConnectionRecord=record
    pipe:LongInt;               {pipeline id of control connection}
    time:LongInt;               {time of last action}
    adr:OneTCPaddressRecord;    {remote address}
    prt:LongInt;                {remote port}
    stat:LongInt;               {status: 0=disconn, 1=init, 2=main, 3=listing}
                                {4=submit, 5=receive, 6=finish}
    file:xFile;                 {file handler}
    cmd:String;                 {command data}
    name:String;                {name of file}
    pos:LongInt;                {position in file}
    siz:LongInt;                {size of file}
    cnt:LongInt;                {counter}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  allowdHostDat:array[1..allowdHostMax] of record
    base:OneTCPaddressRecord;
    mask:OneTCPaddressRecord;
    end;
  allowdHostNum:LongInt;
  printSpoolDat:array[1..printSpoolMax] of record
    name:String;
    path:String;
    end;
  printSpoolNum:LongInt;
Var
  serverPort:LongInt;
  serverAddr:OneTCPaddressRecord;




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

Function getNextWord(var a:String):String;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
while (ab0>0) and (ab[1]=32) do a:=copy(a,2,666);
o:=666;
for i:=ab0 downto 1 do if (ab[i]=32) then o:=i;
getNextWord:=copy(a,1,o-1);
a:=copy(a,o+1,666);
End;

Function repairPath(a:String):String;
Begin;
if (copy(a,length(a),255)<>'\') then a:=a+'\';
repairPath:=a;
End;

Function TestAddrMask(a1,a2,a3:OneTCPaddressRecord):Boolean;
Var i:LongInt;
Begin;
TestAddrMask:=False;
for i:=1 to sizeof(a1) do if (a1[i] and a3[i]<>a2[i] and a3[i]) then exit;
TestAddrMask:=True;
End;

Function findOnePrinter(a:String):LongInt;
Label f1;
Var i:LongInt;
Begin;
a:=kicsi(a);
for i:=1 to printSpoolNum do if (kicsi(printSpoolDat[i].name)=a) then goto f1;
i:=0;
f1:
findOnePrinter:=i;
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

Var
  b:String;
Begin;
if (xtOpen(t,a,true)<>0) then immErr('error opening config!');
serverPort:=BVal(gnl);
allowdHostNum:=0;
printSpoolNum:=0;
while not xtEOF(t) do begin;
  b:=gnl;
  a:=kicsi(getNextWord(b));
  if (a='dir') then begin;
    if (printSpoolNum>=printSpoolMax) then continue;
    inc(printSpoolNum);
    printSpoolDat[printSpoolNum].name:=getNextWord(b);
    printSpoolDat[printSpoolNum].path:=repairPath(b);
    end;
  if (a='host') then begin;
    if (allowdHostNum>=allowdHostMax) then continue;
    inc(allowdHostNum);
    string2ipAddr(getNextWord(b),allowdHostDat[allowdHostNum].base);
    string2ipAddr(getNextWord(b),allowdHostDat[allowdHostNum].mask);
    end;
  end;
xtClose(t);
End;
