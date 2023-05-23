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
a:=' '+a+' ';
kicserel('  ',' ',a);
a:=copy(a,2,length(a)-2);
gnl:=a;
end;

function rep(b:string):string;
begin;
if (copy(b,length(b),255)<>'\') then b:=b+'\';
rep:=b;
end;

Procedure sortHosts;

Function QuickReadOne(n:LongInt):String;
Begin;
QuickReadOne:=hostsListDat[n].nam;
End;

Procedure QuickSwapOne(n1,n2:LongInt);
Begin;
hostsListDat[hostsListMax]:=hostsListDat[n1];
hostsListDat[n1]:=hostsListDat[n2];
hostsListDat[n2]:=hostsListDat[hostsListMax];
End;

{$sysinc quicksrt.inc}

Begin;
if (hostsListNum>1) then QuickSort(1,hostsListNum);
End;

Procedure sortMimes;

Function QuickReadOne(n:LongInt):String;
Begin;
QuickReadOne:=MimeTypesDat[n].ext;
End;

Procedure QuickSwapOne(n1,n2:LongInt);
Begin;
MimeTypesDat[MimeTypesMax]:=MimeTypesDat[n1];
MimeTypesDat[n1]:=MimeTypesDat[n2];
MimeTypesDat[n2]:=MimeTypesDat[MimeTypesMax];
End;

{$sysinc quicksrt.inc}

Begin;
if (MimeTypesNum>1) then QuickSort(1,MimeTypesNum);
End;

Var
  c,b:String;
  i:LongInt;
Begin;
if (xtOpen(t,'c:\system\localHost.text',true)<>0) then immErr('error opening localhost.text');
serverName:=xtReadLn(t,255);
xtClose(t);
if (xtOpen(t,a,true)<>0) then immErr('error opening config!');
serverPortP:=BVal(gnl);
serverPortS:=BVal(gnl);
a:=gnl;
b:=gnl;
TempPath:=rep(gnl);
limitCgis:=BVal(gnl);
limitHost:=BVal(gnl);
TimeOutCgi:=BVal(gnl);
xtClose(t);
if (xtOpen(t,a,true)<>0) then immErr('error opening mimetypes!');
MimeTypesNum:=0;
while not xtEOF(t) do begin;
  a:=gnl;
  if (a='') then continue;
  i:=pos('=',a);
  if (i=0) then continue;
  inc(MimeTypesNum);
  MimeTypesDat[MimeTypesNum].ext:=kicsi(copy(a,1,i-1));
  MimeTypesDat[MimeTypesNum].typ:=copy(a,i+1,255);
  end;
xtClose(t);
if (xtOpen(t,b,true)<>0) then immErr('error opening hosts!');
hostsListNum:=0;
while not xtEOF(t) do begin;
  b:=gnl;
  if (b='') then continue;
  inc(hostsListNum);
  a:=getWord(b);
  kicserel('@',#13,a);
  kicserel(#13,serverName,a);
  if not string2ipAddr(a,c) then a:=ipAddr2string(c);
  hostsListDat[hostsListNum].nam:=kicsi(a);
  a:=kicsi(getWord(b));
  hostsListDat[hostsListNum].dirList:=(pos('d',a)<>0);
  hostsListDat[hostsListNum].readFile:=(pos('r',a)<>0);
  hostsListDat[hostsListNum].execute:=(pos('x',a)<>0);
  hostsListDat[hostsListNum].authentic:=(pos('a',a)<>0);
  hostsListDat[hostsListNum].userId:=BVal(getWord(b));
  if (copy(b,1,1)=':') then begin;
    kicserel('@',#13,b);
    kicserel(#13,serverName,b);
    end else begin;
    b:=rep(b);
    end;
  hostsListDat[hostsListNum].pat:=b;
  end;
xtClose(t);
sortHosts;
sortMimes;
End;
