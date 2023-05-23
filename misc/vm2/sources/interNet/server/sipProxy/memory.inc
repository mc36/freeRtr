Type
  OneConnectionRecord=record
    stat:LongInt;               {status: 0-unreg, 1-regged, 2-infinite}
    time:LongInt;               {time of last action}
    addr:OneTCPaddressRecord;   {sip address}
    port:LongInt;               {sip port}
    name:String;                {username}
    rout:String;                {route numbers}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  localAddr:OneTCPaddressRecord;
  localPort:LongInt;
  localPipe:LongInt;
  lastScand:LongInt;


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

Function xLevesz(a:String):String;
Begin;
a:=' '+a+' ';
kicserel('  ',' ',a);
xLevesz:=copy(a,2,length(a)-2);
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

Function getDateForHeader:String;
Const MonthNames:array[1..12] of String[3]=('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
Var
  a1,a2,a3:Word;
  a:String;
Begin;
xGetDate(a1,a2,a3);
a:=padUpWithZeros(a3)+' '+MonthNames[a2]+' '+padUpWithZeros(a1);
xGetTime(a1,a2,a3);
getDateForHeader:=a+' '+padUpWithZeros(a1)+':'+padUpWithZeros(a2)+':'+padUpWithZeros(a3)+' Z';
End;


Procedure dumpOneMessage(a:String;var buffer;siz:LongInt);
Var
  buf:array[1..1] of char absolute buffer;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
while (ab0<70) do begin;
  inc(ab0);
  ab[ab0]:=$2d;
  end;
WriteLn(a);
for i:=1 to siz do write(buf[i]);
fillchar(a,sizeof(a),$2d);
ab0:=70;
WriteLn(a);
End;



Procedure QuickSwapOne(n1,n2:LongInt);
Var d:OneConnectionRecord;
Begin;
d:=ConnectionDat^[n1];
ConnectionDat^[n1]:=ConnectionDat^[n2];
ConnectionDat^[n2]:=d;
End;

Function QuickReadOne(n:LongInt):String;
Begin;
QuickReadOne:=ConnectionDat^[n].name;
End;

{$sysinc quicksrt.inc}

Function findOneUserName(a:String):LongInt;
Begin;
findOneUserName:=QuickFind(1,ConnectionNum,kicsi(a));
End;

Function findOneRouting(a:String):LongInt;
Label f1;
Var i:LongInt;
Begin;
a:=kicsi(a);
for i:=1 to ConnectionNum do if FileMaskingTestOne(a,ConnectionDat^[i].rout) then goto f1;
i:=0;
f1:
findOneRouting:=i;
End;
