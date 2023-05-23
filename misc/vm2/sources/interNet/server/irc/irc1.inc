Function membershipTest(var con:OneConnectionRecord;i:LongInt):Boolean;

function QuickReadOne(i:LongInt):String;
var
  a:string;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
begin;
ab0:=sizeof(i);
WriteLongMSB(ab[1],con.chanD[i]);
QuickReadOne:=a;
end;

procedure QuickSwapOne(a,b:LongInt);begin;end;

{$sysinc quicksrt.inc}

Var
  a:string;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
ab0:=sizeof(i);
WriteLongMSB(ab[1],i);
membershipTest:=(QuickFind(1,con.chanN,a)<1);
End;



Procedure membershipSort(var con:OneConnectionRecord);

function QuickReadOne(i:LongInt):String;
var
  a:string;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
begin;
ab0:=sizeof(i);
WriteLongMSB(ab[1],con.chanD[i]);
QuickReadOne:=a;
end;

procedure QuickSwapOne(a,b:LongInt);
var c:LongInt;
begin;
c:=con.chanD[a];
con.chanD[a]:=con.chanD[b];
con.chanD[b]:=c;
end;

{$sysinc quicksrt.inc}

Begin;
if (con.chanN<2) then exit;
QuickSort(1,con.chanN);
End;



Function getWord(var a:String):String;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
procedure eatShit;begin; while (ab0>0) and (ab[1]=32) do a:=copy(a,2,666); end;
Begin;
eatShit;
if (ab0>0) and (ab[1]=$3a) then begin;
  getWord:=copy(a,2,666);
  a:='';
  exit;
  end;
i:=pos(' ',a);
if (i<1) then i:=666;
getWord:=copy(a,1,i-1);
a:=copy(a,i+1,255);
eatShit;
End;

Function IsThisBuggyName(a:String):Boolean;
Var i:LongInt;
Begin;
IsThisBuggyName:=True;
if not (length(a) in [2..24]) then exit;
a:=kicsi(a);
if not (a[1] in ['a'..'z']) then exit;
for i:=1 to length(a) do if not (a[i] in ['a'..'z','0'..'9','.','_','-','~','(','[','{','}','}',')']) then exit;
IsThisBuggyName:=False;
End;

Procedure addStr2buf(var buffer;var size:LongInt;a:String);
Var
  buf:array[1..1] of byte absolute buffer;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
move(ab[1],buf[size+1],ab0);
inc(size,ab0);
End;

Procedure addCrlf2buf(var buffer;var size:LongInt);
Var buf:array[1..1] of byte absolute buffer;
Begin;
inc(size);buf[size]:=13;
inc(size);buf[size]:=10;
End;



Procedure broadcastMessage(var con:OneConnectionRecord;from,msg:String);
Var
  buf:array[1..768] of byte;
  siz:LongInt;
  i,o,p:LongInt;
Begin;
siz:=0;
addStr2buf(buf,siz,from);
addStr2buf(buf,siz,msg);
addStr2buf(buf,siz,#13#10);
for i:=1 to ConnectionNum do ConnectionDat^[i].bcast:=0;
pipeLineSend(con.pipe,buf,siz);
inc(con.bcast);
for o:=1 to con.chanN do begin;
  p:=con.chanD[o];
  for i:=1 to ConnectionNum do begin;
    if membershipTest(ConnectionDat^[i],p) then continue;
    if (ConnectionDat^[i].bcast<>0) then continue;
    pipeLineSend(ConnectionDat^[i].pipe,buf,siz);
    inc(ConnectionDat^[i].bcast);
    end;
  end;
End;

Procedure channelMessage(chan:LongInt;from,msg:String);
Var
  buf:array[1..768] of byte;
  siz:LongInt;
  i,o:LongInt;
Begin;
siz:=0;
addStr2buf(buf,siz,from);
addStr2buf(buf,siz,msg);
addStr2buf(buf,siz,#13#10);
for i:=1 to ConnectionNum do begin;
  if membershipTest(ConnectionDat^[i],chan) then continue;
  pipeLineSend(ConnectionDat^[i].pipe,buf,siz);
  end;
End;
