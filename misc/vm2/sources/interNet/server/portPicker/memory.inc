Type
  OneConnectionRecord=record
    pipe1:LongInt;              {pipeline id of accepted connection}
    pipe2:LongInt;              {pipeline id of initiated connection}
    time:LongInt;               {time of last action}
    which:LongInt;              {which service the client connected}
    adr:OneTCPaddressRecord;    {remote address}
    prt:LongInt;                {remote port}
    stat:LongInt;               {status: 0=disconn, 1=init, 2=pick, 3=conning, 4=conned}
    line:String;                {received line}
    end;
Const targetMax=64;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  sourcePort:LongInt;
  sourceAddr:OneTCPaddressRecord;
  targetPort:array[1..targetMax] of LongInt;
  targetAddr:array[1..targetMax] of OneTCPaddressRecord;
  targetText:array[0..targetMax] of String;
  targetNum:LongInt;
  readedTim:LongInt;
  readedMin:LongInt;
  readedMax:LongInt;



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

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;


Procedure convertText(var c:String);
Label f1;
Var
  i,o:LongInt;
  a:String;
Begin;
f1:
o:=pos('\',c);
if (o<1) then exit;
a:=nagy(copy(c,o+1,2));
c:=copy(c,1,o-1)+copy(c,o+3,666);
i:=hextype2byte(a);
if (byte2hextype(i)<>a) then goto f1;
c:=copy(c,1,o-1)+chr(i)+copy(c,o,666);
goto f1;
End;

Function findOneTarget(var txt:String):LongInt;
Label f1;
Var i:LongInt;
Begin;
i:=0;
if (txt='') then goto f1;
for i:=1 to targetNum do if (txt=targetText[i]) then goto f1;
i:=0;
f1:
findOneTarget:=i;
End;

Function findDefTarget:LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to targetNum do if (targetText[i]='') then goto f1;
f1:
findDefTarget:=i;
End;
