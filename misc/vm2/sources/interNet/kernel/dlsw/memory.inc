Type
  OneConnectionRecord=record
    stat:LongInt;               {status: 0-free, 1-start, 2-wait, 3-head, 4-data}
    time:LongInt;               {time of last action}
    addr:OneTCPaddressRecord;   {remote address}
    uppr:LongInt;               {upper pipe number}
    pipe1:LongInt;              {remote-->local}
    pipe2:LongInt;              {local-->remote}
    hdrDat:array[1..96] of byte;{header part received}
    hdrSiz:LongInt;             {size of packet waiting}
    end;
Type
  oneMacAddressRecord=record
    a:LongInt;
    b:Word;
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  CurrentPipeLine:LongInt;
  myMACaddress:oneMacAddressRecord;
  servAddr:OneTCPaddressRecord;
  servPort:LongInt;



Procedure dumpOneBuffer(var buffer;siz:LongInt;nam:String);
Var
  buf:array[1..1] of byte absolute buffer;
  i:LongInt;
Begin;
Write(nam+' '+BStr(siz)+':');
for i:=1 to siz do write(' '+byte2hextype(buf[i]));
WriteLn('');
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

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function FindOneConnection(var addr):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do
 if TCPcompareAddress(ConnectionDat^[i].addr,addr) then goto f1;
i:=0;
f1:
FindOneConnection:=i;
End;
