Type
  OneAddressRecord=array[1..6] of byte;
  OneConnectionRecord=record
    stat:LongInt;               {status: 0-free, 1-alloc, 2-used}
    time:LongInt;               {time of last action}
    cnid:LongInt;               {connection id}
    pipe:LongInt;               {pipeline id}
    addr:OneAddressRecord;      {remote address}
    cmnd:String;                {command string}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  nextConnID:LongInt;
  serviceName:String;
  etherPipe:LongInt;
  localEth:OneAddressRecord;
  broadEth:OneAddressRecord;



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

Function FindConnectByID(id:LongInt):LongInt;
Label f1;
Var
  d:OneConnectionRecord;
  i:LongInt;
Begin;
if (id=0) or (id=$ffff) then begin; i:=0;goto f1; end;
for i:=1 to ConnectionNum do begin;
  d:=ConnectionDat^[i];
  if (d.stat=0) then continue;
  if (d.cnid<>id) then continue;
  goto f1;
  end;
i:=0;
f1:
FindConnectByID:=i;
End;

Function FindConnectByStat(s:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do if (ConnectionDat^[i].stat=s) then goto f1;
i:=0;
f1:
FindConnectByStat:=i;
End;
