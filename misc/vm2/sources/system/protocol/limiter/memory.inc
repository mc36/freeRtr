Type
  oneDescriptorRecord=record
    adr:String[15];
    rxC:LongInt;
    txC:LongInt;
    rxL:LongInt;
    txL:LongInt;
    end;
Var
  userDat:array[1..128] of oneDescriptorRecord;
  userNum:LongInt;
  addrSiz:LongInt;
  packSiz:LongInt;
  localAddr:array[1..32] of byte;
  broadAddr:array[1..32] of byte;
  deviceName:String;
  ioBase:LongInt;
  memBase:LongInt;
  devPipe:LongInt;
  uppPipe:LongInt;


Function QuickReadOne(n:LongInt):String;
Begin;
QuickReadOne:=userDat[n].adr;
End;

Procedure QuickSwapOne(n1,n2:LongInt);
Var d:oneDescriptorRecord;
Begin;
d:=userDat[n1];
userDat[n1]:=userDat[n2];
userDat[n2]:=d;
End;

{$sysinc quicksrt.inc}


Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;



Function convAddr(var data):String;
Var
  d:array[1..1] of byte absolute data;
  i:LongInt;
  a:String;
Begin;
a:='';
for i:=1 to addrSiz do a:=a+'-'+byte2hextype(d[i]);
convAddr:=copy(a,2,666);
End;
