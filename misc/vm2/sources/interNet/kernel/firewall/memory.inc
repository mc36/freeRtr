Type
  OneRuleRecord=record
    srcAddr:OneAddressRecord;   {source address}
    srcMask:OneAddressRecord;   {source mask}
    trgAddr:OneAddressRecord;   {target address}
    trgMask:OneAddressRecord;   {target mask}
    protocol:LongInt;           {protocol: 1=udp, 2=conn, 4=data}
    action:LongInt;             {action to do: 0=deny, 1=permit, 2=srcAdr, 4=trgAdr, 8=srcPrt, 16=trgPrt}
    srcPrtF:LongInt;            {source port first}
    srcPrtL:LongInt;            {source port last}
    trgPrtF:LongInt;            {target port first}
    trgPrtL:LongInt;            {target port last}
    nSrcAdr:OneAddressRecord;   {new source addr}
    nTrgAdr:OneAddressRecord;   {new target addr}
    nSrcPrt:LongInt;            {new source port}
    nTrgPrt:LongInt;            {new target port}
    end;
Var
  RulesData:^array[1..1] of OneRuleRecord;
  RulesNumb:LongInt;
  localAddr:OneAddressRecord;
  PrtclPipe:LongInt;
  PrtclProc:LongInt;


Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Function ResizeMem(n:LongInt):Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
ResizeMem:=True;
i:=n*sizeof(OneRuleRecord);
if (ExtendedMemoryResize(p,i)<i) then exit;
RulesNumb:=n;
RulesData:=p^;
ResizeMem:=False;
End;




Procedure exchangeAddr(var a,b:OneAddressRecord);
Var c:OneAddressRecord;
Begin;
c:=a;
a:=b;
b:=c;
End;

Function compareAddress(var a1,a2:OneAddressRecord):Boolean;
Begin;
compareAddress:=True;
if (a1.a4<>a2.a4) then exit;
if (a1.a1<>a2.a1) then exit;
if (a1.a2<>a2.a2) then exit;
if (a1.a3<>a2.a3) then exit;
compareAddress:=False;
End;

Function compareAddrMsk(var t,a,m:OneAddressRecord):Boolean;
Begin;
compareAddrMsk:=True;
if (t.a4 and m.a4<>a.a4) then exit;
if (t.a1 and m.a1<>a.a1) then exit;
if (t.a2 and m.a2<>a.a2) then exit;
if (t.a3 and m.a3<>a.a3) then exit;
compareAddrMsk:=False;
End;

Procedure truncateAddrMsk(var adr,msk);
Var
  a:array[1..1] of longint absolute adr;
  m:array[1..1] of longint absolute msk;
  i:LongInt;
Begin;
for i:=1 to 4 do a[i]:=a[i] and m[i];
End;
