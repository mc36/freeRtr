Type OneHrdwrAddrRecord=array[1..6] of byte;
Type
  OnePacketRecord=record
    d:array[1..4*1024] of byte;
    a:OneHrdwrAddrRecord;
    s:LongInt;
    end;
Type
  OneConnectionRecord=record
    pipe:LongInt;               {pipeline id of connection}
    proc:LongInt;               {process id of connection}
    adr:String;                 {remote address}
    prt:LongInt;                {local socket}
    stat:LongInt;               {status: 1-command, 2-connected}
    time:Longint;               {time of last action}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  LowerProcess:LongInt;
  LowerPipeline:LongInt;
  localAddress:OneHrdwrAddrRecord;
  bcastAddress:OneHrdwrAddrRecord;
  networkAddrDat:array[1..256] of byte;
  networkAddrSiz:LongInt;
  addressSize:LongInt;
  packetSize:LongInt;
  lastHelloTime:LongInt;
  nextPacketNumber:LongInt;
  uplinkAddr:OneHrdwrAddrRecord;


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

Function hwaddr2str(var buffer):String;
Var
  buf:array[1..1] of byte absolute buffer;
  a:String;
  i:LongInt;
Begin;
a:='';
for i:=1 to addressSize do a:=a+'-'+byte2hextype(buf[i]);
hwaddr2str:=copy(a,2,666);
End;

Function netaddr2str(var buffer;siz:LongInt):String;
Var
  buf:array[1..1] of byte absolute buffer;
  a:String;
  i:LongInt;
Begin;
a:='';
for i:=1 to siz do begin;
  if (i and 1=0) then a:=a+'.';
  a:=a+byte2hextype(buf[i]);
  end;
netaddr2str:=a;
End;

Function FindOneLocalSocket(n:LongInt):LongInt;
Var
  d:OneConnectionRecord;
  i:LongInt;
Begin;
for i:=1 to ConnectionNum do begin;
  d:=ConnectionDat^[i];
  if (d.stat<>2) then continue;
  if (d.prt<>n) then continue;
  FindOneLocalSocket:=i;
  exit;
  end;
FindOneLocalSocket:=0;
End;


Function getPDUtype(i:LongInt):String;
Var a:String;
Begin;
case i and $1f of
  $1c:a:='data';
  $1d:a:='multicast';
  $01:a:='error';
  $1c:a:='echo-request';
  $1d:a:='echo-reply';
  else a:='invalid (0x'+byte2hextype(i)+')';
  end;
getPDUtype:=a;
End;



Function calcChkSum(var buffer;size,sumofs:LongInt):LongInt;
Var
  buf:array[1..1] of byte absolute buffer;
  i,o,p,q:LongInt;
Begin;
buf[sumofs+0]:=0;
buf[sumofs+1]:=0;
o:=0;
p:=0;
for i:=1 to size do begin;
  q:=buf[i];
  inc(o,q);
  inc(p,o);
  end;
o:=o mod $ff;
p:=p mod $ff;
q:=(size-sumOfs+1)*o;
o:=q-o-p;
p:=p-q-1;
if (o<0) then dec(o);
if (p>0) then inc(p);
o:=o mod $ff;
p:=p mod $ff;
if (o=0) then o:=$ff;
if (p=0) then p:=$01;
o:=o and $ff;
p:=p and $ff;
calcChkSum:=(o shl 8)+p;
End;



Function writeOneHeader(var buffer;trg:String;src,typ,siz:LongInt):LongInt;
Var
  buf:array[1..1] of byte absolute buffer;
  ab:array[0..1] of byte absolute trg;
  ab0:byte absolute trg;
  i,o,p:LongInt;
Begin;
inc(nextPacketNumber);
o:=networkAddrSiz+ab0+17;
move(uplinkAddr,buf,addressSize);               {destination address}
buf[addressSize+1]:=$81;                        {nlpid: clns}
buf[addressSize+2]:=o;                          {header length}
buf[addressSize+3]:=$01;                        {version}
buf[addressSize+4]:=$40;                        {holdtime}
buf[addressSize+5]:=typ;                        {pdu type}
WriteWordMSB(buf[addressSize+6],o+siz);         {pdu length}
p:=addressSize+9;
move(ab,buf[p+1],ab0+1);inc(p,ab0+1);           {destination address and length}
inc(p);buf[p]:=networkAddrSiz;                  {source address length}
move(networkAddrDat,buf[p+1],networkAddrSiz);   {source address}
inc(p,networkAddrSiz);
buf[p]:=src;                                    {source port}
WriteWordMSB(buf[p+1],nextPacketNumber);        {data unit id}
inc(p,2);
WriteWordMSB(buf[p+1],0);inc(p,2);              {segment offset}
WriteWordMSB(buf[p+1],o+siz);inc(p,2);          {total length}
i:=calcChkSum(buf[addressSize+1],o,8);
WriteWordMSB(buf[addressSize+8],i);             {checksum}
writeOneHeader:=p;
End;
