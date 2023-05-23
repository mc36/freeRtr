Type
  OneArpCacheEntry=record
    e:OneEtherAddrRec;
    c:Word;
    i:OneInterAddrRec;
    t:LongInt;
    end;
Var
  ArpCacheNum:LongInt;
  ArpCacheDat:^array[1..1] of OneArpCacheEntry;
Const
  ArpCacheExpire=20*60;
  ArpCacheRetry=5*60;


Function ArpCacheFind(inet:OneInterAddrRec):LongInt;
Var i:LongInt;
Begin;
for i:=1 to ArpCacheNum do if EqualIPaddr(ArpCacheDat^[i].i,inet) then begin;
  ArpCacheFind:=i;
  exit;
  end;
ArpCacheFind:=0;
End;

Procedure ArpCacheAppend(inet:OneInterAddrRec;eth:OneEtherAddrRec);
Var
  i,o,p:LongInt;
  pt:Pointer;
  d:OneArpCacheEntry;
Begin;
i:=ArpCacheFind(inet);
if (i=0) then begin;
  i:=(ArpCacheNum+1)*sizeof(OneArpCacheEntry);
  if (ExtendedMemoryResize(pt,i)<i) then begin;
    WriteLn('failed to append arp cache!');
    exit;
    end;
  ArpCacheDat:=pt^;
  inc(ArpCacheNum);
  i:=ArpCacheNum;
  end;
BugOS_KernelUptime(o,d.t,p);
d.i:=inet;
d.e:=eth;
d.c:=0;
ArpCacheDat^[i]:=d;
if (i=1) then gatwyEth:=eth;
End;

Procedure ArpCachePutBad(inet:OneInterAddrRec);
Var
  i,o,p:LongInt;
  pt:Pointer;
  d:OneArpCacheEntry;
Begin;
i:=(ArpCacheNum+1)*sizeof(OneArpCacheEntry);
if (ExtendedMemoryResize(pt,i)<i) then begin;
  WriteLn('failed to append arp cache!');
  exit;
  end;
ArpCacheDat:=pt^;
inc(ArpCacheNum);
d.t:=-99999;
d.i:=inet;
d.e:=broadEth;
d.c:=0;
ArpCacheDat^[ArpCacheNum]:=d;
End;

Procedure ArpCacheDelete(num:LongInt);
Var p:Pointer;
Begin;
if (num<1) or (num>ArpCacheNum) then exit;
if (num=1) then begin;
  ArpCacheDat^[1].c:=0;
  exit;
  end;
ArpCacheDat^[num]:=ArpCacheDat^[ArpCacheNum];
dec(ArpCacheNum);
ExtendedMemoryResize(p,ArpCacheNum*sizeof(OneArpCacheEntry));
ArpCacheDat:=p^;
End;


Function SendArpReq(var d:OneArpCacheEntry):Boolean;
Var
  pck:OneUpperPacketRec;
Begin;
SendArpReq:=False;
if EqualIPaddr(d.i,broadNet) then exit;
if (getTimePast(d.t)<ArpCacheExpire) then exit;
inc(d.c);
if (d.c>12) then begin; SendArpReq:=True;exit; end;
move(d.i,pck.a,sizeof(pck.a));
pck.t:=1;
move(d.e,pck.d,sizeof(d.e));
pipeLineSend(prgPipe,pck,26);
inc(d.t,ArpCacheRetry);
End;

Procedure DisplayArpCache;
Var
  i:LongInt;
  a:String;
Begin;
for i:=1 to ArpCacheNum do
 WriteLn(convEtherAddr(ArpCacheDat^[i].e)+' '+ipAddr2string(ArpCacheDat^[i].i));
End;
