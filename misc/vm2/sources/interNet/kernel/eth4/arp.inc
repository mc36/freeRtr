Type
  OneArpPacketRecord=record
    hrd:Word;
    pro:Word;
    hln:Byte;
    pln:Byte;
    op:Word;
    sha:OneEtherAddrRec;
    spa:LongWord;
    tha:OneEtherAddrRec;
    tpa:LongWord;
    end;
Const
  Op_ARP_req=1;
  Op_ARP_rep=2;
  Op_RARP_req=3;
  Op_RARP_rep=4;
  Op_DRARP_req=5;
  Op_DRARP_rep=6;
  Op_InARP_req=8;
  Op_InARP_rep=9;
Type
  OneArpCacheEntry=record
    e:OneEtherAddrRec;
    c:Word;
    i:LongWord;
    t:LongInt;
    end;
Var
  ArpCacheNum:LongInt;
  ArpCacheDat:^array[1..1] of OneArpCacheEntry;
Const
  ArpCacheExpire=20*60;
  ArpCacheRetry=5*60;


Function ArpCacheFind(inet:LongWord):LongInt;
Var i:LongInt;
Begin;
for i:=1 to ArpCacheNum do if (ArpCacheDat^[i].i=inet) then begin;
  ArpCacheFind:=i;
  exit;
  end;
ArpCacheFind:=0;
End;

Procedure ArpCacheAppend(inet:LongWord;eth:OneEtherAddrRec);
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

Procedure ArpCachePutBad(inet:LongWord);
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



Procedure GotOneArpPacket(var pck:OnePacketRecord);
Var
  arp:OneArpPacketRecord;
  i:LongInt;
Begin;
move(pck.d,arp,sizeof(arp));
if (readWordMSB(arp.hrd)<>1) then begin;
  WriteLn('ARP got a packet with invalid hardware type from '+convEtherAddr(pck.a));
  exit;
  end;
if (readWordMSB(arp.pro)<>$800) then begin;
  WriteLn('ARP got a packet with invalid protocol type from '+convEtherAddr(pck.a));
  exit;
  end;
if (arp.hln<>6) then begin;
  WriteLn('ARP got a packet with invalid hardware address length from '+convEtherAddr(pck.a));
  exit;
  end;
if (arp.pln<>4) then begin;
  WriteLn('ARP got a packet with invalid protocol address length from '+convEtherAddr(pck.a));
  exit;
  end;
case readWordMSB(arp.op) of
  Op_ARP_req:begin;
    ArpCacheAppend(arp.spa,arp.sha);
    if (arp.tpa<>localNet) then exit;
    writeWordMSB(arp.op,Op_ARP_rep);
    arp.tpa:=arp.spa;
    arp.tha:=arp.sha;
    arp.spa:=localNet;
    arp.sha:=localEth;
    move(arp,pck.d,sizeof(arp));
    WriteLn('ARP sends reply to '+convEtherAddr(pck.a));
    pipeLineSend(ethPipe,pck,sizeof(OneArpPacketRecord)+8);
    end;
  Op_ARP_rep:begin;
    WriteLn('ARP got reply from '+convEtherAddr(pck.a));
    ArpCacheAppend(arp.spa,arp.sha);
    end;
  Op_InARP_req:begin;
    for i:=1 to sizeof(localEth) do if (localEth[i]<>arp.tha[i]) then exit;
    writeWordMSB(arp.op,Op_ARP_rep);
    arp.tpa:=arp.spa;
    arp.tha:=arp.sha;
    arp.spa:=localNet;
    arp.sha:=localEth;
    move(arp,pck.d,sizeof(arp));
    WriteLn('ARP sends reply to '+convEtherAddr(pck.a));
    pipeLineSend(ethPipe,pck,sizeof(OneArpPacketRecord)+8);
    end;
  Op_RARP_req:;
  Op_RARP_rep:;
  Op_DRARP_req:;
  Op_DRARP_rep:;
  Op_InARP_rep:;
  else WriteLn('ARP a packet with invalid opcode from '+convEtherAddr(pck.a));
  end;
End;

Function SendArpReq(var d:OneArpCacheEntry):Boolean;
Var
  pck:OnePacketRecord;
  arp:OneArpPacketRecord;
Begin;
SendArpReq:=False;
if (d.i=$00000000) or (d.i=$ffffffff) then exit;
if (getTimePast(d.t)<ArpCacheExpire) then exit;
inc(d.c);
if (d.c>12) then begin; SendArpReq:=True;exit; end;
pck.a:=d.e;
fillchar(arp,sizeof(arp),0);
WriteWordMSB(pck.t,$806);
WriteWordMSB(arp.hrd,1);
WriteWordMSB(arp.pro,$800);
arp.hln:=6;
arp.pln:=4;
WriteWordMSB(arp.op,Op_ARP_req);
arp.sha:=localEth;
arp.spa:=LocalNet;
arp.tpa:=d.i;
move(arp,pck.d,sizeof(arp));
pipeLineSend(ethPipe,pck,sizeof(OneArpPacketRecord)+8);
WriteLn('ARP sends request to '+convInterAddr(d.i));
inc(d.t,ArpCacheRetry);
End;

Procedure DisplayArpCache;
Var
  i:LongInt;
  a:String;
Begin;
for i:=1 to ArpCacheNum do
 WriteLn(convEtherAddr(ArpCacheDat^[i].e)+' '+convInterAddr(ArpCacheDat^[i].i));
End;
