Const
  serviceNameAuth='ssh-userauth';
  serviceNameConn='ssh-connection';
  channelNameSess='session';
  authentKeyboard='keyboard-interactive';
  authentPassword='password';
  authentNothing='none';
  safeRequestList='|pty-req|window-change|xon-xoff|exit-signal|exit-status|signal|x11-req|env|';

Function GenerateEncryptKeys(pck:OnePacketRecord;b:String):String;
Var
  ctx:CryptoContextRecord;
  i,o:LongInt;
  a:String;
Begin;
CryptoBegHasher(ctx,sha1algoNum,'','');
CryptoAddHasher(ctx,pck.d,pck.s);
CryptoAddHasher(ctx,b[1],length(b));
CryptoFinHasher(ctx,b[1],i);
b[0]:=chr(i);
for o:=1 to 11 do begin;
  CryptoBegHasher(ctx,sha1algoNum,'','');
  CryptoAddHasher(ctx,pck.d,pck.s);
  CryptoAddHasher(ctx,b[1],length(b));
  CryptoFinHasher(ctx,a[1],i);
  a[0]:=chr(i);
  b:=b+a;
  end;
GenerateEncryptKeys:=b;
End;

Procedure HasherInitialize(var c1,c2:CryptoContextRecord;alg:Byte;key:String);

Function padup(key:String;num:byte):string;
Var
  dat:array[0..1] of byte absolute key;
  i:LongInt;
Begin;
for i:=dat[0]+1 to 128 do dat[i]:=0;
dat[0]:=64;
for i:=1 to dat[0] do dat[i]:=dat[i] xor num;
padup:=key;
End;

Var
  a:String;
Begin;
CryptoBegHasher(c1,HasherAlgoList[alg].a,'','');
CryptoBegHasher(c2,HasherAlgoList[alg].a,'','');
key:=copy(key,1,HasherAlgoList[alg].s);
a:=padup(key,$36);
CryptoAddHasher(c1,a[1],length(a));
a:=padup(key,$5c);
CryptoAddHasher(c2,a[1],length(a));
End;

Procedure HasherGenerateSum(c1,c2:CryptoContextRecord;seq:LongInt;var packet;var data;var size:LongInt);
Begin;
writeLongMSB(seq,seq);
CryptoAddHasher(c1,seq,sizeof(seq));
CryptoAddHasher(c1,packet,size);
CryptoFinHasher(c1,c1,seq);
CryptoAddHasher(c2,c1,seq);
CryptoFinHasher(c2,data,size);
End;






Procedure add2packetBegin(var pck:OnePacketRecord;typ:Byte);
Begin;
pck.s:=sizeof(typ);
move(typ,pck.d,sizeof(typ));
End;

Procedure add2packetBinary(var pck:OnePacketRecord;var dat;siz:LongInt);
Begin;
move(dat,pck.d[pck.s+1],siz);
inc(pck.s,siz);
End;

Procedure add2packetBinHdr(var pck:OnePacketRecord;var dat;siz:LongInt);
Begin;
writeLongMSB(pck.d[pck.s+1],siz);
move(dat,pck.d[pck.s+5],siz);
inc(pck.s,siz+4);
End;

Procedure add2packetString(var pck:OnePacketRecord;str:String);
Begin;
writeLongMSB(pck.d[pck.s+1],length(str));
move(str[1],pck.d[pck.s+5],length(str));
inc(pck.s,length(str)+4);
End;

Procedure add2packetBignum(var pck:OnePacketRecord;num:BigNumContextRecord);
Var
  i:LongInt;
Begin;
BigNumConvBN2MSB(num,i,num);
writeLongMSB(pck.d[pck.s+1],i);
move(num,pck.d[pck.s+5],i);
inc(pck.s,i+4);
End;

Procedure add2packetInteger(var pck:OnePacketRecord;num:LongInt);
Begin;
writeLongMSB(pck.d[pck.s+1],num);
inc(pck.s,4);
End;

Procedure add2packetBoolean(var pck:OnePacketRecord;bol:Boolean);
Var val:byte absolute bol;
Begin;
if (val<>0) then val:=1;
inc(pck.s,1);
pck.d[pck.s]:=val;
End;





Procedure get4packetBinary(var ps:LongInt;var pck:OnePacketRecord;var dat;siz:LongInt);
Begin;
move(pck.d[ps+1],dat,siz);
inc(ps,siz);
End;

Procedure get4packetBinHdr(var ps:LongInt;var pck:OnePacketRecord;var dat;var siz:LongInt);
Begin;
siz:=readLongMSB(pck.d[ps+1]);
if (siz>sizeof(pck.d)) then begin; siz:=0;exit; end;
move(pck.d[ps+5],dat,siz);
inc(ps,siz+4);
End;

Function get4packetString(var ps:LongInt;var pck:OnePacketRecord):String;
const max=252;
Var
  i,o:LongInt;
  str:String;
Begin;
get4packetString:='';
o:=readLongMSB(pck.d[ps+1]);
if (o>sizeof(pck.d)) then exit;
i:=o;if (i>max) then i:=max;
move(pck.d[ps+5],str[1],i);str[0]:=chr(i);
inc(ps,o+4);
get4packetString:=str;
End;

Procedure get4packetBignum(var ps:LongInt;var pck:OnePacketRecord;var num:BigNumContextRecord);
Var
  i:LongInt;
Begin;
fillchar(num,sizeof(num),0);
i:=readLongMSB(pck.d[ps+1]);
if (i>sizeof(num)) then exit;
move(pck.d[ps+5],num,i);
inc(ps,i+4);
BigNumConvMSB2BN(i,num,num);
End;

Function get4packetInteger(var ps:LongInt;var pck:OnePacketRecord):LongInt;
Begin;
get4packetInteger:=readLongMSB(pck.d[ps+1]);
inc(ps,4);
End;

Function get4packetBoolean(var ps:LongInt;var pck:OnePacketRecord):Boolean;
Begin;
inc(ps);
get4packetBoolean:=(pck.d[ps]<>0);
End;

Function get4listEntry(var a:string):String;
Var i:LongInt;
Begin;
i:=pos(',',a);
if (i<1) then i:=666;
get4listEntry:=copy(a,1,i-1);
a:=copy(a,i+1,255);
End;









Procedure sendPacket(var con:OneConnectionRecord;pck:OnePacketRecord);
Var
  pcb:array[1..1] of byte absolute pck;
  i,o:LongInt;
Begin;
pck.p:=0;
inc(pck.s,5);
i:=random($100);
while (pck.p<4) or (pck.s mod con.encLRs>0) do begin;
  inc(pck.p);
  inc(pck.s);
  pcb[pck.s]:=i;
  end;
o:=pck.s;
writeLongMSB(pck.s,o-4);
if con.secure then begin;
  i:=o;
  HasherGenerateSum(con.macLRa,con.macLRb,con.packLR,pck,pcb[o+1],i);
  CryptoAddEncrypt(con.encLRc,pck,o);
  inc(o,i);
  end;
pipeLineSend(con.pipe,pck,o);
inc(con.packLR);
con.time:=CurrentTime;
lastSent:=TCPprocessId;
End;

Function receivePacket(var con:OneConnectionRecord;var pck:OnePacketRecord):Boolean;
Label f1,f2;
Var
  ctx:CryptoContextRecord;
  pcb:array[1..1] of byte absolute pck;
  buf:array[1..1] of byte absolute ctx;
  i,o,p:LongInt;
Begin;
receivePacket:=True;
pipeLineStats(con.pipe,i,o,p);
if (i=0) then begin;
  con.err:='connection lost';
  con.stat:=0;
  exit;
  end;
if (con.recSiz<1) then begin;
  i:=con.encRLs;
  if (o<i) then exit;
  if (pipeLineRecv(con.pipe,con.recDat,i)<>0) then exit;
  con.recPos:=i;
  if con.secure then begin;
    move(con.recDat,pck,i);
    move(con.encRLc,ctx,sizeof(ctx));
    CryptoAddDecrypt(ctx,pck,i);
    i:=ReadLongMSB(pck)+con.macRLs+4;
    end else i:=ReadLongMSB(con.recDat)+4;
  if (i<con.encRLs) then begin;
    f1:
    con.err:='invalid packet size';
    con.stat:=0;
    exit;
    end;
  if (i>sizeof(con.recDat)) then goto f1;
  con.recSiz:=i;
  con.time:=CurrentTime;
  exit;
  end;
i:=con.recSiz-con.recPos;
if (i>0) then begin;
  if (o<1) then exit;
  if (pipeLineRecv(con.pipe,con.recDat[con.recPos+1],i)<>0) then exit;
  inc(con.recPos,i);
  con.time:=CurrentTime;
  exit;
  end;
fillchar(pck,sizeof(pck),0);
p:=con.recSiz;
move(con.recDat,pck,p);
con.recPos:=0;
con.recSiz:=0;
if con.secure then begin;
  i:=p-con.macRLs;
  CryptoAddDecrypt(con.encRLc,pck,i);
  HasherGenerateSum(con.macRLa,con.macRLb,con.packRL,pck,buf,i);
  if (i<>con.macRLs) then begin;
    f2:
    con.err:='packet integrity fault';
    con.stat:=0;
    exit;
    end;
  while (i>0) do begin;
    if (buf[i]<>pcb[p]) then goto f2;
    pcb[p]:=0;
    dec(i);
    dec(p);
    end;
  end;
i:=ReadLongMSB(pck.s)-pck.p-1;
if (i<1) then goto f1;
if (i>p) then goto f1;
pck.s:=i;
inc(con.packRL);
receivePacket:=False;
End;
