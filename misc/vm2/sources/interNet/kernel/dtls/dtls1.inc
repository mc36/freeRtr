Const
  protocolVersionMin=$0302;
  protocolVersionMax=$0302;


Function recvPackRecLay(var con:OneConnectionRecord;var pck:OnePacketRecord):Boolean;
Label f1,f3,vege,err;
Const maxStep=1024;
Var
  buf:array[1..256] of byte;
  i,o,p:LongInt;
Begin;
recvPackRecLay:=True;
pck.s:=sizeof(pck.d);
if (pipeLineRecv(con.pipeT,pck.d,pck.s)<>0) then pck.s:=0;
if (pck.s<1) then begin;
  pipeLineStats(con.pipeT,o,i,i);
  if (o<>0) then exit;
  con.err:='connection lost';
  con.stat:=0;
  exit;
  end;
dec(pck.s,13);
if (pck.s<0) then begin;
  f1:
  con.err:='invalid record packet size';
  err:
  con.stat:=0;
  con.secure:=0;
  exit;
  end;
pck.t:=pck.d[1];
i:=convertDTLS2SSLversion(ReadWordMSB(pck.d[2]));
if (con.vers=0) then con.vers:=i else if (i<>con.vers) then begin;
  con.err:='invalid record packet version';
  goto err;
  end;
if (con.secure=0) then i:=0 else i:=1;
if (i<>ReadWordMSB(pck.d[4])) then begin;
  con.err:='invalid record packet epoch';
  goto err;
  end;
con.pckRLb:=ReadWordMSB(pck.d[6]);
con.pckRLa:=ReadLongMSB(pck.d[8]);
if (pck.s<>ReadWordMSB(pck.d[12])) then goto f1;
if (con.secure=0) then begin;
  move(pck.d[14],pck.d,pck.s);
  goto vege;
  end;
inc(pck.s,13);
if (ReadWordMSB(pck.d[2])<>convertSSL2DTLSversion(con.vers)) then begin;
  con.err:='version check failed';
  goto err;
  end;
p:=14;
o:=pck.s-13;
while (o>0) do begin;
  if (o>maxStep) then i:=maxStep else i:=o;
  CryptoAddDecrypt(con.encRLc,pck.d[p],i);
  inc(p,i);
  dec(o,i);
  end;
dec(pck.s,pck.d[pck.s]);
dec(pck.s);
dec(pck.s,con.encSiz);
move(pck.d[con.encSiz+14],pck.d[14],pck.s);
dec(pck.s,con.macSiz);
if (pck.s<13) then goto f1;
writeWordMSB(pck.d[12],pck.s-13);
case con.vers of
  $302:o:=dtls_calcSum(con.macRLa,con.macRLb,con.pckRLa,con.pckRLb,pck.d,buf,pck.s);
  else o:=0;
  end;
if (o<>con.macSiz) then begin;
  f3:
  con.err:='integrity check failed';
  goto err;
  end;
for i:=1 to o do if (buf[i]<>pck.d[pck.s+i]) then goto f3;
dec(pck.s,13);
move(pck.d[14],pck.d,pck.s);
vege:
recvPackRecLay:=False;
End;



Procedure updatePackRecLay(var con:OneConnectionRecord;typ:LongInt;var dat;var siz:LongInt);
Label vege;
Var
  buf:array[1..1] of byte absolute dat;
  i,o:LongInt;
Begin;
case con.vers shr 8 of
  3:;
  else begin; con.stat:=0;exit; end;
  end;
move(buf,buf[14],siz);
buf[1]:=typ;
WriteWordMSB(buf[2],convertSSL2DTLSversion(con.vers));
if (con.secure=0) then i:=0 else i:=1;
WriteWordMSB(buf[4],i);
WriteWordMSB(buf[6],con.pckLRb);
WriteLongMSB(buf[8],con.pckLRa);
WriteWordMSB(buf[12],siz);
inc(siz,13);
if (con.secure=0) then goto vege;
case con.vers of
  $302:i:=dtls_calcSum(con.macLRa,con.macLRb,con.pckLRa,con.pckLRb,buf,buf[siz+1],siz);
  else i:=0;
  end;
inc(siz,i);
inc(siz);
i:=(siz-13) mod con.encSiz;
if (i<1) then i:=con.encSiz;
i:=con.encSiz-i;
fillchar(buf[siz],i+1,i);
inc(siz,i);
move(buf[14],buf[con.encSiz+14],siz-13);
fillChar(buf[14],con.encSiz,random($100));
inc(siz,con.encSiz);
CryptoAddEncrypt(con.encLRc,buf[14],siz-13);
writeWordMSB(buf[12],siz-13);
vege:
inc(con.pckLRa);
if (con.pckLRa=0) then inc(con.pckLRb);
End;

Procedure sendPackRecLay(var con:OneConnectionRecord;typ:LongInt;var dat;siz:LongInt);
Begin;
updatePackRecLay(con,typ,dat,siz);
PipeLineSend(con.pipeT,dat,siz);
con.time:=currentTime;
lastSent:=TCPprocessId;
End;



Function recvPackHndshk(var con:OneConnectionRecord;var pck:OnePacketRecord):Boolean;
Label f1,err;
Var i,o,p:LongInt;
Begin;
recvPackHndshk:=True;
if recvPackRecLay(con,pck) then exit;
if (pck.t=21) then exit; {alert}
if (pck.t<>22) then begin;
  con.err:='invalid record packet type';
  goto err;
  end;
if (ReadWordMSB(pck.d[5])<>con.hndsRL) then exit;
if (con.secure=0) then begin;
  CryptoAddHasher(con.macLRa,pck.d,pck.s);
  CryptoAddHasher(con.macLRb,pck.d,pck.s);
  p:=0;
  end else p:=1;
dec(pck.s,12);
if (pck.s<0) then begin;
  f1:
  con.err:='invalid handshake packet size';
  err:
  con.secure:=0;
  con.stat:=0;
  exit;
  end;
pck.t:=pck.d[1];
if (pck.s<>ReadLongMSB(pck.d[1]) and $ffffff) then goto f1;
if (0<>ReadLongMSB(pck.d[6]) and $ffffff) then begin;
  con.err:='invalid handshake packet fragment offset';
  goto err;
  end;
if (pck.s<>ReadLongMSB(pck.d[9]) and $ffffff) then begin;
  con.err:='invalid handshake packet fragment size';
  goto err;
  end;
move(pck.d[13],pck.d,pck.s);
inc(con.hndsRL);
recvPackHndshk:=False;
End;

Procedure sendPackHndshk(var con:OneConnectionRecord;pck:OnePacketRecord);
Label f1;
Begin;
if (pck.t>$100) then begin;
  updatePackRecLay(con,pck.t and $ff,pck.d,pck.s);
  goto f1;
  end;
move(pck.d,pck.d[13],pck.s);
writeLongMSB(pck.d[1],(pck.t shl 24) or pck.s);
WriteWordMSB(pck.d[5],con.hndsLR);
WriteWordMSB(pck.d[7],0);
WriteLongMSB(pck.d[9],pck.s);
inc(pck.s,12);
if (con.secure=0) then begin;
  CryptoAddHasher(con.macLRa,pck.d,pck.s);
  CryptoAddHasher(con.macLRb,pck.d,pck.s);
  end;
inc(con.hndsLR);
updatePackRecLay(con,22,pck.d,pck.s);
f1:
move(pck.s,con.sndDat[con.sndSiz+1],sizeof(pck.s));
inc(con.sndSiz,sizeof(pck.s));
move(pck.d,con.sndDat[con.sndSiz+1],pck.s);
inc(con.sndSiz,pck.s);
con.sndTim:=-99999;
con.sndTry:=0;
End;



Procedure clearHndshkBuf(var con:OneConnectionRecord);
Begin;
con.sndSiz:=0;
con.sndTim:=currentTime;
con.sndTry:=0;
End;

Function resendHndshkBuf(var con:OneConnectionRecord):Boolean;
Label f1;
Var i,o,p:LongInt;
Begin;
resendHndshkBuf:=False;
if (getTimePast(con.sndTim)<10) then exit;
inc(con.sndTry);
if (con.sndTry>10) then begin;
  con.err:='remote not responding';
  con.stat:=0;
  resendHndshkBuf:=true;
  exit;
  end;
con.sndTim:=currentTime;
con.time:=currentTime;
lastSent:=TCPprocessId;
p:=1;
f1:
if (p>con.sndSiz) then exit;
move(con.sndDat[p],o,sizeof(o));
inc(p,sizeof(o));
pipeLineSend(con.pipeT,con.sndDat[p],o);
inc(p,o);
goto f1;
End;
