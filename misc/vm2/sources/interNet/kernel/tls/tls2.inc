Const
  protocolVersionMin=$0300;
  protocolVersionMax=$0302;


Function recvPackRecLay(var con:OneConnectionRecord;var pck:OnePacketRecord):Boolean;
Label f1,f2,f3,f4,vege,err;
Const maxStep=1024;
Var
  buf:array[1..256] of byte;
  i,o,p:LongInt;
Begin;
recvPackRecLay:=True;
if (con.recSiz<1) then begin;
  if (con.recPos<5) then begin;
    i:=5-con.recPos;
    if (pipeLineRecv(con.pipeT,con.recDat[con.recPos+1],i)<>0) then i:=0;
    if (i<1) then begin;
      pipeLineStats(con.pipeT,o,i,i);
      if (o<>0) then exit;
      f1:
      con.err:='connection lost';
      con.stat:=0;
      exit;
      end;
    con.time:=currentTime;
    inc(con.recPos,i);
    if (con.recPos<5) then exit;
    end;
  f3:
  case con.vers shr 8 of
    3:begin;
      con.recSiz:=readWordMSB(con.recDat[4])+5;
      end;
    2:begin;
      con.recSiz:=(readWordMSB(con.recDat[1]) and $7fff)+2;
      end;
    0:begin;
      if (con.recDat[1] and $80<>0) then con.vers:=$0200 else con.vers:=$0300;
      goto f3;
      end;
    else goto f2;
    end;
  if (con.recSiz>sizeof(con.recDat)) then begin;
    f2:
    con.err:='invalid record packet size';
    err:
    con.secure:=0;
    con.stat:=0;
    exit;
    end;
  end;
if (con.recPos<con.recSiz) then begin;
  i:=con.recSiz-con.recPos;
  if (pipeLineRecv(con.pipeT,con.recDat[con.recPos+1],i)<>0) then i:=0;
  if (i<1) then begin;
    pipeLineStats(con.pipeT,o,i,i);
    if (o=0) then goto f1;
    exit;
    end;
  inc(con.recPos,i);
  con.time:=currentTime;
  if (con.recPos<con.recSiz) then exit;
  end;
if (con.vers shr 8=2) then begin;
  pck.s:=con.recSiz-2;
  if (pck.s<1) then goto f2;
  pck.t:=con.recDat[3];
  move(con.recDat[3],pck.d,pck.s);
  goto vege;
  end;
pck.s:=con.recSiz-5;
pck.t:=con.recDat[1];
if (pck.s<0) then goto f2;
if (con.secure=0) then begin;
  move(con.recDat[6],pck.d,pck.s);
  goto vege;
  end;
if (ReadWordMSB(con.recDat[2])<>con.vers) then begin;
  con.err:='version check failed';
  goto err;
  end;
p:=6;
o:=con.recSiz-5;
while (o>0) do begin;
  if (o>maxStep) then i:=maxStep else i:=o;
  CryptoAddDecrypt(con.encRLc,con.recDat[p],i);
  inc(p,i);
  dec(o,i);
  end;
if (con.encSiz>1) then begin;
  dec(con.recSiz,con.recDat[con.recSiz]);
  dec(con.recSiz);
  if (con.vers>=$302) then begin;
    dec(con.recSiz,con.encSiz);
    move(con.recDat[con.encSiz+6],con.recDat[6],con.recSiz);
    end;
  end;
dec(con.recSiz,con.macSiz);
if (con.recSiz<5) then goto f2;
writeWordMSB(con.recDat[4],con.recSiz-5);
case con.vers of
  $302,$301:o:=tls_calcSum(con.macRLa,con.macRLb,con.pckRLa,con.pckRLb,con.recDat,buf,con.recSiz);
  $300:o:=ssl_calcSum(con.macRLa,con.macRLb,con.pckRLa,con.pckRLb,con.recDat,buf,con.recSiz);
  else o:=0;
  end;
if (o<>con.macSiz) then begin;
  f4:
  con.err:='integrity check failed';
  goto err;
  end;
for i:=1 to o do if (buf[i]<>con.recDat[con.recSiz+i]) then goto f4;
pck.s:=con.recSiz-5;
move(con.recDat[6],pck.d,pck.s);
inc(con.pckRLa);
if (con.pckRLa=0) then inc(con.pckRLb);
vege:
con.recSiz:=0;
con.recPos:=0;
recvPackRecLay:=False;
End;





Procedure sendPackRecLay(var con:OneConnectionRecord;typ:LongInt;var dat;siz:LongInt);
Label vege;
Var
  buf:array[1..4*1024] of byte;
  i,o:LongInt;
Begin;
case con.vers shr 8 of
  3:;
  2:begin;
    writeWordMSB(buf[1],(siz+1) or $8000);
    buf[3]:=typ;
    move(dat,buf[4],siz);
    pipeLineSend(con.pipeT,buf,siz+3);
    exit;
    end;
  else begin; con.stat:=0;exit; end;
  end;
buf[1]:=typ;
writeWordMSB(buf[2],con.vers);
writeWordMSB(buf[4],siz);
move(dat,buf[6],siz);
inc(siz,5);
if (con.secure=0) then goto vege;
case con.vers of
  $302,$301:i:=tls_calcSum(con.macLRa,con.macLRb,con.pckLRa,con.pckLRb,buf,buf[siz+1],siz);
  $300:i:=ssl_calcSum(con.macLRa,con.macLRb,con.pckLRa,con.pckLRb,buf,buf[siz+1],siz);
  else i:=0;
  end;
inc(siz,i);
if (con.encSiz>1) then begin;
  inc(siz);
  i:=(siz-5) mod con.encSiz;
  if (i<1) then i:=con.encSiz;
  i:=con.encSiz-i;
  fillchar(buf[siz],i+1,i);
  inc(siz,i);
  if (con.vers>=$302) then begin;
    move(buf[6],buf[con.encSiz+6],siz-5);
    fillChar(buf[6],con.encSiz,random($100));
    inc(siz,con.encSiz);
    end;
  end;
CryptoAddEncrypt(con.encLRc,buf[6],siz-5);
writeWordMSB(buf[4],siz-5);
inc(con.pckLRa);
if (con.pckLRa=0) then inc(con.pckLRb);
vege:
pipeLineSend(con.pipeT,buf,siz);
con.time:=currentTime;
lastSent:=TCPprocessId;
End;






Function recvPackHndshk(var con:OneConnectionRecord;var pck:OnePacketRecord):Boolean;
Label f1,err;
Var i:LongInt;
Begin;
recvPackHndshk:=True;
if recvPackRecLay(con,pck) then exit;
if (pck.t=21) then exit; {alert}
if (pck.t<>22) then begin;
  con.err:='invalid record packet type';
  goto err;
  end;
if (con.secure=0) then begin;
  CryptoAddHasher(con.macLRa,pck.d,pck.s);
  CryptoAddHasher(con.macLRb,pck.d,pck.s);
  end;
dec(pck.s,4)
if (pck.s<0) then begin;
  f1:
  con.err:='invalid handshake packet size';
  err:
  con.secure:=0;
  con.stat:=0;
  exit;
  end;
pck.t:=pck.d[1];
i:=ReadLongMSB(pck.d[1]) and $ffffff;
if (pck.s<>i) then goto f1;
move(pck.d[5],pck.d,pck.s);
recvPackHndshk:=False;
End;




Procedure sendPackHndshk(var con:OneConnectionRecord;pck:OnePacketRecord);
Begin;
if (pck.t>$100) then begin;
  sendPackRecLay(con,pck.t and $ff,pck.d,pck.s);
  exit;
  end;
move(pck.d,pck.d[5],pck.s);
writeLongMSB(pck.d,(pck.t shl 24) or pck.s);
inc(pck.s,4);
if (con.secure=0) then begin;
  CryptoAddHasher(con.macLRa,pck.d,pck.s);
  CryptoAddHasher(con.macLRb,pck.d,pck.s);
  end;
sendPackRecLay(con,22,pck.d,pck.s);
End;

Procedure clearHndshkBuf(var con:OneConnectionRecord);
Begin;
End;
