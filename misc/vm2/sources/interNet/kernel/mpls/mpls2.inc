Procedure sendOneConnPacket(var con:OneConnectionRecord;var buffer;size,from:LongInt);
Var
  buf:array[1..1] of byte  absolute buffer;
  hdr:OneLDPmessageHeader;
  i,o,p:LongInt;
  a:String;
Begin;
{$ifdef debug}
Write('sent');
for i:=1 to size do write(' '+byte2hextype(buf[i]));
WriteLn('');
{$endif}
fillchar(hdr,sizeof(hdr),0);
WriteWordMSB(hdr.ver,1);
WriteWordMSB(hdr.len,size+6);
hdr.lsr:=localAddr;
hdr.lsp:=0;
if (from=0) then begin;
  pipeLineSend(con.pipe,hdr,sizeof(hdr));
  pipeLineSend(con.pipe,buf,size);
  exit;
  end;
move(buf,buf[sizeof(hdr)+1],size);
move(hdr,buf,sizeof(hdr));
a:=convertFromIP4addr(con.addr);
UDPsendPacket(localPipe,a[1],localPort,buf,size+sizeof(hdr));
End;



Procedure sendOneConnKeepalive(var con:OneConnectionRecord);
Var
  buf:array[1..512] of byte;
  p:LongInt;
Begin;
inc(con.sndM);
p:=0;
addOneMsgHeader(buf,p,$201,con.sndM);
sendOneConnPacket(con,buf,p,0);
con.timK:=currentTime;
End;

Procedure sendOneConnHello(var con:OneConnectionRecord);
Var
  buf:array[1..512] of byte;
  p:LongInt;
Begin;
p:=0;
addOneTLV(buf,p,$400,#0#$5a#$c0#0);
addOneTLV(buf,p,$401,convertLong2str(localAddr));
addOneMsgHeader(buf,p,$100,0);
sendOneConnPacket(con,buf,p,1);
con.timH:=currentTime;
End;

Procedure relequish2bcast;
Var
  con:OneConnectionRecord;
  buf:array[1..512] of byte;
  p:LongInt;
Begin;
if (getTimePast(lastBcastMsg)<15) then exit;
lastBcastMsg:=currentTime;
fillchar(con,sizeof(con),0);
WriteLongMSB(con.addr,$e0000002);
p:=0;
addOneTLV(buf,p,$400,#0#$5a#0#0);
addOneTLV(buf,p,$401,convertLong2str(localAddr));
addOneMsgHeader(buf,p,$100,0);
sendOneConnPacket(con,buf,p,1);
End;



Procedure gotOneConnMessage(var con:OneConnectionRecord;var buffer;size,typ:LongInt);
Label f1,f2;
Var
  buf:array[1..1] of byte  absolute buffer;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  ps,i,o,p:LongInt;
  adr,len,vct,lab:LongInt;
Begin;
{$ifdef debug}
WriteLn('msg '+decodeOneMessageType(typ));
{$endif}
ps:=0;
adr:=0;
len:=-666;
vct:=-666;
lab:=-1;
f1:
if (ps>=size) then goto f2;
a:=getOneTLV(buf,ps,o);
if (o=$200) then begin;
  lab:=ReadLongMSB(ab[1]);
  goto f1;
  end;
if (o<>$100) then goto f1;
if (ab[1]=2) and (ab[2]=0) and (ab[3]=1) then begin;
  convertFromIp4prefix(ab[4],adr,len);
  vct:=-1;
  goto f1;
  end;
if (ab[1]=128) then begin;
  vct:=ReadWordMSB(ab[2]);
  adr:=ReadLongMSB(ab[9]);
  len:=-1;
  goto f1;
  end;
goto f1;
f2:
{$ifdef debug}
WriteLn('label='+BStr(lab)+' type='+BStr(vct)+' len='+BStr(len)+' adr='+BStr(adr));
{$endif}
if (typ=$400) then begin;
  if (lab=-1) then exit;
  end else begin;
  if (typ<>$402) then exit;
  lab:=-1;
  end;
if (len=-1) then begin;
  o:=findOneSession(con.conn,adr);
  if (o<1) then exit;
  SessionDat[o].labl:=lab;
  {$ifdef debug}
  WriteLn('--> inner label '+BStr(lab)+' for '+BStr(o));
  {$endif}
  exit;
  end;
if (con.conn<>1) then exit;
if (len<>32) then exit;
o:=findOnePeerAddr(adr);
if (o<1) then exit;
ConnectionDat[o].labl:=lab;
{$ifdef debug}
WriteLn('--> outer label '+BStr(lab)+' for '+BStr(o));
{$endif}
End;



Procedure gotOneConnPacket(var con:OneConnectionRecord;var buffer;size,from:LongInt);
Label f1;
Var
  buf:array[1..1] of byte  absolute buffer;
  hdr:OneMessageHeader;
  i,o,p:LongInt;
Begin;
{$ifdef debug}
Write('got');
for i:=1 to size do write(' '+byte2hextype(buf[i]));
WriteLn('');
{$endif}
con.rcvT:=currentTime;
inc(con.rcvN);
p:=0;
f1:
if (p>=size) then exit;
move(buf[p+1],hdr,sizeof(hdr));
o:=ReadWordMSB(hdr.len)-4;
inc(p,sizeof(hdr));
gotOneConnMessage(con,buf[p+1],o,ReadWordMSB(hdr.typ));
inc(p,o);
goto f1;
End;
