Function relequish2connection(var con:OneConnectionRecord):Boolean;
Label f1,f2;
Var
  buf:array[1..4096] of byte;
  tcp:OneTCPcommandHeader absolute buf;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  siz:LongInt;
  i,o,p:LongInt;
Begin;
if (GetTimePast(con.time)>5*60) then goto f1;
relequish2connection:=false;
case con.stat of
  2:begin; {connection estabilished}
    siz:=sizeof(buf);
    if (pipeLineRecv(con.pipe,buf[$100],siz)<>0) then siz:=0;
    dec(siz);
    if (siz<1) then begin;
      pipeLineStats(con.pipe,i,o,p);
      if (i=0) then goto f1;
      exit;
      end;
    move(buf[$100],a,sizeof(a));
    dec(siz,ab0);
    if (siz<0) then exit;
    i:=writeOneHeader(buf,a,con.prt,$9c,siz);
    move(buf[ab0+$101],buf[i+1],siz);
    pipeLineSend(LowerPipeline,buf,siz+i);
    con.time:=CurrentTime;
    end;
  1:begin; {wait for udp command}
    pipeLineStats(con.pipe,i,o,p);
    if (i=0) then goto f1;
    if (o<sizeof(tcp)) then exit;
    i:=sizeof(tcp);
    if (pipeLineRecv(con.pipe,tcp,i)<>0) then i:=0;
    if (i<>sizeof(tcp)) then exit;
    con.time:=CurrentTime;
    if (tcp.cmd<>4) then goto f1;
    p:=tcp.prt;
    BugOS_ProcessName(con.proc,buf,i,i,o);
    if (o and $40=0) then p:=-1;
    f2:
    i:=FindOneLocalSocket(p);
    if (p and $ff<>p) then i:=1;
    if (i<>0) then begin;
      p:=random($100);
      goto f2;
      end;
    con.prt:=p;
    con.stat:=2;
    fillchar(tcp,sizeof(tcp),0);
    tcp.cmd:=3;
    tcp.prt:=con.prt;
    pipeLineSend(con.pipe,tcp,sizeof(tcp));
    buf[1]:=networkAddrSiz;
    move(networkAddrDat,buf[2],networkAddrSiz);
    buf[networkAddrSiz+1]:=con.prt;
    pipeLineSend(con.pipe,buf,networkAddrSiz+1);
    end;
  else begin; f1:con.stat:=0;relequish2connection:=true; end;
  end;
End;




Procedure relequish2lower;
Label f1;
Var
  buf:array[1..4096] of byte;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  siz,hsz,typ,sck:LongInt;
  i,o,p:LongInt;
Begin;
if (getTimePast(lastHelloTime)>60) then begin;
  WriteLn('ESIS sending esh...');
  fillchar(buf,addressSize,0);
  buf[1]:=$09;
  buf[2]:=$00;
  buf[3]:=$2b;
  buf[4]:=$00;
  buf[5]:=$00;
  buf[6]:=$04;
  siz:=addressSize;
  inc(siz);buf[siz]:=$82;                       {nlpid: esis}
  inc(siz);buf[siz]:=networkAddrSiz+11;         {pdu size}
  inc(siz);buf[siz]:=$01;                       {version}
  inc(siz);buf[siz]:=$00;                       {reserved}
  inc(siz);buf[siz]:=$02;                       {type: esh}
  WriteWordMSB(buf[siz+1],300);inc(siz,2);      {hold time}
  inc(siz,2);                                   {checksum}
  inc(siz);buf[siz]:=$01;                       {number of addresses}
  inc(siz);buf[siz]:=networkAddrSiz;            {size of address}
  move(networkAddrDat,buf[siz+1],networkAddrSiz);
  inc(siz,networkAddrSiz);                      {the net address}
  i:=calcChkSum(buf[addressSize+1],siz-addressSize,8);
  WriteWordMSB(buf[addressSize+8],i);           {checksum}
  pipeLineSend(LowerPipeline,buf,siz);
  lastHelloTime:=currentTime;
  end;
f1:
siz:=sizeof(buf);
if (pipeLineRecv(LowerPipeline,buf,siz)<>0) then siz:=0;
if (siz<1) then exit;
i:=buf[addressSize+1];
if (i=$82) then begin;
  WriteLn('ESIS got from '+hwaddr2str(buf));
  dec(siz,addressSize);
  i:=buf[addressSize+2];
  if (i>siz) then begin;
    WriteLn('got truncated packet!');
    goto f1;
    end;
  siz:=i;
  if (buf[addressSize+3]<>1) then begin;
    WriteLn('got invalid version!');
    goto f1;
    end;
  if (buf[addressSize+5]<>$04) then begin;
    WriteLn('got invalid type!');
    goto f1;
    end;
  i:=ReadWordMSB(buf[addressSize+8]);
  if (i<>0) then if (calcChkSum(buf[addressSize+1],siz,8)<>i) then begin;
    WriteLn('got invalid checksum!');
    goto f1;
    end;
  WriteLn('got ish from '+netaddr2str(buf[addressSize+11],buf[addressSize+10]));
  move(buf,uplinkAddr,sizeof(uplinkAddr));
  goto f1;
  end;
if (i=$83) then begin;
  WriteLn('ISIS got from '+hwaddr2str(buf));
  goto f1;
  end;
if (i<>$81) then begin;
  WriteLn('got unknown (0x'+byte2hextype(i)+') from '+hwaddr2str(buf));
  goto f1;
  end;
dec(siz,addressSize);
hsz:=buf[addressSize+2];
if (siz<hsz) then begin;
  WriteLn('CLNP got truncated packet!');
  goto f1;
  end;
if (buf[addressSize+3]<>1) then begin;
  WriteLn('CLNP got invalid version!');
  goto f1;
  end;
typ:=buf[addressSize+5];
i:=ReadWordMSB(buf[addressSize+6]);
if (i>siz) then begin;
  WriteLn('CLNP got truncated data!');
  goto f1;
  end;
siz:=i-hsz;
if (siz<0) then begin;
  WriteLn('CLNP got too small packet!');
  goto f1;
  end;
i:=ReadWordMSB(buf[addressSize+8]);
if (i<>0) then if (calcChkSum(buf[addressSize+1],hsz,8)<>i) then begin;
  WriteLn('CLNP got invalid checksum!');
  goto f1;
  end;
p:=addressSize+10;
move(buf[p],a,sizeof(a));
inc(p,ab0+1);
o:=0;
for i:=1 to networkAddrSiz-1 do if (networkAddrDat[i]<>ab[i]) then inc(o);
if (ab0<>networkAddrSiz) then inc(o);
if (o<>0) then begin;
  WriteLn('CLNP got for invalid destination '+netaddr2str(ab[1],ab0));
  goto f1;
  end;
sck:=ab[networkAddrSiz];
move(buf[p],a,sizeof(a));
inc(p,ab0+1);
inc(p,2);
i:=ReadWordMSB(buf[p]);inc(p,2);
o:=ReadWordMSB(buf[p]);inc(p,2);
if (i<>0) or (o-hsz<>siz) then begin;
  WriteLn('CLNP got fragment from '+netaddr2str(ab[1],ab0));
  goto f1;
  end;
if (typ and $1f=$1e) then begin;
  WriteLn('CLNP got echo request from '+netaddr2str(ab[1],ab0));
  inc(siz,hsz);
  move(buf[addressSize+1],buf[$100],siz);
  i:=writeOneHeader(buf,a,sck,$9f,siz);
  move(buf[$100],buf[i+1],siz);
  pipeLineSend(LowerPipeline,buf,siz+i);
  goto f1;
  end;
if (typ and $1f<>$1c) then begin;
  WriteLn('CLNP got '+getPDUtype(typ)+' pdu from '+netaddr2str(ab[1],ab0));
  goto f1;
  end;
p:=FindOneLocalSocket(sck);
if (p<1) then begin;
  WriteLn('CLNP got data from '+netaddr2str(ab[1],ab0)+' for port '+BStr(sck));
  goto f1;
  end;
i:=ab0+1;
move(a,buf,i);
move(buf[addressSize+hsz+1],buf[i+1],siz);
pipeLineSend(ConnectionDat^[p].pipe,buf,siz+i);
goto f1;
End;
