Const
  retryValue=8;
  timeoutVal=5;




Function doConn(var con:OneConnectionRecord):Boolean;
Label f1;
Var
  buf:array[1..4*1024] of byte;
  i,o,p:LongInt;
  a,b:String;
Begin;
doConn:=False;
if (GetTimePast(con.time)<timeoutVal) then exit;
con.time:=CurrentTime;
dec(con.retry);
if (con.retry<0) then begin;
  WriteLn('connection timed out with '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  goto f1;
  end;
case con.stat of
  1:begin; {tx}
    WriteWordMSB(buf[1],3);
    WriteWordMSB(buf[3],con.sequ);
    move(con.txBuf,buf[5],con.txSiz);
    UDPsendPacket(pipeDat,con.adr,con.prt,buf,con.txSiz+4);
    end;
  2:begin; {rx}
    WriteWordMSB(buf[1],4);
    WriteWordMSB(buf[3],con.sequ-1);
    UDPsendPacket(pipeDat,con.adr,con.prt,buf,4);
    end;
  else begin; f1: con.stat:=0;doConn:=True;exit; end;
  end;
End;





Function ReceivePacket:Boolean;
Label f1,f2,f3;
Var
  buf:array[1..4*1024] of byte;
  con:OneConnectionRecord;
  conN:LongInt;
  a:OneTCPaddressRecord;
  i,o,p:LongInt;
  b,c:String;
Begin;
ReceivePacket:=False;
p:=sizeof(buf);
if not UDPreceivePacket(pipeCmd,a,o,buf,p) then begin;
  ReceivePacket:=True;
  i:=FindOneAddress(a,o);
  if (i<>0) then begin;
    WriteLn('got request again from '+ipAddr2string(a)+' '+BStr(o));
    exit;
    end;
  fillChar(con,sizeof(con),0);
  con.adr:=a;
  con.prt:=o;
  i:=ReadWordMSB(buf[1]);
  case i of
    1:begin; con.stat:=1;b:='read'; end; {tx}
    2:begin; con.stat:=2;b:='write'; end; {rx}
    else begin;
      b:='invalid opcode';
      p:=4;
      goto f1;
      end;
    end;
  dec(p,2);
  if (p>$ff) then p:=$ff;
  move(buf[3],c[1],p);
  c[0]:=chr(p);
  c:=copy(c,1,pos(#0,c)-1);
  case con.stat of
    1:p:=xGenFilMod_r; {tx}
    2:p:=xGenFilMod_rw; {rx}
    else exit;
    end;
  WriteLn('got '+b+' "'+c+'" from '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  kicserel('/',#0,c);
  kicserel('\','/',c);
  kicserel(#0,'\',c);
  if ResizeMem(ConnectionNum+1) then begin;
    WriteLn('failed to allocate memory!');
    b:='out of memory';
    p:=3;
    exit;
    end;
  if (con.stat=2) then i:=xCreate(c) else i:=0;
  if (i<>0) then begin;
    b:='error creating: '+xGetErrorName(i);
    ResizeMem(ConnectionNum-1);
    p:=1;
    goto f1;
    end;
  i:=xOpen(con.fileH,c,p);
  if (i<>0) then begin;
    b:='error opening: '+xGetErrorName(i);
    ResizeMem(ConnectionNum-1);
    xClose(con.fileH);
    p:=2;
    goto f1;
    end;
  conN:=ConnectionNum;
  con.time:=-99999;
  con.retry:=retryValue;
  con.fileP:=0;
  con.sequ:=0;
  case con.stat of
    1:begin;
      con.fileS:=xFileSize(con.fileH);
      goto f2;
      end;
    2:inc(con.sequ);
    else con.stat:=0;
    end;
  ConnectionDat^[conN]:=con;
  exit;
  f1:
  WriteLn('sending '+b+' to '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  WriteWordMSB(buf[1],5);
  WriteWordMSB(buf[3],p);
  b:=b+#0;
  move(b[1],buf[5],length(b));
  UDPsendPacket(pipeDat,con.adr,con.prt,buf,length(b)+4);
  exit;
  end;
if UDPreceivePacket(pipeDat,a,o,buf,p) then exit;
ReceivePacket:=True;
conN:=FindOneAddress(a,o);
if (conN=0) then begin;
  con.adr:=a;
  con.prt:=o;
  b:='unknown transfer id';
  p:=5;
  goto f1;
  end;
con:=ConnectionDat^[conN];
case con.stat of
  1:begin; {tx}
    i:=ReadWordMSB(buf[1]);
    if (i<>4) then exit;
    i:=ReadWordMSB(buf[3]);
    if (i<>con.sequ and $ffff) then exit;
    if (con.txSiz<transmitBufSize) then begin;
      WriteLn('transmit succeeded with '+ipAddr2string(con.adr)+' '+BStr(con.prt));
      con.stat:=0;
      goto f3;
      end;
    f2:
    inc(con.sequ);
    inc(con.fileP,con.txSiz);
    i:=con.fileS-con.fileP;
    if (i>transmitBufSize) then i:=transmitBufSize;
    xBlockRead(con.fileH,con.txBuf,i);
    con.txSiz:=i;
    end;
  2:begin; {rx}
    i:=ReadWordMSB(buf[1]);
    if (i<>3) then exit;
    i:=ReadWordMSB(buf[3]);
    if (i<>con.sequ and $ffff) then exit;
    dec(p,4);
    xBlockWrite(con.fileH,buf[5],p);
    inc(con.sequ);
    inc(con.fileP,p);
    if (p<transmitBufSize) then WriteLn('receive succeeded with '+ipAddr2string(con.adr)+' '+BStr(con.prt));
    end;
  else con.stat:=0;
  end;
f3:
con.time:=-99999;
con.retry:=retryValue;
ConnectionDat^[conN]:=con;
End;
