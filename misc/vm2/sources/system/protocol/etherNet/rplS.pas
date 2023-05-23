{{$define debug}
{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc param.inc}
{$sysinc hex.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Type
  OneAddressRecord=array[1..6] of byte;
  OneConnectionRecord=record
    stat:LongInt;               {status: 1-wait, 2-send}
    time:LongInt;               {time when action happened}
    addr:OneAddressRecord;      {ethernet address of client}
    seq:LongInt;                {next sequence number}
    max:LongInt;                {block size negotiated}
    sap:LongInt;                {sap address}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  maxPackSize:LongInt;
  etherAddr:OneAddressRecord;
  filHdr:xFile;
  filSiz:LongInt;
  trgOfs:LongInt;
  pipe:LongInt;



Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

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

Function findOneAddr(var a:OneAddressRecord):LongInt;
Label f1;
Var
  b:OneAddressRecord;
  i,o:LongInt;
Begin;
for o:=1 to ConnectionNum do begin;
  b:=ConnectionDat^[o].addr;
  for i:=1 to sizeof(a) do if (a[i]<>b[i]) then goto f1;
  findOneAddr:=o;
  exit;
  f1:
  end;
findOneAddr:=0;
End;

Function convAddr(var a:OneAddressRecord):String;
Var
  i:LongInt;
  b:String;
Begin;
b:='';
for i:=1 to sizeof(a) do b:=b+'-'+byte2hextype(a[i]);
convAddr:=copy(b,2,666);
End;

Procedure openProcess(a:String);
Var
  buf:array[1..1024] of byte;
  broadAddr:OneAddressRecord;
  i,o:LongInt;
Begin;
WriteLn('process: '+a);
o:=BugOS_findProcNam(a);
if (o=0) then immErr('process not found!');
WriteLn('process#: '+BStr(o));
i:=pipeLineCreate(pipe,o,65536,true);
if (i<>0) then immErr('unabled to create pipeline!');
WriteLn('pipeline#: '+BStr(pipe));
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(pipe,buf,i)<>0) then i:=0;
if (i<1) then immErr('initial packet not received!');
move(buf[1],i,sizeof(i));
move(buf[5],maxPackSize,sizeof(maxPackSize));
WriteLn('address size: '+BStr(i));
WriteLn('packet size: '+BStr(maxPackSize));
o:=17;
move(buf[o],etherAddr,sizeof(etherAddr));inc(o,sizeof(etherAddr));
move(buf[o],broadAddr,sizeof(broadAddr));inc(o,sizeof(broadAddr));
WriteLn('station address: '+convAddr(etherAddr));
WriteLn('broadcast address: '+convAddr(broadAddr));
a:='';
while (buf[o]<>0) do begin;
  a:=a+chr(buf[o]);
  inc(o);
  end;
WriteLn('device name: "'+a+'"');
dec(maxPackSize,128);
End;



Function getOneVectorType(i:LongInt):String;
Var a:String;
Begin;
case i of
     $1:a:='find command';
     $2:a:='found frame';
     $4:a:='search vector';
     $8:a:='connect info vector';
    $10:a:='send file request';
    $20:a:='file data response';
  $4003:a:='correlator vector';
  $4006:a:='loader address vector';
  $4007:a:='loader sap vector';
  $4009:a:='frame size sub-vector';
  $400a:a:='connect class sub-vector';
  $400b:a:='response correlator';
  $400c:a:='set address vector';
  $4011:a:='sequence header';
  $4018:a:='file data vector';
  $c005:a:='loader info sub-vector';
  $c014:a:='loader header';
  else a:='unknown:'+BStr(i);
  end;
getOneVectorType:=a;
End;


Procedure dumpOnePacket(var buffer;beg:String);
Label f1;
Var
  buf:array[1..1] of byte absolute buffer;
  siz,ps:LongInt;
  i,o,p:LongInt;
  a:String;
Begin;
siz:=ReadWordMSB(buf[1]);
o:=ReadWordMSB(buf[3]);
writeln(beg+getOneVectorType(o)+' ('+BStr(siz)+' bytes)');
beg:=beg+'  ';
ps:=4;
f1:
if (ps>=siz) then exit;
p:=ReadWordMSB(buf[ps+1]);
o:=ReadWordMSB(buf[ps+3]);
if (p<4) then p:=4;
if (o in [1,2,4,8,16,32]) then begin;
  dumpOnePacket(buf[ps+1],beg);
  inc(ps,p);
  goto f1;
  end;
write(beg+getOneVectorType(o)+':');
inc(ps,4);
for i:=1 to p-4 do begin;
  inc(ps);
  write(' '+byte2hextype(buf[ps]));
  end;
writeln('');
goto f1;
End;

Procedure addOneTag(var buffer;var siz:LongInt;typ,len:LongInt;var data);
Var
  buf:array[1..1] of byte absolute buffer;
Begin;
writeWordMSB(buf[siz+1],len+4);
writeWordMSB(buf[siz+3],typ);
inc(siz,4);
move(data,buf[siz+1],len);
inc(siz,len);
End;



Procedure processPacket(var buffer;siz:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  dat:array[1..512] of byte;
  adr:OneAddressRecord absolute buffer;
  i,o,p:LongInt;
  con:OneConnectionRecord;
  num,pck_sap,pck_max,pck_seq:LongInt;

Procedure parser(var buffer);
Label f1;
Var
  buf:array[1..1] of byte absolute buffer;
  siz,ps:LongInt;
  i,o,p:LongInt;
Begin;
siz:=ReadWordMSB(buf[1]);
o:=ReadWordMSB(buf[3]);
ps:=4;
f1:
if (ps>=siz) then exit;
p:=ReadWordMSB(buf[ps+1]);
o:=ReadWordMSB(buf[ps+3]);
if (p<4) then p:=4;
case o of
  1,2,4,8,16,32:begin;
    parser(buf[ps+1]);
    inc(ps,p);
    goto f1;
    end;
  $4009:pck_max:=ReadWordMSB(buf[ps+5]);
  $4007:pck_sap:=buf[ps+5];
  $4011:pck_seq:=ReadLongMSB(buf[ps+5]);
  end;
inc(ps,p);
goto f1;
End;


Begin;
i:=ReadWordMSB(buf[7]);
o:=ReadWordMSB(buf[9]);
if (i>siz) then begin;
  WriteLn('got truncated packet from '+convAddr(adr));
  exit;
  end;
siz:=i;
WriteLn('got '+getOneVectorType(o)+' from '+convAddr(adr));
if not (o in [1,16]) then exit;
{$ifdef debug}dumpOnePacket(buf[7],'');{$endif}

num:=findOneAddr(adr);
if (num>0) then con:=ConnectionDat^[num] else begin;
  num:=ConnectionNum+1;
  if ResizeMem(num) then begin;
    WriteLn('failed to allocate memory!');
    exit;
    end;
  fillchar(con,sizeof(con),0);
  con.addr:=adr;
  end;

pck_sap:=con.sap;
pck_max:=con.max;
pck_seq:=con.seq;
parser(buf[7]);
if (pck_max>maxPackSize) then pck_max:=maxPackSize;
if (pck_max<512) then pck_max:=512;
pck_max:=pck_max and $fffff0;
con.sap:=pck_sap;
con.max:=pck_max;
con.seq:=pck_seq;
if (o=1) then begin;
  con.stat:=1;
  siz:=0;
  i:=0;
  addOneTag(dat,siz,$4003,4,i); {correlator vector}
  addOneTag(dat,siz,$400b,1,i); {response correlator}
  addOneTag(dat,siz,$400c,sizeof(con.addr),con.addr); {set address vector}
  addOneTag(dat,siz,$4006,sizeof(etherAddr),etherAddr); {loader address vector}
  p:=0;
  WriteWordMSB(i,con.max);
  addOneTag(buf,p,$4009,2,i); {frame size sub-vector}
  i:=0;
  addOneTag(buf,p,$400a,2,i); {connect class sub-vector}
  addOneTag(dat,siz,$8,p,buf); {connect info vector}
  writeWordLSB(i,con.sap);
  addOneTag(dat,siz,$4007,1,i); {loader sap vector}
  move(con.addr,buf,sizeof(con.addr));
  p:=sizeof(con.addr);
  addOneTag(buf,p,$2,siz,dat); {found frame}
  {$ifdef debug}write('sending ');dumpOnePacket(buf[7],'');{$endif}
  pipeLineSend(pipe,buf,p);
  end else begin;
  con.stat:=2;
  end;
con.time:=currentTime;
ConnectionDat^[num]:=con;
End;

Function releq2conn(var con:OneConnectionRecord):Boolean;
Var
  i,o,p,q:LongInt;
  buf1:array[1..1024*2] of byte;
  buf2:array[1..1024*2] of byte;
Begin;
releq2conn:=False;
if (getTimePast(con.time)>60) then con.stat:=0;
case con.stat of
  2:begin; {sending file}
    if (con.time=currentTime) then exit;
    p:=con.seq*con.max;
    o:=filSiz-p;
    if (o>con.max) then o:=con.max;
    if (o<0) then o:=0;
    q:=0;
    writeLongMSB(buf1,con.seq);
    addOneTag(buf2,q,$4011,4,buf1); {sequence header}
    WriteLongMSB(buf1[1],trgOfs+p);
    WriteLongMSB(buf1[5],trgOfs);
    if (o>0) then i:=$20 else begin;
      i:=$c0;
      con.stat:=1;
      end;
    buf1[9]:=i;
    addOneTag(buf2,q,$c014,9,buf1); {loader header}
    xSeek(filHdr,p);
    xBlockRead(filHdr,buf1,o);
    i:=o and $f;
    if (i>0) then inc(o,$10-i);
    addOneTag(buf2,q,$4018,o,buf1); {file data vector}
    move(con.addr,buf1,sizeof(con.addr));
    p:=sizeof(con.addr);
    addOneTag(buf1,p,$20,q,buf2); {found frame}
    {$ifdef debug}write('sending ');dumpOnePacket(buf1[7],'');{$endif}
    pipeLineSend(pipe,buf1,p);
    inc(con.seq);
    con.time:=currentTime;
    end;
  1:begin; {waiting for remote}
    end;
  else releq2conn:=True;
  end;
End;


Label f1;
Var
  buf:array[1..1024*2] of byte;
  i,o:LongInt;
  a:String;
BEGIN;
WriteLn('rpl server v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<>3) then immErr('using: rpl.code <process> <offset> <file>');

trgOfs:=BVal(paramStr(2));
a:=paramStr(3);
WriteLn('opening '+a+'...');
if (xOpen(filHdr,a,xGenFilMod_r)<>0) then immErr('error!');
filSiz:=xFileSize(filHdr);
ResizeMem(0);
openProcess(paramStr(1));
WriteLn('file size: '+BStr(filSiz));
WriteLn('target offset: '+BStr(trgOfs));

BugOS_SignDaemoning;
f1:
relequish;
timer2start;
i:=sizeof(buf);
if (pipeLineRecv(pipe,buf,i)<>0) then i:=0;
if (i>1) then processPacket(buf,i);
for i:=ConnectionNum downto 1 do if releq2conn(ConnectionDat^[i]) then begin;
  ConnectionDat^[i]:=ConnectionDat^[ConnectionNum];
  ResizeMem(ConnectionNum-1);
  end;
goto f1;
END.