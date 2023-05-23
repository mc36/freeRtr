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
Var
  maxPackSize:LongInt;
  loaderSAP:LongInt;
  etherAddr:OneAddressRecord;
  broadAddr:OneAddressRecord;
  serverAdr:OneAddressRecord;
  filHdr:xFile;
  filSeq:LongInt;
  filTim:LongInt;
  pipe:LongInt;
  packDat:array[1..1024*2] of byte;
  packSiz:LongInt;
  packSeq:LongInt;
  packMax:LongInt;
  packSap:LongInt;
  packDon:Boolean;
  packTyp:LongInt;




Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
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

Function compAddr(var ba,bb):Boolean;
Var
  aa:OneAddressRecord absolute ba;
  ab:OneAddressRecord absolute bb;
  i:LongInt;
Begin;
compAddr:=True;
for i:=1 to sizeof(aa) do if (aa[i]<>ab[i]) then exit;
compAddr:=False;
End;

Procedure openProcess(a:String);
Var
  buf:array[1..1024] of byte;
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


Procedure addSearchVect(var buffer;var siz:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  i:LongInt;
Begin;
addOneTag(buf,siz,$4,0,i); {search vector}
End;

Procedure sendFindFrame;
Var
  buf1,buf2:array[1..1024] of byte;
  i,o,p:LongInt;
Begin;
WriteLn('sending find packet...');
p:=0;
o:=0;
addOneTag(buf2,p,$4003,4,o); {correlator vector}
WriteWordMSB(i,maxPackSize);
addOneTag(buf1,o,$4009,2,i); {frame size sub-vector}
WriteWordMSB(i,1);
addOneTag(buf1,o,$400a,2,i); {connect class sub-vector}
addOneTag(buf2,p,$8,o,buf1); {connect info vector}
addOneTag(buf2,p,$4006,sizeof(etherAddr),etherAddr); {loader address vector}
WriteWordLSB(i,loaderSAP);
addOneTag(buf2,p,$4007,1,i); {loader sap vector}
addSearchVect(buf2,p);
o:=sizeof(broadAddr);
move(broadAddr,buf1,sizeof(broadAddr));
addOneTag(buf1,o,$1,p,buf2); {find command}
pipeLineSend(pipe,buf1,o);
End;

Procedure sendReqFrame;
Var
  buf1,buf2:array[1..1024] of byte;
  i,o,p:LongInt;
Begin;
WriteLn('sending file request '+BStr(filSeq)+' packet...');
p:=0;
WriteLongMSB(o,filSeq);
addOneTag(buf2,p,$4011,4,o); {sequence header}
o:=0;
WriteWordMSB(i,maxPackSize);
addOneTag(buf1,o,$4009,2,i); {frame size sub-vector}
WriteWordMSB(i,1);
addOneTag(buf1,o,$400a,2,i); {connect class sub-vector}
addOneTag(buf2,p,$8,o,buf1); {connect info vector}
addOneTag(buf2,p,$4006,sizeof(etherAddr),etherAddr); {loader address vector}
WriteWordLSB(i,loaderSAP);
addOneTag(buf2,p,$4007,1,i); {loader sap vector}
addSearchVect(buf2,p);
o:=sizeof(broadAddr);
move(serverAdr,buf1,sizeof(serverAdr));
addOneTag(buf1,o,16,p,buf2); {send file request}
pipeLineSend(pipe,buf1,o);
End;


Procedure processPacket(var buffer;siz:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  adr:OneAddressRecord absolute buffer;
  i,o,p:LongInt;

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
  $4018:begin;
    packSiz:=p-4;
    if (packSiz<1) then packSiz:=0;
    move(buf[ps+5],packDat,packSiz);
    end;
  $4009:packMax:=ReadWordMSB(buf[ps+5]);
  $4007:packSap:=buf[ps+5];
  $4011:packSeq:=ReadLongMSB(buf[ps+5]);
  $c014:packDon:=(buf[ps+13]<>$20);
  end;
inc(ps,p);
goto f1;
End;


Begin;
packTyp:=-1;
packDon:=False;
packSiz:=-1;
packSeq:=-1;
packMax:=maxPackSize;
packSap:=loaderSAP;
i:=ReadWordMSB(buf[7]);
o:=ReadWordMSB(buf[9]);
if (i>siz) then begin;
  WriteLn('got truncated packet from '+convAddr(adr));
  exit;
  end;
siz:=i;
packTyp:=o;
if not (o in [2,32]) then begin;
  WriteLn('got unknown packet from '+convAddr(adr));
  exit;
  end;
parser(buf[7]);
End;




Label f1,f2;
Var
  buf:array[1..1024*2] of byte;
  i,o:LongInt;
  a:String;
BEGIN;
WriteLn('rpl client v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<>2) then immErr('using: rpl.code <process> <file> [sap]');
a:=paramStr(2);
WriteLn('opening '+a+'...');
xCreate(a);
if (xOpen(filHdr,a,xGenFilMod_rw)<>0) then immErr('error!');
xTruncate(filHdr);
openProcess(paramStr(1));
loaderSAP:=BVal(paramStr(3));
if (loaderSAP=0) then loaderSAP:=$fc;
filSeq:=0;
filTim:=0;

f1:
relequish;
timer2start;
if (getTimePast(filTim)>5) then begin;
  sendFindFrame;
  filTim:=currentTime;
  end;
i:=sizeof(buf);
if (pipeLineRecv(pipe,buf,i)<>0) then i:=0;
if (i<1) then goto f1;
processPacket(buf,i);
if (packTyp<>2) then goto f1;
move(buf,serverAdr,sizeof(serverAdr));
WriteLn('got found packet from '+convAddr(serverAdr));
if (packMax<maxPackSize) then maxPackSize:=packMax;
filTim:=currentTime-9999;

f2:
if (getTimePast(filTim)>5) then begin;
  sendReqFrame;
  filTim:=currentTime;
  end;
i:=sizeof(buf);
if (pipeLineRecv(pipe,buf,i)<>0) then i:=0;
if (i<1) then begin;
  relequish;
  timer2start;
  goto f2;
  end;
if compAddr(buf,serverAdr) then goto f2;
processPacket(buf,i);
if (packTyp<>32) then goto f2;
if (filSeq<>packSeq) then goto f2;
inc(filSeq);
xBlockWrite(filHdr,packDat,packSiz);
Write(BStr(filSeq)+#13);
if not packDon then goto f2;
i:=xFilePos(filHdr);
xClose(filHdr);
WriteLn(BStr(i)+' bytes saved successfully!');
END.