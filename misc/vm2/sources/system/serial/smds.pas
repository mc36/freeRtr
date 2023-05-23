{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$include \sources\internet\kernel\utils\timer2.inc}


Var
  txSeqNum:LongInt;
  rxSeqNum:LongInt;
  srcAddr1,srcAddr2:LongInt;
  trgAddr1,trgAddr2:LongInt;
  devPipe:LongInt;
  addrSiz:LongInt;
  myName:String;
  upperPipe:LongInt;

Const beSizDifference=8;
Type
  OnePacketHeaderRecord=record
    unk1:byte;
    unk2:byte;
    unk3:byte;
    unk4:byte;
    res1:byte;
    betag:byte;
    besiz:word;
    trg1:LongInt;
    trg2:LongInt;
    src1:LongInt;
    src2:LongInt;
    hlpl:byte;
    cibhel:byte;
    brdgng:word;
    hed:array[1..12] of byte;
    dsap:byte;
    ssap:byte;
    ctrl:byte;
    oui:array[1..3] of byte;
    end;
  OnePacketFooterRecord=record
    res1:byte;
    betag:byte;
    besiz:word;
    end;



Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;

Procedure dumpPacket(var buffer;siz:LongInt;act:String);
Var
  buf:array[1..1] of byte absolute buffer;
  i:LongInt;
Begin;
Write(act+' '+BStr(siz)+':');
for i:=1 to siz do write(' '+byte2hextype(buf[i]));
WriteLn('');
End;

Procedure openDevice(a:String);
Var
  buf:array[1..1024] of byte;
  i,o,p:LongInt;
Begin;
WriteLn('process: '+a);
o:=BugOS_findProcNam(a);
if (o=0) then immErr('process not found!');
WriteLn('process#: '+BStr(o));
i:=pipeLineCreate(devPipe,o,65536,true);
if (i<>0) then immErr('unabled to create pipeline!');
WriteLn('pipeline#: '+BStr(devPipe));
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(devPipe,buf,i)<>0) then i:=0;
if (i<1) then immErr('initial packet not received!');
move(buf[1],p,sizeof(p));
WriteLn('address size: '+BStr(p));
addrSiz:=p;
move(buf[5],i,sizeof(i));
WriteLn('packet size: '+BStr(i));
o:=17;
Write('station address: ');
for i:=1 to p do begin;
  write(byte2hextype(buf[o])+'-');
  inc(o);
  end;
WriteLn(#8' ');
Write('broadcast address: ');
for i:=1 to p do begin;
  write(byte2hextype(buf[o])+'-');
  inc(o);
  end;
WriteLn(#8' ');
a:='';
while (buf[o]<>0) do begin;
  a:=a+chr(buf[o]);
  inc(o);
  end;
writeln('device name: "'+a+'"');
myName:='smds on '+a;
End;


Procedure getOneAddress(var a1,a2:LongInt;a:string);
Var
  i,o,p:LongInt;
  b:array[1..8] of byte;
Begin;
kicserel('.','',a);
kicserel('-','',a);
fillchar(b,sizeof(b),$ff);
p:=0;
while (a<>'') do begin;
  inc(p);
  b[p]:=BVal('$'+copy(a,1,2));
  a:=copy(a,3,666);
  end;
move(b[1],a1,sizeof(a1));
move(b[5],a2,sizeof(a2));
End;



Procedure releq2dev;
Label f1;
Var
  i,o,p:LongInt;
  buf:array[1..2048] of byte;
  hdr:OnePacketHeaderRecord absolute buf;
  ftr:OnePacketFooterRecord;
  siz:LongInt;
Begin;
f1:
siz:=sizeof(buf);
pipeLineRecv(devPipe,buf,siz);
if (siz<1) then begin;
  pipeLineStats(devPipe,o,i,i);
  if (o=0) then immErr('device closed pipeline!');
  exit;
  end;

dec(siz,sizeof(ftr));
if (siz<sizeof(hdr)) then begin;
  writeln('got too small packet!');
  exit;
  end;
i:=readWordMSB(hdr.besiz)+beSizDifference;
if (siz<i) then begin;
  writeln('got truncated packet!');
  exit;
  end;
move(buf[i+1],ftr,sizeof(ftr));
if (hdr.betag<>ftr.betag) or (hdr.besiz<>ftr.besiz) then begin;
  writeln('header differs from footer!');
  exit;
  end;
if (hdr.cibhel and 3<>3) then begin;
  writeln('invalid header extension size!');
  exit;
  end;
siz:=i-sizeof(hdr);
dec(siz,hdr.hlpl and 3);
if (hdr.cibhel and 4<>0) then dec(siz,4);
rxSeqNum:=hdr.betag;

buf[sizeof(hdr)]:=$ff;
pipeLineSend(upperPipe,buf[sizeof(hdr)],siz+1);
goto f1;
End;



Procedure releq2upper;
Label f1;
Var
  a:String;
  buf:array[1..2048] of byte;
  hdr:OnePacketHeaderRecord absolute buf;
  ftr:OnePacketFooterRecord;
  i,o,p:LongInt;
  siz:LongInt;
Begin;
while (pipeLineGetIncoming(p)=0) do begin;
  if (upperPipe<>0) then begin;
    pipeLineClose(p);
    continue;
    end;
  upperPipe:=p;
  WriteLn('upper logged in!');
  BugOS_MyProcessInfo(i,o,p);
  i:=i xor p xor o;
  p:=((i shr 16) xor i) and $ffff;
  a:='12341234'#0#0#0#0#0#0#0#0+chr(p)+#255+myName+#0;
  i:=1;move(i,a[1],sizeof(i));
  i:=1500;move(i,a[5],sizeof(i));
  pipeLineSend(upperPipe,a[1],length(a));
  end;
if (upperPipe=0) then exit;
f1:
siz:=sizeof(buf);
pipeLineRecv(upperPipe,buf[sizeof(hdr)],siz);
if (siz<1) then begin;
  pipeLineStats(upperPipe,o,i,i);
  if (o<>0) then exit;
  WriteLn('upper logged out!');
  pipeLineClose(upperPipe);
  upperPipe:=0;
  exit;
  end;
dec(siz);

fillchar(hdr,sizeof(hdr),0);
inc(txSeqNum);
hdr.unk1:=$05;
hdr.unk2:=$03;
hdr.betag:=txSeqNum;
hdr.trg1:=trgAddr1;
hdr.trg2:=trgAddr2;
hdr.src1:=srcAddr1;
hdr.src2:=srcAddr2;
i:=((siz-2) and 3);
if (i>0) then i:=4-i;
inc(siz,i);
hdr.hlpl:=i or 4;
hdr.cibhel:=3;
hdr.brdgng:=0;
hdr.hed[1]:=3;
hdr.hed[3]:=1;
hdr.dsap:=$aa;
hdr.ssap:=$aa;
hdr.ctrl:=3;
inc(siz,sizeof(hdr));
writeWordMSB(hdr.besiz,siz-beSizDifference);
ftr.res1:=hdr.res1;
ftr.betag:=hdr.betag;
ftr.besiz:=hdr.besiz;
move(ftr,buf[siz+1],sizeof(ftr));
inc(siz,sizeof(ftr));

pipeLineSend(devPipe,buf,siz);
goto f1;
End;



Label f1;
Var a:String;
BEGIN;
WriteLn('switched megabit data service v1.0, done by Mc at '#%date' '#%time'.');
a:=paramStr(1);
if (a='') then immErr('using: smds.code <process> <locaddr> <remaddr>');
openDevice(a);
txSeqNum:=0;
rxSeqNum:=0;
upperPipe:=0;
getOneAddress(srcAddr1,srcAddr2,paramStr(2));
getOneAddress(trgAddr1,trgAddr2,paramStr(3));

pipeLineBegListen;
BugOS_SignDaemoning;

f1:
relequish;
releq2dev;
releq2upper;
goto f1;
END.