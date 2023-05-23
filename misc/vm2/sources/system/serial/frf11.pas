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
  devPipe:LongInt;
  addrSiz:LongInt;
  seqTx:LongInt;
  seqRx:LongInt;
  myName:String;
  subCh:LongInt;
  upperPipe:LongInt;


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
myName:='frf11 on '+a;
End;



Procedure releq2dev;
Label f1;
Var
  buf:array[1..2048] of byte;
  i,o,p:LongInt;
  sub,pay,len,siz:LongInt;
Begin;
f1:
len:=sizeof(buf);
pipeLineRecv(devPipe,buf,len);
if (len<1) then begin;
  pipeLineStats(devPipe,o,i,i);
  if (o=0) then immErr('device closed pipeline!');
  exit;
  end;
p:=addrSiz+1;
dec(len,p);
pay:=0;
o:=buf[p];
sub:=o and $3f;
if (o and $80<>0) then begin;
  inc(p);
  dec(len);
  i:=buf[p];
  inc(sub,i and $c0);
  pay:=i and $f;
  end;
if (o and $40<>0) then begin;
  inc(p);
  len:=buf[p]-p+addrSiz;
  if (len<1) then begin;
    WriteLn('got truncated packet!');
    goto f1;
    end;
  end;
if (sub<>subCh) then begin;
  WriteLn('got for unknown subchannel '+Bstr(sub));
  goto f1;
  end;
if (pay<>0) then begin;
  WriteLn('got unknown payload '+BStr(pay));
  goto f1;
  end;
dec(len,2);
o:=ReadWordMSB(buf[p+1]);
inc(p,2);
i:=o shr 13;
o:=o and $1fff;
if (i<>6) then begin;
  WriteLn('got just part of segment!');
  goto f1;
  end;
if (o<>seqRx) then WriteLn(BStr((o-seqRx) and $1fff)+' packets lost!');
seqRx:=(o+1) and $1fff;
buf[p]:=subCh;
pipeLineSend(upperPipe,buf[p],len+1);
goto f1;
End;



Procedure releq2upper;
Label f1;
Var
  buf:array[1..2048] of byte;
  i,o,p:longInt;
  a:String;
Begin;
while (pipeLineGetIncoming(p)=0) do begin;
  if (upperPipe<>0) then begin;
    pipeLineClose(p);
    continue;
    end;
  upperPipe:=p;
  WriteLn('upper logged in!');
  a:='12341234'#0#0#0#0#0#0#0#0+chr(subCh)+#255+myName+#0;
  i:=1;move(i,a[1],sizeof(i));
  i:=1500;move(i,a[5],sizeof(i));
  pipeLineSend(upperPipe,a[1],length(a));
  end;
if (upperPipe=0) then exit;
f1:
p:=sizeof(buf);
pipeLineRecv(upperPipe,buf[addrSiz+3],p);
if (p<1) then begin;
  pipeLineStats(upperPipe,o,i,i);
  if (o<>0) then exit;
  WriteLn('upper logged out!');
  pipeLineClose(upperPipe);
  upperPipe:=0;
  exit;
  end;
fillchar(buf,addrSiz,$ff);
buf[addrSiz+1]:=subCh;
seqTx:=(seqTx+1) and $1fff;
writeWordMSB(buf[addrSiz+2],seqTx or $c000);
pipeLineSend(devPipe,buf,p+addrSiz+2);
goto f1;
End;




Label f1;
Var a:String;
BEGIN;
WriteLn('frameRelay framentation v1.0, done by Mc at '#%date' '#%time'.');
a:=paramStr(1);
if (a='') then immErr('using: frf11.code <process> [subchannel]');
subCh:=BVal(paramStr(2));
if (subCh=0) then subCh:=4;
openDevice(a);
seqRx:=0;
seqTx:=0;
upperPipe:=0;

pipeLineBegListen;
BugOS_SignDaemoning;

f1:
relequish;
releq2dev;
releq2upper;
goto f1;
END.