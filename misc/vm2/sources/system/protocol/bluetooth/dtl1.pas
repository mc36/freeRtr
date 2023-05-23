{{$define debug}
{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc memory.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

{$include \sources\system\serial\serial.inc}



Type
  oneHeaderRecord=record
    typ:byte;
    zro:byte;
    siz:word;
    end;
Var
  pckBufDat:Array[1..4096] of byte;
  pckBufPos:LongInt;
  pckBufNed:LongInt;
  upperProc:LongInt;



Procedure sendOnePacket(var buffer;o:LongInt);
Label f1;
Var
  buf:array[1..1] of byte absolute buffer;
  hdr:oneHeaderRecord absolute buffer;
  i:LongInt;
Begin;
{$ifdef debug}
write('tx:');
for i:=1 to o do write(' '+byte2hextype(buf[i]));
writeLn('');
{$endif}
dec(o);
i:=buf[1];
move(buf[2],buf[sizeof(hdr)+1],o);
fillchar(hdr,sizeof(hdr),0);
hdr.typ:=i or $80;
WriteWordLSB(hdr.siz,o);
inc(o,sizeof(hdr));
while (o and 1<>0) do begin;
  inc(o);
  buf[o]:=0;
  end;
pipeLineSend(serialData,buf,o);
End;


Procedure gotOneChar(i:LongInt);
Label f1;
Var
  hdr:oneHeaderRecord absolute pckBufDat;
  o:LongInt;
Begin;
inc(pckBufPos);
pckBufDat[pckBufPos]:=i;
if (pckBufNed<1) then begin;
  if (pckBufPos<sizeof(hdr)) then exit;
  if not (hdr.typ in [$80..$87]) then begin;
    f1:
    dec(pckBufPos);
    move(pckBufDat[2],pckBufDat,pckBufPos);
    exit;
    end;
  i:=ReadWordLSB(hdr.siz)+sizeof(hdr);
  if (i>sizeof(pckBufDat)) then goto f1;
  pckBufNed:=i;
  end;
if (pckBufPos<pckBufNed) then exit;
o:=pckBufNed-sizeof(hdr)+1;
pckBufDat[1]:=hdr.typ and $7f;
move(pckBufDat[sizeof(hdr)+1],pckBufDat[2],o);
{$ifdef debug}
write('rx:');
for i:=1 to o do write(' '+byte2hextype(pckBufDat[i]));
writeLn('');
{$endif}
pipeLineSend(upperProc,pckBufDat,o);
pckBufPos:=0;
pckBufNed:=0;
End;





Label f1;
Var
  a:String;
  i,o:LongInt;
  buf:array[1..4096] of byte;
BEGIN;
WriteLn('dtl1 driver v1.0, done by Mc at '#%date' '#%time'.');
a:=paramStr(1);
serialProc:=BVal(a);
if (serialProc=0) then serialProc:=BugOS_findProcNam(a);
serialPort:=BVal(paramStr(2));
if (serialPort<1) then immErr('using: dtl1.code <process> <port>');
SerialOpen;

pckBufPos:=0;
pckBufNed:=0;

Write('waiting for upper level...');
BugOS_SignDaemoning;
pipeLineBegListen;
while (pipeLineGetIncoming(upperProc)<>0) do relequish;
pipeLineEndListen;
a:='12341234'#0#0#0#0#0#0#0#0#16#255'bluetooth hci nokia dtl1'#0;
i:=1;move(i,a[1],sizeof(i));
i:=1500;move(i,a[5],sizeof(i));
pipeLineSend(upperProc,a[1],length(a));
WriteLn(' done!');

f1:
relequish;

o:=sizeof(buf);
if (pipeLineRecv(upperProc,buf,o)<>0) then o:=0;
if (o>0) then sendOnePacket(buf,o);

o:=sizeof(buf);
if (pipeLineRecv(serialData,buf,o)<>0) then o:=0;
for i:=1 to o do gotOneChar(buf[i]);

goto f1;
END.