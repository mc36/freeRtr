{{$define debug}
{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}


{$include \sources\internet\kernel\utils\timer2.inc}

Var
  actAsDTE:Boolean;             {acting as dte}
  locAddr:Byte;                 {local address}
  remAddr:Byte;                 {remote address}
  locSeq:LongInt;               {local sequence number}
  remSeq:LongInt;               {remote sequence number}
  mod128:Boolean;               {modulo 128 operation}
  modMsk:LongInt;               {modulo mask value}
  LastSent:LongInt;             {time when sent packet}
  retryCnt:LongInt;             {retry counter}
  devPipe:LongInt;              {device pipeline}
  uppPipe:LongInt;              {upper pipeline}
  myName:String;                {name of my link}
  packDat:array[1..2*1024] of byte; {packet data}
  packSiz:LongInt;              {size of packet}
  packTim:LongInt;              {time packet last sent}



Procedure immErr(a:String);
Begin;
Writeln(a);
Halt(1);
End;

{$ifdef debug}
Procedure dumpPack(var buffer;size:LongInt;dir:String);
Var
  buf:array[1..1] of byte absolute buffer;
  i:LongInt;
Begin;
Write(dir);
for i:=1 to size do Write(' '+byte2hextype(buf[i]));
WriteLn('');
End;
{$endif}

Procedure WaitForUpperLayer;
Var
  i,o,p:LongInt;
  a:String;
Begin;
Write('waiting for upper level...');
BugOS_SignDaemoning;
pipeLineBegListen;
while (pipeLineGetIncoming(uppPipe)<>0) do relequish;
pipeLineEndListen;
BugOS_MyProcessInfo(i,o,p);
i:=i xor p xor o;
p:=((i shr 16) xor i) and $ffff;
a:='12341234'#0#0#0#0#0#0#0#0+chr(p)+#255+myName+#0;
i:=1;move(i,a[1],sizeof(i));
i:=1500;move(i,a[5],sizeof(i));
pipeLineSend(uppPipe,a[1],length(a));
WriteLn(' done!');
End;


Procedure sendOneSABM(pf:Boolean);
Var buf:array[1..16] of byte;
Begin;
WriteLn('sending SABM(E)...');
buf[1]:=locAddr;
if mod128 then buf[2]:=$6f else buf[2]:=$2f;
if pf then buf[2]:=buf[2] or $10;
{$ifdef debug}dumpPack(buf,2,'sending:');{$endif}
pipelineSend(devPipe,buf,2);
End;

Procedure sendOneUA(pf:Boolean);
Var buf:array[1..16] of byte;
Begin;
WriteLn('sending UA...');
buf[1]:=remAddr;
buf[2]:=$63;
if pf then buf[2]:=buf[2] or $10;
{$ifdef debug}dumpPack(buf,2,'sending:');{$endif}
pipelineSend(devPipe,buf,2);
End;

Procedure sendOneRR(rep,pf:Boolean);
Var
  buf:array[1..16] of byte;
  i:LongInt;
Begin;
{$ifdef debug}WriteLn('sending RR (NR='+BStr(remSeq)+')...');{$endif}
if rep then buf[1]:=remAddr else buf[1]:=locAddr;
if mod128 then begin;
  buf[2]:=1;
  buf[3]:=(remSeq shl 1);
  if pf then buf[3]:=buf[3] or $01;
  i:=3;
  end else begin;
  buf[2]:=(remSeq shl 5) or 1;
  if pf then buf[2]:=buf[2] or $10;
  i:=2;
  end;
{$ifdef debug}dumpPack(buf,i,'sending:');{$endif}
pipelineSend(devPipe,buf,i);
End;

Procedure sendOneI(rep,pf:Boolean);
Begin;
{$ifdef debug}WriteLn('sending I (NR='+BStr(remSeq)+' NS='+BStr(locSeq)+')...');{$endif}
if rep then packDat[1]:=remAddr else packDat[1]:=locAddr;
if mod128 then begin;
  packDat[2]:=(locSeq shl 1);
  packDat[3]:=(remSeq shl 1);
  if pf then packDat[3]:=packDat[3] or $01;
  end else begin;
  packDat[2]:=(remSeq shl 5) or (locSeq shl 1);
  if pf then packDat[2]:=packDat[2] or $10;
  end;
{$ifdef debug}dumpPack(packDat,packSiz,'sending:');{$endif}
pipeLineSend(devPipe,packDat,packSiz);
End;

Procedure doConfirm(i:LongInt);
Begin;
if ((i-1) and modMsk<>locSeq) then exit;
packSiz:=0;
locSeq:=(locSeq+1) and modMsk;
End;




Procedure doLower;
Label f1,f2;
Var
  buf:array[1..2*1024] of byte;
  i,o,p,q:LongInt;
Begin;
if (packSiz>0) then if (getTimePast(packTim)>10) then begin;
  sendOneI(false,true);
  packTim:=currentTime;
  lastSent:=currentTime;
  end;
if (getTimePast(lastSent)>5) then begin;
  dec(retryCnt);
  if (retryCnt<0) then immErr('remote possibly dead!');
  if (retryCnt>=7) then sendOneRR(false,true) else begin;
    sendOneSABM(true);
    locSeq:=0;
    remSeq:=0;
    end;
  lastSent:=currentTime;
  end;
o:=sizeof(buf);
if (pipelineRecv(devPipe,buf,o)<>0) then o:=0;
if (o<1) then begin;
  pipeLineStats(devPipe,o,i,i);
  if (o=0) then immErr('lower level closed connection!');
  exit;
  end;
{$ifdef debug}dumpPack(buf,o,'received:');{$endif}
i:=buf[2];
if (i and $01=0) then begin;
  if mod128 then begin;
    i:=buf[3] shr 1;
    q:=buf[2] shr 1;
    p:=3;
    end else begin;
    i:=buf[2] shr 5;
    q:=buf[2] shr 1;
    p:=2;
    end;
  {$ifdef debug}WriteLn('got I (NR='+BStr(i)+' NS='+BStr(q)+')...');{$endif}
  if (q and modMsk=remSeq and modMsk) then begin;
    pipeLineSend(uppPipe,buf[p],o-p+1);
    remSeq:=(remSeq+1) and modMsk;
    end;
  lastSent:=currentTime;
  retryCnt:=16;
  if (buf[1]<>remAddr) then begin;
    sendOneRR(false,false);
    exit;
    end;
  if mod128 then i:=buf[3] and $01 else i:=buf[2] and $10;
  if (i<>0) then sendOneRR(true,true) else sendOneRR(false,false);
  exit;
  end;
if (i and $0f=1) then begin;
  if mod128 then i:=buf[3] shr 1 else i:=buf[2] shr 5;
  {$ifdef debug}WriteLn('got RR (NR='+BStr(i)+')...');{$endif}
  doConfirm(i);
  lastSent:=currentTime;
  retryCnt:=16;
  if (buf[1]<>remAddr) then exit;
  if mod128 then i:=buf[3] and $01 else i:=buf[2] and $10;
  if (i<>0) then sendOneRR(true,true);
  exit;
  end;
if (i and $0f=5) then begin;
  if mod128 then i:=buf[3] shr 1 else i:=buf[2] shr 5;
  {$ifdef debug}WriteLn('got RNR (NR='+BStr(i)+')...');{$endif}
  doConfirm(i);
  lastSent:=currentTime;
  retryCnt:=16;
  if (buf[1]<>remAddr) then exit;
  if mod128 then i:=buf[3] and $01 else i:=buf[2] and $10;
  if (i<>0) then sendOneRR(true,true);
  exit;
  end;
if (i and $0f=9) then begin;
  if mod128 then i:=buf[3] shr 1 else i:=buf[2] shr 5;
  {$ifdef debug}WriteLn('got REJ (NR='+BStr(i)+')...');{$endif}
  doConfirm(i);
  packTim:=-9999;
  lastSent:=-9999;
  retryCnt:=16;
  if (buf[1]<>remAddr) then exit;
  if mod128 then i:=buf[3] and $01 else i:=buf[2] and $10;
  if (i<>0) then sendOneRR(true,true);
  exit;
  end;

if (buf[1]=remAddr) then begin;
  if (i in [$6f,$7f]) then begin;
    if not mod128 then begin;
      WriteLn('got SABME in modulo 8 mode...');
      exit;
      end;
    WriteLn('got SABME...');
    lastSent:=currentTime;
    retryCnt:=16;
    locSeq:=0;
    remSeq:=0;
    if (i and $10<>0) then sendOneUA(true);
    exit;
    end;
  if (i in [$2f,$3f]) then begin;
    if mod128 then begin;
      WriteLn('got SABM in modulo 128 mode...');
      exit;
      end;
    WriteLn('got SABM...');
    lastSent:=currentTime;
    retryCnt:=16;
    locSeq:=0;
    remSeq:=0;
    if (i and $10<>0) then sendOneUA(true);
    exit;
    end;
  if (i in [$0f,$1f]) then begin;
    WriteLn('got SARM...');
    lastSent:=-9999;
    retryCnt:=7;
    if (i and $10<>0) then sendOneUA(true);
    exit;
    end;
  if (i in [$83,$93]) then begin;
    WriteLn('got SNRM...');
    lastSent:=-9999;
    retryCnt:=7;
    exit;
    end;
  if (i in [$87,$97]) then begin;
    WriteLn('got FRMR...');
    lastSent:=currentTime;
    retryCnt:=16;
    exit;
    end;
  if (i in [$43,$53]) then begin;
    WriteLn('got DISC...');
    lastSent:=-9999;
    retryCnt:=7;
    if (i and $10<>0) then sendOneUA(true);
    exit;
    end;
  if (i in [$07,$17]) then begin;
    WriteLn('got RIM...');
    lastSent:=currentTime;
    retryCnt:=16;
    locSeq:=0;
    remSeq:=0;
    if (i and $10<>0) then sendOneUA(true);
    exit;
    end;

  WriteLn('got unknown control field: '+byte2hextype(i));
  exit;
  end;
if (buf[1]<>locAddr) then begin;
  WriteLn('got invalid address field: '+byte2hextype(buf[1]));
  exit;
  end;

if (i in [$63,$73]) then begin;
  WriteLn('got UA...');
  lastSent:=currentTime;
  retryCnt:=16;
  exit;
  end;
if (i in [$43,$53]) then begin;
  WriteLn('got DISC...');
  lastSent:=-9999;
  retryCnt:=7;
  if (i and $10<>0) then sendOneUA(true);
  exit;
  end;
if (i in [$43,$53]) then begin;
  WriteLn('got DISC...');
  lastSent:=-9999;
  retryCnt:=7;
  if (i and $10<>0) then sendOneUA(true);
  exit;
  end;
if (i in [$0f,$1f]) then begin;
  WriteLn('got DM...');
  lastSent:=-9999;
  retryCnt:=7;
  if (i and $10<>0) then sendOneUA(true);
  exit;
  end;
if (i in [$87,$97]) then begin;
  WriteLn('got FRMR...');
  lastSent:=currentTime;
  retryCnt:=16;
  exit;
  end;
if (i in [$07,$17]) then begin;
  WriteLn('got SIM...');
  lastSent:=currentTime;
  retryCnt:=16;
  locSeq:=0;
  remSeq:=0;
  if (i and $10<>0) then sendOneUA(true);
  exit;
  end;

WriteLn('got unknown control field: '+byte2hextype(i));
End;


Procedure doUpper;
Var i,o:LongInt;
Begin;
if (packSiz>0) then exit;
if mod128 then i:=3 else i:=2;
packSiz:=sizeof(packDat);
if (pipeLineRecv(uppPipe,packDat[i],packSiz)<>0) then packSiz:=0;
if (packSiz>0) then begin;
  inc(packSiz,i);
  dec(packSiz);
  packTim:=-9999;
  exit;
  end;
pipeLineStats(uppPipe,o,i,i);
if (o=0) then immErr('upper level closed connection!');
End;



Label f1;
Var
  i,o:LongInt;
  a:String;
BEGIN;
WriteLn('lapb v1.0, done by Mc at '#%date' '#%time'.');

a:=paramStr(1);
devPipe:=BVal(a);
if (devPipe=0) then devPipe:=BugOS_findProcNam(a);
if (devPipe<1) then begin;
  WriteLn('using: lapb.code <process> [options]');
  WriteLn('options:');
  WriteLn('  dte');
  WriteLn('  dce');
  WriteLn('  mod8');
  WriteLn('  mod128');
  Halt(1);
  end;
actAsDTE:=True;
mod128:=False;
for i:=2 to paramCount do begin;
  a:=kicsi(paramStr(i));
  if (a='dte') then begin; actAsDTE:=True;continue; end;
  if (a='dce') then begin; actAsDTE:=False;continue; end;
  if (a='mod8') then begin; mod128:=False;continue; end;
  if (a='mod128') then begin; mod128:=True;continue; end;
  end;
if actAsDTE then locAddr:=$01 else locAddr:=$03;
remAddr:=4-locAddr;
if mod128 then modMsk:=$7f else modMsk:=$07;
locSeq:=0;
remSeq:=0;
packSiz:=0;
uppPipe:=0;

WriteLn('opening link...');
if (pipeLineCreate(devPipe,devPipe,65536,true)<>0) then immErr('error creating connection!');
for i:=1 to 16 do relequish;
i:=255;
if (pipeLineRecv(devPipe,a[1],i)<>0) then immErr('failed to receive initial data!');
a[0]:=chr(i);
a:=copy(a,19,255);
myName:='lapb on '+copy(a,1,pos(#0,a)-1);
WriteLn('my name: '+myName);

lastSent:=-9999;
retryCnt:=7;
WaitForUpperLayer;

f1:
relequish;
timer2start;
doLower;
doUpper;
goto f1;
END.