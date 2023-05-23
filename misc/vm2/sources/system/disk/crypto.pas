{$heap 15k}
{$stack 7k}
{$sysinc system.inc}
{$sysinc crt.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}

Type
  oneRC4stateRecord=record
    d:array[0..255] of byte;
    i:byte;
    j:byte;
    end;
Var
  DriveSize:LongInt;
  DriveBegin:LongInt;
  DriveReadOnly:Boolean;
  geomCyl:LongInt;
  geomHed:LongInt;
  geomSec:LongInt;
  CurrPipe:LongInt;
  CurrProc:LongInt;
  packetBuf:record
    cmd:LongInt;
    dat:array[1..1024*4] of byte;
    end;
  packetSiz:LongInt;
  cryptoState:oneRC4stateRecord;

{$include \sources\filesystem\disk2.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function askUserAuth(qst:String;msk,add:LongInt):String;
Label f1,f2;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
ab0:=0;
Write(qst);
f1:
i:=ReadKey;
if (i and $fe00=0) then begin;{simple key}
  i:=i and $ff;
  if (i in [0,255,13,10,8,9]) then i:=32;
  if (ab0>250) then goto f1;
  inc(ab0);
  ab[ab0]:=i;
  i:=(i and msk)+add;
  write(chr(i));
  goto f1;
  end;
case i of
  $8003:begin;{backspace}
    if (ab0=0) then goto f1;
    Write(#8' '#8);
    dec(ab0);
    goto f1;
    end;
  $8004:goto f2;{enter}
  $8005:begin;{escape}
    WriteLn('');
    ab0:=0;
    Write(qst);
    goto f1;
    end;
  end;
goto f1;
f2:
WriteLn('');
askUserAuth:=a;
End;



Function swapByte(var a,b:Byte):Byte;
Var c:byte;
begin;
c:=a;
a:=b;
b:=c;
swapByte:=a+b;
End;

Procedure rc4_clearState(var d:oneRC4stateRecord);
Var i:LongInt;
Begin;
for i:=0 to 255 do d.d[i]:=i;
d.i:=0;
d.j:=0;
End;

Procedure rc4_setKey(var d:oneRC4stateRecord;var key;siz:LongInt);
Var
  buf:array[0..1] of byte absolute key;
  i:LongInt;
  j:Byte;
Begin;
j:=0;
for i:=0 to 255 do begin;
  inc(j,buf[i mod siz]);
  inc(j,d.d[i]);
  swapByte(d.d[i],d.d[j]);
  end;
End;

Function rc4_getByte(var d:oneRC4stateRecord):Byte;
Begin;
inc(d.i);
inc(d.j,d.d[d.i]);
rc4_getByte:=swapByte(d.d[d.i],d.d[d.j]);
End;


Function doPipe:Boolean;
Var
  i,o,p:LongInt;
  cs:oneRC4stateRecord;
  a:String;
Begin;
doPipe:=True;
packetSiz:=sizeof(packetBuf);
pipeLineRecv(CurrPipe,packetBuf,packetSiz);
if (packetSiz=0) then begin;
  if (pipeLineStats(CurrPipe,i,o,p)<>0) then exit;
  if (i=0) then exit;
  doPipe:=False;
  exit;
  end;
if (packetSiz<4) then exit;
dec(packetSiz,4);
case packetBuf.cmd of
  0:begin;{identify drive}
    i:=0;
    move(i,packetBuf.dat[1],sizeof(i));
    move(geomCyl,packetBuf.dat[5],sizeof(geomCyl));
    move(geomHed,packetBuf.dat[9],sizeof(geomHed));
    move(geomSec,packetBuf.dat[13],sizeof(geomSec));
    i:=0;
    move(driveBegin,packetBuf.dat[17],sizeof(i));
    move(driveSize,packetBuf.dat[21],sizeof(driveSize));
    a:='crypto';
    move(a,packetBuf.dat[25],sizeof(a));
    move(a,packetBuf.dat[281],sizeof(a));
    move(a,packetBuf.dat[537],sizeof(a));
    packetBuf.cmd:=0;
    if (pipeLineSend(CurrPipe,packetBuf,$31c)<>0) then exit;
    doPipe:=False;
    end;
  1:begin;{read one sector}
    move(packetBuf.dat,i,sizeof(i));
    cs:=cryptoState;
    rc4_setKey(cs,i,sizeof(i));
    if (i>=0) and (i<driveSize) then begin;
      o:=DriveRead(i,packetBuf.dat[5]);
      if (o<>0) then WriteLn('error reading drive at '+BStr(i));
      end else o:=1;
    packetBuf.cmd:=o;
    for o:=5 to 516 do dec(packetBuf.dat[o],rc4_getByte(cs));
    if (pipeLineSend(CurrPipe,packetBuf,520)<>0) then exit;
    doPipe:=False;
    end;
  2:begin;{write one sector}
    move(packetBuf.dat,i,sizeof(i));
    cs:=cryptoState;
    rc4_setKey(cs,i,sizeof(i));
    for o:=5 to 516 do inc(packetBuf.dat[o],rc4_getByte(cs));
    if (i>=0) and (i<driveSize) then begin;
      o:=DriveWrite(i,packetBuf.dat[5]);
      if (o<>0) then WriteLn('error writing at '+BStr(i));
      end else o:=1;
    packetBuf.cmd:=o;
    if (pipeLineSend(CurrPipe,packetBuf,8)<>0) then exit;
    doPipe:=False;
    end;
  else writeln('got unknown command: '+BStr(packetBuf.cmd));
  end;
End;



Label f1,f2,f3;
Var
  a:String;
  i,o,p,q:LongInt;
BEGIN;
WriteLn('crypto driver v1.0, done by Mc at '#%date' '#%time'.');

DriveReadOnly:=false;
a:=paramStr(1);
i:=BVal(paramStr(2));
if (a='') then immErr('using; crypto.code <process> <device>');
WriteLn('opening '+a+' '+BStr(i)+'...');
if (DriveOpen(a,i)<>0) then immErr('error!');
WriteLn('disk has '+BStr(DriveSize shr 1)+' kbytes capacity...');

a:=askUserAuth('password:',0,$2a);
rc4_clearState(cryptoState);
rc4_setKey(cryptoState,a[1],length(a));

if (pipeLineBegListen<>0) then immErr('error start listening!');
BugOS_SignDaemoning;

f1:
if (pipeLineGetIncoming(CurrPipe)<>0) then begin;
  Relequish;
  goto f1;
  end;
WriteLn('incoming connection...');
if (pipeLineStats(CurrPipe,CurrProc,i,o)<>0) then goto f2;
WriteLn('pipe='+BStr(CurrPipe)+'  pid='+BStr(CurrProc)+'.');
i:=1;if (pipeLineSend(CurrPipe,i,sizeof(i))<>0) then goto f2;
BugOS_ProcessName(CurrProc,packetBuf,i,o,p);
if (p and $40=0) then goto f2;
WriteLn('asking for drive...');
p:=16;
repeat
  dec(p);
  if (p<0) then goto f2;
  packetSiz:=sizeof(packetBuf);
  pipeLineRecv(CurrPipe,packetBuf,packetSiz);
  if (packetSiz=0) then begin; relequish;continue; end;
  if (packetSiz<>4) then goto f2;
  if (packetBuf.cmd<>0) then goto f2;
  until (packetSiz=4);
WriteLn('serving drive...');

f3:
if doPipe then goto f2;
Relequish;
goto f3;

f2:
pipeLineClose(CurrPipe);
goto f1;
END.