{{$define debug1}
{{$define debug2}
{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

{$include fcs8.inc}

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;

Const maxFrameSize=256;
Var
  pip,dlc:LongInt;
  speed,datbit,stpbit,prty,ctrl:LongInt;

Function decodeCommand(i:LongInt):String;
Var a:String;
Begin;
case i and $ef of
  $2f:a:='sabm';
  $63:a:='ua';
  $0f:a:='dm';
  $43:a:='disc';
  $ef:a:='uih';
  $03:a:='ui';
  else a:='unknown:'+BStr(i);
  end;
decodeCommand:=a;
End;


Function decodeSubcommand(i:LongInt):String;
Var a:String;
Begin;
case i shr 2 of
  $04:a:='nsc';
  $08:a:='test';
  $10:a:='psc';
  $14:a:='rls';
  $18:a:='fcoff';
  $20:a:='pn';
  $24:a:='rpn';
  $28:a:='fcon';
  $30:a:='cld';
  $34:a:='snc';
  $38:a:='msc';
  else a:='unknown:'+BStr(i);
  end;
End;


Procedure dumpOneFrame(a,b:String);
Var
  bb:array[0..1] of byte absolute b;
  i,o:LongInt;
Begin;
{$ifdef debug1}
write(a+':');
for i:=1 to bb[0] do write(' '+byte2hextype(bb[i]));
writeLn('');
{$endif}
{$ifdef debug2}
writeLn(a+':'+decodeCommand(bb[2]));
{$endif}
End;



Function addRFCheader(a:String;adr,ctr,fcs:LongInt):String;
Var
  ab:array[0..1] of byte absolute a;
  i,o:LongInt;
Begin;
i:=ab[0];
if (i<128) then begin;
  a:='123'+a;
  ab[3]:=(i shl 1) or 1;
  end else begin;
  a:='1234'+a;
  ab[3]:=i shl 1;
  ab[4]:=i shr 7;
  end;
ab[1]:=(adr shl 2) or 1;
if (adr and $100<>0) then inc(ab[1],2);
ab[2]:=ctr;
if (fcs>ab[0]) then fcs:=ab[0];
if (fcs>0) then begin;
  i:=-1;
  updateFCS8(i,ab[1],fcs);
  inc(ab[0]);
  ab[ab[0]]:=not i;
  end;
addRFCheader:=a;
End;


Procedure getRFCheader(var a:String;var adr,ctr:LongInt);
Var
  ab:array[0..1] of byte absolute a;
  i,o:LongInt;
Begin;
adr:=ab[1] shr 2;
if (ab[1] and 2<>0) then inc(adr,$100);
ctr:=ab[2];
i:=ab[3];
o:=i shr 1;
if (i and 1<>0) then begin;
  a:=copy(a,4,666);
  end else begin;
  inc(o,ab[4] shl 7);
  a:=copy(a,5,666);
  end;
if (ctr=$ff) then begin;
  a:=copy(a,2,666);
  ctr:=$ef;
  end;
ab[0]:=o;
End;


Function addMPXheader(a:String;cmd:LongInt):String;
Var
  ab:array[0..1] of byte absolute a;
  i,o:LongInt;
Begin;
i:=ab[0];
if (i<128) then begin;
  a:='12'+a;
  ab[2]:=(i shl 1) or 1;
  end else begin;
  a:='123'+a;
  ab[2]:=i shl 1;
  ab[3]:=i shr 7;
  end;
ab[1]:=(cmd shl 2) or 1;
if (cmd and $100<>0) then inc(ab[1],2);
addMPXheader:=a;
End;


Function getMPXheader(var a:String;var cmd:LongInt):String;
Var
  ab:array[0..1] of byte absolute a;
  i,o:LongInt;
Begin;
cmd:=ab[1] shr 2;
if (ab[1] and 2<>0) then inc(cmd,$100);
i:=ab[2];
o:=i shr 1;
if (i and 1<>0) then begin;
  a:=copy(a,3,666);
  end else begin;
  inc(o,ab[3] shl 7);
  a:=copy(a,4,666);
  end;
ab[0]:=o;
End;




Function answerCommands(a:String):Boolean;
Var
  b:String;
  ab:array[0..1] of byte absolute a;
  bb:array[0..1] of byte absolute b;
  i,o,p:LongInt;
Begin;
answerCommands:=true;
if (a='') then exit;
getRFCheader(a,i,o);
if (o<>$ef) then begin;
  writeLn('got unknown frame: '+BStr(o));
  exit;
  end;
if (i and $ff=dlc) then begin;
  answerCommands:=false;
  exit;
  end;
if (i and $ff<>0) then begin;
  writeLn('got for unknown dlc: '+BStr(i));
  exit;
  end;
getMPXheader(a,i);
b:='';
case i and $ff of
  $20:begin; {param nego}
    a:=copy(a,1,8);
    ab[1]:=dlc;
    if (ab[2] shr 4=$f) then ab[2]:=$e0;
    ab[4]:=0;
    i:=readWordLSB(ab[5]);
    if (i>maxFrameSize) then i:=maxFrameSize;
    writeWordLSB(ab[5],i);
    ab[7]:=0;
    ab[8]:=7;
    a:=addMPXheader(a,$020);
    a:=addRFCheader(a,$100,$ef,2);
    pipeLineSend(pip,ab[1],ab[0]);
    dumpOneFrame('tx',a);
    end;
  $38:begin; {msc}
    a:=copy(a,1,2);
    ab[1]:=(dlc shl 2) or 3;
    a:=addMPXheader(a,$038);
    a:=addRFCheader(a,$100,$ef,2);
    pipeLineSend(pip,ab[1],ab[0]);
    dumpOneFrame('tx',a);
    end;
  else writeLn('got unknown command: '+BStr(i));
  end;

End;



Function getFrame:String;
Label f1;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  i,o,t:LongInt;
Begin;
getFrame:='';
timer2start;
t:=currentTime;
f1:
relequish;
timer2start;
if (getTimePast(t)>5) then exit;
i:=sizeof(a);
pipeLineRecv(pip,a[1],i);
if (i<1) then begin;
  pipeLineStats(pip,o,i,i);
  if (o=0) then immErr('pipeline closed!');
  goto f1;
  end;
ab[0]:=i;
dumpOneFrame('rx',a);
getFrame:=a;
End;


Procedure doSABM(dlc:LongInt);
Label f1,f2;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  i,o,r:LongInt;
Begin;
writeLn('sending sabm to '+BStr(dlc)+'...');
r:=0;
f1:
inc(r);
if (r>5) then immErr('timeout!');
a:=addRFCheader('',dlc or $100,$3f,$100);
pipeLineSend(pip,ab[1],ab[0]);
dumpOneFrame('tx',a);
f2:
a:=getFrame;
if (a='') then goto f1;
getRFCheader(a,i,o);
if (i and $ff<>dlc) then goto f2;
if (o<>$73) then goto f2;
End;


Procedure doPN(dlc:LongInt);
Label f1,f2;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  i,o,r:LongInt;
Begin;
writeLn('sending pn to '+BStr(dlc)+'...');
r:=0;
f1:
inc(r);
if (r>5) then immErr('timeout!');
a:=#$00#$f0#$07#$00#$00#$00#$00#$07;
writeWordLSB(ab[5],maxFrameSize);
ab[1]:=dlc;
a:=addMPXheader(a,$120);
a:=addRFCheader(a,$100,$ef,2);
pipeLineSend(pip,ab[1],ab[0]);
dumpOneFrame('tx',a);
f2:
a:=getFrame;
if (a='') then goto f1;
getRFCheader(a,i,o);
if (i and $ff<>0) then goto f2;
if (o<>$ef) then goto f2;
getMPXheader(a,i);
if (i and $ff<>$20) then goto f2;
End;


Procedure doMSC(dlc:LongInt);
Label f1,f2;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  i,o,r:LongInt;
Begin;
writeLn('sending msc to '+BStr(dlc)+'...');
r:=0;
f1:
inc(r);
if (r>5) then immErr('timeout!');
a:=#$00#$00;
ab[1]:=(dlc shl 2) or 3;
ab[2]:=((ctrl and 3) shl 2) or $81;
a:=addMPXheader(a,$138);
a:=addRFCheader(a,$100,$ef,2);
pipeLineSend(pip,ab[1],ab[0]);
dumpOneFrame('tx',a);
f2:
a:=getFrame;
if (a='') then goto f1;
getRFCheader(a,i,o);
if (i and $ff<>0) then goto f2;
if (o<>$ef) then goto f2;
getMPXheader(a,i);
if (i and $ff<>$38) then goto f2;
ctrl:=(ab[2] shr 2) and 3;
End;



Procedure doRPN(dlc:LongInt);
Label f1,f2;
Const
  speedMax=8;
  speedDat:array[0..speedMax] of LongInt=(2400,4800,7200,9600,19200,38400,57600,115200,230400);
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  i,o,r:LongInt;
Begin;
o:=3;
for i:=1 to speedMax do if (speed=speedDat[i]) then o:=i;
speed:=o;
datbit:=(datbit-5) and 3;
stpbit:=(stpbit-1) and 1;
if (prty<>0) then prty:=(prty-1) and 3;
writeLn('sending rpn to '+BStr(dlc)+'...');
r:=0;
f1:
inc(r);
if (r>5) then immErr('timeout!');
a:=#$00#$07#$03#$00#$11#$13#$7f#$00;
ab[1]:=(dlc shl 2) or 3;
ab[2]:=speed;
ab[3]:=(stpbit shl 2) or (prty shl 3) or datbit;
a:=addMPXheader(a,$124);
a:=addRFCheader(a,$100,$ef,2);
pipeLineSend(pip,ab[1],ab[0]);
dumpOneFrame('tx',a);
f2:
a:=getFrame;
if (a='') then goto f1;
getRFCheader(a,i,o);
if (i and $ff<>0) then goto f2;
if (o<>$ef) then goto f2;
getMPXheader(a,i);
if (i and $ff<>$24) then goto f2;
speed:=speedDat[ab[2]];
datbit:=(ab[3] and 3)+5;
stpbit:=((ab[3] shr 2) and 1)+1;
prty:=(ab[3] shr 3) and 7;
if (prty and 1=0) then prty:=0 else prty:=(prty shr 1)+1;
End;


Procedure doCredit(dlc,crd:LongInt);
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  i,o:LongInt;
Begin;
a:=addRFCheader('',dlc or $100,$ff,2);
a:=copy(a,1,ab[0]-1)+chr(crd)+copy(a,ab[0],666);
pipeLineSend(pip,ab[1],ab[0]);
dumpOneFrame('tx',a);
End;


Procedure doText(dlc:LongInt;a:String);
Var
  ab:array[0..1] of byte absolute a;
  i,o:LongInt;
Begin;
a:=addRFCheader(a,dlc or $100,$ef,2);
pipeLineSend(pip,ab[1],ab[0]);
dumpOneFrame('tx',a);
End;



Var
  a,b:String;
  ab:array[0..1] of byte absolute a;
  i,o,p,q:LongInt;
BEGIN;
WriteLn('bluetooth rfcomm v1.0, done by Mc at '#%date' '#%time'.');
a:=ParamStr(1);
if (a='') then immErr('using: rfcomm.code <process> <addr> <psm> <chan>');
WriteLn('process: '+a);
TCPprocessId:=BVal(a);
if (TCPprocessId=0) then TCPprocessId:=BugOS_findProcNam(a);
if (TCPprocessId=0) then immErr('process not found!');

o:=BVal(paramStr(3));
if (o=0) then o:=3;
a:=paramStr(2);
write('connecting to '+a+' '+BStr(o)+'...');
kicserel(':','',a);
kicserel('-','',a);
kicserel('.','',a);
for i:=1 to 6 do ab[i]:=BVal('$'+copy(a,i*2-1,2));
if RTPopenConnection(pip,4096,a[1],o) then immErr('failed!');
writeLn('done!');

dlc:=BVal(paramStr(4));
if (dlc<1) then dlc:=1;
dlc:=dlc*2;
speed:=115200;
datbit:=8;
stpbit:=1;
prty:=0;
ctrl:=3;

doSABM(0);
doPN(dlc);
doSABM(dlc);
doRPN(dlc);
doMSC(dlc);
doCredit(dlc,32);

doText(dlc,'aaa');

writeLn('serving others...');
pipeLineBegListen;
BugOS_SignDaemoning;

while (1=1) do begin;
  a:=getFrame;
  if answerCommands(a) then a:='';
  end;
END.