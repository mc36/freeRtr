{{$define debug}
{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$sysinc pipeline.inc}

Var
  lowerProc:LongInt;
  lowerPipe:LongInt;
  lowerLsap:LongInt;
  upperData:LongInt;
  upperCtrl:LongInt;
  currSpeed:LongInt;    {speed}
  currDataf:LongInt;    {data format}
  currFlowc:LongInt;    {flow control}
  currLines:LongInt;    {line status}
  currBreak:LongInt;    {break status}
  currDTEst:LongInt;    {dte status}
  currDCEst:LongInt;    {dce status}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;


Function getOneParameter(var buffer;var ps:LongInt):String;
Var
  buf:array[1..1] of byte absolute buffer;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
inc(ps);
o:=buf[ps];
inc(ps);
move(buf[ps],a,sizeof(a));
inc(ps,ab0);
getOneParameter:=chr(o)+a;
End;


Function getOneAttrib(var buffer;var ps:LongInt):String;
Var
  buf:array[1..1] of byte absolute buffer;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
getOneAttrib:='';
inc(ps);
case buf[ps] of
  0:;           {missing}
  1:begin;      {integer}
    ab0:=sizeof(i);
    move(buf[ps+1],ab[1],ab0);
    inc(ps,ab0);
    end;
  2:begin;      {octets}
    o:=ReadWordMSB(buf[ps+1]);
    inc(ps,2);
    if (o<255) then ab0:=o else o:=255;
    move(buf[ps+1],ab[1],ab0);
    inc(ps,o);
    end;
  3:begin;      {string}
    inc(ps,2);
    move(buf[ps],a,sizeof(a));
    inc(ps,ab0);
    end;
  else exit;
  end;
getOneAttrib:=a;
End;


Procedure openConnect(mod,lsap:LongInt);
Label f1,f2;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p,r:LongInt;
Begin;
r:=5;
f1:
dec(r);
if (r<0) then immErr('no more retries!');
Write('connecting to '+BStr(lsap)+'...');
a:=#1#1;
ab[2]:=lsap;
if (mod<>0) then inc(ab[1],2);
pipeLineClose(lowerPipe);
pipeLineCreate(lowerPipe,BugOS_findProcNam('irda.code'),4096,true);
pipeLineSend(lowerPipe,a[1],ab0);
f2:
pipeLineStats(lowerPipe,o,i,i);
if (o=0) then begin;
  WriteLn(' failed!');
  goto f1;
  end;
o:=1;
pipeLineRecv(lowerPipe,a,o);
if (o<1) then begin;
  relequish;
  goto f2;
  end;
WriteLn(' ok!');
End;



Procedure getServiceLSAP;
Label f1;
Var
  a,b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
Begin;
openConnect(0,0);
a:=#132#11'IrDA:IrCOMM'#19'IrDA:TinyTP:LsapSel';
pipeLineSend(lowerPipe,ab[1],ab0);
f1:
o:=sizeof(a)-1;
pipeLineRecv(lowerPipe,ab[1],o);
if (o<1) then begin;
  pipeLineStats(lowerPipe,o,i,i);
  if (o=0) then immErr('remote closed connection!');
  relequish;
  goto f1;
  end;
ab0:=o;
if (ab[2]<>0) then exit;
b:=a;
p:=4;
{$ifdef debug}Write('lsap=');{$endif}
for o:=1 to ReadWordMSB(b[3]) do begin;
  inc(p,2);
  a:=getOneAttrib(b[1],p);
  if (ab0<>sizeof(i)) then continue;
  i:=ReadLongMSB(ab[1]);
  if (i<1) then continue;
  {$ifdef debug}Write(' '+BStr(i));{$endif}
  lowerLsap:=i;
  end;
{$ifdef debug}WriteLn('');{$endif}
pipeLineClose(lowerPipe);
End;


Procedure releq2upper;
Label f1;
Var
  bufB:array[1..1024] of Byte;
  bufD:array[1..1] of LongInt absolute bufB;
  bufD1:LongInt absolute bufB;
  i,o,p:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
if (upperData=0) then begin;
  if (pipeLineGetIncoming(upperCtrl)<>0) then exit;
  i:=1;
  pipeLineSend(upperCtrl,i,sizeof(i));
  for i:=1 to 16 do relequish;
  if (pipeLineGetIncoming(upperData)<>0) then begin;
    f1:
    pipeLineClose(upperCtrl);
    pipeLineClose(upperData);
    upperCtrl:=0;
    upperData:=0;
    exit;
    end;
  for i:=1 to 16 do relequish;
  i:=sizeof(a);
  pipeLineRecv(upperCtrl,a,i);
  for i:=1 to 16 do relequish;
  i:=sizeof(a);
  pipeLineRecv(upperCtrl,a,i);
  WriteLn('upper logged in!');
  exit;
  end;
p:=sizeof(bufB);
pipeLineRecv(upperCtrl,bufD,p);
if (p<1) then begin;
  pipeLineStats(upperCtrl,o,i,i);
  if (o=0) then goto f1;
  end else begin;
  if (p<sizeof(bufD1)) then exit;
  case bufD1 of
    00:begin; {read line status counters}
      bufD[2]:=0; {overrun errors}
      bufD[3]:=0; {parity errors}
      bufD[4]:=0; {framing errors}
      bufD[5]:=0; {break detects}
      bufD[6]:=((currLines shr 1) and 7) or ((currBreak and 1) shl 3); {current line status}
      i:=6*sizeof(bufD1);
      end;
    01:begin; {read modem status counters}
      bufD[2]:=0; {cts changes}
      bufD[3]:=0; {dsr changes}
      bufD[4]:=0; {ring indicator changes}
      bufD[5]:=0; {data carrier detect changes}
      bufD[6]:=(currDCEst shr 4) or ((currDTEst shr 1) and 1) or ((currDTEst and 1) shl 1); {current modem status}
      i:=6*sizeof(bufD1);
      end;
    02:begin; {read modem control status}
      bufD[2]:=3; {current modem control}
      i:=2*sizeof(bufD1);
      end;
    03:begin; {set modem control value}
      i:=1*sizeof(bufD1);
      end;
    04:begin; {read line status}
      bufD[2]:=currSpeed; {line speed (bit/sec)}
      bufD[3]:=0; {line speed high dword}
      bufD[4]:=(currDataf and 3)+5; {byte length in bits}
      if (currDataf and 8=0) then i:=0 else i:=((currDataf shr 4)+1);
      bufD[5]:=i; {parity}
      bufD[6]:=((currDataf shr 2) and 1)+1; {stop bits}
      bufD[7]:=0; {send break}
      i:=7*sizeof(bufD1);
      end;
    05:begin; {write line status}
      i:=1*sizeof(bufD1);
      end;
    06:begin; {read flow control}
      i:=0;
      if (currFlowc and $0c<>0) then inc(i,1);
      if (currFlowc and $30<>0) then inc(i,2);
      bufD[2]:=i; {used flow control}
      i:=2*sizeof(bufD1);
      end;
    07:begin; {write flow control}
      i:=1*sizeof(bufD1);
      end;
    08:begin; {driver buffer status}
      bufD[2]:=0; {bytes waiting in rx buffer}
      bufD[3]:=0; {bytes waiting in tx buffer}
      i:=3*sizeof(bufD1);
      end;
    09:begin; {clear driver rx buffer}
      i:=1*sizeof(bufD1);
      end;
    10:begin; {clear driver tx buffer}
      i:=1*sizeof(bufD1);
      end;
    11:begin; {clear driver rx and tx buffers}
      i:=1*sizeof(bufD1);
      end;
    else exit;
    end;
  pipeLineSend(upperCtrl,bufD,i);
  end;
o:=32;
pipeLineRecv(upperData,ab[1],o);
if (o<1) then begin;
  pipeLineStats(upperData,o,i,i);
  if (o=0) then goto f1;
  exit;
  end;
ab0:=o;
a:=#0+a;
pipeLineSend(lowerPipe,ab[1],ab0);
End;



Procedure releq2lower;
Label f1;
Var
  buf:array[1..1024] of byte;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
Begin;
o:=sizeof(buf);
pipeLineRecv(lowerPipe,buf,o);
if (o<1) then begin;
  pipeLineStats(lowerPipe,o,i,i);
  if (o=0) then immErr('irda closed connection!');
  exit;
  end;
if (buf[1]=0) then begin;
  pipeLineSend(upperData,buf[2],o-1);
  exit;
  end;
{$ifdef debug}
Write('cmd=');
for i:=1 to o do write(BStr(buf[i])+' ');
writeln('');
{$endif}
p:=1;
f1:
if (p>=o) then exit;
a:=getOneParameter(buf,p);
{$ifdef debug}
Write('  op='+BStr(ab[1])+' dat=');
for i:=2 to ab0 do write(BStr(ab[i])+' ');
writeln('');
{$endif}
case ab[1] of
  $10:currSpeed:=ReadLongMSB(ab[2]);
  $11:currDataf:=ab[2];
  $12:currFlowc:=ab[2];
  $15:currLines:=ab[2];
  $16:currBreak:=ab[2];
  $20:currDTEst:=ab[2];
  $21:currDCEst:=ab[2];
  else writeLn('got unknown opcode!');
  end;
goto f1;
End;



Label f1;
BEGIN;
WriteLn('ircomm driver v1.0, done by Mc at '#%date' '#%time'.');
lowerProc:=BugOS_findProcNam('irda.code');
if (lowerProc=0) then immErr('failed to find irda process!');
lowerLsap:=BVal(paramStr(1));
if (lowerLsap<1) then getServiceLSAP;
if (lowerLsap<1) then immErr('failed to get lsap!');
openConnect(1,lowerLsap);

pipeLineBegListen;
BugOS_SignDaemoning;
upperData:=0;
upperCtrl:=0;
currSpeed:=28800;
currDataf:=3;
currFlowc:=0;
currLines:=0;
currBreak:=0;
currDTEst:=0;
currDCEst:=0;

f1:
relequish;
releq2upper;
releq2lower;
goto f1;
END.