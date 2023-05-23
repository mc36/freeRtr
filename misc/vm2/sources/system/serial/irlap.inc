Procedure gotOnePacket;
Const
  capabilitiesList:array[0..13] of String[5]=('pnp','pda','pc','print','modem','fax','lan','','tel','file','comm','msg','http','obex');
  additionalBOFs:array[0..7] of byte=(48,24,12,5,3,2,1,0);
  turnAroundBOFs:array[0..7] of byte=(116,24,5,1,1,1,1,0);
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p,q:LongInt;

Function findHighestBit(o:LongInt):LongInt;
Var i:LongInt;
Begin;
findHighestBit:=-1;
for i:=0 to 7 do if ((1 shl i) and o<>0) then findHighestBit:=i;
End;

Procedure procNr(a:String);
Var i,o:LongInt;
Begin;
o:=packetInputBuf[2];
i:=o and $10;
o:=o shr 5;
{$ifdef debug2}WriteLn('p='+BStr(i)+' nr='+BStr(o)+' type='+a+' <--');{$endif}
if (i=0) then remoteHas:=currentTime else remoteHas:=0;
retryCount:=0;
if (queueBufferSiz<>0) then o:=(o-1) and 7;
if (o<>packetTx) then exit;
if (queueBufferSiz<>0) then packetTx:=(packetTx+1) and 7;
queueBufferSiz:=0;
if (fastRounds>16) then exit;
lastSent:=-99999;
inc(fastRounds);
End;

Begin;
if (packetInputSiz<4) then exit;
if (CalcFCS16(packetInputBuf,packetInputSiz)<>$f47) then begin;
  WriteLn('got packet with invalid fcs!');
  exit;
  end;
dec(packetInputSiz,2);
{$ifdef debug1}
Write('rx:');
for i:=1 to packetInputSiz do write(' '+byte2hextype(packetInputBuf[i]));
WriteLn('');
{$endif}
if (packetInputBuf[1]=$fe) and (packetInputBuf[2]=$bf) then begin;
  if (remoteID<>0) then begin;
    WriteLn('more devices...');
    exit;
    end;
  if (packetInputBuf[3]<>1) then exit;
  move(packetInputBuf[8],i,sizeof(i));
  if (i<>localID) then exit;
  move(packetInputBuf[4],i,sizeof(i));
  remoteID:=i;
  o:=15;
  fillchar(a,sizeof(a),0);
  ab0:=0;
  while (packetInputBuf[o] and $80<>0) do begin;
    inc(ab0);
    ab[ab0]:=packetInputBuf[o];
    inc(o);
    end;
  inc(ab0);
  ab[ab0]:=packetInputBuf[o];
  p:=ReadLongLSB(ab[1]);
  a:='';
  for i:=0 to 13 do if ((1 shl i) and p<>0) then a:=a+','+capabilitiesList[i];
  remoteCapa:=copy(a,2,666);
  inc(o,2);
  ab0:=0;
  for i:=o to packetInputSiz do begin;
    p:=packetInputBuf[i];
    if (p=0) then break;
    inc(ab0);
    ab[ab0]:=p;
    end;
  remoteName:=a;
  exit;
  end;
if (packetInputBuf[1]<>remoteAddr) then begin;
  WriteLn('got from unknown address...');
  exit;
  end;
if (packetInputBuf[2] and 1=0) then begin;
  i:=(packetInputBuf[2] shr 1) and 7;
  {$ifdef debug2}Write('ns='+BStr(i)+' ');{$endif}
  procNr('i');
  if (i<>packetRx) then exit;
  packetRx:=(packetRx+1) and 7;
  upperGotOnePacket(packetInputBuf[3],packetInputSiz-2);
  fastRounds:=0;
  lastSent:=-99999;
  exit;
  end;
case packetInputBuf[2] and $f of
  $1:begin; procNr('rr');needStop:=0;exit; end;
  $5:begin; procNr('rnr');needStop:=currentTime;exit; end;
  $9:begin; procNr('rej');exit; end;
  $d:begin; procNr('srej');exit; end;
  end;
case packetInputBuf[2] and $ef of
  $83,$43,$87:begin;    {rnrm, rd, frmr}
    WriteLn('got disconnect request');
    remoteID:=0;
    remoteAddr:=-1;
    exit;
    end;
  $63:begin;    {ua}
    move(packetInputBuf[3],i,sizeof(i));
    if (i<>remoteID) then exit;
    move(packetInputBuf[7],i,sizeof(i));
    if (i<>localID) then exit;
    if (remoteSpeed<>0) then exit;
    p:=10;
    preambSize:=0;
    while (p<packetInputSiz) do begin;
      inc(p);o:=packetInputBuf[p];
      inc(p);i:=packetInputBuf[p];
      q:=packetInputBuf[p+1];
      if (i<1) then q:=0;
      inc(p,i);
      case o of
        $01:begin; {speed}
          i:=findHighestBit(q and $3f);
          if (i>=0) then remoteSpeed:=connectionSpeedValues[i];
          end;
        $83:begin; {data size}
          i:=findHighestBit(q and $3f);
          if (i>=0) then dataSize:=64 shl i;
          end;
        $84:begin; {window size}
          i:=findHighestBit(q and $7f);
          if (i>=0) then windowSize:=i+1;
          end;
        $85:begin; {preamble size}
          i:=findHighestBit(q);
          if (i>=0) then inc(preambSize,additionalBOFs[i]);
          end;
        $86:begin; {turn around time}
          i:=findHighestBit(q);
          if (i>=0) then inc(preambSize,turnAroundBOFs[i]);
          end;
        end;
      end;
    i:=remoteSpeed;
    if (i<1) then i:=1;
    preambSize:=preambSize div (115200 div i);
    exit;
    end;
  end;

WriteLn('got unknown packet type');
End;




Procedure gotOneChar(ch:LongInt);
Begin;
if (packetInputSiz>sizeof(packetInputBuf)) then begin;
  WriteLn('too big packet is arriving!');
  packetInputSiz:=0;
  end;
if packetInputEsc then begin;
  inc(packetInputSiz);
  packetInputBuf[packetInputSiz]:=ch xor $20;
  packetInputEsc:=False;
  exit;
  end;
if (ch=$c0) then begin;
  gotOnePacket;
  packetInputSiz:=0;
  exit;
  end;
if (ch=$c1) then begin;
  gotOnePacket;
  packetInputSiz:=0;
  exit;
  end;
if (ch=$7d) then begin;
  packetInputEsc:=true;
  exit;
  end;
inc(packetInputSiz);
packetInputBuf[packetInputSiz]:=ch;
End;


Procedure doReceivingWork;
Var
  buf:array[1..1024] of byte;
  i,o:LongInt;
Begin;
o:=sizeof(buf);
if (pipeLineRecv(serialData,buf,o)<>0) then exit;
if (o<1) then exit;
for i:=1 to o do gotOneChar(buf[i]);
End;


Procedure flushOutBuf;
Label f1;
Var i:LongInt;
Begin;
if (packetOutputSiz<0) then exit;
f1:
i:=pipeLineSend(serialData,packetOutputBuf,packetOutputSiz);
if (i<>0) then begin; relequish;goto f1; end;
packetOutputSiz:=0;
End;



Procedure putOneChar(ch:LongInt);
Begin;
if (packetOutputSiz>=sizeof(packetOutputBuf)) then flushOutBuf;
inc(packetOutputSiz);
packetOutputBuf[packetOutputSiz]:=ch;
End;




Procedure putOnePacket(var buffer;size,pre:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  i,o:Longint;

procedure put(i:LongInt);
begin;
i:=i and $ff;
if (i in [$7d,$c0,$c1]) then putOneChar($7d);
putOneChar(i);
end;

Begin;
{$ifdef debug1}
Write('tx:');
for i:=1 to size do write(' '+byte2hextype(buf[i]));
WriteLn('');
{$endif}
for i:=1 to pre do putOneChar($c0);
putOneChar($c0);
for i:=1 to size do put(buf[i]);
i:=CalcFCS16(buffer,size);
put(i);
put(i shr 8);
putOneChar($c1);
End;




Procedure putOneXIDpack(size:longint);
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p,q:LongInt;
Begin;
q:=ticksPerSec div 6;
flushOutBuf;
i:=size and 3;
case i of
  0:o:=1;
  1:o:=5;
  2:o:=7;
  3:o:=15;
  end;
a:=#255#63#1#9#9#9#9#255#255#255#255+chr(i)+#0#0;
move(localID,ab[4],sizeof(i));
for i:=0 to o do begin;
  ab[13]:=i;
  putOnePacket(ab[1],ab0,10);
  flushOutBuf;
  p:=currentTime;
  while ((currentTime-p) and $fff<q) do begin; relequish;timer2start; end;
  end;
a:=a+#$84#$25#0+copy(localName,1,20);
ab[13]:=255;
putOnePacket(ab[1],ab0,10);
flushOutBuf;
End;



Procedure putOneSNRMpack;
Var
  buf:array[1..1024] of byte;
  i,o,p:LongInt;

Procedure add(t,v:LongInt);
Begin;
inc(p);buf[p]:=t;
inc(p);buf[p]:=1;
inc(p);buf[p]:=v;
End;

Begin;
buf[1]:=$ff;
buf[2]:=$93;
move(localID,buf[3],sizeof(i));
move(remoteID,buf[7],sizeof(i));
buf[11]:=remoteAddr;
p:=11;
o:=0;
for i:=0 to 5 do if (localSpeed>=connectionSpeedValues[i]) then inc(o,1 shl i);
add($01,o);     {speed}
add($82,1);     {turn around time}
add($83,15);    {max data size}
add($84,15);    {window size}
add($85,0);     {number of bofs}
add($86,4);     {min turn time}
add($08,15);    {disconn time}
putOnePacket(buf,p,10);
flushOutBuf;
End;



Procedure doTransmitWork;
Label f1;
Var
  buf:array[1..maximumBufferSize] of byte;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
Begin;
if (remoteID=0) then exit;
if (needStop<>0) then if (getTimePast(needStop)>10) then begin;
  WriteLn('remote stuck in not ready state!');
  needStop:=0;
  exit;
  end;
if (remoteHas<>0) then begin;
  if (getTimePast(remoteHas)<2) then exit;
  WriteLn('remote timed out!');
  inc(retryCount);
  if (retryCount>15) then begin;
    remoteID:=0;
    remoteAddr:=-1;
    exit;
    end;
  f1:
  buf[1]:=remoteAddr or 1;
  buf[2]:=(packetRx shl 5) or $11;
  {$ifdef debug2}WriteLn('p=1 nr='+BStr(packetRx)+' type=rr -->');{$endif}
  putOnePacket(buf,2,preambSize);
  flushOutBuf;
  remoteHas:=currentTime;
  exit;
  end;
if (getTimePast(lastSent)<1) then exit;
lastSent:=currentTime;
if (queueBufferSiz<1) then begin;
  if (upperQueueSiz<1) then goto f1;
  a:=upperQueueBuf[1];
  move(ab[1],queueBufferDat,ab0);
  queueBufferSiz:=ab0;
  dec(upperQueueSiz);
  for i:=1 to upperQueueSiz do upperQueueBuf[i]:=upperQueueBuf[i+1];
  fastRounds:=0;
  lastSent:=-99999;
  exit;
  end;
buf[1]:=remoteAddr or 1;
buf[2]:=(packetRx shl 5) or (packetTx shl 1) or $10;
move(queueBufferDat,buf[3],queueBufferSiz);
{$ifdef debug2}WriteLn('p=1 ns='+BStr(packetTx)+' nr='+BStr(packetRx)+' type=i -->');{$endif}
putOnePacket(buf,queueBufferSiz+2,preambSize);
flushOutBuf;
remoteHas:=currentTime;
End;
