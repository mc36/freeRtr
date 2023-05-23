Procedure releq2incoming(p:LongInt);
Var i,o:LongInt;
Begin;
if (kernelConsoleCtrl=0) then begin;
  o:=listingOfs(listingPipe,p);
  if (o=0) then exit;
  WriteLongDEF(i,1);
  if pipelineSend(totalMemoryByte^[o],i,sizeof(i),kernelProcNum) then exit;
  kernelConsoleCtrl:=p;
  kernelConsoleSkip:=2;
  exit;
  end;
if (kernelConsoleData=0) then begin;
  kernelConsoleData:=p;
  exit;
  end;
pipelineClose(p,kernelProcNum);
End;



Procedure releq2console;
Label f1,f2;
Const max=200;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ad:array[1..1] of LongInt absolute a;
  i,o,p:LongInt;
Begin;
p:=0;
while (p<max) do begin;
  if not keypressed then break;
  i:=readkey;
  if (kernelConsoleRaw=0) then begin;
    inc(p);
    WriteWordDEF(ab[p],i);
    inc(p);
    continue;
    end;
  case i of
    $8002:i:=9;
    $8003:i:=8;
    $8004:i:=13;
    $8005:i:=27;
    end;
  if (i and $ff<>i) then break;
  inc(p);
  ab[p]:=i;
  end;
if (kernelConsoleData=0) then exit;
o:=listingOfs(listingPipe,kernelConsoleData);
if (o=0) then goto f1;
if (p>0) then pipelineSend(totalMemoryByte^[o],ab[1],p,kernelProcNum);
p:=max;
if pipelineRecv(totalMemoryByte^[o],ab[1],p,kernelProcNum) then p:=0;
if (p>0) then begin;
  ab[0]:=p;
  Write(a);
  exit;
  end;
o:=listingOfs(listingPipe,kernelConsoleCtrl);
if (o=0) then goto f1;
p:=max;
if pipelineRecv(totalMemoryByte^[o],a,p,kernelProcNum) then exit;
if (p<1) then exit;
if (kernelConsoleSkip>0) then begin;
  dec(kernelConsoleSkip);
  exit;
  end;
case readLongDEF(ad[1]) of
  00:begin; {read line status counters}
    ad[2]:=0; {overrun errors}
    ad[3]:=0; {parity errors}
    ad[4]:=0; {framing errors}
    ad[5]:=0; {break detects}
    ad[6]:=0; {current line status}
    p:=6;
    end;
  01:begin; {read modem status counters}
    ad[2]:=0; {cts changes}
    ad[3]:=0; {dsr changes}
    ad[4]:=0; {ring indicator changes}
    ad[5]:=0; {data carrier detect changes}
    ad[6]:=11; {current modem status}
    p:=6;
    end;
  02:begin; {read modem control status}
    ad[2]:=3; {current modem control}
    p:=2;
    end;
  03:begin; {set modem control value}
    p:=1;
    end;
  04:begin; {read line status}
    ad[2]:=28800; {line speed (bit/sec)}
    ad[3]:=0; {line speed high dword}
    ad[4]:=8; {byte length in bits}
    ad[5]:=0; {parity}
    ad[6]:=1; {stop bits}
    ad[7]:=0; {send break}
    p:=7;
    end;
  05:begin; {write line status}
    p:=1;
    end;
  06:begin; {read flow control}
    ad[2]:=1; {used flow control}
    p:=2;
    end;
  07:begin; {write flow control}
    p:=1;
    end;
  08:begin; {driver buffer status}
    ad[2]:=0; {bytes waiting in rx buffer}
    ad[3]:=0; {bytes waiting in tx buffer}
    p:=3;
    end;
  09:begin; {clear driver rx buffer}
    p:=1;
    end;
  10:begin; {clear driver tx buffer}
    p:=1;
    end;
  11:begin; {clear driver rx and tx buffers}
    p:=1;
    end;
  else goto f1;
  end;
for i:=1 to p do WriteLongDEF(ad[i],ad[i]);
pipelineSend(totalMemoryByte^[o],a,p*sizeof(LongInt),kernelProcNum);
exit;
f1:
pipelineClose(kernelConsoleData,kernelProcNum);
pipelineClose(kernelConsoleCtrl,kernelProcNum);
kernelConsoleData:=0;
kernelConsoleCtrl:=0;
End;
