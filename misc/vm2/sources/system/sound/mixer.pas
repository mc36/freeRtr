{$heap 127k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

Const proggyName='wave mixer v1.0, done by Mc at '#%date' '#%time'.';
Const clientMax=64;
Var
  pipe:LongInt;
  size:LongInt;
  deviceDat:array[1..1024] of byte;
  deviceSiz:LongInt;
  clientNum:LongInt;
  clientDat:array[1..clientMax] of LongInt;
  sumBuf:array[1..16384] of LongInt;
  sumNum:LongInt;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function exchange(Var buf;siz:LongInt):LongInt;
Label f1;
Var i,o:LongInt;
Begin;
pipeLineSend(pipe,buf,siz);
f1:
relequish;
siz:=1024*32;
if (pipeLineRecv(pipe,buf,siz)<>0) then siz:=0;
if (siz<1) then begin;
  pipeLineStats(pipe,o,i,i);
  if (o=0) then immErr('sound device closed connection!');
  goto f1;
  end;
exchange:=siz;
End;



Label f1,f2;
Var
  buf:array[1..1024*16] of byte;
  bufI:array[1..1] of Integer absolute buf;
  i,o,p:LongInt;
  a:String;
BEGIN;
WriteLn(proggyName);
if (paramCount<1) then immErr('using: mix.code <process>');
a:=paramStr(1);
i:=BVal(a);
if (i=0) then i:=BugOS_findProcNam(a);
if (i=0) then immErr('process not found!');

if (pipeLineCreate(pipe,i,65536,true)<>0) then immErr('error opening pipeline!');

buf[1]:=1;
p:=exchange(buf,1);
WriteLn('device information:');
a:=proggyName+#13#10'device: ';
move(a[1],deviceDat,sizeof(a));
deviceSiz:=length(a);
move(buf,deviceDat[deviceSiz+1],p);
inc(deviceSiz,p);
for i:=1 to p do write(chr(buf[i]));

buf[1]:=5;
exchange(buf,1);
move(buf,size,sizeof(size));
WriteLn('block size: '+BStr(size)+' bytes.');
size:=size shr 1;

sumNum:=0;
fillchar(sumBuf,sizeof(sumBuf),0);
clientNum:=0;
pipeLineBegListen;
BugOS_SignDaemoning;

f1:
relequish;
while (pipeLineGetIncoming(p)=0) do begin;
  if (clientNum>=clientMax) then begin;
    WriteLn('failed to allocate memory!');
    pipeLineClose(p);
    goto f1;
    end;
  inc(clientNum);
  clientDat[clientNum]:=p;
  end;
for p:=clientNum downto 1 do begin;
  i:=sizeof(buf);
  if (pipeLineRecv(clientDat[p],buf,i)<>0) then i:=0;
  if (i<1) then begin;
    pipeLineStats(clientDat[p],o,i,i);
    if (o<>0) then continue;
    f2:
    pipeLineClose(clientDat[p]);
    clientDat[p]:=clientDat[clientNum];
    dec(clientNum);
    continue;
    end;
  case buf[1] of
    6:begin; {play sample}
      move(buf[2],buf,size shl 1);
      for i:=1 to size do inc(sumBuf[i],bufI[i]);
      inc(sumNum);
      i:=$01010101;
      pipeLineSend(clientDat[p],i,1);
      end;
    8:begin; {play and record sample}
      move(buf[2],buf,size shl 1);
      for i:=1 to size do inc(sumBuf[i],bufI[i]);
      inc(sumNum);
      i:=0;
      pipeLineSend(clientDat[p],i,4);
      end;
    1:begin; {get card identification}
      pipeLineSend(clientDat[p],deviceDat,deviceSiz);
      end;
    2:begin; {get mixer names}
      a:=#0#0#0#0#0#0#0#0'no available volumebars'#0;
      pipeLineSend(clientDat[p],a[1],length(a));
      end;
    3:begin; {get mixer values}
      a:=#0#0#0#0;
      pipeLineSend(clientDat[p],a[1],length(a));
      end;
    4:begin; {set mixer values}
      a:=#0;
      pipeLineSend(clientDat[p],a[1],length(a));
      end;
    5:begin; {get buffer info}
      i:=size shl 1;
      pipeLineSend(clientDat[p],i,sizeof(i));
      end;
    7:begin; {record sample}
      i:=0;
      pipeLineSend(clientDat[p],i,4);
      end;
    9:begin; {dial a number}
      i:=0;
      pipeLineSend(clientDat[p],i,1);
      end;
    10:begin; {hangup call}
      i:=0;
      pipeLineSend(clientDat[p],i,1);
      end;
    else goto f2;
    end;
  end;
Write(BStr(sumNum)+' '#13);
if (sumNum<1) then goto f1;
for i:=1 to size do bufI[i]:=sumBuf[i] div sumNum;
move(buf,buf[2],size shl 1);
buf[1]:=6;
exchange(buf,(size shl 1)+1);
sumNum:=0;
fillchar(sumBuf,sizeof(sumBuf),0);
goto f1;
END.