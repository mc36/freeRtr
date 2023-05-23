{$heap 15k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc param.inc}

Const connectMax=64;
Var
  bufferMax:LongInt;
  bufferBuf:^array[0..0] of char;
  bufferCur:LongInt;
  bufferTyp:String;
  bufferNam:String;
  connectDat:array[1..connectMax] of record
    pip:LongInt;
    upl:Boolean;
    end;
  connectCur:LongInt;
  packetBuf:record
    cmd:LongInt;
    dat:array[1..1024*4] of char;
    end;
  packetSiz:LongInt;


Function resize(n:LongInt):Boolean;
Var
  i:LongInt;
  p:Pointer;
Begin;
resize:=true;
i:=ExtendedMemoryResize(p,n);
if (i<n) then exit;
bufferBuf:=p^;
resize:=false;
End;

Function doOnePipe(n:LongInt):Boolean;
const max=1536;
Var i,o,p:LongInt;
Begin;
doOnePipe:=True;
packetSiz:=sizeof(packetBuf);
pipeLineRecv(connectDat[n].pip,packetBuf,packetSiz);
if (packetSiz=0) then begin;
  if (pipeLineStats(connectDat[n].pip,i,o,p)<>0) then exit;
  if (i=0) then exit;
  doOnePipe:=False;
  exit;
  end;
if (packetSiz<4) then exit;
dec(packetSiz,4);
case packetBuf.cmd of
  0:begin;{read bytes from clipboard}
    move(packetBuf.dat[1],p,sizeof(p));
    move(packetBuf.dat[5],o,sizeof(o));
    if (p>bufferCur) then p:=bufferCur;
    if (p<0) then p:=0;
    i:=bufferCur-p;
    if (o>i) then o:=i;
    if (o>max) then o:=max;
    if (o<0) then o:=0;
    move(bufferBuf^[p],packetBuf.dat,o);
    packetBuf.cmd:=0;
    if (pipeLineSend(connectDat[n].pip,packetBuf,o+4)<>0) then exit;
    doOnePipe:=False;
    end;
  1:begin;{clipboard information}
    move(bufferMax,packetBuf.dat[1],4);
    move(bufferCur,packetBuf.dat[5],4);
    move(bufferNam,packetBuf.dat[9],256);
    move(bufferTyp,packetBuf.dat[265],256);
    packetBuf.cmd:=0;
    if (pipeLineSend(connectDat[n].pip,packetBuf,520+4)<>0) then exit;
    doOnePipe:=False;
    end;
  2:begin;{start uploading}
    for i:=1 to connectCur do if connectDat[i].upl then exit;
    connectDat[n].upl:=true;
    move(packetBuf.dat[1],bufferNam,256);
    move(packetBuf.dat[257],bufferTyp,256);
    bufferCur:=0;
    resize(0);
    doOnePipe:=False;
    end;
  3:begin;{upload next part}
    if not connectDat[n].upl then exit;
    if (packetSiz+bufferCur>bufferMax) then exit;
    i:=bufferCur+packetSiz;
    if (I>bufferMax) then exit;
    if resize(i) then exit;
    move(packetBuf.dat,bufferBuf^[bufferCur],packetSiz);
    inc(bufferCur,packetSiz);
    doOnePipe:=False;
    end;
  4:begin;{stop uploading}
    if not connectDat[n].upl then exit;
    connectDat[n].upl:=false;
    doOnePipe:=False;
    end;
  end;
End;



Label f1;
Var i:LongInt;
BEGIN;
WriteLn('clipboard v1.0, done by Mc at '#%date' '#%time'.');
bufferMax:=BVal(ParamStr(1))*1024;
if (bufferMax<1) then begin;
  WriteLn('using: clipboard.code <max memory in KB>');
  Halt(1);
  end;
bufferCur:=0;
bufferTyp:='';
bufferNam:='';
connectCur:=0;
if (pipeLineBegListen<>0) then begin;
  WriteLn('error start listening...');
  Halt(1);
  end;
WriteLn('maximum acceptable data will be '+BStr(bufferMax)+' bytes.');
resize(0);
BugOS_SignDaemoning;

f1:
for i:=1 to connectCur do if doOnePipe(i) then begin;
  pipeLineClose(connectDat[i].pip);
  move(connectDat[connectCur],connectDat[i],sizeof(connectDat[1]));
  dec(connectCur);
  goto f1;
  end;
if (pipeLineGetIncoming(i)<>0) then begin;
  Relequish;
  goto f1;
  end;
if (connectCur>=connectMax) then begin;
  pipeLineClose(i);
  goto f1;
  end;
inc(connectCur);
connectDat[connectCur].pip:=i;
connectDat[connectCur].upl:=false;
goto f1;
END.