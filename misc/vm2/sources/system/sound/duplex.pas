{$heap 127k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

Const proggyName='duplexer v1.0, done by Mc at '#%date' '#%time'.';
Var
  uppr,pip1,pip2:LongInt;
  size:LongInt;
  deviceDat:array[1..1024] of byte;
  deviceSiz:LongInt;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function exchange(pipe:LongInt;Var buf;siz:LongInt):LongInt;
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



Label f1;
Var
  buf:array[1..1024*16] of byte;
  i,o,p:LongInt;
  a:String;
BEGIN;
WriteLn(proggyName);
if (paramCount<2) then immErr('using: duplex.code <play> <record>');

a:=paramStr(1);
i:=BVal(a);
if (i=0) then i:=BugOS_findProcNam(a);
if (i=0) then immErr('process not found!');
if (pipeLineCreate(pip1,i,65536,true)<>0) then immErr('error opening pipeline!');

a:=paramStr(2);
i:=BVal(a);
if (i=0) then i:=BugOS_findProcNam(a);
if (i=0) then immErr('process not found!');
if (pipeLineCreate(pip2,i,65536,true)<>0) then immErr('error opening pipeline!');

buf[1]:=1;
p:=exchange(pip1,buf,1);
a:=proggyName+#13#10'play:';
move(a[1],deviceDat,sizeof(a));
deviceSiz:=length(a);
move(buf,deviceDat[deviceSiz+1],p);
inc(deviceSiz,p);

buf[1]:=1;
p:=exchange(pip2,buf,1);
a:=#13#10'record:';
move(a[1],deviceDat[deviceSiz+1],sizeof(a));
inc(deviceSiz,length(a));
move(buf,deviceDat[deviceSiz+1],p);
inc(deviceSiz,p);

buf[1]:=5;
exchange(pip1,buf,1);
move(buf,size,sizeof(size));
buf[1]:=5;
exchange(pip2,buf,1);
move(buf,i,sizeof(i));
if (size<>i) then immErr('block size mismatch!');
WriteLn('block size: '+BStr(size)+' bytes.');

pipeLineBegListen;
BugOS_SignDaemoning;
uppr:=0;

f1:
relequish;
if (uppr=0) then begin;
  if (pipeLineGetIncoming(p)<>0) then goto f1;
  uppr:=p;
  end;
i:=sizeof(buf);
if (pipeLineRecv(uppr,buf,i)<>0) then i:=0;
if (i<1) then begin;
  pipeLineStats(uppr,o,i,i);
  if (o<>0) then goto f1;
  pipeLineClose(uppr);
  uppr:=0;
  goto f1;
  end;
case buf[1] of
  8:begin; {play and record sample}
    buf[1]:=6;
    pipeLineSend(pip1,buf,i);
    buf[1]:=7;
    o:=exchange(pip2,buf,1);
    exchange(pip1,buf,-1);
    pipeLineSend(uppr,buf,o);
    end;
  6:begin; {play sample}
    o:=exchange(pip1,buf,i);
    pipeLineSend(uppr,buf,o);
    end;
  7:begin; {record sample}
    o:=exchange(pip2,buf,i);
    pipeLineSend(uppr,buf,o);
    end;
  1:begin; {get card identification}
    pipeLineSend(uppr,deviceDat,deviceSiz);
    end;
  2:begin; {get mixer names}
    a:=#0#0#0#0#0#0#0#0'no available volumebars'#0;
    pipeLineSend(uppr,a[1],length(a));
    end;
  3:begin; {get mixer values}
    a:=#0#0#0#0;
    pipeLineSend(uppr,a[1],length(a));
    end;
  4:begin; {set mixer values}
    a:=#0;
    pipeLineSend(uppr,a[1],length(a));
    end;
  5:begin; {get buffer info}
    i:=size;
    pipeLineSend(uppr,i,sizeof(i));
    end;
  9:begin; {dial a number}
    i:=0;
    pipeLineSend(uppr,i,1);
    end;
  10:begin; {hangup call}
    i:=0;
    pipeLineSend(uppr,i,1);
    end;
  else begin;
    pipeLineClose(uppr);
    uppr:=0;
    end;
  end;
goto f1;
END.