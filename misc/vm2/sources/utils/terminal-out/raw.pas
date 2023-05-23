{$heap 15k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

Const TerminalIniter=#0#0;
Type
  OneDataRecord=record
    inpP:LongInt;               {input pipeline}
    outP:LongInt;               {output pipeline}
    end;

Function doConn(var con:OneDataRecord):Boolean;
Var
  buf:array[1..1024] of byte;
  i,o,p:LongInt;
Begin;
doConn:=False;
pipeLineStats(con.inpP,i,o,p);
if (i=0) then begin; doConn:=true;exit; end;
if (p>256) then begin;
  if (p>sizeof(buf)) then p:=sizeof(buf);
  pipeLineRecv(con.outP,buf,p);
  pipeLineSend(con.inpP,buf,p);
  end;
pipeLineStats(con.outP,i,o,p);
if (i=0) then begin; doConn:=true;exit; end;
if (p>256) then begin;
  if (p>sizeof(buf)) then p:=sizeof(buf);
  if (p<2) then p:=0;
  pipeLineRecv(con.inpP,buf,p);
  pipeLineSend(con.outP,buf,p);
  end;
End;

Var
  ConnectionDat:^array[1..1] of OneDataRecord;
  ConnectionNum:LongInt;

Function ResizeMem(n:LongInt):Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
ResizeMem:=True;
i:=n*sizeof(OneDataRecord);
if (ExtendedMemoryResize(p,i)<i) then exit;
ConnectionNum:=n;
ConnectionDat:=p^;
ResizeMem:=False;
End;


Label f1;
Var
  con:OneDataRecord;
  i,o,p:LongInt;
  a:String;
BEGIN;
WriteLn('terminal (output) filter v1.0, done by Mc at '#%date' '#%time'.');
ConnectionNum:=0;

if pipeLineBegListen then begin;
  writeln('failed to start listening!');
  exit;
  end;

BugOS_SignDaemoning;

f1:
relequish;
while (pipeLineGetIncoming(p)=0) do begin;
  for i:=1 to ConnectionNum do begin;
    con:=ConnectionDat^[i];
    if (con.inpP=p) then goto f1;
    if (con.outP=p) then goto f1;
    end;
  fillchar(con,sizeof(con),0);
  con.inpP:=p;
  for i:=1 to 4 do relequish;
  i:=sizeof(o);
  pipeLineRecv(p,o,i);
  if (i<>sizeof(o)) then begin;
    pipeLineClose(p);
    continue;
    end;
  con.outP:=o;
  if ResizeMem(ConnectionNum+1) then begin;
    pipeLineClose(p);
    pipeLineClose(o);
    continue;
    end;
  ConnectionDat^[ConnectionNum]:=con;
  a:=TerminalIniter;
  pipeLineSend(con.inpP,a[1],length(a));
  end;
for i:=ConnectionNum downto 1 do if doConn(ConnectionDat^[i]) then begin;
  con:=ConnectionDat^[i];
  pipeLineClose(con.inpP);
  pipeLineClose(con.outP);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  end;

goto f1;
END.