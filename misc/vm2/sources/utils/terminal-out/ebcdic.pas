{$heap 31k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$include \sources\internet\kernel\utils\timer2.inc}
{$include ebcdicTable.inc}
{$include ebcdic.inc}

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
timer2start;
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
  con.maxX:=80;
  con.maxY:=25;
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