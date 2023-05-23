Type
  OneConnectionRecord=record
    tcp:LongInt;                {pipeline id of tcp connection}
    time:LongInt;               {time of last action}
    adr:OneTCPaddressRecord;    {remote address}
    prt:LongInt;                {remote port}
    stat:LongInt;               {status: 0-disc, 1-init, 2-vers, 3-auth, 4-shrd, 5-cmd, 6-skip}
                                {7-stPxFrm, 8-stEnc, 9-updtReq, 10-key, 11-mouse, 12-txt, 13-draw}
    disp:LongInt;               {pipeline id of display connection}
    keyb:LongInt;               {pipeline id of keyboard connection}
    mous:LongInt;               {pipeline id of mouse connection}
    proc:LongInt;               {process id of display manager}
    redSR:LongInt;              {red component right rotate}
    redSL:LongInt;              {red component left rotate}
    grnSR:LongInt;              {green component right rotate}
    grnSL:LongInt;              {green component left rotate}
    bluSR:LongInt;              {blue component right rotate}
    bluSL:LongInt;              {blue component left rotate}
    bytes:LongInt;              {bytes per pixel}
    lfbPos:LongInt;             {current lfb position}
    linLft:LongInt;             {current line left}
    linSkp:LongInt;             {line end skip value}
    winBX:LongInt;              {update window begin x}
    winBY:LongInt;              {update window begin y}
    winSX:LongInt;              {update window size x}
    winSY:LongInt;              {update window size y}
    lstSX:LongInt;              {update window size x}
    lstSY:LongInt;              {update window size y}
    shift:LongInt;              {shift status: b0=shift, b1=ctrl, b2=alt, b3..b5=right}
    mousX:LongInt;              {last mouse pointer x}
    mousY:LongInt;              {last mouse pointer y}
    chgd:Boolean;               {data content change}
    msb:Boolean;                {data needed in msb order}
    data:array[0..1] of byte;   {framebuffer in rgb format}
    end;
Var
  ConnectionDat:^array[0..1] of byte;
  ConnectionNum:LongInt;
  ConnectionSiz:LongInt;
  systemName:String;
  systemPass:String;
  screenSizX:LongInt;
  screenSizY:LongInt;
  screenSizP:LongInt;
  processNam:String;
  processPar:String;
  serverPort:LongInt;
  serverAddr:OneTCPaddressRecord;
  lastSent:LongInt;



Function ResizeMem(n:LongInt):Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
ResizeMem:=True;
i:=n*ConnectionSiz;
if (ExtendedMemoryResize(p,i)<i) then exit;
ConnectionNum:=n;
ConnectionDat:=p^;
ResizeMem:=False;
End;


Procedure openConn(var conPtr;p:LongInt);
Var con:OneConnectionRecord absolute conPtr;
Begin;
fillchar(con,ConnectionSiz,0);
con.stat:=1;
con.tcp:=p;
con.time:=CurrentTime;
End;


Procedure closeConn(var conPtr);
Var
  con:OneConnectionRecord absolute conPtr;
  i:LongInt;
Begin;
{$ifdef debug}WriteLn('closing session...');{$endif}
pipeLineClose(con.tcp);
pipeLineClose(con.disp);
pipeLineClose(con.keyb);
pipeLineClose(con.mous);
if (con.proc<>0) then begin;
  BugOS_KillProcess(con.proc);
  xExecBgnd('c:\system\process\killRecursive.code',BStr(con.proc),i);
  end;
move(ConnectionDat^[(ConnectionNum-1)*ConnectionSiz],con,ConnectionSiz);
ResizeMem(ConnectionNum-1);
End;

Function getBitCount(i:LongInt):LongInt;
Var o:LongInt;
Begin;
o:=0;
while (i<>0) do begin;
  inc(o);
  i:=i shr 1;
  end;
getBitCount:=o;
End;

Procedure appendConn(pip,prc:LongInt);
Label f1,f2,f3;
Var
  buf:array[1..128] of byte;
  con:OneConnectionRecord;
  i,o,ps:LongInt;
Begin;
{$ifdef debug}WriteLn('got new pipeline from pid #'+BStr(prc)+'...');{$endif}
ps:=0;
for i:=1 to ConnectionNum do begin;
  move(ConnectionDat^[ps],con,sizeof(con));
  if (con.proc=prc) then goto f2;
  inc(ps,ConnectionSiz);
  end;
f1:
pipeLineClose(pip);
exit;
f2:
if (con.disp=0) then begin;
  con.disp:=pip;
  move(screenSizX,buf[1],sizeof(screenSizX));
  move(screenSizY,buf[5],sizeof(screenSizY));
  pipeLineSend(pip,buf,8);
  goto f3;
  end;
if (con.keyb=0) then begin;
  con.keyb:=pip;
  goto f3;
  end;
if (con.mous=0) then begin;
  con.mous:=pip;
  goto f3;
  end;
goto f1;
f3:
move(con,ConnectionDat^[ps],sizeof(con));
End;
