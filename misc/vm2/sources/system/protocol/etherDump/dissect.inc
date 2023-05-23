Var
  dissectorProc:LongInt;
  dissectorPipe:LongInt;
  dissectorBuf:array[1..1024] of byte;
  dissectorSiz:LongInt;
  dissectorPos:LongInt;



Procedure dissectorSendCommand(a:String);
Var
  buf:array[1..1024] of byte;
  i,o:LongInt;
  ab:array[0..1] of byte absolute a;
  a0:byte absolute a;
  w:Word;
Begin
o:=0;
for i:=1 to a0 do begin;
  w:=ab[i];
  inc(o);
  move(w,buf[o],sizeof(w));
  inc(o);
  end;
w:=$8004;
inc(o);
move(w,buf[o],sizeof(w));
inc(o);
pipeLineSend(dissectorPipe,buf,o);
End;



Function dissectorGetLine:String;
Label f1;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  a0:byte absolute a;
  i,o,p:LongInt;
Begin;
dissectorGetLine:='';
a0:=0;
f1:
inc(dissectorPos);
if (dissectorPos>dissectorSiz) then begin;
  dissectorSiz:=sizeof(dissectorBuf);
  if (pipeLineRecv(dissectorPipe,dissectorBuf,dissectorSiz)<>0) then dissectorSiz:=0;
  dissectorPos:=0;
  if (dissectorSiz>0) then goto f1;
  relequish;
  pipeLineStats(dissectorPipe,p,i,o);
  if (p<>0) then exit;
  goto f1;
  end;
i:=dissectorBuf[dissectorPos];
if (i=10) then begin;
  dissectorGetLine:=a;
  exit;
  end;
if (i=13) then goto f1;
inc(a0);
ab[a0]:=i;
goto f1;
End;



Function dissectorInit:Boolean;
Var a:String;
Begin;
dissectorInit:=true;
dissectorProc:=0;
dissectorPipe:=0;
dissectorSiz:=0;
dissectorPos:=0;
if (xExecInside('\utils\tclsh.code','',dissectorProc,dissectorPipe)<>0) then exit;
dissectorSendCommand('source \sources\system\protocol\dissector\dissector.tcl');
dissectorInit:=false;
End;



Procedure dissectorDeinit;
Begin;
BugOS_KillProcess(dissectorProc);
pipeLineClose(dissectorPipe);
dissectorProc:=0;
dissectorPipe:=0;
dissectorSiz:=0;
dissectorPos:=0;
End;



Function dissectorPacket(var packet;var siz:LongInt):String;
Const sep='-------------------<separator>------------------- ';
Var
  pck:array[1..1] of byte absolute packet;
  i,o,p:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  a0:byte absolute a;
Begin;
dissectorPacket:='';
a:='doDisOnePack ETHER "ffffffffffff';
p:=0;
while (p<siz) do begin;
  inc(p);
  a:=a+byte2hextype(pck[p]);
  if (a0<100) then continue;
  dissectorSendCommand(a+'\');
  repeat a:=dissectorGetLine; until (a<>'');
  a:='';
  end;
dissectorSendCommand(a+'"');
dissectorSendCommand('doDisplayCurrent');
while (dissectorGetLine<>sep+'beg') do;
a:=dissectorGetLine;
if (dissectorGetLine<>sep+'mid') then exit;
while (a0>0) and (ab[1]=32) do a:=copy(a,2,666);
dissectorPacket:=a;
siz:=0;
while (1=1) do begin;
  a:=dissectorGetLine;
  if (a0=0) then continue;
  if (a=sep+'end') then exit;
  move(ab[1],pck[siz+1],a0);
  inc(siz,a0);
  inc(siz);pck[siz]:=13;
  end;
End;
