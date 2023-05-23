Procedure doStreamForward(pipe,conn:LongInt);
Label f1,f2,f3;
Var
  buf:array[1..32*1024] of byte;
  rxSiz:LongInt;
  i,o,p,q:LongInt;
Begin;
rxSiz:=-1;
f1:
relequish;
pipeLineStats(conn,p,i,o);
if (p=0) then begin;
  WriteLn('remote closed connection!');
  exit;
  end;
if (o<32) then o:=0 else if (pipeLineRecv(pipe,buf,o)<>0) then o:=0;
if (o>0) then pipeLineSend(conn,buf,o);

pipeLineStats(pipe,p,i,o);
if (p=0) then begin;
  WriteLn('process closed connection!');
  exit;
  end;
if (o<32) then o:=0 else if (pipeLineRecv(conn,buf,o)<>0) then o:=0;
if (o>0) then pipeLineSend(pipe,buf,o);

goto f1;
End;
