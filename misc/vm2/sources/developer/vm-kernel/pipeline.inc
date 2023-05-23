Function pipelineCreate(caller,answer,siz,blk:LongInt):LongInt;
Const max=64*1024;
Var
  d:onePipelineRecord;
  ofs,id:LongInt;
Begin;
pipelineCreate:=0;
if (siz<memoryPageByte) then siz:=memoryPageByte;
if (siz>max) then siz:=max;
siz:=(siz+memoryPageMask) and memoryPageNegM;
ofs:=listingAdd(listingPipe,siz);
if (ofs=0) then exit;
move(totalMemoryByte^[ofs],id,sizeof(id));
fillchar(d,sizeof(d),0);
d.id:=id;
d.size:=siz;
d.sizHF:=(siz-sizeof(d)) shr 1;
d.block:=blk;
d.pidC:=caller;
d.pidA:=answer;
move(d,totalMemoryByte^[ofs],sizeof(d));
pipelineCreate:=id;
End;



Function pipelineClose(id,pid:LongInt):Boolean;
Label f1,f2;
Var
  d:onePipelineRecord;
  num,ofs:LongInt;
Begin;
pipelineClose:=True;
num:=listingFind(listingPipe,id);
if (num=0) then exit;
ofs:=listingDataOfs(listingPipe,num);
if (ofs=0) then exit;
move(totalMemoryByte^[ofs],d,sizeof(d));
if (pid=d.pidC) then begin; d.pidC:=0;d.sizAC:=0;goto f1; end;
if (pid=d.pidA) then begin; d.pidA:=0;d.sizCA:=0;goto f1; end;
exit;
f1:
move(d,totalMemoryByte^[ofs],sizeof(d));
if (d.pidC=0) and (d.pidA=0) then listingDel(listingPipe,num,d.size);
f2:
pipelineClose:=False;
End;



Function pipelineCountPID(pid:LongInt):LongInt;
Var
  d:onePipelineRecord;
  i,o,p,q:LongInt;
Begin;
q:=0;
for p:=listingUsed(listingPipe) downto 1 do begin;
  o:=listingSeqNum(listingPipe,p);
  if (o=0) then continue;
  i:=listingDataOfs(listingPipe,o);
  if (i=0) then continue;
  move(totalMemoryByte^[i],d,sizeof(d));
  if (d.pidA=pid) or (d.pidC=pid) then inc(q);
  end;
pipelineCountPID:=q;
End;



Procedure pipelineKillPID(pid:LongInt);
Var
  d:onePipelineRecord;
  i,o,p:LongInt;
Begin;
for p:=listingUsed(listingPipe) downto 1 do begin;
  o:=listingSeqNum(listingPipe,p);
  if (o=0) then continue;
  i:=listingDataOfs(listingPipe,o);
  if (i=0) then continue;
  move(totalMemoryByte^[i],d,sizeof(d));
  if (d.pidA=pid) or (d.pidC=pid) then pipelineClose(d.id,pid);
  end;
End;



Function pipelineInfo(var pipe;pid:LongInt;var rem,rxReady,txFree:LongInt):Boolean;
Label f1;
Var d:onePipelineRecord absolute pipe;
Begin;
if (pid=d.pidC) then begin;
  rem:=d.pidA;
  txFree:=d.sizCA;
  rxReady:=d.sizAC;
  goto f1;
  end;
if (pid=d.pidA) then begin;
  rem:=d.pidC;
  txFree:=d.sizAC;
  rxReady:=d.sizCA;
  goto f1;
  end;
rem:=0;
txFree:=0;
rxReady:=0;
pipelineInfo:=true;
exit;
f1:
txFree:=d.sizHF-txFree;
if (d.block<>0) then dec(txFree,sizeof(LongInt));
if (txFree<0) then txFree:=0;
pipelineInfo:=false;
End;




Function pipelineSend(var pipe,buf;siz,pid:LongInt):Boolean;
Label f1,f2,f3;
Var
  d:onePipelineRecord absolute pipe;
  i,o:LongInt;

procedure activate(var proc);
var d:oneProcessRecord absolute proc;
begin;
d.relequish:=0;
end;

Begin;
if (siz=0) then goto f3;
if (siz<1) then goto f2;
if (siz>1024*1024) then goto f2;
if (d.block<>0) then o:=siz+sizeof(LongInt) else o:=siz;
if (pid=d.pidC) then begin;
  if (d.pidA=0) then goto f3;
  if (d.sizCA+o>d.sizHF) then goto f2;
  i:=listingOfs(listingProc,d.pidA);
  if (i<>0) then activate(totalMemoryByte^[i]);
  i:=d.sizCA;
  inc(d.sizCA,o);
  goto f1;
  end;
if (pid=d.pidA) then begin;
  if (d.pidC=0) then goto f3;
  if (d.sizAC+o>d.sizHF) then goto f2;
  i:=listingOfs(listingProc,d.pidC);
  if (i<>0) then activate(totalMemoryByte^[i]);
  i:=d.sizHF+d.sizAC;
  inc(d.sizAC,o);
  goto f1;
  end;
f2:
pipelineSend:=True;
exit;
f1:
if (d.block<>0) then begin;
  move(siz,d.buf[i],sizeof(siz));
  inc(i,4);
  end;
move(buf,d.buf[i],siz);
f3:
pipelineSend:=False;
End;




Function pipelineRecv(var pipe,buf;var siz:LongInt;pid:LongInt):Boolean;
Label f1,f2;
Var
  d:onePipelineRecord absolute pipe;
  i,o,p:LongInt;
Begin;
if (siz<1) then goto f2;
if (pid=d.pidC) then begin;
  i:=d.sizHF;
  o:=d.sizAC;
  goto f1;
  end;
if (pid=d.pidA) then begin;
  i:=0;
  o:=d.sizCA;
  goto f1;
  end;
f2:
pipelineRecv:=True;
siz:=0;
exit;
f1:
if (o<1) then goto f2;
if (d.block<>0) then begin;
  move(d.buf[i],p,sizeof(p));
  if (siz<p) then goto f2;
  inc(i,sizeof(LongInt));
  o:=p+sizeof(LongInt);
  end else begin;
  if (o>siz) then p:=siz else p:=o;
  o:=p;
  end;
move(d.buf[i],buf,p);
siz:=p;
if (pid=d.pidC) then begin;
  dec(d.sizAC,o);
  move(d.buf[d.sizHF+o],d.buf[d.sizHF],d.sizAC);
  end else begin;
  dec(d.sizCA,o);
  move(d.buf[o],d.buf[0],d.sizCA);
  end;
pipelineRecv:=False;
End;
