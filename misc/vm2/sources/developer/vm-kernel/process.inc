Procedure processUpdate(var proc;pid,pip,csx,csy:LongInt);
Var d:oneProcessRecord absolute proc;
Begin;
if (csx=0) then csx:=defaultScreenX;
if (csy=0) then csy:=defaultScreenY;
if (pid>=0) then d.consProc:=pid;
if (pip>=0) then d.consPipe:=pip;
if (csx>=0) then d.consSizX:=csx;
if (csy>=0) then d.consSizY:=csy;
End;



Function processCreate(nam,par,dir:String;ppid,uid,cpid,cid,csx,csy:LongInt):LongInt;
Var
  d:oneProcessRecord;
  ofs,id,i,o:LongInt;
Begin;
processCreate:=0;
if (dir='') then dir:='!:\';
ofs:=listingAdd(listingProc,(processBlockMax*sizeof(LongInt))+sizeof(d));
if (ofs=0) then exit;
move(totalMemoryByte^[ofs],id,sizeof(id));
fillchar(d,sizeof(d),0);
d.pid:=id;
d.ppid:=ppid;
d.cuid:=uid;
d.ouid:=uid;
processUpdate(d,cpid,cid,csx,csy);
d.filNam:=nam;
d.params:=par;
d.curDir:=dir;
d.status:=2;
d.oldStat:=2;
d.regIP:=-1;
d.blockOfs:=(ofs+sizeof(d)-sizeof(LongInt)) div sizeof(LongInt);
d.blockBck:=processBlockMax;
move(d,totalMemoryByte^[ofs],sizeof(d));
o:=d.blockOfs;
for i:=1 to processBlockMax do begin;
  totalMemoryLong^[o]:=0;
  inc(o);
  end;
processCreate:=id;
End;



Function processFindName(a:String):LongInt;
Label f1;
Var
  ab:array[0..1] of byte absolute a;
  proc:oneProcessRecord;
  i,o,p:LongInt;
Begin;
a:=lowCaseString(justFileName(a));
o:=ab[0]+1;
for i:=1 to ab[0] do if (ab[i]=58) then o:=i;
p:=BVal(copy(a,o+1,666));
a:=copy(a,1,o-1);
if (p<1) then p:=1;
for i:=1 to listingUsed(listingProc) do begin;
  o:=listingSeqNum(listingProc,i);
  if (o=0) then continue;
  o:=listingDataOfs(listingProc,o);
  if (o=0) then continue;
  move(totalMemoryByte^[o],proc,sizeof(proc));
  if (lowCaseString(justFileName(proc.filNam))<>a) then continue;
  dec(p);
  if (p>0) then continue;
  o:=proc.pid;
  goto f1;
  end;
o:=0;
f1:
processFindName:=o;
End;



Function processKillPID(pid,err,res:LongInt):Boolean;
Var
  num,ofs:LongInt;
  d:oneProcessRecord;
Begin;
processKillPID:=True;
num:=listingFind(listingProc,pid);
if (num=0) then exit;
ofs:=listingDataOfs(listingProc,num);
if (ofs=0) then exit;
move(totalMemoryByte^[ofs],d,sizeof(d));
d.status:=$ff;
d.oldStat:=$ff;
move(d,totalMemoryByte^[ofs],sizeof(d));
if listingDel(listingProc,num,(processBlockMax*sizeof(LongInt))+sizeof(d)) then exit;
pipelineKillPID(pid);
driveKillPID(pid);
handleKillPID(pid);
memoryKillPID(pid);
processKillPID:=False;
ofs:=listingOfs(listingProc,d.ppid);
if (ofs=0) then exit;
move(totalMemoryByte^[ofs],d,sizeof(d));
if (d.status<>4) then exit;
if (d.execWait<>pid) then exit;
d.execWait:=0;
d.status:=d.oldStat;
d.regs[reg_b]:=err;
d.regs[reg_a]:=res;
move(d,totalMemoryByte^[ofs],sizeof(d));
End;



Function processResize(var proc;siz:LongInt):Boolean;
Label f1;
Var
  d:oneProcessRecord absolute proc;
  i,o:LongInt;
Begin;
processResize:=True;
if (siz<0) then exit;
siz:=(siz+memoryPageMask) shr memoryPageBits;
if (siz>=d.blockBck) then exit;
f1:
if (d.blockFwd=siz) then begin; processResize:=False;exit; end;
if (d.blockFwd>siz) then begin; {decrease}
  dec(d.blockFwd);
  i:=d.blockOfs+d.blockFwd;
  o:=totalMemoryLong^[i];
  totalMemoryLong^[i]:=0;
  memoryAllocate(o,0);
  end else begin; {increase}
  i:=memoryFindFree;
  if (i=0) then exit;
  memoryAllocate(i,d.pid);
  totalMemoryLong^[d.blockOfs+d.blockFwd]:=i;
  inc(d.blockFwd);
  end;
goto f1;
End;



Function processMapMore(var proc;ofs,siz:LongInt):Boolean;
Var
  d:oneProcessRecord absolute proc;
  i,o:LongInt;
Begin;
processMapMore:=True;
if (siz<0) then exit;
siz:=(siz+memoryPageMask) shr memoryPageBits;
if (d.blockFwd+siz+1>=d.blockBck) then exit;
inc(ofs,siz*memoryPageByte);
dec(d.blockBck);
for i:=1 to siz do begin;
  dec(ofs,memoryPageByte);
  dec(d.blockBck);
  totalMemoryLong^[d.blockOfs+d.blockBck]:=ofs;
  end;
processMapMore:=False;
End;



Function processMemOfs(var proc;ofs:LongInt):LongInt;
Var d:oneProcessRecord absolute proc;
Begin;
processMemOfs:=0;
if (ofs<0) then exit;
ofs:=ofs shr memoryPageBits;
if (ofs>processBlockMax) then exit;
processMemOfs:=totalMemoryLong^[d.blockOfs+ofs];
End;



Function processCopyTo(var proc,buf;siz,ofs:LongInt):Boolean;
Var
  d:oneProcessRecord absolute proc;
  b:array[0..1] of byte absolute buf;
  i,o,p,q:LongInt;
Begin;
processCopyTo:=True;
q:=0;
while (siz>0) do begin;
  o:=processMemOfs(d,ofs);
  if (o=0) then exit;
  i:=ofs and memoryPageMask;
  p:=memoryPageByte-i;
  if (p>siz) then p:=siz;
  move(b[q],totalMemoryByte^[o+i],p);
  dec(siz,p);
  inc(ofs,p);
  inc(q,p);
  end;
processCopyTo:=(siz<>0);
End;



Function processCopyFrom(var proc,buf;siz,ofs:LongInt):Boolean;
Var
  d:oneProcessRecord absolute proc;
  b:array[0..1] of byte absolute buf;
  i,o,p,q:LongInt;
Begin;
processCopyFrom:=True;
q:=0;
while (siz>0) do begin;
  o:=processMemOfs(d,ofs);
  if (o=0) then exit;
  i:=ofs and memoryPageMask;
  p:=memoryPageByte-i;
  if (p>siz) then p:=siz;
  move(totalMemoryByte^[o+i],b[q],p);
  dec(siz,p);
  inc(ofs,p);
  inc(q,p);
  end;
processCopyFrom:=(siz<>0);
End;



Function processCheck1(var proc):Boolean;
Var
  d:oneProcessRecord absolute proc;
  hdr:oneExecutableRecord;
Begin;
processCheck1:=True;
if processCopyFrom(d,hdr,sizeof(hdr),0) then exit;
if (ReadLongLSB(hdr.magic)<>executableMagic) then exit;
if (ReadLongLSB(hdr.procN)<0) then exit;
if (ReadLongLSB(hdr.stackS)<0) then exit;
if (ReadLongLSB(hdr.heapS)<0) then exit;
processCheck1:=False;
End;



Function processCheck2(var proc;siz:LongInt):Boolean;
Var
  d:oneProcessRecord absolute proc;
  hdr:oneExecutableRecord;
  buf:array[1..512] of byte;
  i,o,p,q:LongInt;
Begin;
processCheck2:=True;
if (siz<sizeof(hdr)) then exit;
if processCopyFrom(d,hdr,sizeof(hdr),0) then exit;
if (ReadLongLSB(hdr.magic)<>executableMagic) then exit;
if (ReadLongLSB(hdr.codeS)<>siz) then exit;
if processResize(d,siz) then exit;
q:=$ffffffff;
p:=0;
dec(siz,sizeof(i));
while (p<siz) do begin;
  o:=siz-p;
  if (o>sizeof(buf)) then o:=sizeof(buf);
  if processCopyFrom(d,buf,o,p) then exit;
  for i:=1 to o do crc32update(q,buf[i]);
  inc(p,o);
  end;
q:=q xor $ffffffff;
if processCopyFrom(d,buf,sizeof(i),siz) then exit;
i:=ReadLongLSB(buf);
if (i<>q) then exit;
processCheck2:=False;
End;



Function processStart(var proc):Boolean;
Var d:oneProcessRecord absolute proc;

function getofs:LongInt;
begin;
getofs:=(d.blockFwd+1)*memoryPageByte;
end;

function alloc(s:LongInt):Boolean;
var i:LongInt;
begin;
alloc:=true;
i:=getofs;
inc(d.blockFwd);
if processResize(d,i+s) then exit;
alloc:=false;
end;

Var
  hdr:oneExecutableRecord;
  i:LongInt;
Begin;
processStart:=True;
if processCopyFrom(d,hdr,sizeof(hdr),0) then exit;
if (ReadLongLSB(hdr.magic)<>executableMagic) then exit;
d.procTab:=getofs;
i:=ReadLongLSB(hdr.procN);
d.procNum:=i;
if alloc(i*sizeof(LongInt)) then exit;
d.regSP:=getofs;
i:=ReadLongLSB(hdr.stackS);
if alloc(i) then exit;
d.stackEnd:=d.regSP+i;
d.heapFree:=getofs;
d.heapBeg:=getofs;
i:=ReadLongLSB(hdr.heapS);
if alloc(i) then exit;
d.heapEnd:=i+d.heapBeg;
d.extBeg:=getofs;
d.codeSiz:=ReadLongLSB(hdr.codeS);
d.regIP:=ReadLongLSB(hdr.entry);
d.status:=1;
d.oldStat:=1;
processStart:=False;
End;
