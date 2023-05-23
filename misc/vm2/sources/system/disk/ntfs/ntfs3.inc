Type
  OneFileRecordType=record
    begSec:LongInt;
    mftNum:LongInt;
    atrPos:Word;
    residnt:Boolean;
    RunsPos:Word;
    datPos:LongInt;
    datSiz:LongInt;
    bufNum:LongInt;
    end;


Function GetSectorOfMFTfile(fl:LongInt):LongInt;
Label f1;
Var i,o:LongInt;
Begin;
GetSectorOfMFTfile:=0;
fl:=fl*FileRecSize;
o:=fl div ClusterSiz;
fl:=fl mod ClusterSiz;
for i:=1 to MftDat.n do begin;
  if (o<MftDat.d[i].s) then goto f1;
  dec(o,MftDat.d[i].s);
  end;
exit;
f1:
inc(o,MftDat.d[i].b);
inc(fl,GetSectorFromCluster(o));
GetSectorOfMFTfile:=fl;
End;


Function GetNextRunFromRunsList(var d:OneFileRecordType;var ps:Word;var dSiz,dPos:LongInt):Byte;
Var
  BufCnt:Byte;
  posBuf:array[1..16] of Byte;
  posDat:LongInt absolute posBuf;
  sizBuf:array[1..16] of Byte;
  sizDat:LongInt absolute sizBuf;
  i,o,p:Byte;
Begin;
sizDat:=0;
posDat:=0;
BufCnt:=BufferData[d.bufNum][ps+1];inc(ps);
GetNextRunFromRunsList:=BufCnt;
o:=BufCnt and $f;
p:=BufCnt shr 4;
move(BufferData[d.bufNum][ps+1],sizBuf,o);inc(ps,o);
move(BufferData[d.bufNum][ps+1],posBuf,p);inc(ps,p);
if (p<1) then p:=1;
if (posBuf[p] and $80=0) then o:=0 else o:=$ff;
for i:=p+1 to sizeof(posBuf) do posBuf[i]:=o;
dSiz:=sizDat;
dPos:=posDat;
End;


Function DecodeRunsListInMFT(var d:OneFileRecordType;var rl:OneRunsListRecord):Boolean;
Label f1;
Var
  w:Word;
  i,p,q,r:LongInt;
Begin;
DecodeRunsListInMFT:=True;
if d.residnt then exit;
w:=d.atrPos+d.RunsPos;
r:=0;
fillchar(rl,sizeof(rl),0);
f1:
p:=r;
i:=GetNextRunFromRunsList(d,w,q,r);
inc(r,p);
if (i<>0) then begin;
  inc(rl.n);
  if (rl.n>MaxRunsListNum) then exit;
  rl.d[rl.n].b:=r;
  rl.d[rl.n].s:=q;
  goto f1;
  end;
DecodeRunsListInMFT:=False;
End;


Function GetClusterFromRunList(var d:OneFileRecordType;ps:LongInt):LongInt;
Label f1;
Var
  bufPos:Word;
  posDat:LongInt;
  sizDat:LongInt;
  BufCnt:Byte;
Var
  i,o,p:LongInt;
Begin;
GetClusterFromRunList:=0;
bufPos:=d.atrPos+d.RunsPos;
posDat:=0;
f1:
p:=posDat;
BufCnt:=GetNextRunFromRunsList(d,bufPos,sizDat,posDat);
inc(posDat,p);
if (ps<sizDat) then begin;
  GetClusterFromRunList:=posDat+ps;
  exit;
  end;
if (BufCnt=0) then exit;
dec(ps,sizDat);
goto f1;
End;


Function ReadFromRunsLst(var d:OneFileRecordType;ps:LongInt;sz:Word;var buff):Boolean;
Var
  buf:OneSectorRecord absolute buff;
  i,o,p:LongInt;
  sec:OneSectorRecord;
Begin;
ReadFromRunsLst:=True;
p:=0;
while (p<sz) do begin;
  o:=ps shr 9;
  i:=GetClusterFromRunList(d,o div ClusterSiz);
  if (i=0) then exit;
  i:=GetSectorFromCluster(i);
  if (DriveRead(i+o mod ClusterSiz,sec)<>0) then exit;
  i:=ps and $1ff;
  o:=512-i;
  if (o>sz-p) then o:=sz-p;
  move(sec[i+1],buf[p+1],o);
  inc(p,o);
  inc(ps,o);
  end;
ReadFromRunsLst:=False;
End;


Function WriteToRunsLst(var d:OneFileRecordType;ps:LongInt;sz:Word;var buff):Boolean;
Var
  buf:OneSectorRecord absolute buff;
  i,o,p,q:LongInt;
  sec:OneSectorRecord;
Begin;
WriteToRunsLst:=True;
p:=0;
while (p<sz) do begin;
  o:=ps shr 9;
  i:=GetClusterFromRunList(d,o div ClusterSiz);
  if (i=0) then exit;
  i:=GetSectorFromCluster(i);
  q:=i+o mod ClusterSiz;
  i:=ps and $1ff;
  o:=512-i;
  if (o>sz-p) then o:=sz-p;
  if (o<>512) then if (DriveRead(q,sec)<>0) then exit;
  move(buf[p+1],sec[i+1],o);
  if (DriveWrite(q,sec)<>0) then exit;
  inc(p,o);
  inc(ps,o);
  end;
WriteToRunsLst:=False;
End;



Function OpenOneMFTfile(n:LongInt;var d:OneFileRecordType;bufNum:LongInt):Boolean;
Var
  i:LongInt;
  hdr:OneMFTfileHeader;
Begin;
OpenOneMFTfile:=True;
fillchar(d,sizeof(d),0);
d.bufNum:=bufNum;
i:=GetSectorOfMFTfile(n);
if (i=0) then exit;
d.begSec:=i;
d.mftNum:=n;
if BufferReadSectorData(BufferData[bufNum],i,FileRecSize) then exit;
if BufferValidateData(BufferData[bufNum],FileRecSize,4,6) then exit;
move(BufferData[bufNum],hdr,sizeof(hdr));
if (hdr.magic<>$454c4946) then exit;
if (hdr.flags and 1=0) then exit;
d.atrPos:=0;
OpenOneMFTfile:=False;
End;


Function ReadFromAttribute(var d:OneFileRecordType;siz:word;var buf):Word;
Var
  i:LongInt;
Begin;
ReadFromAttribute:=0;
i:=d.datSiz-d.datPos;
if (i<0) then i:=0;
if (siz>i) then siz:=i;
if (siz<1) then exit;
if d.residnt then begin;
  move(BufferData[d.bufNum][d.atrPos+d.RunsPos+d.datPos+1],buf,siz);
  end else begin;
  if ReadFromRunsLst(d,d.datPos,siz,buf) then siz:=0;
  end;
inc(d.datPos,siz);
ReadFromAttribute:=siz;
End;


Function WriteToAttribute(var d:OneFileRecordType;siz:word;var buf):Word;
Var
  i:LongInt;
Begin;
WriteToAttribute:=0;
i:=d.datSiz-d.datPos;
if (i<0) then i:=0;
if (siz>i) then siz:=i;
if (siz<1) then exit;
if d.residnt then begin;
  move(buf,BufferData[d.bufNum][d.atrPos+d.RunsPos+d.datPos+1],siz);
  end else begin;
  if WriteToRunsLst(d,d.datPos,siz,buf) then siz:=0;
  end;
inc(d.datPos,siz);
WriteToAttribute:=siz;
End;


Function CloseOneMFTfile(var d:OneFileRecordType):Boolean;
Label Vege;
Var b:Boolean;
Begin;
CloseOneMFTfile:=True;
if BufferReValidyData(BufferData[d.bufNum],FileRecSize,4,6) then exit;
b:=BufferWriteSectorData(BufferData[d.bufNum],d.begSec,FileRecSize);
if BufferValidateData(BufferData[d.bufNum],FileRecSize,4,6) then exit;
if b then exit;
Vege:
CloseOneMFTfile:=False;
End;
