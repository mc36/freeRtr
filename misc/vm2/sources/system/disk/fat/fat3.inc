Type
  OneStreamRecord=record
    clst1st:LongWord;           {first cluster}
    clstCur:LongWord;           {current cluster}
    clstSeq:LongWord;           {cluster position}
    clstMax:LongWord;           {cluster maximum}
    posCur:LongWord;            {current position}
    posMax:LongWord;            {maximum position}
    parent:LongWord;            {parent dir cluster}
    end;

Function chainOpen(var d:OneStreamRecord;c:LongWord):Boolean;
Label f1;
Begin;
chainOpen:=true;
if (c<1) or (c>=ClustBadB) then exit;
d.clst1st:=c;
d.clstCur:=c;
d.clstSeq:=0;
d.clstMax:=0;
d.posCur:=0;
d.posMax:=0;
f1:
inc(d.clstMax);
c:=ReadOneFatEntry(c);
if (c>=ClustEndB) and (c<=ClustEndE) then begin; chainOpen:=false;exit; end;
if (c=ClustEmpty) then exit;
if (c>=ClustEndB) then exit;
goto f1;
End;

Function chainReopen(var d:OneStreamRecord):Boolean;
Var i,o,p:LongInt;
Begin;
i:=d.parent;
o:=d.posMax;
p:=d.posCur;
chainReopen:=chainOpen(d,d.clst1st);
d.parent:=i;
d.posMax:=o;
d.posCur:=p;
End;

Function chainSeek(var d:OneStreamRecord;c:LongWord):Boolean;
Var p:LongInt;
Begin;
chainSeek:=True;
if (c>=d.clstMax) then exit;
if (c<d.clstSeq) then begin;
  d.clstSeq:=0;
  d.clstCur:=d.clst1st;
  end;
while (d.clstSeq<c) do begin;
  p:=ReadOneFatEntry(d.clstCur);
  if (p=0) or (p>=ClustBadB) then exit;
  d.clstCur:=p;
  inc(d.clstSeq);
  end;
chainSeek:=False;
End;

Function chainAppend(var d:OneStreamRecord;c:LongWord):Boolean;
Var i:LongInt;
Begin;
chainAppend:=True;
if (d.clst1st<1) then exit;
while (d.clstMax<c) do begin;
  if chainSeek(d,d.clstMax-1) then exit;
  i:=findEmptyCluster;
  if (i<1) then exit;
  WriteOneFatEntry(i,ClustEndE);
  WriteOneFatEntry(d.clstCur,i);
  inc(d.clstMax);
  end;
chainAppend:=False;
End;

Function chainTrunc(var d:OneStreamRecord;c:LongWord):Boolean;
Label f1;
Var i:LongInt;
Begin;
chainTrunc:=True;
if chainSeek(d,c) then exit;
if (d.clst1st<1) then exit;
c:=ReadOneFatEntry(d.clstCur);
WriteOneFatEntry(d.clstCur,ClustEndE);
d.clstMax:=d.clstSeq;
chainTrunc:=False;
f1:
if (c=ClustEndE) then begin; chainTrunc:=False;exit; end;
if (c=0) or (c>=ClustBadB) then exit;
i:=ReadOneFatEntry(c);
WriteOneFatEntry(c,ClustEmpty);
c:=i;
goto f1;
End;

Function chainErase(c:LongInt):Boolean;
Label f1;
Var i:LongInt;
Begin;
chainErase:=True;
if (c=0) then begin; chainErase:=False;exit; end;
if (c<1) then exit;
f1:
if (c=ClustEndE) then begin; chainErase:=False;exit; end;
if (c=0) or (c>=ClustBadB) then exit;
i:=ReadOneFatEntry(c);
WriteOneFatEntry(c,ClustEmpty);
c:=i;
goto f1;
End;


Function streamRead(var d:OneStreamRecord;var buf;siz:LongInt):Boolean;
Var
  sec:OneSectorRecord;
  dat:array[1..1] of byte absolute buf;
  i,o,p:LongInt;
Begin;
streamRead:=True;
if (siz<1) or (siz>$10000) then exit;
i:=d.posMax-d.posCur;
if (siz>i) then exit;
p:=1;
while (siz>0) do begin;
  i:=d.posCur shr 9;
  o:=GetClusterSize(d.clstCur);
  if chainSeek(d,i div o) then exit;
  i:=GetClusterBegin(d.clstCur,i mod o);
  if (DriveRead(i,sec)<>0) then exit;
  i:=d.posCur and $1ff;
  o:=512-i;
  if (o>siz) then o:=siz;
  move(sec[i+1],dat[p],o);
  inc(d.posCur,o);
  dec(siz,o);
  inc(p,o);
  end;
streamRead:=False;
End;

Function streamWrite(var d:OneStreamRecord;var buf;siz:LongInt):Boolean;
Var
  sec:OneSectorRecord;
  dat:array[1..1] of byte absolute buf;
  i,o,p,q:LongInt;
Begin;
streamWrite:=True;
if (siz<1) or (siz>$10000) then exit;
i:=GetClusterSize(d.clstCur);
o:=siz+d.posCur;
if (o>d.posMax) then begin;
  if chainAppend(d,((o+i*512-1) div i) shr 9) then exit;
  d.posMax:=o;
  end;
p:=1;
while (siz>0) do begin;
  i:=d.posCur shr 9;
  o:=GetClusterSize(d.clstCur);
  if chainSeek(d,i div o) then exit;
  q:=GetClusterBegin(d.clstCur,i mod o);
  i:=d.posCur and $1ff;
  o:=512-i;
  if (o>siz) then o:=siz;
  if (o<512) then if (DriveRead(q,sec)<>0) then exit;
  move(dat[p],sec[i+1],o);
  if (DriveWrite(q,sec)<>0) then exit;
  inc(d.posCur,o);
  dec(siz,o);
  inc(p,o);
  end;
streamWrite:=False;
End;
