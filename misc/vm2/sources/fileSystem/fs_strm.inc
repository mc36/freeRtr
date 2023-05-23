Function StreamSetPos(var inod:OneInodeRecord;p:numberType):Word;
Begin;
if (p>=MaxFilePosition) then begin; StreamSetPos:=13;exit end;
inod.CurrPos:=p;
StreamSetPos:=0;
End;





Function StreamRead(var inod:OneInodeRecord;len:numberType;var buffer):Word;
Var
  sec:OneSectorRecord;
  buf:OneSectorRecord absolute buffer;
  ps,i,o,p:numberType;
  w:Word;
Begin;
ps:=inod.CurrPos;
if (len>MaxReadSize) then begin; StreamRead:=13;exit end;
o:=len+ps;
if (o>inod.size) then begin; StreamRead:=16;exit end;
p:=1;
while (len>0) do begin;
  w:=InodeGetPosition(inod,(ps div BytesPerSector)+1,o);
  if (w<>0) then begin; StreamRead:=w;exit; end;
  w:=DriveRead(o,sec);
  if (w<>0) then begin; StreamRead:=w;exit; end;
  i:=BytesPerSector-((BytesPerSector-1) and ps);
  if (i>len) then i:=len;
  move(sec[((BytesPerSector-1) and ps)+1],buf[p],i);
  inc(p,i);
  inc(ps,i);
  dec(len,i);
  end;
inod.CurrPos:=ps;
StreamRead:=0;
End;





Function StreamWrite(var inod:OneInodeRecord;len:numberType;var buffer):Word;
Var
  sec:OneSectorRecord;
  buf:OneSectorRecord absolute buffer;
  ps,i,o,p,q:numberType;
  w:Word;
Begin;
ps:=inod.CurrPos;
if (len>MaxReadSize) then begin; StreamWrite:=13;exit end;
o:=len+ps;
if (o>MaxFilePosition) then begin; StreamWrite:=16;exit end;
if (o>inod.size) then begin;
  w:=InodeSetNewSize(inod,(o+BytesPerSector-1) div BytesPerSector);
  if (w<>0) then begin; StreamWrite:=w;exit; end;
  inod.size:=o;
  end;
p:=1;
while (len>0) do begin;
  w:=InodeGetPosition(inod,(ps div BytesPerSector)+1,q);
  if (w<>0) then begin; StreamWrite:=w;exit; end;
  i:=BytesPerSector-((BytesPerSector-1) and ps);
  if (i>len) then i:=len;
  o:=((BytesPerSector-1) and ps)+1;
  if (o<>1) or (i<>512) then begin;
    w:=DriveRead(q,sec);
    if (w<>0) then begin; StreamWrite:=w;exit; end;
    end;
  move(buf[p],sec[o],i);
  w:=DriveWrite(q,sec);
  if (w<>0) then begin; StreamWrite:=w;exit; end;
  inc(p,i);
  inc(ps,i);
  dec(len,i);
  end;
inod.CurrPos:=ps;
StreamWrite:=0;
End;





Function StreamTruncate(var inod:OneInodeRecord):Word;
Var
  w:Word;
Begin;
w:=InodeSetNewSize(inod,(inod.CurrPos+BytesPerSector-1) div BytesPerSector);
if (w<>0) then begin; StreamTruncate:=w;exit; end;
inod.size:=inod.CurrPos;
StreamTruncate:=0;
End;
