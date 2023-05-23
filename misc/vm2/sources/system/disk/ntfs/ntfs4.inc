Function AddAttributeToFileRec(var d:OneFileRecordType;num:LongInt;res:Boolean;val:String):Boolean;
Label f1,f2;
Var
  i,o:LongInt;
  filhdr:OneMFTfileHeader;
  hdr1:OneNonResidAttribHeader;
  hdr0:OneResidentAttribHeader absolute hdr1;
  hdr2:OneResidentAttribHeader;
  hdrS:Byte;
Begin;
AddAttributeToFileRec:=True;
move(BufferData[d.bufNum],filhdr,sizeof(filhdr));
fillchar(hdr1,sizeof(hdr1),0);
if res then hdrS:=sizeof(hdr0) else hdrS:=sizeof(hdr1);
hdr0.typ:=num;
i:=hdrS+length(val);
while (i and 7<>0) do inc(i);
hdr0.totLen:=i;
hdr0.NamLen:=0;
hdr0.NamPos:=hdrS;
hdr0.Flags:=0;
hdr0.attribId:=random($ffff);
if (filhdr.RealSize+hdr0.totLen>filhdr.AlldSize) then Exit;
if res then begin;
  hdr1.Resid:=0;
  hdr0.DataLen:=length(val);
  hdr0.DataOfs:=hdrS;
  hdr0.idx:=0;
  end else begin;
  hdr0.Resid:=1;
  hdr1.BegVCN:=0;
  hdr1.EndVCN:=0;
  hdr1.RunsOfs:=hdrS;
  hdr1.ComprsUnit:=0;
  hdr1.AllocdSiz:=0;
  hdr1.RealSiz:=0;
  hdr1.InitedSiz:=0;
  end;
i:=filhdr.AttribPos+1;
f1:
move(BufferData[d.bufNum][i],hdr2,sizeof(hdr2));
if (hdr2.typ=-1) then goto f2;
inc(i,hdr2.totLen);
if (hdr2.totLen<sizeof(hdr2)) then Exit;
if (i>filhdr.RealSize+1) then Exit;
goto f1;
f2:
if (i+hdr1.totLen+8>filhdr.AlldSize) then Exit;
move(hdr1,BufferData[d.bufNum][i],hdrS);
inc(i,hdrS);
move(val[1],BufferData[d.bufNum][i],length(val));
inc(i,length(val));
o:=-1;move(o,BufferData[d.bufNum][i],sizeof(o));inc(i,sizeof(o));
o:=0;move(o,BufferData[d.bufNum][i],sizeof(o));inc(i,sizeof(o));
filhdr.RealSize:=i-1;
move(filhdr,BufferData[d.bufNum],sizeof(filhdr));
AddAttributeToFileRec:=False;
End;


Function FindNextAttribute(var d:OneFileRecordType):LongInt;
Var
  i:LongInt;
  filhdr:OneMFTfileHeader;
  hdr1:OneNonResidAttribHeader;
  hdr0:OneResidentAttribHeader absolute hdr1;
Begin;
FindNextAttribute:=-1;
d.RunsPos:=0;
d.datPos:=0;
d.datSiz:=0;
if (d.atrPos=0) then begin;
  move(BufferData[d.bufNum],filhdr,sizeof(filhdr));
  d.atrPos:=filhdr.AttribPos;
  end else begin;
  move(BufferData[d.bufNum][d.atrPos+1],hdr0,sizeof(hdr0));
  if (hdr0.typ=-1) then exit;
  inc(d.atrPos,hdr0.totLen);
  end;
move(BufferData[d.bufNum][d.atrPos+1],hdr0,sizeof(hdr0));
if (hdr0.typ=-1) then exit;
if (hdr0.totLen<sizeof(hdr0)) then exit;
d.residnt:=(hdr0.Resid and 1=0);
if d.residnt then begin;
  d.RunsPos:=hdr0.DataOfs;
  d.datSiz:=hdr0.DataLen;
  end else begin;
  move(BufferData[d.bufNum][d.atrPos+1],hdr1,sizeof(hdr1));
  d.RunsPos:=hdr1.RunsOfs;
  d.datSiz:=hdr1.RealSiz;
  end;
FindNextAttribute:=hdr0.typ;
End;


Function FindInAttribList(var d,dd:OneFileRecordType;bufNum:LongInt):LongInt;
Var
  hdr:OneAttribListRecord;
  i:LongInt;
begin;
FindInAttribList:=-1;
i:=ReadFromAttribute(d,sizeof(hdr),hdr);
if (i<sizeof(hdr)) then exit;
i:=hdr.len-sizeof(hdr);
if (i<0) then exit;
inc(d.datPos,i);
if OpenOneMFTfile(hdr.FileRef,dd,bufNum) then exit;
FindInAttribList:=hdr.typ;
End;


Function FindOneAttribute(var d,dd:OneFileRecordType;atr,bufNum:LongInt):Boolean;
Label f1,f2,vege;
Var
  i:LongInt;
Begin;
FindOneAttribute:=True;
d.atrPos:=0;
f1:
i:=FindNextAttribute(d);
if (i=-1) then exit;
if (i=$20) then begin;
  f2:
  i:=FindInAttribList(d,dd,bufNum);
  if (i=-1) then goto f1;
  repeat
    i:=FindNextAttribute(dd);
    if (i=-1) then goto f2;
    if (i=atr) then goto vege;
    until (1<>1);
  end;
if (i<>atr) then goto f1;
dd:=d;
dd.bufNum:=bufNum;
move(BufferData[d.bufNum],BufferData[dd.bufNum],BufferSectorLimit*512);
vege:
FindOneAttribute:=False;
End;


Function ReadOneBitmapStatus(var d:OneFileRecordType;p:LongInt):Byte;
Var i:Byte;
Begin;
ReadOneBitmapStatus:=$ff;
d.datPos:=p shr 3;
if (ReadFromAttribute(d,sizeof(i),i)<>sizeof(i)) then exit;
ReadOneBitmapStatus:=(i shr (p and 7)) and 1;
End;


Function WriteOneBitmapStatus(var d:OneFileRecordType;p:LongInt;s:Byte):Boolean;
Label f1;
Var i,o:Byte;
Begin;
WriteOneBitmapStatus:=True;
d.datPos:=p shr 3;
p:=p and 7;
if (ReadFromAttribute(d,sizeof(i),i)<>sizeof(i)) then exit;
o:=i;
p:=1 shl p;
i:=i or p;
if (s=0) then i:=i xor p;
if (o=i) then goto f1;
dec(d.datPos,sizeof(i));
if (WriteToAttribute(d,sizeof(i),i)<>sizeof(i)) then exit;
f1:
WriteOneBitmapStatus:=False;
End;


Function FindOneEmptyCluster(var d1,d2:OneFileRecordType;p1,p2:LongInt):LongInt;
Label f1;
Var
  i,o,p,m:LongInt;
Begin;
FindOneEmptyCluster:=-1;
if OpenOneMFTfile(6,d1,p1) then Exit;
if FindOneAttribute(d1,d2,$80,p2) then Exit;
p:=LastClst;
m:=d2.datSiz*8;
for i:=1 to m do begin;
  inc(p);
  if (p>=m) then p:=0;
  if (p<32) then p:=32;
  if (ReadOneBitmapStatus(d2,p)=0) then goto f1;
  end;
exit;
f1:
LastClst:=p;
FindOneEmptyCluster:=p;
End;


Function FindOneEmptyMftRec(var d1,d2:OneFileRecordType;p1,p2:LongInt):LongInt;
Label f1;
Var
  i,o,p,m:LongInt;
Begin;
FindOneEmptyMftRec:=-1;
if OpenOneMFTfile(0,d1,p1) then Exit;
if FindOneAttribute(d1,d2,$b0,p2) then Exit;
p:=LastMftf;
m:=d2.datSiz*8;
for i:=1 to m do begin;
  inc(p);
  if (p>=m) then p:=0;
  if (p<32) then p:=32;
  if (ReadOneBitmapStatus(d2,p)=0) then goto f1;
  end;
exit;
f1:
LastMftf:=p;
FindOneEmptyMftRec:=p;
End;


Function DeleteRunsListFromMFT(var d:OneFileRecordType):Boolean;
Var
  fhd:OneMFTfileHeader;
  ahd:OneNonResidAttribHeader;
  q,w:LongInt;
Begin;
DeleteRunsListFromMFT:=True;
if d.residnt then exit;
move(BufferData[d.bufNum],fhd,sizeof(fhd));
move(BufferData[d.bufNum][d.atrPos+1],ahd,sizeof(ahd));
q:=d.atrPos+ahd.RunsOfs;
w:=ahd.totLen-ahd.RunsOfs;
if (w<0) then exit;
dec(fhd.RealSize,w);
dec(ahd.totLen,w);
move(fhd,BufferData[d.bufNum],sizeof(fhd));
move(ahd,BufferData[d.bufNum][d.atrPos+1],sizeof(ahd));
move(BufferData[d.bufNum][q+w+1],BufferData[d.bufNum][q+1],fhd.RealSize-q);
DeleteRunsListFromMFT:=False;
End;


Function InsertRunsListIntoMFT(var d:OneFileRecordType;var rl:OneRunsListRecord):Boolean;
Var
  fhd:OneMFTfileHeader;
  ahd:OneNonResidAttribHeader;
  w:Word;

function addStr(a:String):Boolean;
var ps,pt,bc:Word;
begin;
addStr:=True;
ps:=w+1;
pt:=ps+length(a);
bc:=fhd.RealSize-w;
if (pt+bc>BufferSectorLimit*512) then exit;
move(BufferData[d.bufNum][ps],BufferData[d.bufNum][pt],bc);
bc:=length(a);
move(a[1],BufferData[d.bufNum][ps],bc);
inc(fhd.RealSize,bc);
inc(ahd.totLen,bc);
inc(w,bc);
addStr:=False;
end;

function encode(i:LongInt):String;
label f1;
var
  d:array[1..16] of byte;
  a:String[31];
begin;
if (i<0) then fillchar(d,sizeof(d),$ff) else fillchar(d,sizeof(d),0);
move(i,d,sizeof(i));
for i:=sizeof(d) downto 1 do if (d[i]<>d[sizeof(d)]) then goto f1;
i:=1;
f1:
if (d[i] and $80<>d[sizeof(d)] and $80) then inc(i);
a[0]:=chr(i);
move(d,a[1],i);
encode:=a;
end;

Var
  i,q:Word;
  o:LongInt;
  a,b:String[7];
Begin;
InsertRunsListIntoMFT:=True;
if d.residnt then exit;
move(BufferData[d.bufNum],fhd,sizeof(fhd));
move(BufferData[d.bufNum][d.atrPos+1],ahd,sizeof(ahd));
w:=d.atrPos+ahd.RunsOfs;
o:=0;
for q:=1 to rl.n do begin;
  a:=encode(rl.d[q].s);
  b:=encode(rl.d[q].b-rl.d[q-1].b);
  i:=length(a)+length(b) shl 4;
  if addStr(chr(i)) then exit;
  if addStr(a) then exit;
  if addStr(b) then exit;
  inc(o,rl.d[q].s);
  end;
o:=o*ClusterSiz*512;
ahd.AllocdSiz:=o;
ahd.RealSiz:=d.datSiz;
ahd.InitedSiz:=d.datSiz;
if addStr(#0) then exit;
while (ahd.totLen and 7<>0) do begin;
  if addStr(#0) then exit;
  end;
move(fhd,BufferData[d.bufNum],sizeof(fhd));
move(ahd,BufferData[d.bufNum][d.atrPos+1],sizeof(ahd));
InsertRunsListIntoMFT:=False;
End;


Function AppendRunsListBySome(var d,d1,d2:OneFileRecordType;s:LongInt;b1,b2:LongInt):Boolean;
Label f1,f2,f3;
Var
  w:Word;
  i,o,p,q:LongInt;
  rl:OneRunsListRecord;
Begin;
AppendRunsListBySome:=True;
if d.residnt then exit;
d.datSiz:=s;
i:=ClusterSiz shl 9;
s:=(s+i-1) div i;
if DecodeRunsListInMFT(d,rl) then exit;
o:=0;
for i:=1 to rl.n do inc(o,rl.d[i].s);
dec(s,o);
if (s<1) then goto f3;
q:=-1;
for p:=1 to s do begin;
  i:=FindOneEmptyCluster(d1,d2,b1,b2);
  if (i<0) then exit;
  if (q<0) then q:=i else if (i=q) then exit;
  if (rl.d[rl.n].b+rl.d[rl.n].s=i) then begin;
    inc(rl.d[rl.n].s);
    goto f2;
    end;
  inc(rl.n);
  if (rl.n>MaxRunsListNum) then exit;
  rl.d[rl.n].b:=i;
  rl.d[rl.n].s:=1;
  f2:
  end;
f3:
if DeleteRunsListFromMFT(d) then Exit;
if InsertRunsListIntoMFT(d,rl) then Exit;
if OpenOneMFTfile(6,d1,b1) then Exit;
if FindOneAttribute(d1,d2,$80,b2) then Exit;
for q:=1 to rl.n do begin;
  p:=rl.d[q].b;
  for i:=1 to rl.d[q].s do begin;
    if WriteOneBitmapStatus(d2,p,1) then Exit;
    inc(p);
    end;
  end;
AppendRunsListBySome:=False;
End;


Function TruncateRunsListBySome(var d,d1,d2:OneFileRecordType;s:LongInt;b1,b2:LongInt):Boolean;
Label f1,f3;
Var
  w:Word;
  i,o,p,q:LongInt;
  ol,rl:OneRunsListRecord;

procedure killZeros;
label f1;
begin;
f1:
if (rl.n<1) then exit;
if (rl.d[rl.n].s<1) then begin;
  dec(rl.n);
  goto f1;
  end;
end;

Begin;
TruncateRunsListBySome:=True;
if d.residnt then exit;
d.datSiz:=s;
i:=ClusterSiz shl 9;
s:=(s+i-1) div i;
if DecodeRunsListInMFT(d,rl) then exit;
ol:=rl;
o:=0;
for i:=1 to rl.n do inc(o,rl.d[i].s);
dec(o,s);
f1:
killZeros;
if (o<1) then goto f3;
i:=rl.d[rl.n].s;
if (i>o) then i:=o;
dec(rl.d[rl.n].s,i);
killZeros;
dec(o,i);
goto f1;
f3:
for i:=rl.n+1 to MaxRunsListNum do fillchar(rl.d[i],sizeof(rl.d[i]),0);
if DeleteRunsListFromMFT(d) then Exit;
if InsertRunsListIntoMFT(d,rl) then Exit;
if OpenOneMFTfile(6,d1,b1) then Exit;
if FindOneAttribute(d1,d2,$80,b2) then Exit;
for q:=1 to ol.n do begin;
  p:=ol.d[q].b+rl.d[q].s;
  for i:=rl.d[q].s+1 to ol.d[q].s do begin;
    if WriteOneBitmapStatus(d2,p,0) then Exit;
    inc(p);
    end;
  end;
TruncateRunsListBySome:=False;
End;


Function BufferReadStreamData(var buff;var hnd:OneFileRecordType;siz:LongInt):Boolean;
Const
  BufSizCnt=512;
Var
  buf:OneSectorRecord absolute buff;
  i,o:LongInt;
Begin;
BufferReadStreamData:=True;
if (siz>BufferSectorLimit) then exit;
o:=1;
for i:=1 to siz do begin;
  if (ReadFromAttribute(hnd,BufSizCnt,buf[o])<>BufSizCnt) then exit;
  inc(o,BufSizCnt);
  end;
BufferReadStreamData:=False;
End;


Function BufferWriteStreamData(var buff;var hnd:OneFileRecordType;siz:LongInt):Boolean;
Const
  BufSizCnt=512;
Var
  buf:OneSectorRecord absolute buff;
  i,o:LongInt;
Begin;
BufferWriteStreamData:=True;
if (siz>BufferSectorLimit) then exit;
o:=1;
for i:=1 to siz do begin;
  if (WriteToAttribute(hnd,BufSizCnt,buf[o])<>BufSizCnt) then exit;
  inc(o,BufSizCnt);
  end;
BufferWriteStreamData:=False;
End;
