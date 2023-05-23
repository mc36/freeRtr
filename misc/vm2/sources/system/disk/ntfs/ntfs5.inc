Type
  OneDirectoryDescRecord=record
    mft:LongInt;
    pos:LongInt;
    rot:Boolean;
    begPos:LongInt;
    fil:OneFileRecordType;
    end;
  OneDirectoryEntryRecord=record
    siz:LongInt;
    rgt:LongInt;
    own:LongInt;
    crt:array[1..7] of byte;
    mdf:array[1..7] of byte;
    nam:String;
    dir:Boolean;
    mft:LongInt;
    end;
  OneFileRecord=record
    DirMft:LongInt;
    FileMft:LongInt;
    pos:LongInt;
    size:LongInt;
    end;

Procedure GetDiskStatistics(var free,used,bad:LongInt);
Label f1;
Var
  bct:array[0..255] of byte;
  buf:array[1..512] of byte;
  d,dd:OneFileRecordType;
  i,o:LongInt;

Function CountBitsInByte(c:byte):Byte;
Begin;
CountBitsInByte:= ((c shr 7)      )+((c shr 6) and 1)+
((c shr 5) and 1)+((c shr 4) and 1)+((c shr 3) and 1)+
((c shr 2) and 1)+((c shr 1) and 1)+((c      ) and 1);
End;

Begin;
for i:=0 to 255 do bct[i]:=CountBitsInByte(i);
free:=0;
used:=0;
bad:=0;
if OpenOneMFTfile(6,d,1) then exit;
if FindOneAttribute(d,dd,$80,2) then exit;
repeat
  o:=ReadFromAttribute(dd,sizeof(buf),buf);
  for i:=1 to o do inc(used,bct[buf[i]]);
  inc(free,o*8);
  until (o=0);
dec(free,used);
End;





Function FindNextDirEntry(var dir2,dir:OneDirectoryDescRecord;
                          var dat:OneDirectoryEntryRecord;bufNum:LongInt):Boolean;
Label f0,f1,f2,f3,f4,f5,f6;
Const
  Value2add=$18;
Var
  hdr0:OneINDXheaderRecord;
  hdr1:OneINDXentryRecord;
  hdr2:OneINDXrootHdrRecord;
  i,o,m,p:LongInt;
  a:String;
Begin;
FindNextDirEntry:=True;
f0:
fillchar(dat,sizeof(dat),0);
if dir2.rot then begin;
  if FindOneAttribute(dir2.fil,dir.fil,$90,bufNum) then Exit;
  if (ReadFromAttribute(dir.fil,sizeof(hdr2),hdr2)<>sizeof(hdr2)) then exit;
  p:=dir2.pos;
  if (p=0) then p:=dir.fil.datPos;
  goto f4;
  end;
if FindOneAttribute(dir2.fil,dir.fil,$a0,bufNum) then Exit;
f1:
m:=512*IndxRecSize;
dir.fil.datPos:=dir2.pos and (-m);
if BufferReadStreamData(BufferData[3],dir.fil,IndxRecSize) then exit;
if BufferValidateData(BufferData[3],IndxRecSize,4,6) then exit;
p:=dir2.pos and (m-1);
move(BufferData[3][1],hdr0,sizeof(hdr0));
inc(hdr0.EntryOfs,Value2add);
inc(hdr0.EntrySiz,Value2add);
inc(hdr0.AllocSiz,Value2add);
f2:
if (p=0) then p:=hdr0.EntryOfs;
if (p>=hdr0.EntrySiz) then begin;
  f3:
  dir2.pos:=dir.fil.datPos;
  goto f1;
  end;
dir2.begPos:=p;
move(BufferData[3][p+1],hdr1,sizeof(hdr1));
o:=p+hdr1.entrySiz;
if (hdr1.entrySiz<sizeof(hdr1)) then goto f3;
inc(p,$52);
a:='';
for i:=1 to hdr1.NameSiz do begin;
  inc(p);
  a:=a+chr(BufferData[3][p]);
  inc(p);
  end;
dir2.pos:=dir.fil.datPos-m+o;
f6:
dat.nam:=a;
dat.siz:=hdr1.FileSize;
dat.mft:=hdr1.fileMFTnum;
dat.dir:=(hdr1.Flags and $10000000<>0);
if dat.dir then dat.rgt:=$83 else dat.rgt:=$07;
FindNextDirEntry:=False;
exit;
f5:
dir2.pos:=0;
dir2.rot:=false;
goto f0;
f4:
dir.fil.datPos:=p;
dir2.begPos:=p;
if (ReadFromAttribute(dir.fil,sizeof(hdr1),hdr1)<>sizeof(hdr1)) then goto f5;
if (hdr1.entrySiz<sizeof(hdr1)) then goto f5;
dir2.pos:=p+hdr1.entrySiz;
a:='';
for o:=1 to hdr1.NameSiz do begin;
  if (ReadFromAttribute(dir.fil,2,i)<>2) then goto f5;
  a:=a+chr(i);
  end;
goto f6;
End;




Function FindEntryInDirectory(fn:string;var dir,dir2:OneDirectoryDescRecord;
                              var dat:OneDirectoryEntryRecord;bufNum:LongInt):Boolean;
Label f1;
Begin;
FindEntryInDirectory:=True;
fn:=Kicsi(fn);
dir.pos:=0;
dir.rot:=true;
f1:
if FindNextDirEntry(dir,dir2,dat,bufNum) then exit;
if (kicsi(dat.nam)<>fn) then goto f1;
FindEntryInDirectory:=False;
End;




Function SelectOneDirectory(s:String;var dir:OneDirectoryDescRecord):Boolean;
Label f1,f2;
Var
  dir2:OneDirectoryDescRecord;
  dat:OneDirectoryEntryRecord;
  a:String;
  i:LongInt;
Begin;
SelectOneDirectory:=True;
i:=5;
f1:
if OpenOneMFTfile(i,dir.fil,1) then Exit;
if FindOneAttribute(dir.fil,dir2.fil,$90,2) then Exit;
if (s='') then goto f2;
i:=pos('\',s);
if (i=0) then Exit;
a:=Copy(s,1,i-1);delete(s,1,i);
if FindEntryInDirectory(a,dir,dir2,dat,2) then Exit;
if not dat.dir then exit;
i:=dat.mft;
goto f1;
f2:
dir.mft:=i;
dir.pos:=0;
dir.rot:=true;
SelectOneDirectory:=False;
End;


function UpdateFileSizeInEntry(dirMft,fileMft,fileSiz:LongInt):Boolean;
Label f1;
Var
  d1,d2:OneDirectoryDescRecord;
  ntry:OneDirectoryEntryRecord;
  hdr:OneINDXentryRecord;
  i:LongInt;
Begin;
UpdateFileSizeInEntry:=True;
fillchar(d1,sizeof(d1),0);
if OpenOneMFTfile(dirMft,d1.fil,1) then Exit;
d1.mft:=dirMft;
d1.pos:=0;
d1.rot:=true;
f1:
if FindNextDirEntry(d1,d2,ntry,2) then Exit;
if (ntry.mft<>fileMft) then goto f1;
if d1.rot then begin;
  d2.fil.datPos:=d1.begPos;
  if (ReadFromAttribute(d2.fil,sizeof(hdr),hdr)<>sizeof(hdr)) then Exit;
  end else begin;
  move(BufferData[3][d1.begPos+1],hdr,sizeof(hdr));
  end;
i:=ClusterSiz*512;
i:=(fileSiz+i+1) and (-i);
hdr.AllocdSiz:=i;
hdr.FileSize:=fileSiz;
if d1.rot then begin;
  d2.fil.datPos:=d1.begPos;
  if (WriteToAttribute(d2.fil,sizeof(hdr),hdr)<>sizeof(hdr)) then Exit;
  CloseOneMFTfile(d2.fil);
  end else begin;
  i:=IndxRecSize*512;
  move(hdr,BufferData[3][(d1.begPos and (i-1))+1],sizeof(hdr));
  d2.fil.datPos:=d1.pos and (-i);
  if BufferReValidyData(BufferData[3],IndxRecSize,4,6) then exit;
  if BufferWriteStreamData(BufferData[3],d2.fil,IndxRecSize) then exit;
  end;
UpdateFileSizeInEntry:=False;
End;


Function AddOneEntryToDirectory(dirMft,fileMft,s:LongInt;d:Boolean;n:String):Boolean;
Label f1,f2;
Var
  d1,d2,d3:OneFileRecordType;
  ihd:OneINDXheaderRecord;
  fhd:OneINDXentryRecord;
  a:String;
  i,m:LongInt;
Begin;
AddOneEntryToDirectory:=True;
fillchar(a,sizeof(a),0);
a[0]:=chr(sizeof(fhd));
for i:=1 to length(n) do a:=a+n[i]+#0;
if (length(a)>240) then exit;
fhd.extraSiz:=length(a)-$10;
while (length(a) and 7<>0) do a:=a+#0;
fhd.fileMFTnum:=fileMft;
fhd.entrySiz:=length(a);
fhd.IdxFlg:=0;
fhd.parentMFT:=dirMft;
fhd.FileSize:=s;
m:=ClusterSiz*512;
fhd.AllocdSiz:=(s+m-1) and (-m);
if d then i:=$10000000 else i:=0;
fhd.Flags:=i;
fhd.NameSiz:=length(n);
fhd.NameSpace:=2;
move(fhd,a[1],sizeof(fhd));
m:=0;
f1:
if OpenOneMFTfile(dirMft,d1,1) then Exit;
if FindOneAttribute(d1,d2,$90,2) then Exit;
d2.datPos:=$1c;
if (ReadFromAttribute(d2,1,i)<>1) then i:=-1;
if (i and 1=0) then begin;
  d2.datPos:=$1c;
  i:=i or 1;
  WriteToAttribute(d2,1,i);
  CloseOneMFTfile(d2);
  end;
if FindOneAttribute(d1,d2,$a0,2) then begin;
  if (m<>0) then Exit;
  if AddAttributeToFileRec(d1,$a0,false,#0#0#0#0#0#0#0#0) then Exit;
  if AddAttributeToFileRec(d1,$b0,true,#0#0#0#0#0#0#0#0) then Exit;
  if CloseOneMFTfile(d1) then Exit;
  inc(m);
  goto f1;
  end;
m:=IndxRecSize*512;
f2:
if (d2.datPos>=d2.datSiz) then begin;
  i:=(d2.datSiz+m-1) and (-m);
  inc(i,m);
  if FindOneAttribute(d1,d2,$b0,3) then Exit;
  if WriteOneBitmapStatus(d2,(i div m)-1,1) then Exit;
  if CloseOneMFTfile(d2) then Exit;
  if FindOneAttribute(d1,d2,$a0,2) then Exit;
  if AppendRunsListBySome(d2,d1,d3,i,1,3) then Exit;
  if CloseOneMFTfile(d2) then Exit;
  d2.datPos:=i-m;
  fillchar(BufferData[3],m,0);
  move(BufferData[3],ihd,sizeof(ihd));
  ihd.magic:=$58444e49;
  ihd.UpdatePos:=sizeof(ihd);
  ihd.UpdateSiz:=IndxRecSize+1;
  ihd.LogSeqNum:=0;
  ihd.LogSeqNum_hi:=0;
  ihd.MyVCN:=0;
  ihd.MyVCN_hi:=0;
  i:=$40-$18;
  ihd.EntryOfs:=i;
  ihd.EntrySiz:=i+$10;
  ihd.AllocSiz:=m-$18;
  ihd.HasChild:=False;
  move(ihd,BufferData[3],sizeof(ihd));
  if BufferReValidyData(BufferData[3],IndxRecSize,4,6) then exit;
  if BufferWriteStreamData(BufferData[3],d2,IndxRecSize) then exit;
  dec(d2.datPos,m);
  end;
if BufferReadStreamData(BufferData[3],d2,IndxRecSize) then exit;
if BufferValidateData(BufferData[3],IndxRecSize,4,6) then exit;
move(BufferData[3],ihd,sizeof(ihd));
i:=ihd.AllocSiz-ihd.EntrySiz;
if (i<length(a)) then goto f2;
i:=ihd.EntrySiz+8;
move(a[1],BufferData[3][i+1],length(a));
inc(i,length(a));
a:=#0#0#0#0#0#0#0#0#16#0#0#0#2#0#0#0;
move(a[1],BufferData[3][i+1],length(a));
ihd.EntrySiz:=i-8;
move(ihd,BufferData[3],sizeof(ihd));
if BufferReValidyData(BufferData[3],IndxRecSize,4,6) then exit;
dec(d2.datPos,m);
if BufferWriteStreamData(BufferData[3],d2,IndxRecSize) then exit;
AddOneEntryToDirectory:=False;
End;


Function DeleteEntryFromDirectory(dirMft:LongInt;a:String):Boolean;
Var
  d1,d2:OneDirectoryDescRecord;
  ntry:OneDirectoryEntryRecord;
  rhd:OneINDXrootHdrRecord;
  ihd:OneINDXheaderRecord;
  fhd:OneINDXentryRecord;
  i,o,p,m:LongInt;
Begin;
DeleteEntryFromDirectory:=True;
if OpenOneMFTfile(dirMft,d1.fil,1) then Exit;
if FindEntryInDirectory(a,d1,d2,ntry,2) then Exit;
d1.pos:=d1.begPos;
if d1.rot then begin;
  if FindOneAttribute(d1.fil,d2.fil,$90,2) then Exit;
  d2.fil.datPos:=d1.pos;
  if (ReadFromAttribute(d2.fil,sizeof(fhd),fhd)<>sizeof(fhd)) then exit;
  m:=fhd.entrySiz;
  d2.fil.datPos:=0;
  if (ReadFromAttribute(d2.fil,sizeof(rhd),rhd)<>sizeof(rhd)) then exit;
  dec(rhd.RealSize,m);
  dec(rhd.AllcSize,m);
  d2.fil.datPos:=0;
  if (WriteToAttribute(d2.fil,sizeof(rhd),rhd)<>sizeof(rhd)) then exit;
  o:=d1.pos;
  p:=o+m;
  m:=d2.fil.datSiz;
  while (p<m) do begin;
    i:=m-p;
    if (i>sizeof(a)) then i:=sizeof(a);
    d2.fil.datPos:=p;
    if (ReadFromAttribute(d2.fil,i,a)<>i) then exit;
    d2.fil.datPos:=o;
    if (WriteToAttribute(d2.fil,i,a)<>i) then exit;
    inc(o,i);
    inc(p,i);
    end;
  CloseOneMFTfile(d2.fil);
  end else begin;
  m:=IndxRecSize*512;
  if FindOneAttribute(d1.fil,d2.fil,$a0,2) then Exit;
  d2.fil.datPos:=d1.pos and (-m);
  if BufferReadStreamData(BufferData[3],d2.fil,IndxRecSize) then exit;
  if BufferValidateData(BufferData[3],IndxRecSize,4,6) then exit;
  move(BufferData[3],ihd,sizeof(ihd));
  i:=d1.pos and (m-1);
  move(BufferData[3][i+1],fhd,sizeof(fhd));
  o:=fhd.entrySiz;
  move(BufferData[3][i+o+1],BufferData[3][i+1],m-i);
  dec(ihd.EntrySiz,o);
  move(ihd,BufferData[3],sizeof(ihd));
  if BufferReValidyData(BufferData[3],IndxRecSize,4,6) then exit;
  dec(d2.fil.datPos,m);
  if BufferWriteStreamData(BufferData[3],d2.fil,IndxRecSize) then exit;
  end;
DeleteEntryFromDirectory:=False;
End;
