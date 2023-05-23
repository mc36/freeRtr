Const BufferSectorLimit=120;
Var BufferData:array[1..3] of array[1..BufferSectorLimit*512] of byte;
Type
  OneMFTfileHeader=record
    magic:longint;
    UpdatePos:Word;
    UpdateSiz:Word;
    LogSeqNum:LongInt;
    LogSeqNum_hi:LongInt;
    SequNum:Word;
    LinkCount:Word;
    AttribPos:Word;
    Flags:Word;
    RealSize:LongInt;
    AlldSize:LongInt;
    BaseFileNum:LongInt;
    BaseFileNum_hi:LongInt;
    NextAttribId:Word;
    res1:array[1..2] of char;
    MyNumber:LongInt;
    end;
Type
  OneResidentAttribHeader=record
    typ:LongInt;
    totLen:LongInt;
    Resid:Byte;
    NamLen:Byte;
    NamPos:Word;
    Flags:Word;
    attribId:Word;

    DataLen:LongInt;
    DataOfs:Word;
    idx:Byte;
    res1:Byte;
    end;
  OneNonResidAttribHeader=record
    typ:LongInt;
    totLen:LongInt;
    Resid:Byte;
    NamLen:Byte;
    NamPos:Word;
    Flags:Word;
    attribId:Word;

    BegVCN:LongInt;
    BegVCN_hi:LongInt;
    EndVCN:LongInt;
    EndVCN_hi:LongInt;
    RunsOfs:Word;
    ComprsUnit:Word;
    res1:array[1..4] of char;
    AllocdSiz:LongInt;
    AllocdSiz_hi:LongInt;
    RealSiz:LongInt;
    RealSiz_hi:LongInt;
    InitedSiz:LongInt;
    InitedSiz_hi:LongInt;
    end;
Type
  OneAttribListRecord=record
    typ:LongInt;
    len:Word;
    NameOfs:Byte;
    NameLen:Byte;
    StartVCN:LongInt;
    StartVCN_hi:LongInt;
    FileRef:LongInt;
    FileRef_hi:LongInt;
    id:Word;
    end;
Type
  OneINDXrootHdrRecord=record
    attrib:LongInt;
    collat:LongInt;
    EntrySize:LongInt;
    ClustPerRec:Byte;
    res1:array[1..3] of char;
    firstOfs:LongInt;
    RealSize:LongInt;
    AllcSize:LongInt;
    flags:Byte;
    res2:array[1..3] of char;
    end;
  OneINDXheaderRecord=record
    magic:longInt;
    UpdatePos:Word;
    UpdateSiz:Word;
    LogSeqNum:LongInt;
    LogSeqNum_hi:LongInt;
    MyVCN:LongInt;
    MyVCN_hi:LongInt;
    EntryOfs:LongInt;
    EntrySiz:LongInt;
    AllocSiz:LongInt;
    HasChild:Boolean;
    res1:array[1..3] of char;
    end;
  OneINDXentryRecord=record
    fileMFTnum:LongInt;
    fileMFTnum_hi:LongInt;
    entrySiz:Word;
    extraSiz:Word;
    IdxFlg:Word;
    res1:array[1..2] of char;
    parentMFT:LongInt;
    parentMFT_hi:LongInt;
    Created:LongInt;
    Created_hi:LongInt;
    Modify1:LongInt;
    Modify1_hi:LongInt;
    Modify2:LongInt;
    Modify2_hi:LongInt;
    Accessd:LongInt;
    Accessd_hi:LongInt;
    AllocdSiz:LongInt;
    AllocdSiz_hi:LongInt;
    FileSize:LongInt;
    FileSize_hi:LongInt;
    Flags:LongInt;
    Flags_hi:LongInt;
    NameSiz:Byte;
    NameSpace:Byte;
    end;



Function GetSectorFromCluster(clst:LongInt):LongInt;
Begin;
GetSectorFromCluster:=clst*ClusterSiz;
End;


Function BufferReadSectorData(var buff;sec,siz:LongInt):Boolean;
Var
  buf:OneSectorRecord absolute buff;
  i,o:LongInt;
Begin;
BufferReadSectorData:=True;
if (siz>BufferSectorLimit) then exit;
o:=1;
for i:=1 to siz do begin;
  if (DriveRead(sec,buf[o])<>0) then exit;
  inc(sec);
  inc(o,512);
  end;
BufferReadSectorData:=False;
End;


Function BufferWriteSectorData(var buff;sec,siz:LongInt):Boolean;
Var
  buf:OneSectorRecord absolute buff;
  i,o:LongInt;
Begin;
BufferWriteSectorData:=True;
if (siz>BufferSectorLimit) then exit;
o:=1;
for i:=1 to siz do begin;
  if (DriveWrite(sec,buf[o])<>0) then exit;
  inc(sec);
  inc(o,512);
  end;
BufferWriteSectorData:=False;
End;


Function BufferValidateData(var buff;siz,upOfs,upSiz:LongInt):Boolean;
Var
  buf:OneSectorRecord absolute buff;
  i,o:LongInt;
  w,ww:Word;
Begin;
BufferValidateData:=True;
move(buf[upOfs+1],upOfs,2);
inc(upOfs);
inc(upSiz);
move(buf[upOfs],ww,sizeof(ww));
o:=511;
for i:=1 to siz do begin;
  move(buf[o],w,sizeof(w));
  if (w<>ww) then exit;
  inc(o,512);
  end;
move(buf[upSiz],ww,sizeof(ww));
upSiz:=longint(ww)-1;
if (upSiz>siz) then exit;
o:=511;
for i:=1 to upSiz do begin;
  inc(upOfs,sizeof(w));
  move(buf[upOfs],w,sizeof(w));
  move(w,buf[o],sizeof(w));
  inc(o,512);
  end;
BufferValidateData:=False;
End;


Function BufferReValidyData(var buff;siz,upOfs,upSiz:LongInt):Boolean;
Var
  buf:OneSectorRecord absolute buff;
  i,o:LongInt;
  w,ww:Word;
Begin;
BufferReValidyData:=True;
move(buf[upOfs+1],upOfs,2);
inc(upOfs);
inc(upSiz);
w:=siz+1;
move(w,buf[upSiz],sizeof(w));
ww:=random($ffff);
move(ww,buf[upOfs],sizeof(ww));
o:=511;
for i:=1 to siz do begin;
  move(buf[o],w,sizeof(w));
  inc(upOfs,sizeof(ww));
  move(w,buf[upOfs],sizeof(w));
  move(ww,buf[o],sizeof(ww));
  inc(o,512);
  end;
BufferReValidyData:=False;
End;
