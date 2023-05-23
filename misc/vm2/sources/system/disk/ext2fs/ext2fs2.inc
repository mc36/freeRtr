Function ReadOneGroup(n:LongInt;var gd:OneGroupDataRecord):Boolean;
Var
  o:LongInt;
  sec:OneSectorRecord;
Begin;
ReadOneGroup:=True;
if (n<0) then exit;
if (n>=groupNM) then exit;
n:=n*sizeof(gd);
o:=n mod sizeof(sec);
n:=n div sizeof(sec);
inc(n,(1+FirstDat)*blockSiz);
if (DriveRead(n,sec)<>0) then exit;
move(sec[o+1],gd,sizeof(gd));
ReadOneGroup:=False;
End;

Function GetInodeDatPos(num:LongInt;var grp,sec,ofs:LongInt):Boolean;
Var
  gd:OneGroupDataRecord;
  ips:LongInt;
Begin;
GetInodeDatPos:=True;
ips:=sizeof(OneSectorRecord) div InodeSZ;
if (ips<1) then ips:=1;
dec(num);
if (num<0) then exit;
if (num>=inodeNM) then exit;
grp:=num div InodsPG;
num:=num mod InodsPG;
if ReadOneGroup(grp,gd) then exit;
sec:=gd.inode_table*blockSiz;
ofs:=(num mod ips)*InodeSZ;
inc(sec,num div ips);
GetInodeDatPos:=False;
End;

Function GetInodeBitPos(num:LongInt;var grp,sec,bit:LongInt):Boolean;
Var
  gd:OneGroupDataRecord;
  dps:LongInt;
Begin;
GetInodeBitPos:=True;
dps:=8*sizeof(OneSectorRecord);
dec(num);
if (num<0) then exit;
if (num>=inodeNM) then exit;
grp:=num div InodsPG;
num:=num mod InodsPG;
if ReadOneGroup(grp,gd) then exit;
sec:=gd.inode_bitmap*blockSiz;
bit:=num mod dps;
inc(sec,num div dps);
GetInodeBitPos:=False;
End;

Function GetBlockBitPos(num:LongInt;var grp,sec,bit:LongInt):Boolean;
Var
  gd:OneGroupDataRecord;
  dps:LongInt;
Begin;
GetBlockBitPos:=True;
dps:=8*sizeof(OneSectorRecord);
dec(num);
if (num<0) then exit;
if (num>=blockNM) then exit;
grp:=num div BlockPG;
num:=num mod BlockPG;
if ReadOneGroup(grp,gd) then exit;
sec:=gd.block_bitmap*blockSiz;
bit:=num mod dps;
inc(sec,num div dps);
GetBlockBitPos:=False;
End;


Function GetBlkNumOfStream(var id:OneInodeDataRecord;blk:LongInt):LongInt;
Var
  dpb,dps:LongInt;
  sd:OneSectorRecord;
  i:LongInt;

Function ReadDesc(blk,num:LongInt):LongInt;
Begin;
ReadDesc:=0;
if (blk<1) then exit;
num:=num mod dpb;
blk:=blk*blockSiz;
inc(blk,num div dps);
num:=num mod dps;
if (DriveRead(blk,sd)<>0) then exit;
move(sd[num shl 2+1],num,sizeof(num));
ReadDesc:=num;
End;

Begin;
GetBlkNumOfStream:=0;
dps:=sizeof(OneSectorRecord) shr 2;
if (blk<1) then exit;
dec(blk);
if (blk<12) then begin;
  GetBlkNumOfStream:=id.block[1+blk];
  exit;
  end;
dec(blk,12);
dpb:=dps*blockSiz;
if (blk<dpb) then begin;
  i:=ReadDesc(id.block[13],blk);
  GetBlkNumOfStream:=i;
  exit;
  end;
dec(blk,dpb);
if (blk<dpb*dpb) then begin;
  i:=ReadDesc(id.block[14],blk div dpb);
  i:=ReadDesc(i,blk);
  GetBlkNumOfStream:=i;
  exit;
  end;
dec(blk,dpb*dpb);
if (blk<dpb*dpb*dpb) then begin;
  i:=ReadDesc(id.block[15],blk div (dpb*dpb));
  i:=ReadDesc(i,blk div dpb);
  i:=ReadDesc(i,blk);
  GetBlkNumOfStream:=i;
  exit;
  end;
GetBlkNumOfStream:=0;
End;


Function ReadInodeData(num:Longint;var grp:Longint;var id:OneInodeDataRecord):Boolean;
Var
  sec,ofs:LongInt;
  sd:OneSectorRecord;
Begin;
ReadInodeData:=True;
if GetInodeDatPos(num,grp,sec,ofs) then exit;
if (DriveRead(sec,sd)<>0) then exit;
move(sd[ofs+1],id,sizeof(id));
ReadInodeData:=False;
End;

Function WriteInodeData(num:Longint;var id:OneInodeDataRecord):Boolean;
Var
  grp,sec,ofs:LongInt;
  sd:OneSectorRecord;
Begin;
WriteInodeData:=True;
if GetInodeDatPos(num,grp,sec,ofs) then exit;
if (DriveRead(sec,sd)<>0) then exit;
move(id,sd[ofs+1],sizeof(id));
if (DriveWrite(sec,sd)<>0) then exit;
WriteInodeData:=False;
End;


Function ReadInodeBitmap(num:Longint;var grp:Longint):ShortInt;
Var
  sec,ofs:LongInt;
  sd:OneSectorRecord;
Begin;
ReadInodeBitmap:=-1;
if GetInodeBitPos(num,grp,sec,ofs) then exit;
if (DriveRead(sec,sd)<>0) then exit;
ReadInodeBitmap:=bitUtilsBT(sd,ofs);
End;

Function WriteInodeBitmap(num:Longint;val:boolean):Boolean;
Var
  grp,sec,ofs:LongInt;
  sd:OneSectorRecord;
Begin;
WriteInodeBitmap:=True;
if GetInodeBitPos(num,grp,sec,ofs) then exit;
if (DriveRead(sec,sd)<>0) then exit;
if (bitUtilsBT(sd,ofs)<>val) then begin;
  if val then bitUtilsBS(sd,ofs) else bitUtilsBR(sd,ofs);
  if (DriveWrite(sec,sd)<>0) then exit;
  end;
WriteInodeBitmap:=False;
End;

Function ReadBlockBitmap(num:Longint;var grp:Longint):ShortInt;
Var
  sec,ofs:LongInt;
  sd:OneSectorRecord;
Begin;
ReadBlockBitmap:=-1;
if GetBlockBitPos(num,grp,sec,ofs) then exit;
if (DriveRead(sec,sd)<>0) then exit;
ReadBlockBitmap:=bitUtilsBT(sd,ofs);
End;

Function WriteBlockBitmap(num:Longint;val:boolean):Boolean;
Var
  grp,sec,ofs:LongInt;
  sd:OneSectorRecord;
Begin;
WriteBlockBitmap:=True;
if GetBlockBitPos(num,grp,sec,ofs) then exit;
if (DriveRead(sec,sd)<>0) then exit;
if (bitUtilsBT(sd,ofs)<>val) then begin;
  if val then bitUtilsBS(sd,ofs) else bitUtilsBR(sd,ofs);
  if (DriveWrite(sec,sd)<>0) then exit;
  end;
WriteBlockBitmap:=False;
End;


Function FindEmptyBlock:LongInt;
Label f1;
Var
  i,o:LongInt;
Begin;
for o:=1 to blockNM do begin;
  inc(LastBlockTested);
  if (LastBlockTested>blockNM) then LastBlockTested:=1;
  if (ReadBlockBitmap(LastBlockTested,i)=0) then begin;
    FindEmptyBlock:=LastBlockTested;
    exit;
    end;
  end;
FindEmptyBlock:=0;
End;

Function FindEmptyInode:LongInt;
Label f1;
Var
  i,o:LongInt;
Begin;
for o:=1 to inodeNM do begin;
  inc(LastInodeTested);
  if (LastInodeTested>inodeNM) then LastInodeTested:=1;
  if (ReadInodeBitmap(LastInodeTested,i)=0) then begin;
    FindEmptyInode:=LastInodeTested;
    exit;
    end;
  end;
FindEmptyInode:=0;
End;



Function ReserveOneBlock(fillzero:boolean):LongInt;
Var
  sd:OneSectorRecord;
  i,o:LongInt;
Begin;
ReserveOneBlock:=0;
i:=FindEmptyBlock;
if (i<1) then Exit;
if WriteBlockBitmap(i,true) then exit;
ReserveOneBlock:=i;
if not fillzero then exit;
o:=i*blockSiz;
fillchar(sd,sizeof(sd),0);
for i:=1 to blockSiz do begin;
  if (DriveWrite(o,sd)<>0) then exit;
  inc(o);
  end;
End;
