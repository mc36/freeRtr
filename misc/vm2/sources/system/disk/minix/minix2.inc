Type
  OneInodeDataRecord=record
    mode:Word;                    {file type, protection, etc}
    nlinks:Word;                  {how many links to this file}
    uid:Word;                     {user id of the file's owner}
    gid:Word;                     {group number}
    size:LongInt;                 {current file size in bytes}
    atime:LongInt;                {when was file data last accessed}
    mtime:LongInt;                {when was file data last changed}
    ctime:LongInt;                {when was inode data last changed}
    zone:array[1..10] of LongInt; {block nums for direct, ind, and dbl ind}
    end;
  OneDirectoryEntryRecord=record
    ino:Word;                     {inode number}
    nam:array[1..14] of Char;     {name of file}
    end;


Function GetInodeDatPos(num:LongInt;var sec,ofs:LongInt):Boolean;
Var
 ips:LongInt;
Begin;
GetInodeDatPos:=True;
ips:=sizeof(OneSectorRecord) div sizeof(OneInodeDataRecord);
dec(num);
if (num<0) then exit;
if (num>=inodMax) then exit;
sec:=inodBeg+(num div ips);
ofs:=(num mod ips)*sizeof(OneInodeDataRecord);
GetInodeDatPos:=False;
End;


Function GetInodeBitPos(num:LongInt;var sec,bit:LongInt):Boolean;
Var
  dps:LongInt;
Begin;
GetInodeBitPos:=True;
dps:=8*sizeof(OneSectorRecord);
if (num<0) then exit;
if (num>=inodMax) then exit;
sec:=imapBeg+(num div dps);
bit:=num mod dps;
GetInodeBitPos:=False;
End;

Function GetBlockBitPos(num:LongInt;var sec,bit:LongInt):Boolean;
Var
  dps:LongInt;
Begin;
GetBlockBitPos:=True;
dps:=8*sizeof(OneSectorRecord);
if (num>zoneMax) then exit;
dec(num,zone1st);
inc(num);
if (num<1) then exit;
sec:=zmapBeg+(num div dps);
bit:=num mod dps;
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
blk:=blk*BlockSiz;
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
if (blk<7) then begin;
  GetBlkNumOfStream:=id.zone[1+blk];
  exit;
  end;
dec(blk,7);
dpb:=dps*BlockSiz;
if (blk<dpb) then begin;
  i:=ReadDesc(id.zone[8],blk);
  GetBlkNumOfStream:=i;
  exit;
  end;
dec(blk,dpb);
if (blk<dpb*dpb) then begin;
  i:=ReadDesc(id.zone[9],blk div dpb);
  i:=ReadDesc(i,blk);
  GetBlkNumOfStream:=i;
  exit;
  end;
{dec(blk,dpb*dpb);
if (blk<dpb*dpb*dpb) then begin;
  i:=ReadDesc(id.zone[10],blk div (dpb*dpb));
  i:=ReadDesc(i,blk div dpb);
  i:=ReadDesc(i,blk);
  GetBlkNumOfStream:=i;
  exit;
  end;}
GetBlkNumOfStream:=0;
End;


Function ReadInodeData(num:Longint;var id:OneInodeDataRecord):Boolean;
Var
  sec,ofs:LongInt;
  sd:OneSectorRecord;
Begin;
ReadInodeData:=True;
if GetInodeDatPos(num,sec,ofs) then exit;
if (DriveRead(sec,sd)<>0) then exit;
move(sd[ofs+1],id,sizeof(id));
ReadInodeData:=False;
End;

Function WriteInodeData(num:Longint;var id:OneInodeDataRecord):Boolean;
Var
  sec,ofs:LongInt;
  sd:OneSectorRecord;
Begin;
WriteInodeData:=True;
if GetInodeDatPos(num,sec,ofs) then exit;
if (DriveRead(sec,sd)<>0) then exit;
move(id,sd[ofs+1],sizeof(id));
if (DriveWrite(sec,sd)<>0) then exit;
WriteInodeData:=False;
End;


Function ReadInodeBitmap(num:Longint):ShortInt;
Var
  sec,ofs:LongInt;
  sd:OneSectorRecord;
Begin;
ReadInodeBitmap:=-1;
if GetInodeBitPos(num,sec,ofs) then exit;
if (DriveRead(sec,sd)<>0) then exit;
ReadInodeBitmap:=bitUtilsBT(sd,ofs);
End;

Function WriteInodeBitmap(num:Longint;val:boolean):Boolean;
Var
  sec,ofs:LongInt;
  sd:OneSectorRecord;
Begin;
WriteInodeBitmap:=True;
if GetInodeBitPos(num,sec,ofs) then exit;
if (DriveRead(sec,sd)<>0) then exit;
if (bitUtilsBT(sd,ofs)<>val) then begin;
  if val then bitUtilsBS(sd,ofs) else bitUtilsBR(sd,ofs);
  if (DriveWrite(sec,sd)<>0) then exit;
  end;
WriteInodeBitmap:=False;
End;


Function ReadBlockBitmap(num:Longint):ShortInt;
Var
  sec,ofs:LongInt;
  sd:OneSectorRecord;
Begin;
ReadBlockBitmap:=-1;
if GetBlockBitPos(num,sec,ofs) then exit;
if (DriveRead(sec,sd)<>0) then exit;
ReadBlockBitmap:=ord(bitUtilsBT(sd,ofs));
End;

Function WriteBlockBitmap(num:Longint;val:boolean):Boolean;
Var
  sec,ofs:LongInt;
  sd:OneSectorRecord;
Begin;
WriteBlockBitmap:=True;
if GetBlockBitPos(num,sec,ofs) then exit;
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
for o:=1 to zoneMax do begin;
  inc(LastBlockTested);
  if (LastBlockTested>zoneMax) then LastBlockTested:=1;
  if (ReadBlockBitmap(LastBlockTested)=0) then begin;
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
for o:=1 to inodMax do begin;
  inc(LastInodeTested);
  if (LastInodeTested>inodMax) then LastInodeTested:=1;
  if (ReadInodeBitmap(LastInodeTested)=0) then begin;
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
o:=i*BlockSiz;
fillchar(sd,sizeof(sd),0);
for i:=1 to BlockSiz do begin;
  if (DriveWrite(o,sd)<>0) then exit;
  inc(o);
  end;
End;
