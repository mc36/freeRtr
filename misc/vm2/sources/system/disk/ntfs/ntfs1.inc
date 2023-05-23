Const
  MaxRunsListNum=512;
  ProggyName='ntfs filesystem v1.0';
Type
  OneRunsListRecord=record
    n:Word;
    d:array[0..MaxRunsListNum] of record
      b:LongInt;
      s:LongInt;
      end;
    end;
  OneSectorRecord=array[1..512] of char;
  OneBootSectorRecord=record
    jump:array[1..3] of char;    {3       jump to executable code}
    FSid:array[1..8] of char;    {8       OEM ID and version}
    BytesPerSec:word;            {word    bytes per sector}
    SecsPerClust:byte;           {byte    sectors per cluster}
    res1:array[1..7] of byte;    {7       reserved}
    MediaDesc:byte;              {byte    media descriptor byte}
    res2:array[1..2] of byte;    {2       reserved}
    SecsPerTrack:word;           {word    sectors per track}
    NumOfHeads:word;             {word    number of heads}
    res3:array[1..12] of byte;   {12      reserved}
    PartitionSize:LongInt;       {dword   sectors in storage unit 1/2}
    PartitionSize_hi:LongInt;    {4       sectors in storage unit 2/2}
    MFT1_LCN:LongInt;            {dword   $mft lcn of vcn0 1/2}
    MFT1_LCN_hi:LongInt;         {4       $mft lcn of vcn0 2/2}
    MFT2_LCN:LongInt;            {dword   $mftmirr lcn of vcn0 1/2}
    MFT2_LCN_hi:LongInt;         {4       $mftmirr lcn of vcn0 2/2}
    FileRecSize:LongInt;         {dword   file record size in clusters}
    IndxBufSize:LongInt;         {dword   index buffer size in clusters}
    SerialNum:longint;           {dword   volume serial number 1/2}
    SerialNum_hi:longint;        {4       volume serial number 2/2}
    res4:array[1..13] of byte;   {13      reserved}
    code:array[1..$1a1] of char; {417     os loader code}
    sign:word;                   {word    signature word (55aah)}
    end;
Var
  ClusterSiz:LongInt;           {in sectors}
  MftCluster:LongInt;           {cluster number}
  MftMirrClust:LongInt;         {cluster number}
  FileRecSize:LongInt;          {in sectors}
  IndxRecSize:LongInt;          {in sectors}
  LastClst:LongInt;             {last tested cluster}
  LastMftf:LongInt;             {last tested mft}
  MftDat:OneRunsListRecord;     {cluster list of mft}
  driveLetter:Char;             {drive letter}
  CurPath:String;               {current path}



Function ReadUpBootSector:Boolean;
Label f1;
Var
  bs:OneBootSectorRecord;
  a:String;
  c:Byte;
  i:LongInt;

function calc(i:LongInt):LongInt;
Var
  ii:integer absolute i;
  is:shortint absolute i;
  o:LongInt;
begin;
o:=ii;
if (i shr 16=0) and (o<0) then i:=o;
o:=is;
if (i shr 8=0) and (o<0) then i:=o;
if (i>0) then i:=i*ClusterSiz*512 else i:=1 shl abs(i);
i:=i shr 9;
calc:=i;
end;

Begin;
ReadUpBootSector:=True;
if (DriveRead(0,bs)<>0) then Exit;
if (bs.sign<>$aa55) then Exit;
a:='';
for i:=1 to sizeof(bs.FsID) do a:=a+bs.FsID[i];
kicserel(#0,'',a);
kicserel(' ','',a);
if (a<>'NTFS') then Exit;
ClusterSiz:=bs.SecsPerClust;
MftCluster:=bs.MFT1_LCN;
MftMirrClust:=bs.MFT2_LCN;
FileRecSize:=calc(bs.FileRecSize);
IndxRecSize:=calc(bs.IndxBufSize);
WriteLn('serial: '+BStr(bs.SerialNum)+':'+BStr(bs.SerialNum_hi));
WriteLn('sector/cluster: '+BStr(ClusterSiz));
WriteLn('sector/filerec: '+BStr(FileRecSize));
WriteLn('sector/indexrec: '+BStr(IndxRecSize));
WriteLn('mft main: '+BStr(MftCluster));
WriteLn('mft mirror: '+BStr(MftMirrClust));
LastClst:=0;
LastMftf:=0;
ReadUpBootSector:=False;
End;
