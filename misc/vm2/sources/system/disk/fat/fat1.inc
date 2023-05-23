Type
  OneSectorRecord=array[1..512] of byte;
  OneSTRdirectoryEntryRecord=record
    nam:array[1..8] of char;
    ext:array[1..3] of char;
    attr:byte; {1=readOnly, 2=hidden, 4=sys, 8=vol, 16=dir, 32=archive}
    casee:byte;
    creaTime2:byte;
    creaTime:word;
    creaDate:word;
    accsDate:word;
    clst2:word;
    modfTime:word; {11..15=hour, 5..10=min, 0..4=sec}
    modfDate:word; {9..15=year1980, 5..8=month, 0..4=day}
    clst1:word;
    size:LongInt;
    end;
  OneLFNdirectoryEntryRecord=record
    counter:byte; {64=last one}
    nam1:array[1..5] of word;
    attr:byte; {should be $0f}
    res1:byte; {reserved, should be 0}
    chksum:byte; {checksum of old name}
    nam2:array[1..6] of word;
    clst1:word;
    nam3:array[1..2] of word;
    end;
  OneBoot16sectorRecord=record
    jump:array[1..3] of char;    {jump to executable code}
    name:array[1..8] of char;    {OEM ID and version}
    BytesPerSec:word;            {bytes per sector}
    SecsPerClust:byte;           {sectors per cluster}
    ReservedSecs:word;           {number of reserved sectors}
    FatCopies:byte;              {number of FAT's on disk}
    RootEntry:word;              {number of root directory entries}
    TotSec16:word;               {number of total sectors (<32mb)}
    MediaDesc:byte;              {media descriptor byte}
    SecsPerFat16:word;           {sectors per FAT}
    SecsPerTrack:word;           {sectors per track}
    NumOfHeads:word;             {number of heads}
    HiddenSecs:longint;          {number of hidden sectors}
    TotSec32:longint;            {number of total sectors (>32mb)}
    DriveNum:byte;               {physical drive number}
    res1:byte;                   {reserved}
    DosSign:byte;                {signature byte (29h)}
    SerialNum:longint;           {volume serial number}
    VolLb:array[1..11] of char;  {volume label}
    FsID:array[1..8] of char;    {file system ID}
    code:array[1..$1c0] of char; {os loader code}
    sign:word;                   {signature word (55aah)}
    end;
Type
  OneBoot32sectorRecord=record
    jump:array[1..3] of char;    {jump to executable code}
    name:array[1..8] of char;    {OEM ID and version}
    BytesPerSec:word;            {bytes per sector}
    SecsPerClust:byte;           {sectors per cluster}
    ReservedSecs:word;           {number of reserved sectors}
    FatCopies:byte;              {number of FAT's on disk}
    RootEntry:word;              {number of root directory entries}
    TotSec16:word;               {number of total sectors}
    MediaDesc:byte;              {media descriptor byte}
    SecsPerFat16:word;           {sectors per FAT}
    SecsPerTrack:word;           {sectors per track}
    NumOfHeads:word;             {number of heads}
    HiddenSecs:longint;          {number of hidden sectors}
    TotSec32:longint;            {number of total sectors}
    SecsPerFat32:longint;        {sectors per FAT}
    ExtFlags:word;               {extended flags}
    FSver:word;                  {file system version}
    RootDirBeg:longint;          {first cluster of root directory}
    FSinfoSec:word;              {fs information sector number}
    BackupBootSec:word;          {sector of backup boot sector}
    res2:array[1..12] of char;   {reserved}
    DriveNum:byte;               {physical drive number}
    res1:byte;                   {reserved}
    DosSign:byte;                {signature byte (29h)}
    SerialNum:longint;           {volume serial number}
    VolLb:array[1..11] of char;  {volume label}
    FsID:array[1..8] of char;    {file system ID}
    code:array[1..$1a4] of char; {os loader code}
    sign:word;                   {signature word (55aah)}
    end;



{$ifdef fat12}
Const BitsInFatEntry=12;proggyName='fat12';Type OneBootSectorRecord=OneBoot16sectorRecord;
{$endif}
{$ifdef fat16}
Const BitsInFatEntry=16;proggyName='fat16';Type OneBootSectorRecord=OneBoot16sectorRecord;
{$endif}
{$ifdef fat32}
Const BitsInFatEntry=28;proggyName='fat32';Type OneBootSectorRecord=OneBoot32sectorRecord;
{$endif}

Const
  FatClusterMask=(1 shl BitsInFatEntry)-1;
  ClustEmpty=$00000000 and FatClusterMask;
  ClustBadB=$0ffffff0 and FatClusterMask;
  ClustBadE=$0ffffff7 and FatClusterMask;
  ClustEndB=$0ffffff8 and FatClusterMask;
  ClustEndE=$0fffffff and FatClusterMask;



Function compressDateForDirent:Word;
Var w1,w2,w3:Word;
Begin;
xGetDate(w1,w2,w3);
compressDateForDirent:=((w1-1980) shl 9) or (w2 shl 5) or w3;
End;

Function compressTimeForDirent:Word;
Var w1,w2,w3:Word;
Begin;
xGetTime(w1,w2,w3);
compressTimeForDirent:=(w1 shl 11) or (w2 shl 5) or (w3 shr 1);
End;

Function calculateCheckSum(data:string):byte;
Var
  t:array[0..511] of byte;
  name:array[0..12] of byte absolute data;
  i,o:LongInt;

procedure mover(c:longint);
var i:byte;
begin;
move(t,t[256],c);
move(t[c],t,256-c);
move(t[256],t[256-c],c);
i:=t[(256-c)];
move(t[257-c],t[256-c],c-1);
t[255]:=i;
end;

Begin;
for i:=0 to 255 do t[i]:=(i shr 2) or (i shl 6);
for i:=2 downto 0 do for o:=0 to name[i+9]-1 do mover(1 shl i);
for i:=7 downto 0 do for o:=0 to name[i+1]-1 do mover(1 shl i);
calculateCheckSum:=t[0];
End;
