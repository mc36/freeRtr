Type
  OneBootSectorRecord=record
    ninodes:Word;         {# usable inodes on the minor device}
    nzones:Word;          {total device size, including bit maps etc}
    imap_blocks:Word;     {# of blocks used by inode bit map}
    zmap_blocks:Word;     {# of blocks used by zone bit map}
    firstdatazone:Word;   {number of first data zone}
    log_zone_size:Word;   {log2 of blocks/zone}
    max_size:LongInt;     {maximum file size on this device}
    magic:Word;           {magic number to recognize super-blocks}
    res1:Word;            {try to avoid compiler-dependent padding}
    zones:LongInt;        {number of zones (replaces nzones in V2)}
    res2:array[1..488] of char;
    end;
Type
  OneSectorRecord=array[1..512] of char;


Var
  BlockSiz:LongInt;                 {size of one block}
  imapBeg:LongInt;                  {inode map beginning}
  zmapBeg:LongInt;                  {block map beginning}
  inodBeg:LongInt;                  {inodes beginning}
  zone1st:LongInt;                  {first zone number}
  inodMax:LongInt;                  {number of inodes}
  zoneMax:LongInt;                  {number of blocks}
  LastInodeTested:LongInt;          {last tested inode}
  LastBlockTested:LongInt;          {last tested block}
  DriveLetter:Char;                 {current drive letter}
  CurPath:String;                   {current path}


Function ReadUpBootSector:Boolean;
Var
  bs:OneBootSectorRecord;
  a:String;
  c:Byte;
  i:LongInt;
Begin;
ReadUpBootSector:=True;
if (DriveRead(2,bs)<>0) then exit;
if (bs.magic<>$2468) then exit;
BlockSiz:=2 shl bs.log_zone_size;
imapBeg:=2*BlockSiz;
zmapBeg:=imapBeg+bs.imap_blocks*BlockSiz;
inodBeg:=zmapBeg+bs.zmap_blocks*BlockSiz;
zone1st:=bs.firstdatazone;
inodMax:=bs.ninodes;
zoneMax:=bs.zones;
LastInodeTested:=0;
LastBlockTested:=0;
WriteLn('sectors/block: '+BStr(BlockSiz));
WriteLn('inode map begin: '+BStr(imapBeg));
WriteLn('zone map begin: '+BStr(zmapBeg));
WriteLn('inode begin: '+BStr(inodBeg));
WriteLn('block begin: '+BStr(zone1st));
WriteLn('number of inodes: '+BStr(inodMax));
WriteLn('number of blocks: '+BStr(zoneMax));
ReadUpBootSector:=False;
End;


Function bitUtilsBT(var buffer;n:LongInt):LongInt;
Var buf:array[0..1] of byte absolute buffer;
Begin;
bitUtilsBT:=(buf[n shr 3] shr (n and 7)) and 1;
End;

Procedure bitUtilsBS(var buffer;n:LongInt);
Var
  buf:array[0..1] of byte absolute buffer;
  i:LongInt;
Begin;
i:=n shr 3;
buf[i]:=buf[i] or (1 shl (n and 7));
End;

Procedure bitUtilsBR(var buffer;n:LongInt);
Var
  buf:array[0..1] of byte absolute buffer;
  i:LongInt;
Begin;
i:=n shr 3;
buf[i]:=($ff-(1 shl (n and 7))) and buf[i];
End;

Function CompareBinaryData(var buf1,buf2;siz:LongInt):Boolean;
Var
  dat1:array[1..1] of byte absolute buf1;
  dat2:array[1..1] of byte absolute buf2;
  i:LongInt;
Begin;
CompareBinaryData:=True;
for i:=1 to siz do if (dat1[i]<>dat2[i]) then exit;
CompareBinaryData:=False;
End;
