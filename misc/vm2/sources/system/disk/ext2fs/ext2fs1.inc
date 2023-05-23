Const
  proggyName='ext2fs filesystem v1.0';
Type
  OneSectorRecord=array[1..512] of char;
  OneBootSectorRecord=record
    inodes_count:LongInt;         {inodes count}
    blocks_count:LongInt;         {blocks count}
    r_blocks_count:LongInt;       {reserved blocks count}
    free_blocks_count:LongInt;    {free blocks count}
    free_inodes_count:LongInt;    {free inodes count}
    first_data_block:LongInt;     {first data block}
    log_block_size:LongInt;       {block size}
    log_frag_size:LongInt;        {fragment size}
    blocks_per_group:LongInt;     {# blocks per group}
    frags_per_group:LongInt;      {# fragments per group}
    inodes_per_group:LongInt;     {# inodes per group}
    mtime:LongInt;                {mount time}
    wtime:LongInt;                {write time}
    mnt_count:Word;               {mount count}
    max_mnt_count:Word;           {maximal mount count}
    magic:Word;                   {magic signature}
    state:Word;                   {file system state}
    errors:Word;                  {behaviour when detecting errors}
    minor_rev_level:Word;         {minor revision level}
    lastcheck:LongInt;            {time of last check}
    checkinterval:LongInt;        {max. time between checks}
    creator_os:LongInt;           {os}
    rev_level:LongInt;            {revision level}
    def_resuid:Word;              {default uid for reserved blocks}
    def_resgid:Word;              {default gid for reserved blocks}
    {these fields are for EXT2_DYNAMIC_REV superblocks only}
    first_ino:LongInt;            {first non-reserved inode}
    inode_size:Word;              {size of inode structure}
    block_group_nr:Word;          {block group # of this superblock}
    feature_compat:LongInt;       {compatible feature set}
    feature_incompat:LongInt;     {incompatible feature set}
    feature_ro_compat:LongInt;    {readonly-compatible feature set}
    uuid:array [1..16] of byte;            {128-bit uuid for volume}
    volume_name:array[1..16] of char;      {volume name}
    last_mounted:array[1..64] of char;     {directory where last mounted}
    algorithm_usage_bmap:LongInt; {for compression}
    {performance hints}
    prealloc_blocks:Byte;         {nr of blocks to try to preallocate}
    prealloc_dir_blocks:Byte;     {nr to preallocate for dirs}
    res1:array[1..306] of char;   {padding to the end of the block}
    end;
  OneGroupDataRecord=record
    block_bitmap:LongInt;     {blocks bitmap block}
    inode_bitmap:LongInt;     {inodes bitmap block}
    inode_table:LongInt;      {inodes table block}
    free_blocks_count:Word;   {free blocks count}
    free_inodes_count:Word;   {free inodes count}
    used_dirs_count:Word;     {directories count}
    res1:array[1..14] of byte;
    end;
  OneInodeDataRecord=record
    mode:Word;            {file mode}
    uid:Word;             {owner uid}
    size:LongInt;         {size in bytes}
    atime:LongInt;        {access time}
    ctime:LongInt;        {creation time}
    mtime:LongInt;        {modification time}
    dtime:LongInt;        {deletion time}
    gid:Word;             {group id}
    links_count:Word;     {links count}
    blocks:LongInt;       {blocks count}
    flags:LongInt;        {file flags}
    res1:array[1..4] of byte;
    block:array[1..15] of LongInt;  {pointers to blocks}
    version:LongInt;      {file version (for nfs)}
    file_acl:LongInt;     {file acl}
    dir_acl:LongInt;      {directory acl}
    faddr:LongInt;        {fragment address}
    frag:Byte;            {fragment number}
    fsize:Byte;           {fragment size}
    res2:array[1..10] of byte;
    end;
  OneDirectoryEntryRecord=record
    ino:LongInt;              {inode number}
    recsiz:Word;              {directory entry length}
    namlen:Byte;              {name length}
    typ:Byte;                 {type of file}
    namdat:array[1..256] of Char;  {name of file}
    end;


Var
  BlockSiz:LongInt;                 {sectors per block}
  FragSiz:LongInt;                  {sectors per fragment}
  FirstDat:LongInt;                 {first data block}
  inodBeg:LongInt;                  {first nonreserved inode}
  BlockPG:LongInt;                  {block per group}
  FragsPG:LongInt;                  {frags per group}
  InodsPG:LongInt;                  {inodes per group}
  BlockNM:LongInt;                  {number of blocks}
  InodeNM:LongInt;                  {number of inodes}
  InodeSZ:LongInt;                  {size of one inode}
  GroupNM:LongInt;                  {number of groups}
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
if (bs.magic<>$ef53) then exit;
if (bs.rev_level>1) then exit;
{if (bs.feature_ro_compat<>0) then exit;}
BlockSiz:=2 shl bs.log_block_size;
FragSiz:=2 shl bs.log_frag_size;
inodBeg:=11;
if (bs.rev_level>0) then inodBeg:=bs.first_ino;
BlockPG:=bs.blocks_per_group;
FragsPG:=bs.frags_per_group;
InodsPG:=bs.inodes_per_group;
FirstDat:=bs.first_data_block;
BlockNM:=bs.blocks_count;
InodeNM:=bs.inodes_count;
InodeSZ:=bs.inode_size;
if (InodeSZ<sizeof(OneInodeDataRecord)) then InodeSZ:=sizeof(OneInodeDataRecord);
GroupNM:=(BlockNM-FirstDat+BlockPG-1) div BlockPG;
LastInodeTested:=0;
LastBlockTested:=0;
WriteLn('sectors/block: '+BStr(BlockSiz));
WriteLn('sectors/fragment: '+BStr(FragSiz));
WriteLn('first data block: '+BStr(FirstDat));
WriteLn('first usable inode: '+BStr(inodBeg));
WriteLn('blocks/group: '+BStr(BlockPG));
WriteLn('fragments/group: '+BStr(FragsPG));
WriteLn('inodes/group: '+BStr(InodsPG));
WriteLn('size of inode: '+BStr(InodeSZ));
WriteLn('total blocks: '+BStr(BlockNM));
WriteLn('total inodes: '+BStr(InodeNM));
WriteLn('total groups: '+BStr(GroupNM));
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
