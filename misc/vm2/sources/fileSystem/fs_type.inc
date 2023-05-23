{$define noCase}         {filenames are NOT case sensitive}

Const FilesysName='BugOS FileSystem v2.0';

Const                                               {basic sizes}
  BytesPerNumber=4;                                   {bytes per number}
  BitsPerOneByte=8;                                   {bits per byte}
  BytesPerSector=512;                                 {bytes per sector}
  BitsPerSector=BytesPerSector*BitsPerOneByte;        {bits per sector}
  NumbersPerSector=BytesPerSector div BytesPerNumber; {numbers per sector}
  UIDofAdmin=0;                                       {uid of administrator}
Type                                                     {basic types}
  numberType=longword;                                     {the number format}
  OneSectorRecord=array[1..BytesPerSector] of byte;        {one sector format}
  OneInodesList=array[1..NumbersPerSector] of numberType;  {inode list type}
  OneDateTimeRecord=record  {date&time format}
    year:Word;                {year}
    month:Byte;               {month}
    day:Byte;                 {day}
    hour:Byte;                {hour}
    minute:Byte;              {minute}
    second:Byte;              {second}
    end;
Var                       {open disk data}
  DriveSize:numberType;     {size of disk}
  DriveBegin:numberType;    {beginning of disk}
  DriveLetter:Char;         {drive letter}
  DriveBitmap:numberType;   {position of usage bitmap}
  DriveRoot:numberType;     {inode of root directory}
  DriveLastMap:numberType;  {last bitmap tested}
  DriveReadOnly:Boolean;    {only read access allowed}
  DriveFlags:numberType;    {flags before opened}
Var                       {current user data}
  CurrentUser:numberType;   {uid of user}
  CurrentPath:String;       {path of user}
Const                             {identification numbers}
  IdentificationFilesys=$32305342;  {filesystem id value}
  IdentificationInodes=$646f6e69;   {inode id value}
  IdentificationEntry=$7972746e;    {dir entry id value}
Const                             {fs limits}
  MaxFilePosition=$7fffffff;        {maximum file position}
  MaxReadSize=$fff0;                {maximum read size}
  BootsectorBegin=16;               {where data is in bootsec}
  InodeLimit0=32;                   {number of direct entries}
  InodeLimit3=16;                   {number of triple entries}
  EntryNameLen=247;                 {maximum filename length}
  FullPathnameLen=254;              {maximum pathname length}
Const                             {flags}
  Flags_Dirty=$00000001;            {must check filesystem}
  Flags_HasBoot=$00000002;          {has boot image}
Const                             {rights}
  Rights_OwnRead=$01;               {read by owner}
  Rights_OwnWrite=$02;              {write by owner}
  Rights_OwnExec=$04;               {execute by owner}
  Rights_AnyRead=$08;               {read by any}
  Rights_AnyWrite=$10;              {write by any}
  Rights_AnyExec=$20;               {execute by any}
  Rights_HasRootPriv=$40;           {root privileges}
  Rights_Directory=$80;             {directory}
Type                              {extended types}
  OneBootSectorRecord=record        {boot sector format}
    Identify:numberType;              {identification value}
    DiskBegin:numberType;             {beginning of drive}
    DiskSize:numberType;              {size of drive}
    BitmapBegin:numberType;           {position of usage bitmap}
    RootDirInode:numberType;          {inode of root directory}
    BootFileSize:numberType;          {size of boot file}
    LastMap:numberType;               {last tested map}
    Flags:numberType;                 {flags}
    DiskAdd:numberType;               {beginning of disk}
    end;
  OneInodeRecord=record                          {one inode record}
    size:numberType;                               {size of the file}
    rights:numberType;                             {file rights}
    owner:numberType;                              {owner of file}
    create:OneDateTimeRecord;                      {date of file created}
    modify:OneDateTimeRecord;                      {date of file mdified}
    Identify:numberType;                           {identification value}
    level0:array[1..InodeLimit0] of numberType;    {direct inodes}
    level3:array[1..InodeLimit3] of numberType;    {triple inodes}
    linkCount:numberType;                          {link count on file}
    InodeNumber:numberType;                        {inode number}
    CurrPos:numberType;                            {current position}
    end;
  OneDirEntryRecord=record            {directory record}
    Identify:numberType;                {identification value}
    inode:numberType;                   {inode number}
    name:string[EntryNameLen];          {name of the file}
    end;




{---------------------------------------------------------------------------}
{---------------------------------------------------------------------------}
{---------------------------------------------------------------------------}

Procedure ReadCurrentDate(var d:OneDateTimeRecord);
Var w1,w2,w3:Word;
Begin;
xGetDate(w1,w2,w3);
d.year:=w1;
d.month:=w2;
d.day:=w3;
xGetTime(w1,w2,w3);
d.hour:=w1;
d.minute:=w2;
d.second:=w3;
End;

Function FillUpDirEntry(var d:OneDirEntryRecord;pat,nam:String;inod:numberType):Word;
Var i:Word;
Begin;
if (length(nam)>EntryNameLen) then begin; FillUpDirEntry:=17;exit; end;
if (length(pat)+length(nam)>FullPathnameLen) then begin; FillUpDirEntry:=17;exit; end;
if (nam='') or (nam='..') or (nam='.') then begin; FillUpDirEntry:=17;exit; end;
if (pos(':',nam) or pos('\',nam) or pos(#0,nam) or pos(#255,nam)<>0) then begin; FillUpDirEntry:=17;exit; end;
d.Identify:=IdentificationEntry;
d.inode:=inod;
d.name:=nam;
for i:=length(nam)+1 to EntryNameLen do d.name[i]:=#0;
FillUpDirEntry:=0;
End;
