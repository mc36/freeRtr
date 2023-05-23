Var
  FatCopies:LongInt;            {number of fats}
  FatSizes:LongInt;             {size of one fat in sectors}
  FatBegins:LongInt;            {beginning of fat}
  ClusterBeg:LongInt;           {sector number of clusters}
  ClusterMax:LongInt;           {number of clusters on disk}
  ClusterSiz:LongInt;           {size of one cluster in sectors}
  ClusterLst:LongInt;           {last tested cluster}
  RootMax:LongInt;              {max number of entries in root directory}
  RootBeg:LongInt;              {cluster of root directory}
  UseLFN:Boolean;               {0=no, 1=yes, 2=detect}
  currentPath:String;           {current path}
  driveLetter:Char;             {drive letter}



Function ReadUpBootSector:Boolean;
Var bs:OneBootSectorRecord;

function x(var d;s:word):string;
var a:string;i:word;
begin;
a[0]:=chr(s);
move(d,a[1],s);
i:=pos(#0,a);
if (i>0) then a:=copy(a,1,i-1);
a:=' '+a+' ';
kicserel('  ',' ',a);
x:=copy(a,2,length(a)-2);
end;

Begin;
ReadUpBootSector:=True;
if (DriveRead(0,bs)<>0) then exit;
if (kicsi(x(bs.fsId,sizeof(bs.fsId)))<>kicsi(proggyName)) then exit;
FatCopies:=bs.FatCopies;
FatSizes:=bs.SecsPerFat16;
FatBegins:=bs.ReservedSecs;
{$ifdef fat32}
if (FatSizes=0) then FatSizes:=bs.SecsPerFat32;
RootMax:=0;
RootBeg:=bs.RootDirBeg;
{$else}
RootMax:=bs.RootEntry;
RootBeg:=0;
{$endif}
ClusterSiz:=bs.SecsPerClust;
ClusterBeg:=(((RootMax*32)+511) div 512)+(FatSizes*FatCopies)+FatBegins;
ClusterMax:=(DriveSize-ClusterBeg) div ClusterSiz;
WriteLn('oem id: '+x(bs.name,sizeof(bs.name)));
WriteLn('volume: '+x(bs.volLb,sizeof(bs.volLb)));
WriteLn('# of fats: '+BStr(FatCopies));
WriteLn('sectors/fat: '+BStr(FatSizes));
WriteLn('# of clusters: '+BStr(ClusterMax));
WriteLn('sectors/cluster: '+BStr(ClusterSiz));
WriteLn('1st fat begins: '+BStr(FatBegins));
WriteLn('1st cluster begins: '+BStr(ClusterBeg));
WriteLn('root dir limit: '+BStr(RootMax));
WriteLn('root dir cluster: '+BStr(RootBeg));
ClusterLst:=0;
ReadUpBootSector:=False;
End;



Function ReadOneFatEntry(n:LongWord):LongWord;
Var
  dat:array[0..1024] of byte;
  p,o:LongInt;
  w:Word;
Begin;
ReadOneFatEntry:=0;

{$ifdef fat12}
o:=n;
n:=(n*3) shr 1;
p:=(n shr 9)+FatBegins;
n:=n and $1ff;
DriveRead(p,dat);
if (n>510) then DriveRead(p+1,dat[512]);
w:=ReadWordLSB(dat[n]);
if (o and 1=0) then ReadOneFatEntry:=w and $fff else ReadOneFatEntry:=w shr 4;
{$endif}

{$ifdef fat16}
DriveRead((n shr 8)+FatBegins,dat);
ReadOneFatEntry:=ReadWordLSB(dat[(n and $ff) shl 1]);
{$endif}

{$ifdef fat32}
DriveRead((n shr 7)+FatBegins,dat);
ReadOneFatEntry:=ReadLongLSB(dat[(n and $7f) shl 2]);
{$endif}

End;



Procedure WriteOneFatEntry(n,d:LongWord);
Var
  dat:array[0..1023] of byte;
  o,p,q:LongInt;
  w:Word;
Begin;

{$ifdef fat12}
o:=n;
n:=(n*3) shr 1;
p:=(n shr 9)+FatBegins;
n:=n and $1ff;
DriveRead(p,dat);
if (n>510) then DriveRead(p+1,dat[512]);
w:=ReadWordLSB(dat[n]);
if (o and 1=0) then w:=(w and $f000) or (d and $fff) else w:=(w and $f) or (d shl 4);
WriteWordLSB(dat[n],w);
for q:=1 to FatCopies do begin;
  DriveWrite(p,dat);
  if (n>510) then DriveWrite(p+1,dat[512]);
  inc(p,FatSizes);
  end;
{$endif}

{$ifdef fat16}
p:=(n shr 8)+FatBegins;
DriveRead(p,dat);
WriteWordLSB(dat[(n and $ff) shl 1],d);
for q:=1 to FatCopies do begin;
  DriveWrite(p,dat);
  inc(p,FatSizes);
  end;
{$endif}

{$ifdef fat32}
p:=(n shr 7)+FatBegins;
DriveRead(p,dat);
WriteLongLSB(dat[(n and $7f) shl 2],d);
for q:=1 to FatCopies do begin;
  DriveWrite(p,dat);
  inc(p,FatSizes);
  end;
{$endif}

End;

Function GetClusterSize(c:LongInt):LongInt;
Begin;
if (c=0) then c:=(RootMax*32+511) div 512 else c:=ClusterSiz;
GetClusterSize:=c;
End;

Function GetClusterBegin(c,s:LongInt):LongInt;
Begin;
if (c=0) then c:=(FatSizes*FatCopies)+FatBegins+s else c:=((c-2)*ClusterSiz)+ClusterBeg+s;
GetClusterBegin:=c;
End;



Function findEmptyCluster:LongWord;
Var i:LongInt;
Begin;
for i:=0 to ClusterMax do begin;
  inc(ClusterLst);
  if (ClusterLst>=ClusterMax) then ClusterLst:=0;
  if (ReadOneFatEntry(ClusterLst)<>0) then continue;
  findEmptyCluster:=ClusterLst;
  exit;
  end;
findEmptyCluster:=0;
End;

Procedure CountDiskUsage(var free,used,bad,blockSize:longInt);
Var i:LongInt;
Begin;
free:=0;
used:=0;
bad:=0;
blockSize:=512*ClusterSiz;
for i:=0 to ClusterMax do case ReadOneFatEntry(i) of
  ClustEmpty:inc(free);
  ClustBadB..ClustBadE:inc(bad);
  else inc(used);
  end;
End;
