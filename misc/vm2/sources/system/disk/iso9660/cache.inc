Type
  OneSectorData=array[1..2048] of byte;
  OneSectorCacheRecord=record
    n:LongWord;
    d:OneSectorData;
    end;
  OneSearchRecord=record
    p:LongInt;
    e:LongInt;
    beg:LongInt;
    siz:LongInt;
    nam:String;
    dat:xDirEntryDateTimeRec;
    flg:Byte;                   {1-file, 2-dir}
    end;
  OneDirectoryRecord=record
    pat:String;
    beg:LongInt;
    siz:LongInt;
    end;
  OneFileRecord=record
    beg:LongInt;
    pos:LongInt;
    siz:LongInt;
    end;
Var
  CacheDat:^Array[1..1] of OneSectorCacheRecord;
  CacheNum:defInt;
  CachePos:defInt;
Var
  DriveLetter:Char;             {drive letter to use}
  volumeBeg:LongInt;            {first sector of volume}
  volumeSiz:LongInt;            {sectors in volume}
  volumeSeq:LongInt;            {volume sequence number}
  volumeRot:OneDirectoryRecord; {root directory}





Function InitSectorCache:Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
InitSectorCache:=True;
CachePos:=0;
i:=CacheNum*sizeof(OneSectorCacheRecord);
if (ExtendedMemoryResize(p,i)<i) then exit;
CacheDat:=p^;
FillChar(CacheDat^,i,$ff);
InitSectorCache:=False;
End;

Function CachedReadOne(s:LongInt;var d:OneSectorData):Boolean;
Label f1;
Var i:defWord;
Begin;
for i:=1 to CacheNum do if (CacheDat^[i].n=s) then begin;
  d:=CacheDat^[i].d;
  CachedReadOne:=false;
  Exit;
  end;
i:=0;
f1:
if CDdriverReadSector(s,d) then begin;
  WriteLn('error reading sector '+BStr(s)+'...');
  inc(i);
  if (i<5) then goto f1;
  CachedReadOne:=true;
  exit;
  end;
inc(CachePos);
if (CachePos>CacheNum) then CachePos:=1;
CacheDat^[CachePos].d:=d;
CacheDat^[CachePos].n:=s;
CachedReadOne:=false;
End;


Function CDreadBytes(pos:LongInt;var buf;len:defWord):Boolean;
Var
  sec:OneSectorData;
  bufD:array[1..1] of char absolute buf;
  BufP:defWord;
  PosS:LongWord;
  PosB:defWord;
  w:defWord;
Begin;
posS:=pos shr 11;
posB:=pos and $7ff;
bufP:=0;
while (bufP<len) do begin;
  if CachedReadOne(posS,sec) then begin; CDreadBytes:=True;exit; end;
  w:=sizeof(sec)-posB;
  if (len-bufP<w) then w:=len-bufP;
  move(sec[posB+1],bufD[bufP+1],w);
  inc(bufP,w);
  inc(posS);
  posB:=0;
  end;
CDreadBytes:=False;
End;







Function discErr:Boolean;
Var b:Boolean;
Begin;
discErr:=True;
if CDdriverCheckDisk(b) then exit;
discErr:=not b;
End;

Procedure FindSpecifiedDataTrack(which:LongInt);
Var
  i,o,p,q:LongInt;
  b:Byte;
Begin;
if CDdriverReadTOChdr(i,o) then ImmErr('error reading toc head!');
WriteLn('list of tracks ('+BStr(i)+'..'+BStr(o)+'):');
volumeBeg:=-1;
volumeSiz:=-1;
q:=0;
for i:=i to o do begin;
  if CDdriverReadTOCntry(i,o,p,b) then continue;
  Write('track #'+BStr(i)+': (');
  Write(alakit(o)+'..'+alakit(p)+') ');
  p:=p-o;
  Write(alakit((p)*2)+' k ');
  case b of
    1:Write('data');
    2:Write('audio');
    else Write('unknown');
    end;
  WriteLn('');
  if (b<>1) then continue;
  inc(q);
  if (which=-1) then q:=which;
  if (q<>which) then continue;
  volumeBeg:=o;
  volumeSiz:=p;
  end;
if (volumeBeg<0) or (volumeSiz<1) then immErr('failed to find data track #'+BStr(which)+'!');
End;
