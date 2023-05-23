Function InodeWriteBack(var inod:OneInodeRecord;updateTime:Boolean):Word;
Var
  sec:OneSectorRecord;
  w:Word;
Begin;
if updateTime then ReadCurrentDate(inod.modify);
fillchar(sec,BytesPerSector,0);
move(inod,sec,sizeof(inod));
w:=CacheWrite(inod.InodeNumber,sec);
InodeWriteBack:=w;
End;





Function InodeReadUp(var inod:OneInodeRecord;num:numberType):Word;
Var
  sec:OneSectorRecord;
  w:Word;
Begin;
w:=CacheRead(num,sec);
if (w<>0) then begin; InodeReadUp:=w;exit; end;
move(sec,inod,sizeof(inod));
if (inod.Identify<>IdentificationInodes) then begin; InodeReadUp:=9;exit; end;
if (inod.InodeNumber<>num) then begin; InodeReadUp:=9;exit; end;
inod.CurrPos:=0;
InodeReadUp:=0;
End;





Function InodeReRead(var inod:OneInodeRecord):Word;
Var
  i,o,p:numberType;
  w:word;
Begin;
i:=inod.InodeNumber;
o:=inod.CurrPos;
p:=inod.owner;
w:=InodeReadUp(inod,i);
if (w<>0) then begin; InodeReRead:=w;exit end;
if (p<>inod.owner) then begin; InodeReRead:=9;exit end;
inod.CurrPos:=o;
InodeReRead:=0;
End;





Function InodeGetSize(var inod:OneInodeRecord;var size:numberType):Word;
Var
  sec:OneSectorRecord;
  ils:OneInodesList absolute sec;

Function doit(blk:numberType;n:byte):Word;
Label f1;
Var
  i,o:numberType;
  w:Word;
Begin;
w:=CacheRead(blk,sec);
if (w<>0) then begin; doit:=w;exit; end;
for i:=1 to NumbersPerSector do if (ils[i]=0) then begin;
  dec(i);
  goto f1;
  end;
i:=NumbersPerSector;
f1:
if (i<1) then begin; doit:=0;exit; end;
case n of
  1:inc(size,i);
  2:inc(size,(i-1)*NumbersPerSector);
  3:inc(size,(i-1)*NumbersPerSector*NumbersPerSector);
  else begin; doit:=9;exit; end;
  end;
if (n>1) then w:=doit(ils[i],n-1) else w:=0;
doit:=w;
End;

Label f1;
Var
  i,o:numberType;
  w:Word;
Begin;
for o:=1 to InodeLimit0 do if (inod.level0[o]=0) then begin;
  InodeGetSize:=0;
  size:=o-1;
  exit;
  end;
size:=InodeLimit0;
for o:=1 to InodeLimit3 do if (inod.level3[o]=0) then begin;
  dec(o);
  goto f1;
  end;
o:=InodeLimit3;
f1:
if (o<1) then begin; InodeGetSize:=0;exit; end;
inc(size,(o-1)*NumbersPerSector*NumbersPerSector*NumbersPerSector);
w:=doit(inod.level3[o],3);
InodeGetSize:=w;
End;





Function InodeGetPosition(var inod:OneInodeRecord;num:numberType;var pos:numberType):Word;
Var
  sec:OneSectorRecord;
  ils:OneInodesList absolute sec;

Function doit(blk:numberType;n:byte):Word;
Label f1;
Var
  i,o:numberType;
  w:Word;
Begin;
w:=CacheRead(blk,sec);
if (w<>0) then begin; doit:=w;exit; end;
case n of
  1:begin;
    i:=num;
    end;
  2:begin;
    i:=num div NumbersPerSector;
    num:=num mod NumbersPerSector;
    end;
  3:begin;
    i:=num div (NumbersPerSector*NumbersPerSector);
    num:=num mod (NumbersPerSector*NumbersPerSector);
    end;
  else begin; doit:=13;exit; end;
  end;
inc(i);
if (i<1) or (i>NumbersPerSector) then begin; doit:=13;exit; end;
o:=ils[i];
if (o=0) then begin; doit:=13;exit; end;
if (n<=1) then begin; pos:=o;doit:=0;exit; end;
w:=doit(o,n-1);
doit:=w;
End;

Var
  i,o:numberType;
  w:Word;
Begin;
if (num<1) then begin; InodeGetPosition:=13;exit; end;
if (num<=InodeLimit0) then begin;
  pos:=inod.level0[num];
  InodeGetPosition:=0;
  exit;
  end;
dec(num,InodeLimit0+1);
i:=(num div (NumbersPerSector*NumbersPerSector*NumbersPerSector))+1;
num:=num mod (NumbersPerSector*NumbersPerSector*NumbersPerSector);
if (i<1) or (i>InodeLimit3) then begin; InodeGetPosition:=13;exit; end;
o:=inod.level3[i];
if (o=0) then begin; InodeGetPosition:=13;exit; end;
pos:=0;
w:=doit(o,3);
InodeGetPosition:=w;
End;






Function InodeAppendOne(var inod:OneInodeRecord;var new:numberType):Word;
Var
  sec:OneSectorRecord;
  ils:OneInodesList absolute sec;

Function doit(blk:numberType;n:byte):Word;
Label f1;
Var
  i,o:numberType;
  w:Word;
Begin;
w:=CacheRead(blk,sec);
if (w<>0) then begin; doit:=w;exit; end;
for i:=1 to NumbersPerSector do if (ils[i]=0) then goto f1;
i:=NumbersPerSector+1;
f1:
if (n<=1) then begin;
  if (i>NumbersPerSector) then begin;
    doit:=3;
    exit;
    end;
  ils[i]:=new;
  w:=CacheWrite(blk,sec);
  doit:=w;
  exit;
  end;
if (i<=1) then w:=3 else w:=doit(ils[i-1],n-1);
if (w=3) and (i<=NumbersPerSector) then begin;
  w:=BitmapFindEmpty(o);
  if (w<>0) then begin; doit:=w;exit; end;
  w:=BitmapWriteValue(o,true);
  if (w<>0) then begin; doit:=w;exit; end;
  fillchar(sec,BytesPerSector,0);
  w:=CacheWrite(o,sec);
  if (w<>0) then begin; doit:=w;exit; end;
  w:=CacheRead(blk,sec);
  if (w<>0) then begin; doit:=w;exit; end;
  ils[i]:=o;
  w:=CacheWrite(blk,sec);
  if (w<>0) then begin; doit:=w;exit; end;
  w:=doit(o,n-1);
  end;
doit:=w;
End;

Label f1;
Var
  i,o:numberType;
  w:Word;
Begin;
if (new=0) then begin;
  w:=BitmapFindEmpty(new);
  if (w<>0) then begin; InodeAppendOne:=w;exit; end;
  w:=BitmapWriteValue(new,true);
  if (w<>0) then begin; InodeAppendOne:=w;exit; end;
  end;
for i:=1 to InodeLimit0 do if (inod.level0[i]=0) then begin;
  inod.level0[i]:=new;
  InodeAppendOne:=0;
  exit;
  end;
for i:=1 to InodeLimit3 do if (inod.level3[i]=0) then begin;
  dec(i);
  goto f1;
  end;
i:=InodeLimit3;
f1:
if (i<1) then w:=3 else w:=doit(inod.level3[i],3);
if (w=3) and (i<InodeLimit3) then begin;
  w:=BitmapFindEmpty(o);
  if (w<>0) then begin; InodeAppendOne:=w;exit; end;
  w:=BitmapWriteValue(o,true);
  if (w<>0) then begin; InodeAppendOne:=w;exit; end;
  fillchar(sec,BytesPerSector,0);
  w:=CacheWrite(o,sec);
  if (w<>0) then begin; InodeAppendOne:=w;exit; end;
  inod.level3[i+1]:=o;
  w:=doit(o,3);
  end;
InodeAppendOne:=w;
End;





Function InodeDeleteLast(var inod:OneInodeRecord):Word;
Var
  sec:OneSectorRecord;
  ils:OneInodesList absolute sec;

Function doit(blk:numberType;n:byte;var backLink:Boolean):Word;
Label f1,vege;
Var
  i,o:numberType;
  bl:Boolean;
  w:Word;
Begin;
backLink:=false;
w:=CacheRead(blk,sec);
if (w<>0) then begin; doit:=w;exit; end;
for i:=1 to NumbersPerSector do if (ils[i]=0) then begin;
  dec(i);
  goto f1;
  end;
i:=NumbersPerSector;
f1:
if (i<1) then begin; doit:=3;exit; end;
if (n<=1) then begin;
  w:=BitmapWriteValue(ils[i],false);
  if (w<>0) then begin; doit:=w;exit; end;
  ils[i]:=0;
  w:=CacheWrite(blk,sec);
  if (w<>0) then begin; doit:=w;exit; end;
  bl:=true;
  goto vege;
  end;
w:=doit(ils[i],n-1,bl);
if (w<>0) then begin; doit:=w;exit; end;
if not bl then goto vege;
w:=CacheRead(blk,sec);
if (w<>0) then begin; doit:=w;exit; end;
o:=ils[i];
ils[i]:=0;
w:=CacheWrite(blk,sec);
if (w<>0) then begin; doit:=w;exit; end;
w:=BitmapWriteValue(o,false);
if (w<>0) then begin; doit:=w;exit; end;
vege:
if bl and (i<=1) then backLink:=true;
doit:=0;
End;

Label f1,f2,f3;
Var
  bl:Boolean;
  i,o:numberType;
  w:Word;
Begin;
for o:=1 to InodeLimit3 do if (inod.level3[o]=0) then begin;
  dec(o);
  goto f1;
  end;
o:=InodeLimit3;
f1:
if (o<1) then goto f2;
w:=doit(inod.level3[o],3,bl);
if (w<>0) then begin; InodeDeleteLast:=w;exit; end;
if bl then begin;
  w:=BitmapWriteValue(inod.level3[o],false);
  if (w<>0) then begin; InodeDeleteLast:=w;exit; end;
  inod.level3[o]:=0;
  end;
InodeDeleteLast:=0;
exit;
f2:
for o:=1 to InodeLimit0 do if (inod.level0[o]=0) then begin;
  dec(o);
  goto f3;
  end;
o:=InodeLimit0;
f3:
if (o<1) then begin; InodeDeleteLast:=3;exit; end;
w:=BitmapWriteValue(inod.level0[o],false);
if (w<>0) then begin; InodeDeleteLast:=w;exit; end;
inod.level0[o]:=0;
InodeDeleteLast:=0;
End;





Function InodeSetNewSize(var inod:OneInodeRecord;siz:numberType):Word;
Var
  i,o,p:numberType;
  w:Word;
Begin;
w:=InodeGetSize(inod,o);
if (w<>0) then begin; InodeSetNewSize:=w;exit; end;
for i:=siz+1 to o do begin;
  w:=InodeDeleteLast(inod);
  if (w<>0) then begin; InodeSetNewSize:=w;exit; end;
  end;
for i:=o+1 to siz do begin;
  p:=0;
  w:=InodeAppendOne(inod,p);
  if (w<>0) then begin; InodeSetNewSize:=w;exit; end;
  end;
w:=InodeGetSize(inod,o);
if (w<>0) then begin; InodeSetNewSize:=w;exit; end;
if (o<>siz) then begin; InodeSetNewSize:=10;exit; end;
InodeSetNewSize:=0;
End;





Function InodeCreateNew(var inod:OneInodeRecord;dir:boolean):Word;
Var
  i,o:numberType;
  w:Word;
Begin;
w:=BitmapFindEmpty(i);
if (w<>0) then begin; InodeCreateNew:=w;exit; end;
w:=BitmapWriteValue(i,true);
if (w<>0) then begin; InodeCreateNew:=w;exit; end;
inod.InodeNumber:=i;
inod.size:=0;
inod.rights:=Rights_OwnRead or Rights_OwnWrite;
if dir then inod.rights:=inod.rights or Rights_Directory;
inod.owner:=CurrentUser;
ReadCurrentDate(inod.create);
inod.modify:=inod.create;
inod.Identify:=IdentificationInodes;
for w:=1 to InodeLimit0 do inod.level0[w]:=0;
for w:=1 to InodeLimit3 do inod.level3[w]:=0;
inod.linkCount:=1;
inod.CurrPos:=0;
w:=InodeWriteBack(inod,false);
InodeCreateNew:=w;
End;





Function InodeEraseOne(var inod:OneInodeRecord):Word;
Var w:Word;
Begin;
w:=InodeSetNewSize(inod,0);
if (w<>0) then begin; InodeEraseOne:=w;exit; end;
w:=BitmapWriteValue(inod.InodeNumber,false);
if (w<>0) then begin; InodeEraseOne:=w;exit; end;
inod.Identify:=not IdentificationInodes;
inod.size:=0;
w:=InodeWriteBack(inod,false);
InodeEraseOne:=w;
End;
