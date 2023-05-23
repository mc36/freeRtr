Function AppendOneBlockToInode(var id:OneInodeDataRecord):Boolean;
Label f1,vege;
Var
  dpb,dps:LongInt;
  sd:OneSectorRecord;
  sd2:array[0..1] of longint absolute sd;

Function AppendOne(blk:LongInt;rec:Byte):Byte; {0-ok, 1-error, 2-full}
Label f1,f2,full,ok;
Var
  i,o,p:LongInt;
Begin;
AppendOne:=1;
blk:=blk*BlockSiz;
dec(blk);
for o:=0 to dpb-1 do begin;
  i:=o mod dps;
  if (i=0) then begin;
    inc(blk);
    if (DriveRead(blk,sd)<>0) then exit;
    end;
  if (sd2[i]=0) then goto f1;
  p:=sd2[i];
  end;
o:=dpb;
f1:
if (rec=1) then begin;
  if (o>=dpb) then goto full;
  i:=ReserveOneBlock(false);
  if (i=0) then exit;
  sd2[o mod dps]:=i;
  if (DriveWrite(blk,sd)<>0) then exit;
  goto ok;
  end;
if (o=0) then goto f2;
i:=AppendOne(p,rec-1);
case i of
  0:goto ok;
  1:exit;
  end;
if (o>=dpb) then goto full;
f2:
if (DriveRead(blk,sd)<>0) then exit;
p:=ReserveOneBlock(true);
if (p=0) then exit;
sd2[o mod dps]:=p;
if (DriveWrite(blk,sd)<>0) then exit;
i:=AppendOne(p,rec-1);
if (i=0) then goto ok;
exit;

full:
AppendOne:=2;
exit;

ok:
AppendOne:=0;
exit;
End;

Var
  i,o:LongInt;
Begin;
AppendOneBlockToInode:=True;
dpb:=dps*BlockSiz;
dps:=sizeof(OneSectorRecord) shr 2;
f1:
o:=0;
for i:=1 to 10 do if (id.zone[i]<>0) then o:=i;
if (o<7) then begin;
  i:=ReserveOneBlock(false);
  if (i=0) then exit;
  id.zone[o+1]:=i;
  goto vege;
  end;
if (o=7) then begin;
  i:=ReserveOneBlock(true);
  if (i=0) then exit;
  id.zone[o+1]:=i;
  goto f1;
  end;
if (o<10) then begin;
  i:=AppendOne(id.zone[o],o-7);
  case i of
    0:goto vege;
    1:exit;
    end;
  if (o=9) then exit;
  i:=ReserveOneBlock(true);
  if (i=0) then exit;
  id.zone[o+1]:=i;
  goto f1;
  end;

exit;
vege:
AppendOneBlockToInode:=False;
End;


Function KillLastBlockFromInode(var id:OneInodeDataRecord):Boolean;
Label f1,vege;
Var
  dpb,dps:LongInt;
  sd:OneSectorRecord;
  sd2:array[0..1] of longint absolute sd;

Function DeleteOne(blk:LongInt;rec:Byte):Byte; {0-ok, 1-error, 2-empty}
Label f1,f2,empty,ok;
Var
  oldblk,i,o:LongInt;
Begin;
DeleteOne:=1;
oldblk:=blk;
f2:
blk:=oldblk*BlockSiz;
inc(blk,BlockSiz);
for o:=dpb-1 downto 0 do begin;
  i:=o mod dps;
  if (i=dps-1) then begin;
    dec(blk);
    if (DriveRead(blk,sd)<>0) then exit;
    end;
  if (sd2[i]<>0) then goto f1;
  end;
goto empty;
f1:
if (rec=1) then begin;
  if WriteBlockBitmap(sd2[o mod dps],false) then exit;
  sd2[o mod dps]:=0;
  if (DriveWrite(blk,sd)<>0) then exit;
  goto ok;
  end;
i:=DeleteOne(sd2[o mod dps],rec-1);
case i of
  0:goto ok;
  1:exit;
  end;
if (DriveRead(blk,sd)<>0) then exit;
if WriteBlockBitmap(sd2[o mod dps],false) then exit;
sd2[o mod dps]:=0;
if (DriveWrite(blk,sd)<>0) then exit;
goto f2;

empty:
DeleteOne:=2;
exit;

ok:
DeleteOne:=0;
exit;
End;


Var
  i,o:LongInt;
Begin;
KillLastBlockFromInode:=True;
dpb:=dps*BlockSiz;
dps:=sizeof(OneSectorRecord) shr 2;
f1:
o:=0;
for i:=1 to 10 do if (id.zone[i]<>0) then o:=i;
if (o=0) then exit;
if (o<8) then begin;
  if WriteBlockBitmap(id.zone[o],false) then exit;
  id.zone[o]:=0;
  goto vege;
  end;
if (o<10) then begin;
  i:=DeleteOne(id.zone[o],o-7);
  case i of
    0:goto vege;
    1:exit;
    end;
  if WriteBlockBitmap(id.zone[o],false) then exit;
  id.zone[o]:=0;
  goto f1;
  end;

exit;
vege:
KillLastBlockFromInode:=False;
End;
