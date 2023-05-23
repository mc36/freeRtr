Var
  bufDat:array[1..4*1024] of byte;
  bufPos:LongInt;
  bufSiz:LongInt;
  filHdr:xFile;
  filSiz:LongInt;
  filNam:String;

Procedure fillBuffer(p:LongInt);
Begin;
dec(p,sizeof(bufDat) shr 2);
if (p<0) then p:=0;
bufPos:=p;
bufSiz:=filSiz-bufPos;
if (bufSiz>sizeof(bufDat)) then bufSiz:=sizeof(bufDat);
if (bufSiz<0) then bufSiz:=0;
if (xSeek(filHdr,bufPos)<>0) then immErr('error seeking in file!');
if (xBlockRead(filHdr,bufDat,bufSiz)<>0) then immErr('error reading file!');
End;

Function readCharacter(p:LongInt):Byte;
Begin;
if (p<0) or (p>=filSiz) then begin;
  readCharacter:=0;
  exit;
  end;
if (p>=bufPos) and (p<bufPos+bufSiz) then begin;
  readCharacter:=bufDat[p-bufPos+1];
  exit;
  end;
fillBuffer(p);
readCharacter:=bufDat[p-bufPos+1];
End;
