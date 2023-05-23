Function GetFileSize:LongInt;
Begin;
GetFileSize:=binarySize;
End;

Procedure SeekFile(i:LongInt);
Begin;
binaryPosi:=i;
End;

Procedure TruncFile;
Begin;
binarySize:=binaryPosi;
End;

Procedure writeFile(ch:Byte;vl:Boolean);
Begin;
inc(binaryPosi);
if (binaryPosi>binaryAllc) then begin;
  binaryAllc:=binaryPosi+4096;
  allocateMem;
  end;
TheExtMemBufBinDat^[binaryBegn+binaryPosi]:=ch;
if (binaryPosi>binarySize) then binarySize:=binaryPosi;
if not vl then exit;
if (binarySize>binaryVald) then binaryVald:=binarySize;
End;

Procedure FlushFile;
Const max=1024;
Var i,o,p:LongInt;
Begin;
xSeek(fileHandler,0);
p:=0;
while (p<binarySize) do begin;
  Write(#13+'('+copy(BStr(p)+'       ',1,6));
  o:=binarySize-p;
  if (o>max) then o:=max;
  xBlockWrite(fileHandler,TheExtMemBufBinDat^[binaryBegn+p+1],o);
  inc(p,o);
  end;
xTruncate(fileHandler);
End;
