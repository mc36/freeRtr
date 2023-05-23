Function GetFileSize:LongInt;
Begin;
GetFileSize:=xFileSize(fileHandler);
End;

Procedure SeekFile(i:LongInt);
Begin;
xSeek(fileHandler,i);
End;

Procedure TruncFile;
Begin;
xTruncate(fileHandler);
End;

Procedure writeFile(ch:Byte;vl:Boolean);
Var i:LongInt;
Begin;
xBlockWrite(fileHandler,ch,sizeof(ch));
if not vl then exit;
i:=GetFileSize;
if (i>binaryVald) then binaryVald:=i;
End;

Procedure FlushFile;
Begin;
End;
