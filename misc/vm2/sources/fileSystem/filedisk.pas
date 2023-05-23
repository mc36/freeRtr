{$heap 143k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc datetime.inc}
{$sysinc bugos.inc}

{$include fs_all.inc}

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Const proggyName=FilesysName;

{$include \sources\filesystem\filedisk1.inc}



Function ListUpSectorsOfGivenFile(b:String):Boolean;
Var
  inod1:OneinodeRecord;
  i,o:LongInt;
Begin;
ListUpSectorsOfGivenFile:=True;
currentPath:='\';
CacheInit;
if (FilesysOpen(false)<>0) then immErr('error opening filesystem!');
CurrentUser:=0;
if (OpenOneFile(b,inod1,3)<>0) then immErr('error opening file!');
for i:=1 to inod1.size div BytesPerSector do begin;
  if (InodeGetPosition(inod1,i,o)<>0) then immErr('error processing chain!');
  appendSomeSectorToList(o,1);
  end;
CacheFlush;
ListUpSectorsOfGivenFile:=False;
End;



{$include \sources\filesystem\filedisk2.inc}
