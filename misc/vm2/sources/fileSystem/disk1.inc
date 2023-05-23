{$sysinc filesys.inc}
Var DriveInternalHandler:xFile;

Function DriveOpen(nam:String;num:numberType):Word;
Begin;
DriveOpen:=15;
if (xOpen(DriveInternalHandler,nam,xGenFilMod_rw)<>0) then exit;
DriveSize:=xFileSize(DriveInternalHandler) shr 9;
DriveBegin:=$19791025;
DriveOpen:=0;
End;

Function DriveClose:Word;
Begin;
xClose(DriveInternalHandler);
DriveClose:=0;
End;

Function DriveRead(sec:numberType;var buf):Word;
Begin;
xSeek(DriveInternalHandler,sec*512);
xBlockRead(DriveInternalHandler,buf,512);
DriveRead:=0;
End;

Function DriveWrite(sec:numberType;var buf):Word;
Begin;
if DriveReadOnly then begin; DriveWrite:=4;exit; end;
xSeek(DriveInternalHandler,sec*512);
xBlockWrite(DriveInternalHandler,buf,512);
DriveWrite:=0;
End;
