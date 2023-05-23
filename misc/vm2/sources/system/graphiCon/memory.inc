Const
  pixelMax=512;
Type
  memoryRecordType=array[1..1] of byte;
  oneCharacterRecord=array[1..16] of byte;
  oneWindowRecord=record
    scrX:LongInt;               {x position in display}
    scrY:LongInt;               {y position in display}
    sizX:LongInt;               {x size of screen}
    sizY:LongInt;               {y size of screen}
    sizW:Longint;               {screen size in words}
    mapB:LongInt;               {change map beginning in buffer}
    scrB:LongInt;               {screen beginning in buffer}
    scrP:Longint;               {beginning of display in buffer}
    curP:LongInt;               {cursor position in buffer}
    curC:LongInt;               {color of writer pen}
    pipe:LongInt;               {pipeline number}
    proc:LongInt;               {process number}
    updt:LongInt;               {update counter}
    last:LongInt;               {time of last rx}
    cmdS:LongInt;               {command size}
    cmdB:array[1..8] of byte;   {command buffer}
    buf:array[0..65535] of byte;{console buffer}
    end;
  oneColorRecord=record
    r:byte;
    g:byte;
    b:byte;
    end;
Var
  fontHeight:LongInt;
  fontData:array[0..255] of oneCharacterRecord;
  colorData:array[0..16] of oneColorRecord;
  memoryRecordData:^memoryRecordType;
  screenSizX:LongInt;
  screenSizY:LongInt;
  screenSizP:LongInt;
  windowNum:LongInt;
  windowSel:LongInt;
  pixelBuf:record
    x:LongInt;
    y:LongInt;
    d:array[1..pixelMax] of oneColorRecord;
    end;
  pixelPip:LongInt;
  pixelSiz:LongInt;
  pixelNxX:LongInt;
  pixelNxY:LongInt;
  pixelCur:LongInt;
  keybPipe:LongInt;
  keybLast:LongInt;
  nextWinPos:LongInt;
  serviceName:String;
  myProcId:LongInt;
  loggedUid:LongInt;
  monitorOn:String;
  monitorOf:String;
  loginProc:String;
  askerProc:String;
  shellProc:String;
  cmndrProc:String;
  prcmnProc:String;



Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;


Function findProcSep(a:String):LongInt;
Var i:LongInt;
Begin;
i:=pos(' ',a);
if (i<1) then i:=666;
findProcSep:=i;
End;


Function readUpFont(a:String):Boolean;
Var f:xFile;
Begin;
readUpFont:=true;
if (xOpen(f,a,xGenFilMod_r)<>0) then exit;
xBlockRead(f,fontData,sizeof(fontData));
xClose(f);
readUpFont:=false;
End;



Function readUpColor(a:String):Boolean;
Var
  f:xFile;
  i:LongInt;
Begin;
readUpColor:=true;
if (xOpen(f,a,xGenFilMod_r)<>0) then exit;
xSeek(f,0);
xBlockRead(f,colorData[0],sizeof(oneColorRecord)*8);
xSeek(f,xFileSize(f)-sizeof(oneColorRecord)*8);
xBlockRead(f,colorData[8],sizeof(oneColorRecord)*8);
xClose(f);
for i:=0 to 15 do begin;
  colorData[i].r:=colorData[i].r shl 2;
  colorData[i].g:=colorData[i].g shl 2;
  colorData[i].b:=colorData[i].b shl 2;
  end;
i:=200;
colorData[16].r:=i;
colorData[16].g:=i;
colorData[16].b:=i;
readUpColor:=false;
End;



Function MemoryResize(o:LongInt):Boolean;
Var p:Pointer;
Begin;
MemoryResize:=True;
if (ExtendedMemoryResize(p,o)<o) then exit;
memoryRecordData:=p^;
MemoryResize:=False;
End;
