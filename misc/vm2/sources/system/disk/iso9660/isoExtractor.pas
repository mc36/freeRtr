{$heap 127k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc datetime.inc}
{$sysinc param.inc}
{$sysinc memory.inc}

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Var
  diskHandler:xFile;
  targetPath:String;

Function CDdriverReadSector(sec:LongInt;var buf):Boolean;
Begin;
xSeek(diskHandler,sec shl 11);
CDdriverReadSector:=(xBlockRead(diskHandler,buf,2048)<>0);
End;

Function CDdriverCheckDisk(var ready:Boolean):Boolean;
Begin;
ready:=true;
CDdriverCheckDisk:=false;
End;

Function CDdriverReadTOChdr(var first,last:LongInt):Boolean;
Begin;
first:=1;
last:=1;
CDdriverReadTOChdr:=false;
End;

Function CDdriverReadTOCntry(trk:LongInt;Var bg,ed:LongInt;Var tp:Byte):Boolean;
Begin;
bg:=0;
ed:=xFileSize(diskHandler) shr 11;
tp:=1;
CDdriverReadTOCntry:=false;
End;

{$include cache.inc}
{$include isofs1.inc}
{$include isofs2.inc}


Var
  cd:OneDirectoryRecord;

Procedure doFile(a:String);
Var
  fl:OneFileRecord;
  buf:Array[1..2*1024] of byte;
  i,p,s:LongInt;
  f:xFile;
Begin;
Write('   % '+a);
Write(#13);
cd.pat:='\';
if (OpenOneFile(cd,fl,a)<>0) then immErr('error opening iso file!');
a:=targetPath+copy(a,4,255);
if (xCreate(a)<>0) then immErr('error creating local file!');
if (xOpen(f,a,xGenFilMod_rw)<>0) then immErr('error opening local file!');
p:=0;
s:=fl.siz;
while (p<s) do begin;
  i:=s-p;
  if (i>sizeof(buf)) then i:=sizeof(buf);
  if (ReadFromFile(fl,i,buf)<>0) then immErr('error reading iso file!');
  if (xBlockWrite(f,buf,i)<>0) then immErr('error writing local file!');
  inc(p,i);
  write(BStr(p*100 div s)+#13);
  end;
xClose(f);
WriteLn('ok! ');
End;

Procedure doDir(path:String);
Label f1;
Var sr:OneSearchRecord;
Begin;
WriteLn(path);
fillchar(cd,sizeof(cd),0);
if (ChangeDir(cd,path)<>0) then immErr('error changing iso directory!');
BeginFindNext(cd,sr);
f1:
if (FindNextDirEntry(sr)<>0) then immErr('error reading iso directory!');
if (sr.nam='') then exit;
case sr.flg of
  1:begin; {file}
    doFile(path+sr.nam);
    end;
  2:begin; {dir}
    xMkDir(targetPath+copy(path,4,255)+sr.nam);
    doDir(path+sr.nam+'\');
    end;
  else WriteLn(path+sr.nam+' has unknown type in iso!');
  end;
goto f1;
End;


BEGIN;
WriteLn('iso9660 extractor v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<1) then ImmErr('using: isoextract.code <isofile> [target]');
if (xOpen(diskHandler,paramStr(1),xGenFilMod_r)<>0) then immErr('error opening iso!');
targetPath:=paramStr(2);
if (targetPath='') then targetPath:='.';
if (copy(targetPath,length(targetPath),1)<>'\') then targetPath:=targetPath+'\';
CacheNum:=1;
if InitSectorCache then ImmErr('error allocating cache!');
DriveLetter:='!';
FindSpecifiedDataTrack(1);
FindISO9660idInTrack;
WriteLn('going to extract to '+targetPath+'...');
doDir(DriveLetter+':\');
xClose(diskHandler);
WriteLn('successful!');
END.