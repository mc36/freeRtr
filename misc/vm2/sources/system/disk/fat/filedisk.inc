{$heap 127k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc crt.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$sysinc datetime.inc}
Var
  DriveSize:LongInt;
  DriveBegin:LongInt;
  DriveReadOnly:Boolean;
{$include \sources\filesystem\disk2.inc}

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

{$include fat1.inc}
{$include fat2.inc}
{$include fat3.inc}
{$include fat4.inc}
{$include fat5.inc}


{$include \sources\filesystem\filedisk1.inc}


Function ListUpSectorsOfGivenFile(b:String):Boolean;
Var
  sr:OneSearchedRecord;
  fl:OneStreamRecord;
  i,o,p:LongInt;
  a:String;
Begin;
ListUpSectorsOfGivenFile:=True;
currentPath:='\';
if ReadUpBootSector then immErr('this is not a '+proggyName+' drive!');
DetectLongFileNameUsage;
if UseLFN then a:='enabled' else a:='disabled';
WriteLn('lfn usage '+a+'.');
if SelectFileName(b,sr) then exit;
if chainOpen(fl,sr.cls) then exit;
for i:=0 to fl.clstMax-1 do begin;
  if chainSeek(fl,i) then exit;
  o:=GetClusterBegin(fl.clstCur,0);
  if (o<1) then immErr('error processing chain!');
  appendSomeSectorToList(o,ClusterSiz);
  end;
TruncateTheSectorList(sr.siz div 512);
ListUpSectorsOfGivenFile:=False;
End;

{$include \sources\filesystem\filedisk2.inc}
