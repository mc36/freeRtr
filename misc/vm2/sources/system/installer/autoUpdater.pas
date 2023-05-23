{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc memory.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc datetime.inc}
{$sysinc crypto.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc hex.inc}

Const
  myDirectoryName='autoUpdater.temporary';
  installBaseName='instCD.';
Var
  workingPath:String;
  sourceUrlName:String;
  logFileName:String;
  releaseFile:String;
  processList:String;
  oldImageHash:String;
  newImageHash:String;
  neededDiskSpace:LongInt;
  keyFileName:String;


Procedure immErr(a:String);
Begin;
if (a='') then a:='error!';
WriteLn(a);
Halt(1);
End;


Function xLevesz(a:String):String;
Var i:byte;
Begin;
i:=pos(';',a);
if (i<>0) then a:=copy(a,1,i-1);
Kicserel(#0,' ',a);
Kicserel(#255,' ',a);
Kicserel(#9,' ',a);
a:=' '+a+' ';
Kicserel('  ',' ',a);
a:=copy(a,2,length(a)-2);
xLevesz:=a;
End;

Function repairPath(a:String):String;
Begin;
if (copy(a,length(a),1)<>'\') then a:=a+'\';
repairPath:=a;
End;

Function binary2hexString(a:String):String;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  b:String;
  i:LongInt;
Begin;
b:='';
for i:=1 to ab0 do b:=b+byte2hexType(ab[i]);
binary2hexString:=b;
End;

Function hexString2binary(a:String):String;
Var b:String;
Begin;
b:='';
while (length(a)>=2) do begin;
  b:=b+chr(BVal('$'+copy(a,1,2)));
  a:=copy(a,3,255);
  end;
hexString2binary:=b;
End;


Function GetDateTime(sep:Boolean):String;
Var
  a:String;
  w1,w2,w3:Word;

procedure num(i:longint);
var b:String;
begin;
b:=bstr(i);
while (length(b)<2) do b:='0'+b;
a:=a+b;
end;

procedure sig(i:longint);
begin;
a:=a+chr(i);
end;

Begin;
a:='';
xGetDate(w1,w2,w3);
num(w1);
if sep then sig($2d);
num(w2);
if sep then sig($2d);
num(w3);
if sep then sig($20);
xGetTime(w1,w2,w3);
num(w1);
if sep then sig($3a);
num(w2);
if sep then sig($3a);
num(w3);
GetDateTime:=a;
End;


Procedure AppendLog(b:String);
Var t:xtText;
Begin;
WriteLn(b);
xCreate(logFileName);
if (xtOpen(t,logFileName,false)<>0) then exit;
xtWrite(t,GetDateTime(true)+' - ');
xtWriteLn(t,b);
xtClose(t);
End;


Function generateHashOnFile(var a:String):LongInt;
Var
  b:String;
  ab:array[0..1] of byte absolute a;
  bb:array[0..1] of byte absolute b;
  buf:array[1..1024] of byte;
  md5,sha1:CryptoContextRecord;
  i,p,s:LongInt;
  f:xFile;
Begin;
generateHashOnFile:=-1;
WriteLn('generating checksum of '+a+'...');
CryptoGetHasherList(md5,s);
i:=CryptoFindOneAlgo(md5,s,'sha1');
if (i<1) then exit;
CryptoBegHasher(sha1,i,'','');
i:=CryptoFindOneAlgo(md5,s,'md5');
if (i<1) then exit;
CryptoBegHasher(md5,i,'','');
if (xOpen(f,a,xGenFilMod_r)<>0) then exit;
s:=xFileSize(f);
p:=0;
Write(#13'            /'+BStr(s)+#13);
while (p<s) do begin;
  i:=s-p;
  if (i>sizeof(buf)) then i:=sizeof(buf);
  if (xBlockRead(f,buf,i)<>0) then exit;
  CryptoAddHasher(md5,buf,i);
  CryptoAddHasher(sha1,buf,i);
  inc(p,i);
  write(BStr(p)+#13);
  end;
WriteLn('');
xClose(f);
CryptoFinHasher(md5,ab[1],i);
ab[0]:=i;
CryptoFinHasher(sha1,bb[1],i);
bb[0]:=i;
a:=binary2hexString(b)+'-'+binary2hexString(a);
generateHashOnFile:=s;
End;


Function doExecute(a,b:String):Boolean;
Var w1,w2:Word;
Begin;
doExecute:=True;
w1:=xExec(a,b,w2);
if (w1<>0) then begin;
  WriteLn('error executing code!');
  exit;
  end;
if (w2<>0) then begin;
  WriteLn('executed successfully, return code='+BStr(w2)+'.');
  exit;
  end;
doExecute:=False;
End;







Procedure readUpDetailsFile(var fn,chk,rel,sig:String;var siz:LongInt);
Label f1;
Var
  t:xtText;
  a,b:String;
  i,o:LongInt;
Begin;
a:=fn;
fn:='';
chk:='';
rel:='';
sig:='';
siz:=-1;
if (xtOpen(t,a,true)<>0) then exit;
f1:
if xtEOF(t) then begin;
  xtClose(t);
  exit;
  end;
a:=xtReadLn(t,255);
i:=pos(' ',a);
b:=xLevesz(copy(a,i,255));
a:=kicsi(xLevesz(copy(a,1,i)));
if (a='+*+filename') then fn:=b;
if (a='+*+filesize') then siz:=BVal(b);
if (a='+*+checksum') then chk:=b;
if (a='+*+released') then rel:=b;
if (a='+*+signature') then sig:=b;
goto f1;
End;



Procedure generateDetailsFile(a:String);
Var
  b,c,d,e,g:String;
  i,o,p:LongInt;
  t:xtText;
  f:xFile;
Begin;
b:=a;
p:=generateHashOnFile(b);
if (p<0) then immErr('error!');
g:=xFileName(a,1)+xFileName(a,2)+'.html';
a:=xFileName(a,2)+xFileName(a,3);
c:=GetDateTime(false);
WriteLn('creating '+g+'...');
if (xCreate(g)<>0) then immErr('error creating file!');
if (xtOpen(t,g,false)<>0) then immErr('error opening file!');
xtWriteLn(t,BStr(p));
xtWriteLn(t,b);
xtWriteLn(t,c);
xtClose(t);
e:=xFileName(a,1)+xFileName(a,2)+myDirectoryName;
if doExecute('c:\utils\digsig\filesigner_dss.code',keyFileName+' '+g+' '+e) then immErr('error signing file!');
if (xOpen(f,e,xGenFilMod_r)<>0) then immErr('error opening signature file!');
i:=xFileSize(f);
if (i>127) then immErr('signature too big!');
d[0]:=chr(i);
xBlockRead(f,d[1],i);
xClose(f);
xErase(e);
d:=binary2hexString(d);
if (xtOpen(t,g,false)<>0) then immErr('error opening file!');
xtSetPos(t,0);
xtWriteLn(t,'<html>');
xtWriteLn(t,'<head>');
xtWriteLn(t,'<title>details of '+a+'</title>');
xtWriteLn(t,'</head>');
xtWriteLn(t,'<body>');
xtWriteLn(t,'');
xtWriteLn(t,copy(GetDateTime(true),1,10));
xtWriteLn(t,'');
xtWriteLn(t,'<!-- machine readable part follows...');
xtWriteLn(t,'+*+fileName '+a);
xtWriteLn(t,'+*+fileSize '+BStr(p));
xtWriteLn(t,'+*+checkSum '+b);
xtWriteLn(t,'+*+released '+c);
xtWriteLn(t,'+*+signature '+d);
xtWriteLn(t,'machine readable part ends here... -->');
xtWriteLn(t,'');
xtWriteLn(t,'</body>');
xtWriteLn(t,'</html>');
xtClose(t);
End;


Function checkDetailsFile(a:String):Boolean;
Var
  b,c,d,e:String;
  i,o:LongInt;
  t:xtText;
  f:xFile;
Begin;
checkDetailsFile:=True;
d:=a;
readUpDetailsFile(a,b,c,e,o);
a:=xFileName(d,1)+a;
i:=generateHashOnFile(a);
if (i<>o) then begin; WriteLn('filesize mismatch!');exit; end;
if (a<>b) then begin; WriteLn('checksum mismatch!');exit; end;
e:=hexString2binary(e);
a:=xFileName(d,1)+xFileName(d,2)+myDirectoryName+'1';
d:=xFileName(d,1)+xFileName(d,2)+myDirectoryName+'2';
xCreate(a);
if (xtOpen(t,a,false)<>0) then begin; writeln('error opening file!');exit; end;
xtWriteLn(t,BStr(o));
xtWriteLn(t,b);
xtWriteLn(t,c);
xtClose(t);
xCreate(d);
if (xOpen(f,d,xGenFilMod_rw)<>0) then begin; writeln('error opening file!');exit; end;
xBlockWrite(f,e[1],length(e));
xClose(f);
if doExecute('c:\utils\digsig\fileverify_dss.code',keyFileName+' '+a+' '+d) then begin; writeln('signature error!');exit; end;
xErase(d);
xErase(a);
WriteLn('file ok, released at '+c+'.');
checkDetailsFile:=False;
End;


Procedure readUpConfig(a:String);
Var t:xtText;
Begin;
WriteLn('reading config...');
if (xtOpen(t,a,true)<>0) then immErr('error opening file!');
sourceUrlName:=xLevesz(xtReadLn(t,255));
logFileName:=xLevesz(xtReadLn(t,255));
releaseFile:=xLevesz(xtReadLn(t,255));
processList:=xLevesz(xtReadLn(t,255));
workingPath:=repairPath(xLevesz(xtReadLn(t,255)));
xtClose(t);
End;


Procedure doChgDir(a:String);
Begin;
if (xChDir(repairPath(a))=0) then exit;
AppendLog('error creating temporary directory!');
immErr('');
End;


Function doDownload(a:String;retry:Boolean):Boolean;
Var
  t:xtText;
  b,c:String;
Begin;
doDownload:=True;
if (xtOpen(t,'c:\system\localhost.text',true)=0) then begin;
  c:=xtReadLn(t,255);
  xtClose(t);
  end else c:='???';
b:=workingPath+myDirectoryName+'\'+installBaseName+'urlfile';
xErase(b);
xCreate(b);
if (xtOpen(t,b,false)<>0) then exit;
xtSetPos(t,0);
xtWriteLn(t,'browser [update@'+c+']');
if retry then xtWriteLn(t,'automode 1');
xtWriteLn(t,'get '+sourceUrlName+a);
xtWriteLn(t,'quit');
xtTruncate(t);
xtClose(t);
a:=workingPath+myDirectoryName+'\'+a;
xErase(a);
if doExecute('c:\internet\client\http.code',b) then exit;
if (xtOpen(t,a,true)<>0) then exit;
xtClose(t);
xErase(b);
doDownload:=False;
End;


Function doNeedUpdate:Boolean;
Label f1;
Var
  a,b,c,d:String;
  t:xtText;
Begin;
doNeedUpdate:=True;
a:=workingPath+myDirectoryName+'\'+installBaseName+'html';
readUpDetailsFile(a,b,c,d,neededDiskSpace);
if (a='') then begin;
  f1:
  AppendLog('incomplete details file!');
  exit;
  end;
if (b='') then goto f1;
if (c='') then goto f1;
if (d='') then goto f1;
if (neededDiskSpace<1) then goto f1;
newImageHash:=c+'-'+b+'-'+BStr(neededDiskSpace);
xCreate(releaseFile);
if (xtOpen(t,releaseFile,true)<>0) then begin;
  AppendLog('failed to open version tracking file!');
  exit;
  end;
oldImageHash:=xtReadLn(t,255);
xtClose(t);
if (oldImageHash='') then oldImageHash:='12345678901234';
if (oldImageHash=newImageHash) then begin;
  WriteLn('i have the newest release!');
  exit;
  end;
if (oldImageHash>newImageHash) then begin;
  AppendLog('i have a newer release!');
  exit;
  end;
doNeedUpdate:=False;
End;


Function doCheckDiskSpace:Boolean;
Var
  i,o,p:LongInt;
Begin;
doCheckDiskSpace:=True;
if (xDiskInfo(o,i,i,p)<>0) then begin;
  AppendLog('unabled to check disk space!');
  exit;
  end;
if (o<neededDiskSpace div p) then begin;
  AppendLog('not enough disk space!');
  exit;
  end;
doCheckDiskSpace:=False;
End;


Function updateTrackFile(a:String):Boolean;
Var t:xtText;
Begin;
updateTrackFile:=True;
xCreate(releaseFile);
if (xtOpen(t,releaseFile,false)<>0) then exit;
xtSetPos(t,0);
xtWriteLn(t,a);
xtTruncate(t);
xtClose(t);
updateTrackFile:=False;
End;


Procedure doKillProcesses;
Label f1;
Var
  a,b:String;
  i,o:longInt;
Begin;
b:=repairPath(processList);
f1:
if (b='') then exit;
i:=pos('\',b);
a:=copy(b,1,i-1);
b:=copy(b,i+1,255);
i:=BugOS_findProcNam(a);
if (i=0) then begin;
  AppendLog('process '+a+' not found!');
  goto f1;
  end;
BugOS_KillProcess(i);
goto f1;
End;





Label clear;
Var
  i,o:LongInt;
  a,b:String;
BEGIN;
WriteLn('automatic updater v1.0, done by Mc at '#%date' '#%time'.');
if CryptoStartActions then immErr('failed to find crypto process!');
a:=paramStr(1);
b:=paramStr(2);
keyFileName:=paramStr(3);
if (keyFileName='') then begin;
  keyFileName:=paramStr(0);
  keyFileName:=xFileName(keyFileName,1)+xFileName(keyFileName,2)+'.key';
  end;
if (a='generate') then begin;
  generateDetailsFile(b);
  exit;
  end;
if (a='check') then begin;
  if checkDetailsFile(b) then immErr('corrupted file!');
  exit;
  end;
if (a<>'update') then begin;
  WriteLn('using: <command> <file> [key]');
  WriteLn('command: generate, check, update');
  exit;
  end;

a:=paramStr(0);
a:=xFileName(a,2)+xFileName(a,3);
BugOS_MyProcessInfo(o,i,i);
if (o<>BugOS_findProcNam(a)) then immErr('one instance already running!');
readUpConfig(b);
a:=workingPath+myDirectoryName;
xMkDir(a);
doChgDir(a);
doExecute('c:\utils\shell.code','rec-del '+a+'\*');
doChgDir(a);
if doDownload(installBaseName+'html',false) then begin;
  AppendLog('failed to download details file!');
  goto clear;
  end;
if doNeedUpdate then goto clear;
if doCheckDiskSpace then goto clear;
AppendLog('update needed, downloading image...');
if doCheckDiskSpace then goto clear;
doDownload('autoUpdater',false);
if doDownload(installBaseName+'iso',true) then begin;
  AppendLog('failed to download image file!');
  goto clear;
  end;
a:=workingPath+myDirectoryName+'\'+installBaseName+'html';
if checkDetailsFile(a) then begin;
  AppendLog('corrupted image file!');
  goto clear;
  end;
if doCheckDiskSpace then goto clear;
a:=workingPath+myDirectoryName+'\';
b:=a+installBaseName+'iso';
if doExecute('c:\system\otherfs\isoextractor.code',b+' '+a) then begin;
  AppendLog('unabled to extract image!');
  goto clear;
  end;
if doCheckDiskSpace then goto clear;
xErase(b);
a:=workingPath+myDirectoryName+'\install\';
if doExecute('c:\utils\packer.code','x '+a+'instcd.pck '+a) then begin;
  AppendLog('unabled to extract image!');
  goto clear;
  end;
if updateTrackFile(newImageHash) then goto clear;
AppendLog('starting update...');
doKillProcesses;
b:=workingPath+myDirectoryName+'\packages\ autoupdate';
doExecute(a+'systeminstaller.code',b);
for i:=1 to 128 do relequish;

AppendLog('failed to update, trying to restart!');
updateTrackFile(oldImageHash);
doExecute('c:\utils\shell.code','script c:\utils\scripts\restartcomputer.script');
AppendLog('everything failed, giving up!');
immErr('');

clear:
a:=workingPath+myDirectoryName;
doChgDir(workingPath);
doExecute('c:\utils\shell.code','rec-del '+a+'\*');
xRmDir(a);
END.