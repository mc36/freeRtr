Const BufferSize=32*1024;
Var
  WasError:Boolean;
  overWriter:Boolean;

Function xLevesz(a:String):String;
Begin;
Kicserel(#9,' ',a);
Kicserel(#255,' ',a);
Kicserel(#0,' ',a);
while (Copy(a,1,1)=' ') do Delete(a,1,1);
xLevesz:=Levesz(a);
End;

Function GetNextWord(var a:string):String;
Var i:Word;
Begin;
i:=pos(' ',a);
if (i<1) then i:=$666;
GetNextWord:=copy(a,1,i-1);
delete(a,1,i);
End;

Function RepairPath(a:String):String;
Begin;
if (a='') then a:='.';
if (copy(a,length(a),1)<>'\') then a:=a+'\';
RepairPath:=a;
End;

Function IsThisPath(a:String):Boolean;
Begin;
IsThisPath:=(copy(a,length(a),1)='\')
End;



Function CopyOneFile(src,trg:String):String;
Label vege;
Var
  buf:array[1..BufferSize] of char;
  f1,f2:xFile;
  p,s,b:LongInt;
  q,w:Word;
Begin;
CopyOneFile:='unknown error';
WriteLn(src+' --> '+trg);
q:=xOpen(f1,src,xGenFilMod_r);
if (q<>0) then begin;
  CopyOneFile:='error opening source: '+xGetErrorName(q);
  goto vege;
  end;
if overWriter then xErase(trg);
q:=xCreate(trg);
if (q<>0) then begin;
  CopyOneFile:='error creating target: '+xGetErrorName(q);
  goto vege;
  end;
q:=xOpen(f2,trg,xGenFilMod_rw);
if (q<>0) then begin;
  xErase(trg);
  CopyOneFile:='error opening target: '+xGetErrorName(q);
  goto vege;
  end;
s:=xFileSize(f1);
p:=0;
Write('0           /'+BStr(s)+#13);
while (p<s) do begin;
  b:=s-p;
  if (b>sizeof(buf)) then b:=sizeof(buf);
  q:=xBlockRead(f1,buf,b);
  if (q<>0) then begin;
    xClose(f1);
    xClose(f2);
    CopyOneFile:='error reading source: '+xGetErrorName(q);
    goto vege;
    end;
  q:=xBlockWrite(f2,buf,b);
  if (q<>0) then begin;
    xClose(f1);
    xClose(f2);
    CopyOneFile:='error writing target: '+xGetErrorName(q);
    goto vege;
    end;
  inc(p,b);
  Write(BStr(p)+#13);
  end;
xClose(f1);
q:=xClose(f2);
if (q<>0) then begin;
  CopyOneFile:='error closing target: '+xGetErrorName(q);
  goto vege;
  end;
CopyOneFile:='';
vege:
Write('                         '#13);
End;

Procedure CopySomeFiles(src,trg:String);
Label f1,vege;
Var
  msk:String;
  sr:xFile;
  ntry:xDirEntryRec;
  w:Word;
  a:String;
  cnt:LongInt;
Begin;
WasError:=True;
cnt:=0;
if not FileMaskingIsThis(src) then begin;
  a:=CopyOneFile(src,trg);
  inc(cnt);
  goto vege;
  end;
trg:=RepairPath(trg);
msk:=xFileName(src,2)+xFileName(src,3);
src:=RepairPath(xFileName(src,1));
w:=xDirOpen(sr,src);
if (w<>0) then begin;
  a:='error opening directory: '+xGetErrorName(w);
  goto vege;
  end;
f1:
w:=xDirRead(sr,ntry);
if (w<>0) then begin;
  xDirClose(sr);
  a:='error reading directory: '+xGetErrorName(w);
  goto vege;
  end;
a:=ntry.name;
if (a='') then begin;
  xDirClose(sr);
  a:='';
  goto vege;
  end;
if (ntry.rights and xRights_Directory<>0) then goto f1;
if not FileMaskingTestOne(a,msk) then goto f1;
inc(cnt);
a:=CopyOneFile(src+a,trg+a);
if (a='') then goto f1;
xDirClose(sr);
vege:
if (a<>'') then begin;
  WriteLn(a);
  Exit;
  end;
WriteLn(BStr(cnt)+' file(s) copied.');
WasError:=False;
End;

Function LinkOneFile(src,trg:String):String;
Var i:LongInt;
Begin;
WriteLn(src+' ==> '+trg);
if overWriter then xErase(trg);
i:=xMkLink(src,trg);
if (i<>0) then LinkOneFile:='error creating link: '+xGetErrorName(i) else LinkOneFile:='';
End;

Procedure LinkSomeFiles(src,trg:String;justFiles:Boolean);
Label f1,vege;
Var
  msk:String;
  sr:xFile;
  ntry:xDirEntryRec;
  w:Word;
  a:String;
  cnt:LongInt;
Begin;
WasError:=True;
cnt:=0;
if not FileMaskingIsThis(src) then begin;
  a:=LinkOneFile(src,trg);
  inc(cnt);
  goto vege;
  end;
trg:=RepairPath(trg);
msk:=xFileName(src,2)+xFileName(src,3);
src:=RepairPath(xFileName(src,1));
w:=xDirOpen(sr,src);
if (w<>0) then begin;
  a:='error opening directory: '+xGetErrorName(w);
  goto vege;
  end;
f1:
w:=xDirRead(sr,ntry);
if (w<>0) then begin;
  xDirClose(sr);
  a:='error reading directory: '+xGetErrorName(w);
  goto vege;
  end;
a:=ntry.name;
if (a='') then begin;
  xDirClose(sr);
  a:='';
  goto vege;
  end;
if justFiles then if (ntry.rights and xRights_Directory<>0) then goto f1;
if not FileMaskingTestOne(a,msk) then goto f1;
inc(cnt);
a:=LinkOneFile(src+a,trg+a);
if (a='') then goto f1;
xDirClose(sr);
vege:
if (a<>'') then begin;
  WriteLn(a);
  Exit;
  end;
WriteLn(BStr(cnt)+' link(s) created.');
WasError:=False;
End;

Procedure EraseSomeFiles(fn:String);
Label f0,f1,vege;
Var
  msk:String;
  sr:xFile;
  ntry:xDirEntryRec;
  w:Word;
  a:String;
  cnt:LongInt;
Begin;
WasError:=True;
cnt:=0;
if not FileMaskingIsThis(fn) then begin;
  WriteLn(fn);
  w:=xErase(fn);
  if (w=0) then a:='' else a:='error erasing file: '+xGetErrorName(w);
  inc(cnt);
  goto vege;
  end;
msk:=xFileName(fn,2)+xFileName(fn,3);
fn:=RepairPath(xFileName(fn,1));
f0:
w:=xDirOpen(sr,fn);
if (w<>0) then begin;
  a:='error opening directory: '+xGetErrorName(w);
  goto vege;
  end;
f1:
w:=xDirRead(sr,ntry);
if (w<>0) then begin;
  xDirClose(sr);
  a:='error reading directory: '+xGetErrorName(w);
  goto vege;
  end;
a:=ntry.name;
if (a='') then begin;
  xDirClose(sr);
  a:='';
  goto vege;
  end;
if (ntry.rights and xRights_Directory<>0) then goto f1;
if not FileMaskingTestOne(a,msk) then goto f1;
inc(cnt);
WriteLn(fn+a);
w:=xErase(fn+a);
if (w<>0) then begin;
  xDirClose(sr);
  a:='error erasing file: '+xGetErrorName(w);
  goto vege;
  end;
xDirClose(sr);
goto f0;
vege:
if (a<>'') then begin;
  WriteLn(a);
  Exit;
  end;
WriteLn(BStr(cnt)+' file(s) erased.');
WasError:=False;
End;


Procedure ChownSomeFiles(fn:String;own:LongInt);
Label f1,vege;
Var
  msk:String;
  sr:xFile;
  ntry:xDirEntryRec;
  w:Word;
  a:String;
  cnt:LongInt;
Begin;
WasError:=True;
cnt:=0;
msk:=xFileName(fn,2)+xFileName(fn,3);
fn:=RepairPath(xFileName(fn,1));
w:=xDirOpen(sr,fn);
if (w<>0) then begin;
  a:='error opening directory: '+xGetErrorName(w);
  goto vege;
  end;
f1:
w:=xDirRead(sr,ntry);
if (w<>0) then begin;
  xDirClose(sr);
  a:='error reading directory: '+xGetErrorName(w);
  goto vege;
  end;
a:=ntry.name;
if (a='') then begin;
  xDirClose(sr);
  a:='';
  goto vege;
  end;
if (ntry.rights and xRights_Directory<>0) then goto f1;
if not FileMaskingTestOne(a,msk) then goto f1;
inc(cnt);
WriteLn(fn+a);
w:=xSetRight(fn+a,own,ntry.rights);
if (w<>0) then begin;
  xDirClose(sr);
  a:='error changing file: '+xGetErrorName(w);
  goto vege;
  end;
goto f1;
vege:
if (a<>'') then begin;
  WriteLn(a);
  Exit;
  end;
WriteLn(BStr(cnt)+' file(s) changed.');
WasError:=False;
End;


Procedure ChmodSomeFiles(fn:String;mode:LongInt);
Label f1,vege;
Var
  msk:String;
  sr:xFile;
  ntry:xDirEntryRec;
  w:Word;
  a:String;
  cnt:LongInt;
Begin;
WasError:=True;
cnt:=0;
msk:=xFileName(fn,2)+xFileName(fn,3);
fn:=RepairPath(xFileName(fn,1));
w:=xDirOpen(sr,fn);
if (w<>0) then begin;
  a:='error opening directory: '+xGetErrorName(w);
  goto vege;
  end;
f1:
w:=xDirRead(sr,ntry);
if (w<>0) then begin;
  xDirClose(sr);
  a:='error reading directory: '+xGetErrorName(w);
  goto vege;
  end;
a:=ntry.name;
if (a='') then begin;
  xDirClose(sr);
  a:='';
  goto vege;
  end;
if (ntry.rights and xRights_Directory<>0) then goto f1;
if not FileMaskingTestOne(a,msk) then goto f1;
inc(cnt);
WriteLn(fn+a);
w:=xSetRight(fn+a,ntry.owner,mode);
if (w<>0) then begin;
  xDirClose(sr);
  a:='error changing file: '+xGetErrorName(w);
  goto vege;
  end;
goto f1;
vege:
if (a<>'') then begin;
  WriteLn(a);
  Exit;
  end;
WriteLn(BStr(cnt)+' file(s) changed.');
WasError:=False;
End;


Procedure RenameSomeFiles(src,trg:String);
Label f0,f1,vege;
Var
  msk:String;
  sr:xFile;
  ntry:xDirEntryRec;
  w:Word;
  a:String;
  cnt:LongInt;
Begin;
WasError:=True;
cnt:=0;
if not FileMaskingIsThis(src) then begin;
  if overWriter then xErase(trg);
  w:=xRename(src,trg);
  if (w=0) then a:='' else a:='error renaming file: '+xGetErrorName(w);
  inc(cnt);
  goto vege;
  end;
trg:=RepairPath(trg);
msk:=xFileName(src,2)+xFileName(src,3);
src:=RepairPath(xFileName(src,1));
f0:
w:=xDirOpen(sr,src);
if (w<>0) then begin;
  a:='error opening directory: '+xGetErrorName(w);
  goto vege;
  end;
f1:
w:=xDirRead(sr,ntry);
if (w<>0) then begin;
  xDirClose(sr);
  a:='error reading directory: '+xGetErrorName(w);
  goto vege;
  end;
a:=ntry.name;
if (a='') then begin;
  xDirClose(sr);
  a:='';
  goto vege;
  end;
if (ntry.rights and xRights_Directory<>0) then goto f1;
if not FileMaskingTestOne(a,msk) then goto f1;
inc(cnt);
WriteLn(src+a+' -> '+trg+a);
if overWriter then xErase(trg+a);
w:=xRename(src+a,trg+a);
if (w<>0) then begin;
  xDirClose(sr);
  a:='error renaming file: '+xGetErrorName(w);
  goto vege;
  end;
xDirClose(sr);
goto f0;
vege:
if (a<>'') then begin;
  WriteLn(a);
  Exit;
  end;
WriteLn(BStr(cnt)+' file(s) renamed.');
WasError:=False;
End;


Procedure DirectoryList(a:String);
Label f1,vege;
Var
  cntf,cntd,cntb:LongInt;
  sr:xFile;
  ntry:xDirEntryRec;
  w:Word;
  i:LongInt;

function x(i:word):string;var a:string;begin; a:=bstr(i);while (length(a)<2) do a:='0'+a;x:=a; end;
function y(m:longint;c:char):string;begin; if (i and m=m) then y:=c else y:='-'; end;

Begin;
WasError:=True;
if (a='') then a:='.\';
a:=RepairPath(a);
cntf:=0;
cntd:=0;
cntb:=0;
w:=xDirOpen(sr,a);
if (w<>0) then begin;
  a:='error opening directory: '+xGetErrorName(w);
  goto vege;
  end;
WriteLn('directory listing of '+a);
f1:
w:=xDirRead(sr,ntry);
if (w<>0) then begin;
  xDirClose(sr);
  a:='error reading directory: '+xGetErrorName(w);
  goto vege;
  end;
if (ntry.name='') then begin;
  xDirClose(sr);
  a:='';
  goto vege;
  end;
i:=ntry.rights;
a:=y(xRights_HasRootPriv,'U')+y(xRights_Directory,'d')+
   y(xRights_OwnRead,'r')+y(xRights_OwnWrite,'w')+y(xRights_OwnExec,'x')+
   y(xRights_AnyRead,'R')+y(xRights_AnyWrite,'W')+y(xRights_AnyExec,'X')+
   ' ';
Write(a);
Write(copy(bstr(ntry.owner)+'            ',1,12));
a:=x(ntry.modified.year)+'-'+x(ntry.modified.month)+'-'+x(ntry.modified.day)
   +' '+
   x(ntry.modified.hour)+':'+x(ntry.modified.minute)+':'+x(ntry.modified.second)
   +' ';
Write(a);
Write(copy(bstr(ntry.size)+'            ',1,12));
WriteLn(ntry.name);
if (ntry.rights and xRights_Directory<>0) then inc(cntd) else inc(cntf);
inc(cntb,ntry.size);
goto f1;
vege:
if (a<>'') then begin;
  WriteLn(a);
  Exit;
  end;
WriteLn(BStr(cntd)+' directories, '+BStr(cntf)+' files, '+BStr(cntb)+' bytes listed.');
WasError:=False;
End;



Procedure RecursiveRename(src,trg:String);
Label f0,f1,vege;
Var
  msk:String;
  sr:xFile;
  ntry:xDirEntryRec;
  w:Word;
  a:String;
Begin;
RenameSomeFiles(src,trg);
if WasError then exit;
trg:=RepairPath(trg);
msk:=xFileName(src,2)+xFileName(src,3);
src:=RepairPath(xFileName(src,1));
f0:
w:=xDirOpen(sr,src);
if (w<>0) then begin;
  a:='error opening directory: '+xGetErrorName(w);
  goto vege;
  end;
f1:
w:=xDirRead(sr,ntry);
if (w<>0) then begin;
  xDirClose(sr);
  a:='error reading directory: '+xGetErrorName(w);
  goto vege;
  end;
a:=ntry.name;
if (a='') then begin;
  xDirClose(sr);
  a:='';
  goto vege;
  end;
if (ntry.rights and xRights_Directory=0) then goto f1;
WriteLn('ren '+src+a+'\'+msk+' '+trg+a);
w:=xMkDir(trg+a);
if (w<>0) then begin;
  xDirClose(sr);
  a:='error creating directory: '+xGetErrorName(w);
  goto vege;
  end;
RecursiveRename(src+a+'\'+msk,trg+a);
if WasError then begin;
  xDirClose(sr);
  exit;
  end;
w:=xRmDir(src+a);
if (w<>0) then begin;
  WriteLn('error erasing directory: '+xGetErrorName(w));
  goto f1;
  end;
xDirClose(sr);
goto f0;
vege:
if (a<>'') then begin;
  WriteLn(a);
  Exit;
  end;
WasError:=False;
End;


Procedure RecursiveCopy(src,trg:String);
Label f1,vege;
Var
  msk:String;
  sr:xFile;
  ntry:xDirEntryRec;
  w:Word;
  a:String;
Begin;
CopySomeFiles(src,trg);
if WasError then exit;
trg:=RepairPath(trg);
msk:=xFileName(src,2)+xFileName(src,3);
src:=RepairPath(xFileName(src,1));
w:=xDirOpen(sr,src);
if (w<>0) then begin;
  a:='error opening directory: '+xGetErrorName(w);
  goto vege;
  end;
f1:
w:=xDirRead(sr,ntry);
if (w<>0) then begin;
  xDirClose(sr);
  a:='error reading directory: '+xGetErrorName(w);
  goto vege;
  end;
a:=ntry.name;
if (a='') then begin;
  xDirClose(sr);
  a:='';
  goto vege;
  end;
if (ntry.rights and xRights_Directory=0) then goto f1;
WriteLn('copy '+src+a+'\'+msk+' '+trg+a);
w:=xMkDir(trg+a);
if (w<>0) then begin;
  xDirClose(sr);
  a:='error creating directory: '+xGetErrorName(w);
  goto vege;
  end;
RecursiveCopy(src+a+'\'+msk,trg+a);
if not WasError then goto f1;
xDirClose(sr);
exit;
vege:
if (a<>'') then begin;
  WriteLn(a);
  Exit;
  end;
WasError:=False;
End;


Procedure RecursiveErase(src:String);
Label f0,f1,vege;
Var
  msk:String;
  sr:xFile;
  ntry:xDirEntryRec;
  w:Word;
  a:String;
Begin;
EraseSomeFiles(src);
if WasError then exit;
msk:=xFileName(src,2)+xFileName(src,3);
src:=RepairPath(xFileName(src,1));
f0:
w:=xDirOpen(sr,src);
if (w<>0) then begin;
  a:='error opening directory: '+xGetErrorName(w);
  goto vege;
  end;
f1:
w:=xDirRead(sr,ntry);
if (w<>0) then begin;
  xDirClose(sr);
  a:='error reading directory: '+xGetErrorName(w);
  goto vege;
  end;
a:=ntry.name;
if (a='') then begin;
  xDirClose(sr);
  a:='';
  goto vege;
  end;
if (ntry.rights and xRights_Directory=0) then goto f1;
WriteLn('del '+src+a+'\'+msk);
xDirClose(sr);
RecursiveErase(src+a+'\'+msk);
if WasError then exit;
w:=xRmDir(src+a);
if (w=0) then goto f0;
a:='error erasing directory: '+xGetErrorName(w);
vege:
if (a<>'') then begin;
  WriteLn(a);
  Exit;
  end;
WasError:=False;
End;


Procedure RecursiveChown(src:String;own:LongInt);
Label f1,vege;
Var
  msk:String;
  sr:xFile;
  ntry:xDirEntryRec;
  w:Word;
  a:String;
Begin;
ChownSomeFiles(src,own);
if WasError then exit;
msk:=xFileName(src,2)+xFileName(src,3);
src:=RepairPath(xFileName(src,1));
w:=xDirOpen(sr,src);
if (w<>0) then begin;
  a:='error opening directory: '+xGetErrorName(w);
  goto vege;
  end;
f1:
w:=xDirRead(sr,ntry);
if (w<>0) then begin;
  xDirClose(sr);
  a:='error reading directory: '+xGetErrorName(w);
  goto vege;
  end;
a:=ntry.name;
if (a='') then begin;
  xDirClose(sr);
  a:='';
  goto vege;
  end;
if (ntry.rights and xRights_Directory=0) then goto f1;
WriteLn('chown '+src+a+'\'+msk+' '+BStr(own));
w:=xSetRight(src+a,own,ntry.rights);
if (w<>0) then begin;
  xDirClose(sr);
  a:='error changing directory: '+xGetErrorName(w);
  goto vege;
  end;
RecursiveChown(src+a+'\'+msk,own);
if not WasError then goto f1;
xDirClose(sr);
exit;
vege:
if (a<>'') then begin;
  WriteLn(a);
  Exit;
  end;
WasError:=False;
End;


Procedure RecursiveChmod(src:String;mode:LongInt);
Label f1,vege;
Var
  msk:String;
  sr:xFile;
  ntry:xDirEntryRec;
  w:Word;
  a:String;
Begin;
ChmodSomeFiles(src,mode);
if WasError then exit;
msk:=xFileName(src,2)+xFileName(src,3);
src:=RepairPath(xFileName(src,1));
w:=xDirOpen(sr,src);
if (w<>0) then begin;
  a:='error opening directory: '+xGetErrorName(w);
  goto vege;
  end;
f1:
w:=xDirRead(sr,ntry);
if (w<>0) then begin;
  xDirClose(sr);
  a:='error reading directory: '+xGetErrorName(w);
  goto vege;
  end;
a:=ntry.name;
if (a='') then begin;
  xDirClose(sr);
  a:='';
  goto vege;
  end;
if (ntry.rights and xRights_Directory=0) then goto f1;
WriteLn('chmod '+src+a+'\'+msk+' '+BStr(mode));
w:=xSetRight(src+a,ntry.owner,mode);
if (w<>0) then begin;
  xDirClose(sr);
  a:='error changing directory: '+xGetErrorName(w);
  goto vege;
  end;
RecursiveChmod(src+a+'\'+msk,mode);
if not WasError then goto f1;
xDirClose(sr);
exit;
vege:
if (a<>'') then begin;
  WriteLn(a);
  Exit;
  end;
WasError:=False;
End;






Function ProcessCommand(c:String):String;
Label f1;
Var
  a,b:String;
  w,ww:Word;
Begin;
ProcessCommand:='';
c:=xLevesz(c);
f1:
if (c='') then exit;
a:=GetNextWord(c);
a:=kicsi(a);
if (copy(a,length(a),1)=':') then exit;
if (a='help') or (a='?') then begin;
  WriteLn('help                     - this help screen');
  WriteLn('ver                      - display shell version');
  WriteLn('overwrite [yes/no]       - change overwrite mode for all operations');
  WriteLn('echo [text]              - display the text');
  WriteLn('nop [anything]           - no operation');
  WriteLn('exit [code]              - leave this shell');
  WriteLn('iferr <command>          - execute command if error happened');
  WriteLn('copy <src> <trg>         - copy files');
  WriteLn('link <src> <trg>         - link files');
  WriteLn('ren <src> <trg>          - rename/move files');
  WriteLn('chown <src> <owner>      - set owner of files');
  WriteLn('chmod <src> <mode>       - set mode of files');
  WriteLn('del <src>                - delete files');
  WriteLn('rec-copy <src> <trg>     - recursive copy files');
  WriteLn('rec-link <src> <trg>     - link files, directories');
  WriteLn('rec-ren <src> <trg>      - recursive rename/move files');
  WriteLn('rec-chown <src> <owner>  - recursive set owner of files');
  WriteLn('rec-chmod <src> <mode>   - recursive set mode of files');
  WriteLn('rec-del <src>            - recursive delete files');
  WriteLn('md <dir>                 - make directory');
  WriteLn('rd <dir>                 - erase directory');
  WriteLn('cd <dir>                 - change directory');
  WriteLn('dir [dir]                - list directory');
  WriteLn('goto <label>             - jump to a label');
  WriteLn('<label>:                 - define a label');
  WriteLn('script <file>            - run a shell script');
  WriteLn('run <file> [par]         - run a program with parameters');
  Exit;
  end;
if (a='exit') then begin;
  Halt(BVal(c));
  end;
if (a='overwrite') then begin;
  a:=kicsi(c);
  if (a='yes') or (a='y') then a:='1';
  overWriter:=(BVal(a)=1);
  if not overWriter then a:=' not' else a:='';
  WriteLn('all operations are'+a+' in overwrite mode now.');
  exit;
  end;
if (a='goto') then begin;
  ProcessCommand:=#1+c;
  exit;
  end;
if (a='script') then begin;
  ProcessCommand:=#2+c;
  exit;
  end;
if (a='run') then begin;
  a:=GetNextWord(c);
  w:=xExec(a,c,ww);
  WasError:=False;
  if (w=0) then begin;
    WriteLn('');
    WriteLn('executed successfully, exit code is: '+BStr(ww));
    Exit;
    end;
  WriteLn('error executing program: '+xGetErrorName(w));
  WasError:=True;
  exit;
  end;
if (a='iferr') then begin;
  if WasError then goto f1;
  exit;
  end;
if (a='echo') then begin;
  WriteLn(c);
  Exit;
  end;
if (a='nop') then exit;
if (a='ver') then begin;
  WriteLn(PrgTxt);
  WriteLn('');
  Exit;
  end;
if (a='copy') then begin;
  a:=GetNextWord(c);
  b:=GetNextWord(c);
  CopySomeFiles(a,b);
  Exit;
  end;
if (a='link') then begin;
  a:=GetNextWord(c);
  b:=GetNextWord(c);
  LinkSomeFiles(a,b,true);
  Exit;
  end;
if (a='del') then begin;
  a:=GetNextWord(c);
  EraseSomeFiles(a);
  Exit;
  end;
if (a='chown') then begin;
  a:=GetNextWord(c);
  b:=GetNextWord(c);
  ChownSomeFiles(a,BVal(b));
  Exit;
  end;
if (a='chmod') then begin;
  a:=GetNextWord(c);
  b:=GetNextWord(c);
  ChmodSomeFiles(a,BVal(b));
  Exit;
  end;
if (a='ren') then begin;
  a:=GetNextWord(c);
  b:=GetNextWord(c);
  RenameSomeFiles(a,b);
  Exit;
  end;
if (a='rec-copy') then begin;
  a:=GetNextWord(c);
  b:=GetNextWord(c);
  RecursiveCopy(a,b);
  Exit;
  end;
if (a='rec-link') then begin;
  a:=GetNextWord(c);
  b:=GetNextWord(c);
  LinkSomeFiles(a,b,false);
  Exit;
  end;
if (a='rec-del') then begin;
  a:=GetNextWord(c);
  RecursiveErase(a);
  Exit;
  end;
if (a='rec-chown') then begin;
  a:=GetNextWord(c);
  b:=GetNextWord(c);
  RecursiveChown(a,BVal(b));
  Exit;
  end;
if (a='rec-chmod') then begin;
  a:=GetNextWord(c);
  b:=GetNextWord(c);
  RecursiveChmod(a,BVal(b));
  Exit;
  end;
if (a='rec-ren') then begin;
  a:=GetNextWord(c);
  b:=GetNextWord(c);
  RecursiveRename(a,b);
  Exit;
  end;
if (a='md') then begin;
  WasError:=False;
  a:=GetNextWord(c);
  w:=xMkDir(a);
  if (w=0) then Exit;
  WriteLn('error creating directory: '+xGetErrorName(w));
  WasError:=True;
  Exit;
  end;
if (a='rd') then begin;
  WasError:=False;
  a:=GetNextWord(c);
  w:=xRmDir(a);
  if (w=0) then Exit;
  WriteLn('error erasing directory: '+xGetErrorName(w));
  WasError:=True;
  Exit;
  end;
if (a='cd') then begin;
  WasError:=False;
  a:=GetNextWord(c);
  w:=xChDir(a);
  if (w=0) then Exit;
  WriteLn('error changing directory: '+xGetErrorName(w));
  WasError:=True;
  Exit;
  end;
if (a='dir') then begin;
  a:=GetNextWord(c);
  DirectoryList(a);
  Exit;
  end;

WriteLn('unknown command: '+a+' '+c);
WasError:=True;
End;

Procedure RunOneScript(a:String);
Label f1;
Var
  b:String;
  t:xtText;
Begin;
if (xtOpen(t,a,true)<>0) then begin;
  WriteLn('error opening script file: '+a+'!');
  Exit;
  end;
xtSetPos(t,0);
f1:
if xtEOF(t) then begin;
  xtClose(t);
  exit;
  end;
b:=xtReadLn(t,255);
b:=ProcessCommand(b);
if (b='') then goto f1;
case b[1] of
  #1:begin;
    xtSetPos(t,0);
    delete(b,1,1);
    while not xtEOF(t) do begin;
      a:=xtReadLn(t,255);
      a:=xLevesz(a);
      if (b+':'=a) then goto f1;
      end;
    WriteLn('label "'+b+'" not fount!');
    goto f1;
    end;
  #2:begin;
    WriteLn('embedded scripts are not allowed!');
    goto f1;
    end;
  else WriteLn('invalid error!');
  end;
goto f1;
End;


Function ReadLine:String;
Label f0,f1;
Var
  a:String;
  w:Word;
Begin;
f0:
a:='';
Write(#13+xGetDir+'>');
f1:
w:=ReadKey;
if (w and $fe00=0) then begin;{simple key}
  w:=w and $ff;
  if (w in [0,255,13,10,8,9]) then w:=ord(' ');
  if (length(a)>250) then goto f1;
  a:=a+chr(w);
  write(chr(w));
  goto f1;
  end;
case w of
  $8001:begin;{redraw}
    clrscr;
    goto f0;
    end;
  $8003:begin;{backspace}
    if (a='') then goto f1;
    Write(#8' '#8);
    a:=copy(a,1,length(a)-1);
    goto f1;
    end;
  $8004:begin;{enter}
    WriteLn('');
    ReadLine:=a;
    exit;
    end;
  $8005:begin;{escape}
    WriteLn('');
    goto f0;
    end;
  end;
goto f1;
End;
