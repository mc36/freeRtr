Const maxCopyBufferSize=32*1024;

Function RepairPath(a:String):String;
Begin;
if (a='') then a:='.';
if (copy(a,length(a),1)<>'\') then a:=a+'\';
RepairPath:=a;
End;

Procedure doSelectAllFiles(var d:OnePanelRecord;justFiles:Boolean);
Var
  dd:OneFileRecord;
  i,o:LongInt;
Begin;
o:=d.mem-1;
for i:=0 to d.max-1 do begin;
  inc(o);
  dd:=entriesBuf^[o];
  if justFiles then if (dd.flg and xRights_Directory<>0) then continue;
  dd.sel:=not dd.sel;
  entriesBuf^[o]:=dd;
  end;
End;

Procedure doSelectFileMask(var d:OnePanelRecord;msk:String;value:Boolean);
Var
  dd:OneFileRecord;
  i,o:LongInt;
Begin;
o:=d.mem-1;
for i:=0 to d.max-1 do begin;
  inc(o);
  dd:=entriesBuf^[o];
  if not FileMaskingTestOne(dd.nam,msk) then continue;
  dd.sel:=value;
  entriesBuf^[o]:=dd;
  end;
End;




Procedure DoWorkDelete(var d:OnePanelRecord);
Var
  xp,yp,xs,ys:Word;
  a,b:String;
  sr:xFile;
  ntry:xDirEntryRec;
  w:Word;

Function erOn(nam:string;dir:boolean):boolean;
Label f1,vege;
Begin;
erOn:=true;
a:=nam;
while (length(a)<xs) do a:=a+' ';
while (length(a)>xs) do a:=copy(a,2,255);
GotoXY(xp,yp+1);Write(a);
if not dir then begin;
  w:=xErase(d.pat+nam);
  if ShowErrorMessage(w,'erase '+nam) then exit;
  goto vege;
  end;
f1:
if (xRmDir(d.pat+nam)=0) then goto vege;
a:=nam+'\';
w:=xDirOpen(sr,d.pat+a);
if ShowErrorMessage(w,'open '+a) then exit;
w:=xDirRead(sr,ntry);
xDirClose(sr);
if ShowErrorMessage(w,'read '+a) then exit;
b:=ntry.name;
if (b='') then begin;
  w:=xRmDir(d.pat+nam);
  if ShowErrorMessage(w,'erase '+a) then exit;
  goto vege;
  end;
a:=a+b;
if (ntry.rights and xRights_Directory<>0) then begin;
  if erOn(a,true) then exit;
  goto f1;
  end;
w:=xErase(d.pat+a);
if ShowErrorMessage(w,'erase '+a) then exit;
goto f1;
if erOn(a,true) then exit;
goto f1;
vege:
erOn:=false;
End;

Var i,o,p:LongInt;
Begin;
PanelCountSelected(d,i,o,p,true,true);
if (o+p<1) then begin;
  p:=d.cur;
  if (p<1) or (p>d.max) then exit;
  p:=d.mem+p-1;
  a:=entriesBuf^[p].nam;
  if (a='..') or (a='.') or (a='\') or (copy(a,2,1)=':') then exit;
  a:='the current file is: '+a;
  b:='do you really want to erase it?';
  end else begin;
  a:='there are '+bstr(o)+' files and '+bstr(p)+' directories selected.';
  b:='do you really want to erase them?';
  p:=-1;
  end;
xs:=ScrSizX-10;
ys:=2;
PutOutWindow(xp,yp,xs,ys,'erase',ColErrB,ColErrT);
GotoXY(xp,yp+0);Write(a);
GotoXY(xp,yp+1);Write(b+' (y/N)');
a:=ReadChrKey;
if (a<>'Y') then exit;
xs:=ScrSizX-10;
ys:=2;
PutOutWindow(xp,yp,xs,ys,'erase',ColWinB,ColWinT);
GotoXY(xp,yp+0);Write('erasing the file/directory:');
if (p>=0) then begin;
  erOn(entriesBuf^[p].nam,entriesBuf^[p].flg and xRights_Directory<>0);
  exit;
  end;
o:=d.mem-1;
for i:=0 to d.max-1 do begin;
  inc(o);
  if not entriesBuf^[o].sel then continue;
  if erOn(entriesBuf^[o].nam,entriesBuf^[o].flg and xRights_Directory<>0) then exit;
  entriesBuf^[o].sel:=false;
  end;
End;




Procedure DoWorkRename(var d:OnePanelRecord;b:String);
Var
  i,o,p:LongInt;
  w:Word;
  a:String;
Begin;
PanelCountSelected(d,i,o,p,true,true);
if (o+p<1) then begin;
  p:=d.cur;
  if (p<1) or (p>d.max) then exit;
  p:=d.mem+p-1;
  b:=entriesBuf^[p].nam;
  a:=b;
  if AskForOneText('rename','enter new name of '+a+':',a) then exit;
  if (a='') then exit;
  w:=xRename(d.pat+b,d.pat+a);
  if ShowErrorMessage(w,'rename '+b+'-->'+a) then exit;
  entriesBuf^[p].nam:=a;
  exit;
  end;
inc(o,p);
if AskForOneText('rename','enter new path of selected '+BStr(o)+' entries:',b) then exit;
if (b='') then exit;
b:=RepairPath(b);
o:=d.mem-1;
for i:=1 to d.max do begin;
  inc(o);
  if not entriesBuf^[o].sel then continue;
  a:=entriesBuf^[o].nam;
  w:=xRename(d.pat+a,b+a);
  if ShowErrorMessage(w,'rename '+a+' to '+b) then exit;
  entriesBuf^[o].sel:=false;
  end;
End;




Procedure DoWorkCopy(var d:OnePanelRecord;trg:String);
Var
  totFil,totDir,totByt:LongInt;
  xp,yp,xs,ys:Word;
  ntry:xDirEntryRec;
  a:String;
  w:Word;

Procedure WriteTotals;
Begin;
GotoXY(xp,yp+1);
Write('dirs: '+alakit(totDir));
Write('   files: '+alakit(totFil));
Write('   bytes: '+alakit(totByt));
End;

Function cpFl(nam:String):Boolean;
Label j1,j2,vege;
Var
  f1,f2:xFile;
  buf:array[1..maxCopyBufferSize] of byte;
  sz,ps:LongInt;
  i:LongInt;
  pb:Word;
Begin;
cpFl:=True;
a:=nam;
while (length(a)<xs) do a:=a+' ';
while (length(a)>xs) do a:=copy(a,2,255);
GotoXY(xp,yp+3);Write(a);
a:='';while (length(a)<xs) do a:=a+'°';
GotoXY(xp,yp+4);Write(a);
a:='';while (length(a)<xs) do a:=a+' ';
GotoXY(xp,yp+5);Write(a);
pb:=0;
w:=xOpen(f1,d.pat+nam,xGenFilMod_r);
if ShowErrorMessage(w,'open '+nam) then exit;
w:=xCreate(trg+nam);
if ShowErrorMessage(w,'create '+nam) then begin;
  xClose(f1);
  exit;
  end;
w:=xOpen(f2,trg+nam,xGenFilMod_rw);
if ShowErrorMessage(w,'open '+nam) then begin;
  xClose(f1);
  exit;
  end;
ps:=0;
sz:=xFileSize(f1);
GotoXY(xp,yp+5);Write('position:                /'+alakit(sz));
j1:
GotoXY(xp+pb,yp+4);
if (sz=0) then i:=0 else i:=ps*xs div sz;
if (i>xs) then i:=xs;
dec(i,pb);
if (i<0) then i:=0;
inc(pb,i);
a:='';
while (length(a)<i) do a:=a+'Û';
write(a);
a:=alakit(ps);
while (length(a)<15) do a:=' '+a;
GotoXY(xp+10,yp+5);
Write(a);
if (ps>=sz) then begin;
  xClose(f1);
  xClose(f2);
  goto vege;
  end;
i:=sz-ps;
if (i>sizeof(buf)) then i:=sizeof(buf);
w:=xBlockRead(f1,buf,i);
if ShowErrorMessage(w,'read '+nam) then goto j2;
w:=xBlockWrite(f2,buf,i);
if ShowErrorMessage(w,'write '+nam) then goto j2;
inc(ps,i);
goto j1;
j2:
xClose(f1);
xClose(f2);
exit;
vege:
inc(totFil);
inc(totByt,sz);
cpFl:=False;
End;

Function cpOn(nam:string):boolean;
Label f1,f2,vege;
Var
  owner,rights:LongInt;
  sr:xFile;
Begin;
cpOn:=True;
a:=nam;
while (length(a)<xs) do a:=a+' ';
while (length(a)>xs) do a:=copy(a,2,255);
GotoXY(xp,yp+3);Write(a);
a:='';while (length(a)<xs) do a:=a+' ';
GotoXY(xp,yp+4);Write(a);
GotoXY(xp,yp+5);Write(a);
owner:=ntry.owner;
rights:=ntry.rights;
if (rights and xRights_Directory=0) then begin;
  if cpFl(nam) then exit;
  WriteTotals;
  goto vege;
  end;
w:=xMkDir(trg+nam);
if ShowErrorMessage(w,'mkdir '+nam) then exit;
w:=xDirOpen(sr,d.pat+nam+'\');
if ShowErrorMessage(w,'open '+nam+'\') then exit;
f1:
w:=xDirRead(sr,ntry);
if ShowErrorMessage(w,'read '+nam+'\') then goto f2;
a:=ntry.name;
if (a='') then begin;
  xDirClose(sr);
  inc(totDir);
  goto vege;
  end;
if cpOn(nam+'\'+a) then goto f2;
WriteTotals;
goto f1;
f2:
xDirClose(sr);
exit;
vege:
xSetRight(trg+nam,owner,rights);
cpOn:=False;
End;

Procedure gtCr(p:LongInt);
Var dat:OneFileRecord;
Begin;
dat:=entriesBuf^[p];
fillchar(ntry,sizeof(ntry),0);
ntry.rights:=dat.flg;
ntry.owner:=dat.own;
ntry.name:=dat.nam;
End;

Var i,o,p:LongInt;
Begin;
w:=xChDir(d.pat);
if ShowErrorMessage(w,'chdir '+d.pat) then exit;
PanelCountSelected(d,i,o,p,true,true);
if (o+p<1) then begin;
  p:=d.cur;
  if (p<1) or (p>d.max) then exit;
  p:=d.mem+p-1;
  a:=entriesBuf^[p].nam;
  end else begin;
  a:=bstr(o+p)+' entries';
  p:=-1;
  end;
if AskForOneText('copy','enter path where to copy '+a+':',trg) then exit;
if (trg='') then exit;
trg:=RepairPath(trg);
totFil:=0;
totDir:=0;
totByt:=0;
xs:=ScrSizX-10;
ys:=6;
PutOutWindow(xp,yp,xs,ys,'copy',ColWinB,ColWinT);
GotoXY(xp,yp+0);Write('summary of copy:');
WriteTotals;
GotoXY(xp,yp+2);Write('copying the file/directory:');
if (p>=0) then begin;
  gtCr(p);
  cpOn(ntry.name);
  exit;
  end;
p:=d.mem-1;
for i:=1 to d.max do begin;
  inc(p);
  if not entriesBuf^[p].sel then continue;
  gtCr(p);
  if cpOn(ntry.name) then exit;
  entriesBuf^[p].sel:=false;
  end;
End;




Procedure DoWorkCompare(var d1,d2:OnePanelRecord);
Label f1,f2,f3;
Var
  optRecursive,optFileSizes,optContents:Boolean;
  totFil,totDir,totByt:LongInt;
  xp,yp,xs,ys:Word;
  a:String;
  ntry:xDirEntryRec;
  w:Word;
  q:LongInt;

Procedure WriteTotals;
Begin;
GotoXY(xp,yp+1);
Write('dirs: '+alakit(totDir));
Write('   files: '+alakit(totFil));
Write('   bytes: '+alakit(totByt));
End;

Function cmFl(nam:string):Boolean;
Label j1,j2,vege;
Var
  f1,f2:xFile;
  buf1,buf2:array[1..maxCopyBufferSize] of byte;
  sz,ps:LongInt;
  i,o:LongInt;
  pb:Word;
Begin;
cmFl:=true;
inc(totFil);
if (xOpen(f1,d1.pat+nam,xGenFilMod_r)<>0) then exit;
sz:=xFileSize(f1);
inc(totByt,sz);
if (xOpen(f2,d2.pat+nam,xGenFilMod_r)<>0) then begin;
  xClose(f1);
  exit;
  end;
if not optFileSizes then goto vege;
ps:=xFileSize(f2);
if (sz<>ps) then goto j2;
if not optContents then goto vege;
ps:=0;
a:=nam;
while (length(a)<xs) do a:=a+' ';
while (length(a)>xs) do a:=copy(a,2,255);
GotoXY(xp,yp+3);Write(a);
a:='';while (length(a)<xs) do a:=a+'°';
GotoXY(xp,yp+4);Write(a);
a:='';while (length(a)<xs) do a:=a+' ';
GotoXY(xp,yp+5);Write(a);
pb:=0;
GotoXY(xp,yp+5);Write('position:                /'+alakit(sz));
j1:
GotoXY(xp+pb,yp+4);
if (sz=0) then i:=0 else i:=ps*xs div sz;
if (i>xs) then i:=xs;
dec(i,pb);
if (i<0) then i:=0;
inc(pb,i);
a:='';
while (length(a)<i) do a:=a+'Û';
write(a);
a:=alakit(ps);
while (length(a)<15) do a:=' '+a;
GotoXY(xp+10,yp+5);
Write(a);
if (ps>=sz) then goto vege;
i:=sz-ps;
if (i>sizeof(buf1)) then i:=sizeof(buf1);
if (xBlockRead(f1,buf1,i)<>0) then goto j2;
if (xBlockRead(f2,buf2,i)<>0) then goto j2;
inc(ps,i);
for o:=1 to i do if (buf1[o]<>buf2[o]) then goto j2;
goto j1;
j2:
xClose(f1);
xClose(f2);
exit;
vege:
xClose(f1);
xClose(f2);
cmFl:=false;
End;

Function CountEntries(a:String):LongInt;
Label f1;
Var
  sr:xFile;
  n:LongInt;
Begin;
CountEntries:=-1;
if (xDirOpen(sr,a)<>0) then exit;
n:=0;
f1:
if (xDirRead(sr,ntry)<>0) then begin;
  xDirClose(sr);
  exit;
  end;
if (ntry.name='') then begin;
  xDirClose(sr);
  CountEntries:=n;
  exit;
  end;
inc(n);
goto f1;
End;

Function cmOn(nam:string;dir:boolean):Boolean;
Label f1,f2,vege;
Var sr:xFile;
Begin;
cmOn:=true;
a:=nam;
while (length(a)<xs) do a:=a+' ';
while (length(a)>xs) do a:=copy(a,2,255);
GotoXY(xp,yp+3);Write(a);
a:='';while (length(a)<xs) do a:=a+' ';
GotoXY(xp,yp+4);Write(a);
GotoXY(xp,yp+5);Write(a);
if not dir then begin;
  cmOn:=cmFl(nam);
  if optContents then WriteTotals;
  exit;
  end;
inc(totDir);
if (xChDir(d2.pat+nam+'\')<>0) then exit;
if not optRecursive then goto vege;
a:=d1.pat+nam+'\';
q:=CountEntries(a);
if (q<0) then exit;
if (q<>CountEntries(d2.pat+nam+'\')) then exit;
if (xDirOpen(sr,a)<>0) then exit;
f1:
if (xDirRead(sr,ntry)<>0) then goto f2;
a:=ntry.name;
if (a='') then begin;
  xDirClose(sr);
  goto vege;
  end;
if (ntry.rights and xRights_Directory=0) then begin;
  if cmFl(nam+'\'+a) then goto f2;
  if optContents then WriteTotals;
  goto f1;
  end;
if cmOn(nam+'\'+a,true) then goto f2;
goto f1;
f2:
xDirClose(sr);
WriteTotals;
exit;
vege:
WriteTotals;
cmOn:=false;
End;

Var i,o,p:LongInt;
Begin;
optRecursive:=false;
optFileSizes:=false;
optContents:=false;
xs:=ScrSizX-12;
ys:=5;
PutOutWindow(xp,yp,xs,ys,'compare',ColWinB,ColWinT);
GotoXY(xp,yp+0);Write('compare panels with following options:');
GotoXY(xp,yp+4);Write('press number to toggle or enter to start!');
f1:
if optRecursive then a:='[X]' else a:='[ ]';
GotoXY(xp,yp+1);Write(a+' 1-recursive into directories');
if optFileSizes then a:='[X]' else a:='[ ]';
GotoXY(xp,yp+2);Write(a+' 2-compare file sizes');
if optContents then a:='[X]' else a:='[ ]';
GotoXY(xp,yp+3);Write(a+' 3-compare file contents');
GotoXY(xp,yp+4);
f2:
w:=ReadKey;
case w of
  $8001:begin;{refresh}
    RefreshScr:=$ff;
    exit;
    end;
  $8005:begin;{escape}
    exit;
    end;
  $8004:begin;{enter}
   goto f3;
   end;
  else begin;
    if (w and $fe00<>0) then goto f2;
    dec(w,ord('0'));
    w:=w and $ff;
    case w of
      1:optRecursive:=not optRecursive;
      2:optFileSizes:=not optFileSizes;
      3:optContents:=not optContents;
      else goto f2;
      end;
    goto f1;
    end;
  end;
goto f2;
f3:
if optContents then optFileSizes:=true;
totFil:=0;
totDir:=0;
totByt:=0;
xs:=ScrSizX-10;
ys:=6;
PutOutWindow(xp,yp,xs,ys,'compare',ColWinB,ColWinT);
GotoXY(xp,yp+0);Write('summary of compare:');
WriteTotals;
GotoXY(xp,yp+2);Write('comparing the file/directory:');
if d2.rot then p:=d2.mem else p:=d2.mem+1;
for i:=p to d2.mem+d2.max-1 do entriesBuf^[i].sel:=true;
if d1.rot then p:=d1.mem else p:=d1.mem+1;
for i:=p to d1.mem+d1.max-1 do entriesBuf^[i].sel:=false;
for i:=p to d1.mem+d1.max-1 do begin;
  o:=PanelFindOneFile(d2,entriesBuf^[i].nam);
  if (o<1) then begin;
    entriesBuf^[i].sel:=true;
    continue;
    end;
  o:=d2.mem+o-1;
  if cmOn(entriesBuf^[i].nam,entriesBuf^[i].flg and xRights_Directory<>0) then
   entriesBuf^[i].sel:=true else entriesBuf^[o].sel:=false;
  end;
WriteTotals;
a:='';while (length(a)<xs) do a:=a+' ';
GotoXY(xp,yp+2);Write(a);
GotoXY(xp,yp+3);Write(a);
GotoXY(xp,yp+4);Write(a);
GotoXY(xp,yp+5);Write(a);
a:='';
if optRecursive then a:=a+', recursive';
if optFileSizes then a:=a+', sizes';
if optContents then a:=a+', contents';
a:=copy(a,3,255);
if (a='') then a:='none';
GotoXY(xp,yp+3);Write('parameters: '+a);
PanelCountSelected(d1,i,o,p,true,true);
GotoXY(xp,yp+4);Write(alakit(o+p)+' differring entries found!');
ReadChrKey;
End;



Procedure DoWorkFileInfo(a,b:String);
Label f1;
Var
  xp,yp,xs,ys:Word;
  sr:xFile;
  ntry:xDirEntryRec;
  w:Word;
  i,o,p:LongInt;

Procedure Count(a:String);
Label f1,f2;
Var sr:xFile;
Begin;
if (xDirOpen(sr,a)<>0) then exit;
f1:
if (xDirRead(sr,ntry)<>0) then goto f2;
if (ntry.name='') then goto f2;
inc(p,ntry.size);
if (ntry.rights and xRights_Directory=0) then begin;
  inc(i);
  goto f1;
  end;
inc(o);
Count(a+ntry.name+'\');
goto f1;
f2:
xDirClose(sr);
End;


Begin;
w:=xDirOpen(sr,a);
if ShowErrorMessage(w,'open '+a) then exit;
b:=kicsi(b);
f1:
w:=xDirRead(sr,ntry);
if ShowErrorMessage(w,'read '+a) then begin;
  xDirClose(sr);
  exit;
  end;
if (ntry.name='') then begin;
  xDirClose(sr);
  ShowErrorMessage(1,'find '+b);
  exit;
  end;
if (kicsi(ntry.name)<>b) then goto f1;
xDirClose(sr);
xs:=ScrSizX-8;
ys:=7;
if (ntry.rights and xRights_Directory<>0) then inc(ys,3);
PutOutWindow(xp,yp,xs,ys,'entry information',ColWinB,ColWinT);
GotoXY(xp,yp+0);Write('entry name:');
GotoXY(xp,yp+1);Write(ntry.name);
GotoXY(xp,yp+2);Write('  rights: '+ConvertFileMode2string(ntry.rights));
GotoXY(xp,yp+3);Write('   owner: 0x'+long2hex(ntry.owner));
GotoXY(xp,yp+4);Write('    size: '+alakit(ntry.size));
GotoXY(xp,yp+5);Write(' created: '+ConvertDateTime2string(ntry.created));
GotoXY(xp,yp+6);Write('modified: '+ConvertDateTime2string(ntry.modified));
if (ntry.rights and xRights_Directory<>0) then begin;
  GotoXY(xp,yp+7);Write('counting...');
  i:=0;
  o:=0;
  p:=0;
  Count(a+b+'\');
  GotoXY(xp,yp+7);Write('   files: '+alakit(i));
  GotoXY(xp,yp+8);Write(' subdirs: '+alakit(o));
  GotoXY(xp,yp+9);Write('   bytes: '+alakit(p));
  end;
GotoXY(xp,yp+1);
ReadChrKey;
End;




Procedure DoWorkDiskInfo(a:string);
Var
  xp,yp,xs,ys:Word;
  w:Word;
  i,o,p,q:LongInt;

procedure x(a:string;v:longint);
var m:longint;
begin;
m:=i+o+p;
if (m=0) then m:=1;
while (length(a)<5) do a:=a+' ';
Write(a);
a:=alakit(v);
while (length(a)<15) do a:=' '+a;
Write(a);
a:=alakit(v*q);
while (length(a)<19) do a:=' '+a;
Write(a);
a:=bstr(v*100 div m);
while (length(a)<5) do a:=' '+a;
Write(a);
end;

Begin;
if (copy(a,2,2)<>':\') then exit;
w:=xChDir(a);
if ShowErrorMessage(w,'chdir '+a) then exit;
w:=xDiskInfo(i,o,p,q);
if ShowErrorMessage(w,'diskinfo') then exit;
xs:=44;
ys:=7;
PutOutWindow(xp,yp,xs,ys,'disk information',ColWinB,ColWinT);
GotoXY(xp,yp+0);Write('drive letter: '+copy(a,1,3));
GotoXY(xp,yp+1);Write('  block size: '+alakit(q)+' bytes');
GotoXY(xp,yp+2);Write('              blocks              bytes    %');
GotoXY(xp,yp+3);x('total',i+o+p);
GotoXY(xp,yp+4);x('used',o);
GotoXY(xp,yp+5);x('bad',p);
GotoXY(xp,yp+6);x('free',i);
GotoXY(xp+14,yp+0);
ReadChrKey;
End;




Procedure DoWorkExecPrg(prg,par:string;waitAfter:Boolean);
Var w,ww:Word;
Begin;
RefreshScr:=RefreshScr or $ff;
w:=xChDir(PanelDat[PanelCur].pat);
if ShowErrorMessage(w,'chdir '+PanelDat[PanelCur].pat) then exit;
TextColor(ColNorm);clrscr;
WriteLn('path: "'+PanelDat[PanelCur].pat+'"');
WriteLn('program: "'+prg+'"');
WriteLn('parameter: "'+par+'"');
w:=xExec(prg,par,ww);
WriteLn('');
while keypressed do readkey;
if (w=0) then begin;
  Write('successful, exitcode='+BStr(ww)+'.');
  end else begin;
  Write('error: '+xGetErrorName(w)+'.');
  end;
if waitAfter then begin;
  Write(' press any key to continue!');
  ReadChrKey;
  end;
PanelRereadOneNow(1);
PanelRereadOneNow(2);
PanelSelectCurr;
End;



Procedure DoWorkFileRights(a,b:String);
Label f1,f2,f3,f4;
Var
  xp,yp,xs,ys:Word;
  sr:xFile;
  ntry:xDirEntryRec;
  w:Word;
  i,o:LongInt;
  date:xDirEntryDateTimeRec;

function x(n:string;w:word):String;
var a:string;
begin;
if (w=0) then a:='successful!' else a:='ERROR: '+xGetErrorName(w);
a:=n+': '+a;
while (length(a)<xs) do a:=a+' ';
x:=a;
end;

Begin;
w:=xDirOpen(sr,a);
if ShowErrorMessage(w,'open '+a) then exit;
b:=kicsi(b);
f1:
w:=xDirRead(sr,ntry);
if ShowErrorMessage(w,'read '+a) then begin;
  xDirClose(sr);
  exit;
  end;
if (ntry.name='') then begin;
  xDirClose(sr);
  ShowErrorMessage(1,'find '+b);
  exit;
  end;
if (kicsi(ntry.name)<>b) then goto f1;
xDirClose(sr);
b:=a+ntry.name;
xs:=ScrSizX-8;
ys:=10;
PutOutWindow(xp,yp,xs,ys,'set rights',ColWinB,ColWinT);
GotoXY(xp,yp+0);Write('entry name:');
GotoXY(xp,yp+1);Write(ntry.name);
GotoXY(xp,yp+2);Write('size: '+alakit(ntry.size));
GotoXY(xp,yp+8);Write('press number to change or enter to start!');
f2:
a:='1-rights: '+ConvertFileMode2string(ntry.rights)+'  (0x'+long2hex(ntry.rights)+')';
while (length(a)<xs) do a:=a+' ';
GotoXY(xp,yp+3);Write(a);
a:='2-owner: 0x'+long2hex(ntry.owner);
while (length(a)<xs) do a:=a+' ';
GotoXY(xp,yp+4);Write(a);
a:='3-created: '+ConvertDateTime2string(ntry.created);
while (length(a)<xs) do a:=a+' ';
GotoXY(xp,yp+5);Write(a);
a:='4-modified: '+ConvertDateTime2string(ntry.modified);
while (length(a)<xs) do a:=a+' ';
GotoXY(xp,yp+6);Write(a);
GotoXY(xp,yp+8);
f3:
w:=ReadKey;
case w of
  $8001:begin;{refresh}
    RefreshScr:=$ff;
    exit;
    end;
  $8005:begin;{escape}
    exit;
    end;
  $8004:begin;{enter}
   goto f4;
   end;
  else begin;
    if (w and $fe00<>0) then goto f3;
    dec(w,ord('0'));
    w:=w and $ff;
    case w of
      1:begin;
        a:=ConvertFileMode2string(-1);
        GotoXY(xp+8,yp+3);Write(' ('+a+'):  ');
        a:=ConvertFileMode2string(ntry.rights);
        if AskUserForText(a,xp+22,yp+3,xs-22) then goto f2;
        if ConvertString2fileMode(a,i) then goto f2;
        i:=i and (not xRights_Directory);
        i:=i or (ntry.rights and xRights_Directory);
        ntry.rights:=i;
        end;
      2:begin;
        a:=long2hex(ntry.owner);
        if AskUserForText(a,xp+11,yp+4,xs-11) then goto f2;
        a:=nagy(a);
        i:=BVal('$'+a);
        if (long2hex(i)<>a) then goto f2;
        ntry.owner:=i;
        end;
      3:begin;
        a:=ConvertDateTime2string(ntry.created);
        if AskUserForText(a,xp+11,yp+5,xs-11) then goto f2;
        if ConvertDateString2dateTime(a,date) then goto f2;
        ntry.created:=date;
        goto f2;
        end;
      4:begin;
        a:=ConvertDateTime2string(ntry.modified);
        if AskUserForText(a,xp+12,yp+6,xs-12) then goto f2;
        if ConvertDateString2dateTime(a,date) then goto f2;
        ntry.modified:=date;
        goto f2;
        end;
      else goto f3;
      end;
    goto f2;
    end;
  end;
goto f3;
f4:
a:='';
while (length(a)<xs) do a:=a+' ';
w:=xSetRight(b,ntry.owner,ntry.rights);
GotoXY(xp,yp+8);Write(x('owner, rights',w));
w:=xSetDate(b,ntry.created,ntry.modified);
GotoXY(xp,yp+9);Write(x('create, modify',w));
GotoXY(xp,yp+7);
ReadChrKey;
End;




Function DoWorkChangeDrv(a:String;c:string):String;
Label f1,f2;
Var
  xp,yp,xs,ys:Word;
  w:Word;
  i,o,p:LongInt;
  cc:char absolute i;
  b:String;
Begin;
DoWorkChangeDrv:='';
b:='';
for i:=1 to 254 do begin;
  if (cc in ['A'..'Z']) then continue;
  w:=xChDir(cc+':\');
  if (w<>0) then continue;
  b:=b+cc;
  end;
if (copy(c,2,2)=':\') then begin;
  c:=kicsi(copy(c,1,1));
  p:=pos(c,b);
  end else p:=0;
if (p<1) then p:=1;
xs:=20;
ys:=8;
PutOutWindow(xp,yp,xs,ys,a+' panel',ColWinB,ColWinT);
GotoXY(xp,yp+0);Write('choose '+a+' letter:');
GotoXY(xp,yp+4);Write('---===> [ ] <===---');
f1:
for i:=-3 to +3 do begin;
  o:=p+i;
  if (o<1) or (o>length(b)) then a:=' ' else a:=copy(b,o,1);
  GotoXY(xp+9,yp+4+i);
  Write(a);
  end;
GotoXY(xp+9,yp+4);
f2:
w:=ReadKey;
o:=p;
case w of
  $8001:begin;{refresh}
    RefreshScr:=$ff;
    exit;
    end;
  $8005:exit;{escape}
  $8004:begin;{enter}
   DoWorkChangeDrv:=copy(b,p,1)+':\';
   exit;
   end;
  $800c:dec(p);{up}
  $800d:inc(p);{down}
  $800a:dec(p,3);{pgup}
  $800b:inc(p,3);{pgdn}
  $8008:p:=0;{home}
  $8009:p:=$666;{end}
  end;
if (p>length(b)) then p:=length(b);
if (p<1) then p:=1;
if (o<>p) then goto f1;
goto f2;
End;

Procedure DoWorkUserMenu(var a,b:String);
Const
  menuMax=200;
Type
  OneEntryRecord=record
    key:Char;
    scr:String[80];
    cmd:String;
    end;
Var cn,rn:String;

Function readLine(var t:xtText):String;
Label f1;
Var
  a,b,c:String;
  i:Word;
Begin;
c:=xtReadLn(t,255);
b:='';
f1:
i:=pos('\',c);
if (i=0) then begin; readLine:=b+c;exit; end;
b:=b+copy(c,1,i-1);
a:=copy(c,i+1,1)+#0;
c:=copy(c,i+2,255);
case a[1] of
  '\':a:='\';
  '1':a:=PanelDat[PanelCur].pat;
  '2':a:=xFileName(cn,2);
  '3':a:=xFileName(cn,3);
  '4':a:=PromptString;
  '5':a:=PanelDat[3-PanelCur].pat;
  '6':a:=xFileName(rn,2);
  '7':a:=xFileName(rn,3);
  else a:=copy(a,1,length(a)-1);
  end;
b:=b+a;
goto f1;
End;

Label kiir,key,exec;
Var
  menuDat:array[1..menuMax] of OneEntryRecord;
  menuNum:Word;
  t:xtText;
  xp,yp,xs,ys:Word;
  bg:LongInt;
  i,o,p:LongInt;
  w:Word;
Begin;
a:=xFileName(ParamStr(0),1)+xFileName(ParamStr(0),2)+'.menu';
if (xtOpen(t,a,true)<>0) then begin; a:='';b:='';exit; end;
i:=abs(PanelDat[PanelCur].cur);
if (i<1) or (i>PanelDat[PanelCur].max) then cn:='' else cn:=entriesBuf^[PanelDat[PanelCur].mem+i-1].nam;
i:=abs(PanelDat[3-PanelCur].cur);
if (i<1) or (i>PanelDat[3-PanelCur].max) then rn:='' else rn:=entriesBuf^[PanelDat[3-PanelCur].mem+i-1].nam;
menuNum:=0;
while not xtEOF(t) do begin;
  a:=readLine(t);
  if (a<>'---') then continue;
  if (menuNum>=menuMax) then continue;
  a:=readLine(t)+#0;
  inc(menuNum);
  menuDat[menuNum].key:=a[1];
  menuDat[menuNum].scr:=readLine(t);
  menuDat[menuNum].cmd:=readLine(t);
  end;
xtClose(t);
xs:=ScrSizX-12;
ys:=ScrSizY-7;
if (xs>200) then xs:=200;
if (ys and 1=0) then dec(ys);
PutOutWindow(xp,yp,xs,ys,'user menu',ColWinB,ColWinT);
textColor(ColWinB);
gotoXY(xp-2,yp+ys div 2);Write('Ì>');
gotoXY(xp+xs,yp+ys div 2);Write('<¹');
textColor(ColWinT);
bg:=1;
kiir:
p:=ys div 2;
for o:=-p to p do begin;
  i:=bg+o;
  if (i<1) or (i>menuNum) then a:='' else a:=menuDat[i].key+' '+menuDat[i].scr;
  if (a[1]=#0) then a[1]:=' ';
  while (length(a)<xs) do a:=a+' ';
  gotoXY(xp,yp+p+o);
  Write(a);
  end;
gotoXY(xp,yp+p);
key:
p:=bg;
w:=ReadKey;
a:='';b:='';
case w of
  $8001:begin; RefreshScr:=$ff;exit; end;
  $800c:dec(bg);{up}
  $800d:inc(bg);{down}
  $800a:dec(bg,ys shr 1);{pgup}
  $800b:inc(bg,ys shr 1);{pgdn}
  $8005:exit;{esc}
  $801c:exit;{f9}
  $8004:goto exec;{enter}
  end;
if (w and $8000=0) then begin;
  a:=chr(w);
  o:=0;
  for i:=1 to menuNum do if (menuDat[i].key=a) then o:=i;
  if (o>0) then begin; bg:=o;goto exec; end;
  end;
if (bg>menuNum) then bg:=menuNum;
if (bg<1) then bg:=1;
if (bg<>p) then goto kiir;
goto key;
exec:
if (bg<1) or (bg>menuNum) then exit;
a:=menuDat[bg].cmd;
i:=pos(' ',a);
if (i=0) then i:=666;
b:=copy(a,i+1,255);
a:=copy(a,1,i-1);
End;
