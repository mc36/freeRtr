Type
  OneFileRecord=record
    nam:String[31];
    dir:Boolean;
    lev:Byte;
    num:Word;
    dat:xDirEntryDateTimeRec;
    par:LongInt;
    beg:LongInt;
    siz:LongInt;
    end;
  FileListType=array[1..1] of OneFileRecord;
  OneBufferRecord=record
    d:array[1..2048] of byte;
    s:LongInt;
    end;
Var
  filesDat:^FileListType;
  filesNum:LongInt;
  filesOut:xFile;
  filesInp:xtText;



Procedure resize(n:LongInt);
Var p:Pointer;
Begin;
n:=n*sizeof(OneFileRecord);
if (ExtendedMemoryResize(p,n)<n) then immErr('error allocating memory!');
filesDat:=p^;
End;

Function FindOneEntry(p:LongInt;a:String):LongInt;
Var i:LongInt;
Begin;
a:=kicsi(a);
for i:=1 to filesNum do begin;
  if (filesDat^[i].par<>p) then continue;
  if (kicsi(filesDat^[i].nam)<>a) then continue;
  FindOneEntry:=i;
  exit;
  end;
FindOneEntry:=0;
End;



Procedure GeneratePathEntry(var d:OneBufferRecord;lsb:Boolean;nam:String;beg,par:LongInt);
Var
  namB:array[0..1] of byte absolute nam;
  nam0:byte absolute namB;
Begin;
d.d[1]:=nam0;
d.d[2]:=0;
if lsb then WriteLongLSB(d.d[3],beg) else WriteLongMSB(d.d[3],beg);
if lsb then WriteWordLSB(d.d[7],par) else WriteWordMSB(d.d[7],par);
move(namB[1],d.d[9],nam0);
d.s:=8+nam0;
if (d.s and 1<>0) then begin;
  inc(d.s);
  d.d[d.s]:=0;
  end;
End;

Procedure GenerateDirEntry(var d:OneBufferRecord;nam:String;dir:Boolean;beg,siz:LongInt;dat:xDirEntryDateTimeRec);
Var
  namB:array[0..1] of byte absolute nam;
  nam0:byte absolute namB;
Begin;
d.s:=0;
d.d[2]:=0;
WriteLongLSB(d.d[3],beg);
WriteLongMSB(d.d[7],beg);
WriteLongLSB(d.d[11],siz);
WriteLongMSB(d.d[15],siz);
d.d[19]:=dat.year-1900;
d.d[20]:=dat.month;
d.d[21]:=dat.day;
d.d[22]:=dat.hour;
d.d[23]:=dat.minute;
d.d[24]:=dat.second;
d.d[25]:=0;
if dir then d.d[26]:=$02 else d.d[26]:=$00;
d.d[27]:=$00;
d.d[28]:=$00;
WriteWordLSB(d.d[29],1);
WriteWordMSB(d.d[31],1);
d.d[33]:=nam0;
move(namB[1],d.d[34],nam0);
d.s:=33+nam0;
if (d.s and 1<>0) then begin;
  inc(d.s);
  d.d[d.s]:=0;
  end;
d.d[1]:=d.s;
End;




Procedure PutFillerToOutput(skipEmpty:boolean);
Var
  d:OneBufferRecord;
  i:LongInt;
Begin;
i:=$800-(xFilePos(filesOut) and $7ff);
if skipEmpty then if (i>$7ff) then exit;
fillchar(d,i,0);
xBlockWrite(filesOut,d,i);
End;



Procedure GenerateHeader;
Label f1,f2;
Var
  publisher,system,volume,volset,preparer,
  copyright,abstracts,bibliogrp,
  boot:String;
  d:OneBufferRecord;
  w1,w2,w3:Word;
  i,o,p,s:LongInt;
  a,b:String;
  f:xFile;

procedure put(p,m:LongInt;a:string);
begin;
a:=copy(a,1,m);
while (length(a)<m) do a:=a+' ';
move(a[1],d.d[p],m);
end;

function pad(i:longint):string;
var a:string;
begin;
a:=bstr(i);
while (length(a)<2) do a:='0'+a;
pad:=a;
end;

Begin;
WriteLn('generating header...');
for i:=1 to 16 do PutFillerToOutput(false);
publisher:='';system:='';volume:='';volset:='';preparer:='';copyright:='';abstracts:='';bibliogrp:='';boot:='';
f1:
a:=xtReadLn(filesInp,255);
if (a='') then goto f2;
i:=pos('=',a);
b:=copy(a,i+1,255);
a:=kicsi(copy(a,1,i-1));
if (a='publisher') then publisher:=b;
if (a='system') then system:=b;
if (a='volume') then volume:=b;
if (a='volset') then volset:=b;
if (a='preparer') then preparer:=b;
if (a='copyright') then copyright:=b;
if (a='abstracts') then abstracts:=b;
if (a='bibliogrp') then bibliogrp:=b;
if (a='boot') then boot:=b;
goto f1;
f2:
fillchar(d,sizeof(d),0);
d.d[1]:=1;
put(2,5,'CD001');
d.d[7]:=1;
put(9,32,system);
put(41,32,volume);
WriteWordLSB(d.d[121],1);WriteWordMSB(d.d[123],1);
WriteWordLSB(d.d[125],1);WriteWordMSB(d.d[127],1);
WriteWordLSB(d.d[129],sizeof(d.d));WriteWordMSB(d.d[131],sizeof(d.d));
put(191,128,volset);
put(319,128,publisher);
put(447,128,preparer);
put(575,128,proggyName+' for BugOS');
put(703,37,copyright);
put(740,37,abstracts);
put(777,37,bibliogrp);
xGetDate(w1,w2,w3);
a:=pad(w1)+pad(w2)+pad(w3);
xGetTime(w1,w2,w3);
a:=a+pad(w1)+pad(w2)+pad(w3)+'00'#0;
put(814,17,a);
put(831,17,a);
a:='0000000000000000'#0;
put(848,17,a);
put(865,17,a);
d.d[882]:=1;
xBlockWrite(filesOut,d.d,sizeof(d.d));
if (boot<>'') then begin;
  fillchar(d,sizeof(d),0);
  d.d[1]:=0;
  put(2,5,'CD001');
  d.d[7]:=1;
  put(8,23,'EL TORITO SPECIFICATION');
  WriteWordLSB(d.d[72],(xFilePos(filesOut) div sizeof(d.d))+2);
  xBlockWrite(filesOut,d.d,sizeof(d.d));
  end;
fillchar(d,sizeof(d),0);
d.d[1]:=$ff;
put(2,5,'CD001');
d.d[7]:=1;
xBlockWrite(filesOut,d.d,sizeof(d.d));
if (boot='') then exit;
WriteLn('generating boot block...');
fillchar(d,sizeof(d),0);
d.d[1]:=1;
d.d[2]:=0;
put(5,24,xFileName(boot,2)+xFileName(boot,3));
d.d[31]:=$55;
d.d[32]:=$aa;
o:=0;
for i:=1 to 16 do inc(o,ReadWordLSB(d.d[(i*2)-1]));
WriteWordLSB(d.d[29],(not o)+1);
d.d[33]:=$88;
d.d[34]:=$02;
WriteWordLSB(d.d[35],$7c0);
d.d[37]:=$06;
d.d[39]:=1;
WriteLongLSB(d.d[41],(xFilePos(filesOut) div sizeof(d.d))+1);
xBlockWrite(filesOut,d.d,sizeof(d.d));
if (xOpen(f,boot,xGenFilMod_r)<>0) then immErr('error opening boot image!');
s:=xFileSize(f);
p:=0;
while (p<s) do begin;
  i:=s-p;
  if (i>sizeof(d.d)) then i:=sizeof(d.d);
  xBlockRead(f,d,i);
  xBlockWrite(filesOut,d,i);
  inc(p,i);
  end;
xClose(f);
PutFillerToOutput(true);
End;





Procedure GenerateFileList;
Label f1,f2;
Var
  a,b:String;
  par:LongInt;
  lev:LongInt;
  dirs:LongInt;
  i,o,p,s:LongInt;
  buf:OneBufferRecord;
  sr:xDirEntryRec;
  d:OneFileRecord;
  f:xFile;
Begin;
WriteLn('writing files...');
filesNum:=0;
dirs:=0;
f1:
inc(dirs);
repeat
  if xtEOF(filesInp) then exit;
  a:=xtReadLn(filesInp,255);
  until (a<>'');
if (copy(a,length(a),255)='\') then a:=copy(a,1,length(a)-1);
if (a[1]='\') then a:=copy(a,2,255);
WriteLn('\'+a);
b:=xFileName(a,2)+xFileName(a,3);
a:=xFileName(a,1);
par:=1;
lev:=0;
while (a<>'') do begin;
  inc(lev);
  i:=pos('\',a);
  o:=FindOneEntry(par,copy(a,1,i-1));
  if (o=0) then immErr('missing parent!');
  if not filesDat^[o].dir then immErr('invalid parent!');
  a:=copy(a,i+1,255);
  par:=o;
  end;
if (FindOneEntry(par,b)<>0) then immErr('duplicate name!');
fillchar(d,sizeof(d),0);
d.nam:=b;
d.dir:=true;
d.par:=par;
d.lev:=lev;
d.num:=dirs;
GetCurrentDate(d.dat);
inc(filesNum);
resize(filesNum);
filesDat^[filesNum]:=d;
par:=filesNum;
f2:
a:=xtReadLn(filesInp,255);
if (a='') then goto f1;
i:=pos(' ',a);
if (i<1) then i:=666;
b:=copy(a,i+1,255);
a:=copy(a,1,i-1);
if (b='') then b:=xFileName(a,2)+xFileName(a,3);
WriteLn('  '+b);
if (FindOneEntry(par,b)<>0) then immErr('duplicate name!');
fillchar(d,sizeof(d),0);
d.nam:=b;
d.dir:=false;
d.par:=par;
d.lev:=lev;
d.beg:=xFilePos(filesOut) div sizeof(buf.d);
if FindOneFile(a,sr) then immErr('file not found!');
d.dat:=sr.modified;
if (xOpen(f,a,xGenFilMod_r)<>0) then immErr('error opening!');
s:=xFileSize(f);
p:=0;
while (p<s) do begin;
  i:=s-p;
  if (i>sizeof(buf.d)) then i:=sizeof(buf.d);
  xBlockRead(f,buf,i);
  xBlockWrite(filesOut,buf,i);
  inc(p,i);
  end;
xClose(f);
PutFillerToOutput(true);
d.siz:=s;
inc(filesNum);
resize(filesNum);
filesDat^[filesNum]:=d;
goto f2;
End;






Procedure GenerateDirectories;
Var d,dd:OneBufferRecord;

procedure addOne;
Label f1;
begin;
f1:
if (dd.s+d.s<=sizeof(dd.d)) then begin;
  move(d.d,dd.d[dd.s+1],d.s);
  inc(dd.s,d.s);
  exit;
  end;
xBlockWrite(filesOut,dd.d,sizeof(dd.d));
fillchar(dd,sizeof(dd),0);
goto f1;
end;

Procedure codeOne(num:LongInt);
Var i:LongInt;
Begin;
WriteLn('  '+filesDat^[num].nam);
fillchar(dd,sizeof(dd),0);
filesDat^[num].beg:=xFilePos(filesOut) div sizeof(d.d);
GenerateDirEntry(d,#0,filesDat^[num].dir,filesDat^[num].beg,filesDat^[num].siz,filesDat^[num].dat);
addOne;
i:=filesDat^[num].par;
GenerateDirEntry(d,#1,filesDat^[i].dir,filesDat^[i].beg,filesDat^[i].siz,filesDat^[i].dat);
addOne;
for i:=1 to filesNum do if (filesDat^[i].par=num) then begin;
  if (i=num) then continue;
  GenerateDirEntry(d,filesDat^[i].nam,filesDat^[i].dir,filesDat^[i].beg,filesDat^[i].siz,filesDat^[i].dat);
  addOne;
  end;
fillchar(d,sizeof(d),0);
d.s:=sizeof(d.d);
addOne;
filesDat^[num].siz:=xFilePos(filesOut)-(filesDat^[num].beg*sizeof(d.d));
End;

Var i,o,p:LongInt;
Begin;
WriteLn('writing directories 1/2...');
o:=xFilePos(filesOut);
for i:=1 to filesNum do if filesDat^[i].dir then codeOne(i);
p:=xFilePos(filesOut);
WriteLn('writing directories 2/2...');
xSeek(filesOut,o);
for i:=1 to filesNum do if filesDat^[i].dir then codeOne(i);
if (xFilePos(filesOut)<>p) then immErr('bug!');
WriteLn('updating primary descriptor...');
GenerateDirEntry(dd,#0,filesDat^[1].dir,filesDat^[1].beg,filesDat^[1].siz,filesDat^[1].dat);
if (dd.s<>34) then immErr('bug!');
o:=xFilePos(filesOut);xSeek(filesOut,$8000);xBlockRead(filesOut,d.d,sizeof(d.d));
move(dd.d,d.d[157],34);
xSeek(filesOut,$8000);xBlockWrite(filesOut,d.d,sizeof(d.d));xSeek(filesOut,o);
End;





Procedure GeneratePathTable;
Var d,dd,ddd:OneBufferRecord;

Function doLevel(lev:LongInt;lsb:Boolean):Boolean;
Const brk=1024;
Var i,o:LongInt;
Begin;
WriteLn('  level '+BStr(lev));
o:=0;
for i:=2 to filesNum do begin;
  if not filesDat^[i].dir then continue;
  if (filesDat^[i].lev<>lev) then continue;
  inc(o);
  GeneratePathEntry(d,lsb,filesDat^[i].nam,filesDat^[i].beg,filesDat^[filesDat^[i].par].num);
  move(d.d,dd.d[dd.s+1],d.s);inc(dd.s,d.s);
  if (dd.s<brk) then continue;
  xBlockWrite(filesOut,dd.d,brk);
  dec(dd.s,brk);
  move(dd.d[brk+1],dd.d,dd.s);
  end;
doLevel:=(o>0);
End;

Var
  i,o:LongInt;
Begin;
i:=xFilePos(filesOut);xSeek(filesOut,$8000);xBlockRead(filesOut,ddd.d,sizeof(ddd.d));xSeek(filesOut,i);
WriteLn('writing path table (1/2)...');
o:=xFilePos(filesOut);
GeneratePathEntry(dd,true,#0,filesDat^[1].beg,1);
i:=0;while doLevel(i,true) do inc(i);
xBlockWrite(filesOut,dd.d,dd.s);
WriteLongLSB(ddd.d[133],xFilePos(filesOut)-o);
WriteLongLSB(ddd.d[141],o div sizeof(d.d));
PutFillerToOutput(true);
WriteLn('writing path table (2/2)...');
o:=xFilePos(filesOut);
GeneratePathEntry(dd,false,#0,filesDat^[1].beg,1);
i:=0;while doLevel(i,false) do inc(i);
xBlockWrite(filesOut,dd.d,dd.s);
WriteLongMSB(ddd.d[137],xFilePos(filesOut)-o);
WriteLongMSB(ddd.d[149],o div sizeof(d.d));
PutFillerToOutput(true);
WriteLn('updating primary descriptor...');
i:=xFilePos(filesOut);xSeek(filesOut,$8000);xBlockWrite(filesOut,ddd.d,sizeof(ddd.d));xSeek(filesOut,i);
End;





Procedure GenerateFooters;
Var
  d:OneBufferRecord;
  i,o:LongInt;
Begin;
WriteLn('generating footer...');
for i:=1 to 64 do PutFillerToOutput(false);
WriteLn('updating primary descriptor...');
o:=xFilePos(filesOut);xSeek(filesOut,$8000);xBlockRead(filesOut,d.d,sizeof(d.d));
i:=xFileSize(filesOut) div sizeof(d.d);
WriteLongLSB(d.d[81],i);
WriteLongMSB(d.d[85],i);
xSeek(filesOut,$8000);xBlockWrite(filesOut,d.d,sizeof(d.d));xSeek(filesOut,o);
xTruncate(filesOut);
End;
