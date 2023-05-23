{$stack 16k}
{$heap 128k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc alap.inc}
{$sysinc bin.inc}
{$sysinc hex.inc}
{$sysinc datetime.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc memory.inc}

{$include link4sasm.inc}
{$include memory.inc}
{$include file_mem.inc}

{$sysinc quicksrt.inc}

Const PrgTxt='Link v1.8 for sAsm, done by Mc at '#%date' '#%time'.';




Const
  partSep=':';
  cmndSep='\';
Var
  cd:OneRecordType;
  objFileName:String;
  objDelAfter:Boolean;
  objLineNum:Longint;
  objLoadOfs:LongInt;
  rangeCheck:Boolean;
  fillerByte:Byte;




Procedure IncludeBinaryFile(n:String);
Var
  ff:xFile;
  buf:Array[1..1024] of Byte;
  fs,fp,bs:LongInt;
  i:LongInt;
  a:String;
Begin;
Write('B'+#8);
a:=RepairOneFileName(n,objFileName);
if (xOpen(ff,a,xGenFilMod_r)<>0) then WriteErr('error opening '+a+'!',0);
fs:=xFileSize(ff);
fp:=0;
While (fp<fs) do begin;
  bs:=fs-fp;
  if (bs>sizeof(buf)) then bs:=sizeof(buf);
  xBlockRead(ff,buf,bs);
  for i:=1 to bs do writeFile(buf[i],True);
  inc(fp,bs);
  end;
xClose(ff);
Write(')'+#8);
End;







Procedure processParts(cmd:String);
Label f1;

function getStr:String;
var i:LongInt;
begin;
i:=pos(partSep,cmd);
if (i<1) then i:=666;
getStr:=copy(cmd,1,i-1);
cmd:=copy(cmd,i+1,255);
end;

function getNum:LongInt;
begin;
getNum:=BVal('$'+getStr);
end;

Var
  i,o:LongInt;
  a:String;
Begin;
o:=0;
for i:=1 to length(cmd) do if (cmd[i]=partSep) then o:=i;
cd.Nam:=copy(cmd,o+1,255);
cmd:=copy(cmd,1,o-1);
cd.vAnd:=$ffffffff;
cd.Typ:=3;
cd.sCnt:=True;
f1:
a:=getStr;
if (a='byte') then begin; cd.Len:=1;goto f1; end;
if (a='word') then begin; cd.Len:=2;goto f1; end;
if (a='dword') then begin; cd.Len:=4;goto f1; end;
if (a='signed') then begin; cd.Sig:=True;goto f1; end;
if (a='rela2beg') then begin; cd.rel2:=1;goto f1; end;
if (a='rela2end') then begin; cd.rel2:=2;goto f1; end;
if (a='rela2no') then begin; cd.rel2:=0;goto f1; end;
if (a='numofs') then begin; cd.Typ:=4;goto f1; end;
if (a='ofs') then begin; cd.Typ:=3;cd.sLab:=True;cd.sCnt:=False;goto f1; end;
if (a='val') then begin; cd.Typ:=3;cd.sLab:=False;cd.sCnt:=True;goto f1; end;
if (a='ofs?') then begin; cd.Typ:=3;cd.sLab:=True;cd.sCnt:=True;goto f1; end;
if (a='or') then begin; cd.vOr:=getNum;goto f1; end;
if (a='and') then begin; cd.vAnd:=getNum;goto f1; end;
if (a='add') then begin; cd.vAdd:=getNum;goto f1; end;
if (a='shr') then begin; cd.vShr:=getNum;goto f1; end;
if (a='shl') then begin; cd.vShl:=getNum;goto f1; end;
if (a='mul') then begin; cd.vMul:=getNum;goto f1; end;
if (a='div') then begin; cd.vDiv:=getNum;goto f1; end;
if (a='mod') then begin; cd.vMod:=getNum;goto f1; end;
if (a='msb') then begin; cd.Ord:=True;goto f1; end;
if (a='lsb') then begin; cd.Ord:=False;goto f1; end;
if (a='nop') then goto f1;
if (a<>'') then WriteErr('',objLineNum);
if (cd.Len<1) then WriteErr('',objLineNum);
if (cd.Nam='?') then begin;
  for i:=1 to cd.Len do writeFile(fillerByte,False);
  exit;
  end;
if (cd.Typ<1) then WriteErr('',objLineNum);
if (cd.rel2<>0) then dec(cd.vAdd,objLoadOfs);
for i:=1 to cd.Len do writeFile($61,True);
if (cd.rel2<>0) then cd.Rng:=true;
appendRec(cd);
End;






Procedure processOneCommand(Var lin:String);

function getPart(var c:string;s:String):String;
var i:LongInt;
begin;
i:=pos(s,c);
if (i<1) then i:=666;
getPart:=copy(c,1,i-1);
c:=copy(c,length(s)+i,255);
end;

Var
  i,o:LongInt;
  a,b:String;
  cmd:String;
Begin;
FillChar(cd,SizeOf(cd),0);
cmd:=getPart(lin,cmndSep);
if (cmd='') then exit;
if (Length(cmd)=2) then begin;
  i:=h2b(cmd);
  if (b2h(i)<>cmd) then WriteErr('',objLineNum);
  writeFile(i,True);
  exit;
  end;
a:=getPart(cmd,partSep);
if (a='nop') then exit;
if (a='filebeg') then exit;
if (a='fileend') then exit;
if (a='org') then begin;
  objLoadOfs:=BVal('$'+cmd);
  exit;
  end;
if (a='range') then begin;
  rangeCheck:=(BVal('$'+cmd)<>0);
  exit;
  end;
if (a='incbin') then begin;
  Kicserel('/','\',cmd);
  IncludeBinaryFile(cmd);
  exit;
  end;
if (a='align') then begin;
  i:=BVal('$'+cmd);
  o:=GetFileSize;
  o:=o mod i;
  if (o<1) then exit;
  o:=i-o;
  for i:=1 to o do writeFile(fillerByte,False);
  exit;
  end;
if (a='dup') then begin;
  if (cmd<>'beg') then WriteErr('',objLineNum);
  b:=getPart(lin,cmndSep+'dup'+partSep);
  if (getPart(lin,partSep)<>'end') then WriteErr('',objLineNum);
  o:=BVal(getPart(lin,cmndSep));
  for i:=1 to o do begin;
    a:=b+cmndSep;
    while (a<>'') do processOneCommand(a);
    end;
  exit;
  end;

cd.Pos:=GetFileSize;
cd.Lin:=objLineNum;
cd.Rng:=rangeCheck;

if (a='label') then begin;
  cd.Typ:=1;
  cd.Nam:=cmd;
  inc(cd.Pos,objLoadOfs);
  appendRec(cd);
  exit;
  end;
if (a='const') then begin;
  cd.Typ:=2;
  a:=getPart(cmd,partSep);
  cd.Nam:=a;
  cd.Pos:=BVal('$'+cmd);
  appendRec(cd);
  exit;
  end;

processParts(a+partSep+cmd);
End;






Procedure processOneLine(lin:String);
Var
  linBegOfs:LongInt;
  linEndOfs:LongInt;
  i:LongInt;
Begin;
if (Copy(lin,1,1)<>cmndSep) then WriteErr('',objLineNum);
if (Copy(lin,Length(lin),1)<>cmndSep) then WriteErr('',objLineNum);
lin:=copy(lin,2,255);
recordMark:=recordUsed+1;
linBegOfs:=GetFileSize;
while (lin<>'') do processOneCommand(lin);
linEndOfs:=GetFileSize;

for i:=recordMark to recordUsed do begin;
  readRec(i,cd);
  case cd.Rel2 of
    0:continue;
    1:dec(cd.vAdd,linBegOfs);
    2:dec(cd.vAdd,linEndOfs);
    else WriteErr('',objLineNum);
    end;
  writeRec(i,cd);
  end;
End;





Procedure sortDataRecords;
Var
  i:LongInt;
  a:String;
Begin;
recordMark:=0;
for i:=1 to recordUsed do begin;
  readRec(i,cd);
  if (cd.Typ in [1,2]) then begin;
    inc(recordMark);
    quickSwapOne(i,recordMark);
    end;
  end;
if (recordMark>1) then QuickSort(1,recordMark);
a:='';
for i:=1 to recordMark do begin;
  readRec(i,cd);
  if (cd.Nam=a) then WriteErr('duplicate label: '+a,cd.Lin);
  a:=cd.Nam;
  end;
End;




Procedure processReferenceRecord;
Var
  od:OneRecordType;
  pp:Array[1..16] of Byte;
  pd:LongInt absolute pp;
  i,o:LongInt;
Begin;
case cd.Typ of
  3:begin;
    o:=QuickFind(1,recordMark,cd.Nam);
    if (o<1) then WriteErr('object not found: '+cd.Nam,cd.Lin);
    readRec(o,od);
    case od.Typ of
      1:if not cd.sLab then WriteErr('label not allowed here!',cd.Lin);
      2:if not cd.sCnt then WriteErr('constant not allowed here!',cd.Lin);
      else WriteErr('',cd.lin);
      end;
    o:=od.Pos;
    end;
  4:begin;
    o:=BVal('$'+cd.Nam);
    end;
  else WriteErr('',cd.lin);
  end;
inc(o,cd.vAdd);
if (cd.vShr>0) then o:=o shr cd.vShr;
if (cd.vShl>0) then o:=o shl cd.vShl;
if (cd.vMul>0) then o:=o*cd.vMul;
if (cd.vDiv>0) then o:=o div cd.vDiv;
if (cd.vMod>0) then o:=o mod cd.vMod;
o:=(o and cd.vAnd) or cd.vOr;
WriteLongLSB(pp,o);
SeekFile(cd.Pos);
if cd.Ord then begin;
  for o:=cd.Len downto 1 do writeFile(pp[o],True);
  end else begin;
  for o:=1 to cd.Len do writeFile(pp[o],True);
  end;
if not cd.Rng then exit;
pp[5]:=pp[4];
o:=0;
if cd.Sig then if (pp[cd.Len] and $80<>pp[cd.Len+1] and $80) then inc(o);
for i:=cd.Len+1 to 4 do if (pp[4]<>pp[i]) then inc(o);
if (o<>0) then WriteErr('value out of range!',cd.Lin);
End;





Var
  t:xtText;
  a,b:String;
  i,o:LongInt;
BEGIN;
WriteLn(PrgTxt);

objFileName:=ParamStr(1);
if (xFileName(objFileName,2)='') then begin;
  WriteLn('Using: link4sasm.code <objFile> [options]');
  WriteLn('Options: /dob    - delete obj when finished');
  WriteLn('         /cod    - change extension of output to cod');
  WriteLn('         /extXYZ - change extension of output to XYZ');
  Halt(2);
  end;

b:='.code';objDelAfter:=False;
for i:=2 to ParamCount do begin;
  a:=ParamStr(i)+' ';
  if (a[1] in ['/','-']) then a:=copy(a,2,255);
  a:=Kicsi(Levesz(a));
  if (a='dob') then objDelAfter:=True;
  if (pos(#13+a+#13,#13'com'#13'cod'#13'code'#13'bin'#13)<>0) then a:='ext'+a;
  if (Copy(a,1,3)='ext') then b:='.'+copy(a,4,255);
  end;
if (b='') then b:='';
WriteLn('Linking '+objFileName+'...');
Write('(      ) Processing code...');

if (xtOpen(t,objFileName,true)<>0) then WriteErr('error opening source!',0);
a:=xFileName(objFileName,1)+xFileName(objFileName,2)+b;
xErase(a);
if (xCreate(a)<>0) then WriteErr('error creating output!',0);
if (xOpen(fileHandler,a,xGenFilMod_rw)<>0) then WriteErr('error opening output!',0);

objLineNum:=0;
recordUsed:=0;
recordAllc:=0;
recordMark:=0;
binaryBegn:=0;
binaryVald:=0;
binarySize:=0;
binaryAllc:=0;
binaryPosi:=0;
objLoadOfs:=$100;
fillerByte:=$90;
rangeCheck:=True;

while not xtEOF(t) do begin;
  a:=xtReadLn(t,255);
  inc(objLineNum);
  Write(#13+'('+BStr(objLineNum));
  i:=Pos(';',a);if (i=0) then i:=666;
  processOneLine(Levesz(Copy(a,1,i-1)));
  end;

xtClose(t);

ClrEol;Write('(      ) Updating data...');

SeekFile(binaryVald);
TruncFile;

sortDataRecords;

for i:=recordMark+1 to recordUsed do begin;
  readRec(i,cd);
  Write(#13+'('+copy(BStr(cd.Lin)+'       ',1,6));
  processReferenceRecord;
  end;


ClrEol;Write('(      ) Writing file...');

FlushFile;
xClose(fileHandler);

if objDelAfter then xErase(objFileName);
ClrEol;WriteLn('Successfully finished!');
Halt(0);
END.