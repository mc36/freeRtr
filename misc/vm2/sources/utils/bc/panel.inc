Type
  OneFileRecord=record
    sel:Boolean;
    nam:String;
    flg:LongInt;
    own:LongInt;
    siz:LongInt;
    end;
  entriesType=array[0..1] of OneFileRecord;
  OnePanelRecord=record
    pat:String;
    rot:Boolean;
    mem:LongInt;
    max:LongInt;
    beg:LongInt;
    cur:LongInt;
    scrBX:Word;
    scrBY:Word;
    scrSX:Word;
    scrSY:Word;
    scrSN:Word;
    scrSS:Word;
    end;
Var
  PanelCur:Byte;
  PanelDat:array[1..2] of OnePanelRecord;
  entriesBuf:^entriesType;


Function entriesResize(o:LongInt):Boolean;
Var p:pointer;
Begin;
entriesResize:=true;
if (ExtendedMemoryResize(p,o)<o) then exit;
entriesBuf:=p^;
entriesResize:=false;
End;

Function dup(n:Byte;c:Char):String;
Var
  a:string;
  i:word;
Begin;
a:='';
for i:=1 to n do a:=a+c;
dup:=a;
End;

Function long2hex(i:longint):String;
Begin
long2hex:=nagy(byte2hextype(i shr 24)+byte2hextype(i shr 16)+byte2hextype(i shr 8)+byte2hextype(i));
End;

Procedure PanelReadUpOne(pn:LongInt);
Label f1,f2;
Var
  sr:xFile;
  ntry:xDirEntryRec;
  dd:OneFileRecord;
  max:LongInt;

Procedure add;
Var i:LongInt;
Begin;
if entriesResize((max+1)*sizeof(dd)) then exit;
entriesBuf^[max]:=dd;
inc(PanelDat[pn].max);
inc(max);
End;

Begin;
if (PanelDat[pn].mem<1) then begin;
  move(entriesBuf^[PanelDat[3-pn].mem],entriesBuf^,PanelDat[3-pn].max*sizeof(dd));
  PanelDat[3-pn].mem:=0;
  end;
PanelDat[pn].mem:=PanelDat[3-pn].max;
PanelDat[pn].max:=0;
PanelDat[pn].beg:=0;
PanelDat[pn].cur:=1;
PanelDat[pn].rot:=(copy(PanelDat[pn].pat,2,255)=':\');
max:=PanelDat[pn].mem;
entriesResize(max*sizeof(dd));
if not PanelDat[pn].rot then begin;
  fillchar(dd,sizeof(dd),0);
  dd.nam:='..';
  dd.flg:=xRights_Directory;
  add;
  end;
if (xDirOpen(sr,PanelDat[pn].pat)<>0) then exit;
f1:
if (xDirRead(sr,ntry)<>0) then goto f2;
dd.nam:=ntry.name;
dd.siz:=ntry.size;
dd.sel:=false;
dd.flg:=ntry.rights;
dd.own:=ntry.owner;
if (dd.nam='') then goto f2;
add;
goto f1;
f2:
xDirClose(sr);
End;

Procedure PanelSortOne(pn:LongInt);
Var
  dd:OneFileRecord;
  i:LongInt;

Function QuickReadOne(n:LongInt):String;
Var a:String;
Begin
dd:=entriesBuf^[n];
if (dd.flg and xRights_Directory<>0) then a:='1' else a:='2';
QuickReadOne:=a+kicsi(dd.nam);
End;

Procedure QuickSwapOne(a,b:LongInt);
Begin;
dd:=entriesBuf^[a];
entriesBuf^[a]:=entriesBuf^[b];
entriesBuf^[b]:=dd;
End;

{$sysinc quicksrt.inc}
Begin;
if PanelDat[pn].rot then i:=1 else i:=2;
if (i>=PanelDat[pn].max) then exit;
QuickSort(PanelDat[pn].mem+i-1,PanelDat[pn].mem+PanelDat[pn].max-1);
End;

Function PanelGetEntry(var d:OnePanelRecord;n:LongInt;var dd:OneFileRecord):Boolean;
Begin;
if (n<1) or (n>d.max) then begin;
  fillchar(dd,sizeof(dd),0);
  PanelGetEntry:=True;
  exit;
  end;
dd:=entriesBuf^[d.mem+n-1];
PanelGetEntry:=False;
End;

Function PanelCurrEntry(var d:OneFileRecord):Boolean;
Begin;
PanelCurrEntry:=PanelGetEntry(PanelDat[PanelCur],PanelDat[PanelCur].cur,d);
End;

Procedure PanelAlignCheckOne(var d:OnePanelRecord);
Var
  dd:OneFileRecord;
  i:LongInt;
Begin;
if (d.cur>d.max) then d.cur:=d.max;
if (d.cur<1) then d.cur:=1;
i:=d.ScrSY-4;
if (d.cur<=d.beg) then d.beg:=d.cur-1;
if (d.cur>d.beg+i) then d.beg:=d.cur-i;
if d.rot then exit;
dd:=entriesBuf^[d.mem];
dd.sel:=false;
entriesBuf^[d.mem]:=dd;
End;

Procedure PanelSelectCurr;
Var i:byte;
Begin;
PanelCur:=2-(PanelCur and 1);
for i:=1 to 2 do PanelDat[i].cur:=abs(PanelDat[i].cur);
i:=3-PanelCur;
PanelDat[i].cur:=-PanelDat[i].cur;
End;

Function PanelFindOneFile(var d:OnePanelRecord;n:String):LongInt;
Var
  dd:OneFileRecord;
  i:LongInt;
Begin;
n:=kicsi(n);
for i:=0 to d.max-1 do begin;
  dd:=entriesBuf^[d.mem+i];
  if (kicsi(dd.nam)<>n) then continue;
  PanelFindOneFile:=i+1;
  exit;
  end;
PanelFindOneFile:=-1;
End;

Procedure PanelCountSelected(var d:OnePanelRecord;var byt,fil,dir:LongInt;msk,ned:Boolean);
Var
  dd:OneFileRecord;
  i:LongInt;
Begin;
byt:=0;
dir:=0;
fil:=0;
if d.rot then i:=0 else i:=1;
for i:=i to d.max-1 do begin;
  dd:=entriesBuf^[d.mem+i];
  if (dd.sel and msk<>ned) then continue;
  inc(byt,dd.siz);
  if (dd.flg and xRights_Directory=0) then inc(fil) else inc(dir);
  end;
End;

Procedure PanelRereadOneNow(pn:LongInt);
Var
  dd:OneFileRecord;
  i:LongInt;
Begin;
PanelGetEntry(PanelDat[pn],abs(PanelDat[pn].cur),dd);
PanelReadUpOne(pn);
PanelSortOne(pn);
i:=PanelFindOneFile(PanelDat[pn],kicsi(dd.nam));
if (i>0) then PanelDat[pn].cur:=i;
PanelAlignCheckOne(PanelDat[pn]);
End;

Const
  FileModesMax=8;
  FileModesDat:array[1..FileModesMax] of record f:longint;c:char; end=(
    (f:xRights_HasRootPriv;c:'U'),(f:xRights_Directory;c:'d'),
    (f:xRights_OwnRead;c:'r'),(f:xRights_OwnWrite;c:'w'),(f:xRights_OwnExec;c:'x'),
    (f:xRights_AnyRead;c:'R'),(f:xRights_AnyWrite;c:'W'),(f:xRights_AnyExec;c:'X')
  );

Function ConvertFileMode2string(f:LongInt):String;
Var
  i,o:LongInt;
  a:String;
Begin;
a:='';
for i:=1 to FileModesMax do begin;
  o:=FileModesDat[i].f;
  if (f and o=o) then a:=a+FileModesDat[i].c else a:=a+'-';
  end;
ConvertFileMode2string:=a;
End;

Function ConvertString2fileMode(a:String;var f:LongInt):Boolean;
Var
  i:LongInt;
Begin;
ConvertString2fileMode:=True;
f:=0;
if (length(a)<>FileModesMax) then exit;
for i:=1 to FileModesMax do begin;
  if (a[i]='-') then continue;
  if (a[i]<>FileModesDat[i].c) then exit;
  f:=f or FileModesDat[i].f;
  end;
ConvertString2fileMode:=False;
End;


Function ConvertDateTime2string(var d:xDirEntryDateTimeRec):String;
function x(n:word):string;var a:String; begin;a:=bstr(n);while (length(a)<2) do a:='0'+a;x:=a; end;
Begin;
ConvertDateTime2string:=x(d.year)+'-'+x(d.month)+'-'+x(d.day)+' '
                +x(d.hour)+':'+x(d.minute)+':'+x(d.second);
End;

Function ConvertDateString2dateTime(a:String;var d:xDirEntryDateTimeRec):Boolean;

Function x(s:char):LongInt;
Var
  b:String;
  i:LongInt;
Begin;
x:=-1;
i:=pos(s,a);
b:=copy(a,1,i-1);
a:=copy(a,i+1,255);
if (b='') then exit;
while (copy(b,1,1)='0') do b:=copy(b,2,255);
if (b='') then b:='0';
i:=BVal(b);
if (BStr(i)<>b) then exit;
x:=i;
End;

Var i:LongInt;
Begin;
ConvertDateString2dateTime:=True;
a:=a+' ';
i:=x('-');
if (i<1000) or (i>4000) then exit;
d.year:=i;
i:=x('-');
if (i<1) or (i>12) then exit;
d.month:=i;
i:=x(' ');
if (i<1) or (i>31) then exit;
d.day:=i;
i:=x(':');
if (i<0) or (i>23) then exit;
d.hour:=i;
i:=x(':');
if (i<0) or (i>59) then exit;
d.minute:=i;
i:=x(' ');
if (i<0) or (i>59) then exit;
d.second:=i;
if (a<>'') then exit;
ConvertDateString2dateTime:=False;
End;
