uses dos,crt;

type
  longword=longint;
  defword=longint;
  defint=longint;

Function Bstr(a:LongInt):String;
var
  b:String;
begin;
str(a,b);
Bstr:=b;
end;

Function Bval(a:String):LongInt;
var
  Code:Integer;
  i:LongInt;
begin;
Val(a,i,Code);
Bval:=i;
if (Code<>0) then Bval:=0;
end;

Function LowCase(c:Char):Char;
Begin;
if (c in ['A'..'Z']) then LowCase:=Chr(Ord(c)+32) else LowCase:=c;
End;

Function Kicsi(a:String):String;
var
  i:LongInt;
begin
if (a<>'') then for i:=1 to Length(a) do a[i]:=LowCase(a[i]);
Kicsi:=a;
end;

Function Nagy(a:String):String;
var
  i:LongInt;
begin
if (a<>'') then for i:=1 to Length(a) do a[i]:=UpCase(a[i]);
Nagy:=a;
end;

Function Kicserel(f,t:String;Var b:String):LongInt;
Var
  i,o:LongInt;
Begin;
o:=0;
Repeat
  i:=Pos(f,b);
  inc(o);
  if (i<>0) then begin;
    Delete(b,i,Length(f));
    Insert(t,b,i);
    end;
  Until i=0;
Kicserel:=o-1;
End;

Function random(a:longint):longint;
Var
  i,o:LongInt;
  d:array[1..4] of byte absolute o;
Begin;
for i:=1 to 4 do d[i]:=system.random($100);
random:=abs(o) mod a;
End;

Function Swap32(i:Longint):LongInt;
Begin;
swap32:=((i and $000000ff) shl 24) or
        ((i and $0000ff00) shl 8) or
        ((i and $00ff0000) shr 8) or
        ((i and $ff000000) shr 24);
End;

Function Swap16(i:word):word;
Begin;
Swap16:=((i and $00ff) shl 8) or
        ((i and $ff00) shr 8);
End;

Function readWordLSB(var buf):Word;
Var i:Word;
Begin;
move(buf,i,sizeof(i));
readWordLSB:=i;
End;

Function readWordMSB(var buf):Word;
Var i:Word;
Begin;
move(buf,i,sizeof(i));
readWordMSB:=swap16(i);
End;

Function readLongLSB(var buf):LongInt;
Var i:LongInt;
Begin;
move(buf,i,sizeof(i));
readLongLSB:=i;
End;

Function readLongMSB(var buf):LongInt;
Var i:LongInt;
Begin;
move(buf,i,sizeof(i));
readLongMSB:=swap32(i);
End;

Procedure writeWordLSB(var buf;dat:Word);
Begin;
move(dat,buf,sizeof(dat));
End;

Procedure writeWordMSB(var buf;dat:Word);
Begin;
dat:=swap16(dat);
move(dat,buf,sizeof(dat));
End;

Procedure writeLongLSB(var buf;dat:LongInt);
Begin;
move(dat,buf,sizeof(dat));
End;

Procedure writeLongMSB(var buf;dat:LongInt);
Begin;
dat:=swap32(dat);
move(dat,buf,sizeof(dat));
End;

Function GetAllParameters:String;
Var
  a:String;
  i:Byte;
Begin;
a:='';
for i:=1 to ParamCount do a:=a+' '+ParamStr(i);
a:=copy(a,2,255);
GetAllParameters:=a;
End;

Function GetMyFullFileName:String;
Begin;
GetMyFullFileName:=ParamStr(0);
End;


Function ReadKey:Word;
Label f1;
Var
  a:String;
  w,q:Word;
Begin;
f1:
a:=crt.ReadKey;
if (a=#0) then a:=a+crt.ReadKey;
w:=ord(a[1]);
case w of
   9:w:=$8002;
   8:w:=$8003;
  10:w:=$8204;
  13:w:=$8004;
  27:w:=$8005;
  29:w:=$025d;
  else if (w in [1..26]) then w:=$0260+w;
  end;
if (w=0) then case ord(a[2]) of
  $52,$04,$05:w:=$8006;
  $53,$06,$07:w:=$8007;
  $47,$77:w:=$8008;
  $4f,$75:w:=$8009;
  $49,$84:w:=$800a;
  $51,$76:w:=$800b;
  $48:w:=$800c;
  $50:w:=$800d;
  $4b,$73:w:=$800e;
  $4d,$74:w:=$800f;
  $3b,$5e,$68,$54:w:=$8014;
  $3c,$5f,$69,$55:w:=$8015;
  $3d,$60,$6a,$56:w:=$8016;
  $3e,$61,$6b,$57:w:=$8017;
  $3f,$62,$6c,$58:w:=$8018;
  $40,$63,$6d,$59:w:=$8019;
  $41,$64,$6e,$5a:w:=$801a;
  $42,$65,$6f,$5b:w:=$801b;
  $43,$66,$70,$5c:w:=$801c;
  $44,$67,$71,$5d:w:=$801d;
  $85:w:=$801e;
  $86:w:=$801f;
  $10:w:=$0400+ord('q');
  $11:w:=$0400+ord('w');
  $12:w:=$0400+ord('e');
  $13:w:=$0400+ord('r');
  $14:w:=$0400+ord('t');
  $15:w:=$0400+ord('y');
  $16:w:=$0400+ord('u');
  $17:w:=$0400+ord('i');
  $18:w:=$0400+ord('o');
  $19:w:=$0400+ord('p');
  $1e:w:=$0400+ord('a');
  $1f:w:=$0400+ord('s');
  $20:w:=$0400+ord('d');
  $21:w:=$0400+ord('f');
  $22:w:=$0400+ord('g');
  $23:w:=$0400+ord('h');
  $24:w:=$0400+ord('j');
  $25:w:=$0400+ord('k');
  $26:w:=$0400+ord('l');
  $2c:w:=$0400+ord('z');
  $2d:w:=$0400+ord('x');
  $2e:w:=$0400+ord('c');
  $2f:w:=$0400+ord('v');
  $30:w:=$0400+ord('b');
  $31:w:=$0400+ord('n');
  $32:w:=$0400+ord('m');
  $78:w:=$0400+ord('1');
  $79:w:=$0400+ord('2');
  $7a:w:=$0400+ord('3');
  $7b:w:=$0400+ord('4');
  $7c:w:=$0400+ord('5');
  $7d:w:=$0400+ord('6');
  $7e:w:=$0400+ord('7');
  $7f:w:=$0400+ord('8');
  $80:w:=$0400+ord('9');
  $81:w:=$0400+ord('0');
  $82:w:=$0400+ord('-');
  $83:w:=$0400+ord('=');
  $08:w:=$8403;
  else goto f1;
  end;
{$ifndef win32}
q:=memW[$40:$17];
if (q and 3<>0) then w:=w or $100;
if (q and 4<>0) then w:=w or $200;
if (q and 8<>0) then w:=w or $400;
{$endif}
ReadKey:=w;
End;


Procedure ConsoleSize(var x,y:Word);
Begin;
x:=78;
y:=24;
End;

Procedure TextColor(b:Byte);
Begin;
TextAttr:=b;
End;

Procedure xGetDate(var year,month,day:Word);
Var w:word;
Begin;
getDate(year,month,day,w);
End;

Procedure xGetTime(var hour,minute,second:Word);
Var w:word;
Begin;
GetTime(hour,minute,second,w);
End;

Type
  xDirEntryDateTimeRec=packed record
    year:Word;
    month:Byte;
    day:Byte;
    hour:Byte;
    minute:Byte;
    second:Byte;
    end;
  xDirEntryRec=packed record
    size:LongInt;
    rights:LongInt;
    owner:LongInt;
    created:xDirEntryDateTimeRec;
    modified:xDirEntryDateTimeRec;
    name:String;
    end;
Const
  xFileMode_read=0;
  xFileMode_write=1;
  xFileMode_allowRead=0;
  xFileMode_allowWrite=0;
  xGenFilMod_r=0;
  xGenFilMod_rw=2;
Const
  xRights_OwnRead=$01;
  xRights_OwnWrite=$02;
  xRights_OwnExec=$04;
  xRights_AnyRead=$08;
  xRights_AnyWrite=$10;
  xRights_AnyExec=$20;
  xRights_HasRootPriv=$40;
  xRights_Directory=$80;
Type
  xFile=record
    case what:byte of
      1:(sr:searchRec);
      2:(f:file);
    end;

Function xDirOpen(var f:xFile;n:String):Word;
Begin;
f.what:=1;
FindFirst(n+'*.*',AnyFile,f.sr);
while (copy(f.sr.name,1,1)='.') and (doserror=0) do findNext(f.sr);
xDirOpen:=DosError;
if (dosError<>0) then fillchar(f,sizeof(f),0);
if (DosError=18) then xDirOpen:=0;
End;

Function xDirRead(var f:xFile;var d:xDirEntryRec):Word;
var
  dt:dateTime;
  i,o:LongInt;
Begin
if (f.what<>1) then begin;
  fillchar(d,sizeof(d),0);
  xDirRead:=1;
  exit;
  end;
unpacktime(f.sr.time,dt);
d.size:=f.sr.size;
i:=f.sr.attr;o:=0;
if (i and Directory<>0) then inc(o,xRights_Directory);
if (i and SysFile<>0) then inc(o,xRights_HasRootPriv);
inc(i,xRights_OwnRead);
if (i and Archive<>0) then inc(o,xRights_OwnWrite);
if (i and Hidden<>0) then inc(o,xRights_OwnExec);
d.rights:=o;
d.owner:=0;
d.name:=kicsi(f.sr.name);
d.created.year:=dt.year;
d.created.month:=dt.month;
d.created.day:=dt.day;
d.created.hour:=dt.hour;
d.created.minute:=dt.min;
d.created.second:=dt.sec;
d.modified:=d.created;
xDirRead:=0;
findNext(f.sr);
if (dosError<>0) then fillchar(f,sizeof(f),0);
End;

Function xDirClose(var f:xFile):Word;
Begin;
xDirClose:=0;
fillchar(f,sizeof(f),0);
End;

Function xExec(fn,par:String;var ret:Word):Word;
Begin;
SwapVectors;
Exec(fn,par);
SwapVectors;
xExec:=DosError;
ret:=DosExitCode;
End;

Function xGetDir:String;
Var a:String;
Begin;
getDir(0,a);
if (length(a)>3) then a:=a+'\';
xGetDir:=a;
End;

Function xChDir(fn:String):Word;
Begin;
{$I-}
ChDir(fn);
{$I+}
xChDir:=IOresult;
End;

Function xMkDir(fn:String):Word;
Begin;
{$I-}
MkDir(fn);
{$I+}
xMkDir:=IOresult;
End;

Function xRmDir(fn:String):Word;
Begin;
{$I-}
RmDir(fn);
{$I+}
xRmDir:=IOresult;
End;

Function xRename(a,b:String):Word;
Var f:File;
Begin;
{$I-}
Assign(f,a);
Rename(f,b);
{$I+}
xRename:=IOresult;
End;

Function xErase(fn:String):Word;
Var f:File;
Begin;
{$I-}
Assign(f,fn);
Erase(f);
{$I+}
xErase:=IOresult;
End;

Function xCreate(fn:String):Word;
Var
  f:File;
  i:LongInt;
Begin;
{$I-}
Assign(f,fn);
ReSet(f,1);
{$I+}
if (IOresult=0) then begin; Close(f);exit; end;
{$I-}
ReWrite(f,1);
xCreate:=IOresult;
Close(f);
i:=IOresult;
{$I+}
End;

Function xOpen(var f:xFile;fn:String;md:LongInt):Word;
Begin;
f.what:=2;
FileMode:=md;
{$I-}
Assign(f.f,fn);
ReSet(f.f,1);
{$I+}
xOpen:=IOresult;
End;

Function xBlockRead(f:xFile;var buf;siz:Word):Word;
Begin;
{$I-}
BlockRead(f.f,buf,siz);
{$I+}
xBlockRead:=IOresult;
End;

Function xBlockWrite(f:xFile;var buf;siz:Word):Word;
Begin;
{$I-}
BlockWrite(f.f,buf,siz);
{$I+}
xBlockWrite:=IOresult;
End;

Function xSeek(f:xFile;ps:LongInt):Word;
Begin;
{$I-}
Seek(f.f,ps);
{$I+}
xSeek:=IOresult;
End;

Function xFileSize(f:xFile):LongInt;
Begin;
xFileSize:=FileSize(f.f);
End;

Function xFilePos(f:xFile):LongInt;
Begin;
xFilePos:=FilePos(f.f);
End;

Function xTruncate(f:xFile):Word;
Begin;
{$I-}
Truncate(f.f);
{$I+}
xTruncate:=IOresult;
End;

Function xClose(var f:xFile):Word;
Begin;
if (f.what=2) then Close(f.f);
xClose:=0;
fillchar(f,sizeof(f),0);
End;

Function xDiskInfo(var free,used,bad,blocksize:LongInt):Word;
Begin;
free:=DiskFree(0) div 512;
used:=DiskSize(0) div 512;
dec(used,free);
bad:=0;
blocksize:=512;
xDiskInfo:=0;
End;

Function xSetRight(fn:String;owner,rights:LongInt):Word;
Begin;
xSetRight:=1;
End;

Function xSetDate(fn:String;var create,modify:xDirEntryDateTimeRec):Word;
Begin;
xSetDate:=1;
End;

Function xMkLink(src,trg:String):Word;
Begin;
xMkLink:=1;
End;

Procedure BugOS_WriteCustomChar(c:char);
Begin;
if (c in [#0,#255]) then c:=#32;
if (c in [#0,#7,#8,#9,#10,#13,#255]) then c:='.';
write(c);
End;

Procedure BugOS_KernelUptime(var days,ticks,tickPerSec:LongInt);
Var a,b,c,d:Word;
Begin;
days:=0;
tickPerSec:=1;
GetTime(a,b,c,d);
ticks:=(((longint(a)*24)+longint(b))*60)+longint(c);
End;

Procedure Relequish;
Begin;
End;

Function pipeLineBegListen:Word;
Begin;
pipeLineBegListen:=0;
End;

Function pipeLineEndListen:Word;
Begin;
pipeLineEndListen:=0;
End;

Function pipeLineGetIncoming(var pip:LongInt):Word;
Begin;
pip:=0;
pipeLineGetIncoming:=1;
End;

Function pipeLineCreate(var pip:LongInt;pid,siz:LongInt;blockMode:Boolean):Word;
Begin;
pipeLineCreate:=1;
End;

Function pipeLineClose(pip:LongInt):Word;
Begin;
pipeLineClose:=1;
End;

Function pipeLineStats(pip:LongInt;var pid,rx,tx:LongInt):Word;
Begin;
pipeLineStats:=1;
pid:=1;
rx:=0;
tx:=0;
End;

Function pipeLineRecv(pip:LongInt;var buf;var siz:LongInt):Word;
Begin;
pipeLineRecv:=1;
siz:=0;
End;

Function pipeLineSend(pip:LongInt;var buf;siz:LongInt):Word;
Begin;
pipeLineSend:=1;
End;
