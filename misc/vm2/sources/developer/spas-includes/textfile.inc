Type
  xtText=record
    CurrentFileRec:xFile;
    ReadMode:Boolean;
    FileSize:LongInt;
    FilePos:LongInt;
    BufferDat:Array[1..1024] of Byte;
    BufferLen:Integer;
    BufferPos:Integer;
    end;



Procedure xtFlushBuf(var t:xtText);
Var w:LongInt;
Begin;
w:=t.BufferPos;
if t.ReadMode then begin;
  if (w>t.BufferLen) then w:=t.BufferLen;
  end else begin;
  if (w<>0) then xBlockWrite(t.CurrentFileRec,t.BufferDat,w);
  end;
inc(t.FilePos,w);
t.BufferLen:=0;
t.BufferPos:=0;
End;

Function xtOpen(var t:xtText;fn:String;Read:Boolean):Byte;
Var i:LongInt;
Begin;
xtOpen:=1;
if read then i:=xGenFilMod_r else i:=xGenFilMod_rw;
FillChar(t,SizeOf(t),0);
if (xOpen(t.CurrentFileRec,fn,i)<>0) then Exit;
t.ReadMode:=Read;
t.FileSize:=xFileSize(t.CurrentFileRec);
if Read then t.FilePos:=0 else t.FilePos:=t.FileSize;
t.BufferLen:=0;
t.BufferPos:=0;
xSeek(t.CurrentFileRec,t.FilePos);
xtOpen:=0;
End;

Procedure xtClose(var t:xtText);
Begin;
xtFlushBuf(t);
xClose(t.CurrentFileRec);
FillChar(t,SizeOf(t),0);
End;



Function xtGetPos(var t:xtText):LongInt;
Var w:LongInt;
Begin;
w:=t.BufferPos;
if t.ReadMode and (w>t.BufferLen) then w:=t.BufferLen;
xtGetPos:=t.FilePos+w;
End;

Function xtFileSize(var t:xtText):LongInt;
Begin;
xtFileSize:=t.FileSize;
End;

Procedure xtSetPos(var t:xtText;p:LongInt);
Begin;
xtFlushBuf(t);
t.FilePos:=p;
xSeek(t.CurrentFileRec,p);
End;

Procedure xtTruncate(var t:xtText);
Begin;
if t.ReadMode then Exit;
xtFlushBuf(t);
xTruncate(t.CurrentFileRec);
End;



Procedure xtPutOneChar(var t:xtText;c:Byte);
Begin;
if (t.BufferPos>=SizeOf(t.BufferDat)) then xtFlushBuf(t);
inc(t.BufferPos);
t.BufferDat[t.BufferPos]:=c;
End;

Function xtGetOneChar(var t:xtText):LongInt;
Var i:LongInt;
Begin;
if (t.BufferPos>=t.BufferLen) then begin;
  xtFlushBuf(t);
  i:=t.FileSize-t.FilePos;
  if (i>SizeOf(t.BufferDat)) then i:=SizeOf(t.BufferDat);
  if (i<1) then begin; xtGetOneChar:=-1;Exit; end;
  xBlockRead(t.CurrentFileRec,t.BufferDat,i);
  t.BufferLen:=i;
  end;
inc(t.BufferPos);
xtGetOneChar:=t.BufferDat[t.BufferPos];
End;

Procedure xtUndoLastRead(var t:xtText);
Begin;
if (t.BufferPos>0) then dec(t.BufferPos);
End;

Procedure xtWrite(var t:xtText;s:String);
Var
  i:LongInt;
  sb:array[0..1] of byte absolute s;
  sb0:byte absolute s;
Begin;
if t.ReadMode then Exit;
for i:=1 to sb0 do xtPutOneChar(t,sb[i]);
End;

Procedure xtWriteLn(var t:xtText;s:String);
Var
  i:LongInt;
  sb:array[0..1] of byte absolute s;
  sb0:byte absolute s;
Begin;
if t.ReadMode then Exit;
for i:=1 to sb0 do xtPutOneChar(t,sb[i]);
xtPutOneChar(t,13);
xtPutOneChar(t,10);
End;


Function xtSkipEOL(var t:xtText):Boolean;
Label f1;
Var a,b:LongInt;
Begin;
xtSkipEOL:=False;
a:=xtGetOneChar(t);
if (a<0) then Goto f1;
if not (a in [13,10]) then Goto f1;
xtSkipEOL:=True;
b:=xtGetOneChar(t);
if (b<0) then Goto f1;
if not (b in [13,10]) then Goto f1;
if (a=b) then Goto f1;
Exit;
f1:
xtUndoLastRead(t);
End;

Function xtRead(var t:xtText;mls:LongInt):String;
Label f1,f2;
Const max=255;
Var
  a:LongInt;
  b:String;
  bb:array[0..1] of byte absolute b;
  bb0:byte absolute b;
Begin;
bb0:=0;
if not t.ReadMode then Goto f2;
if (mls>max) then mls:=max;
f1:
if (bb0>=mls) then Goto f2;
a:=xtGetOneChar(t);
if (a<0) then Goto f2;
if (a in [13,10]) then begin;
  xtUndoLastRead(t);
  Goto f2;
  end;
inc(bb0);
bb[bb0]:=a;
Goto f1;
f2:
xtRead:=b;
End;

Function xtReadLn(var t:xtText;mls:LongInt):String;
Begin;
if not t.ReadMode then Exit;
xtReadLn:=xtRead(t,mls);
while (xtRead(t,255)<>'') do;
xtSkipEOL(t);
End;

Function xtEOL(var t:xtText):Boolean;
Var a:LongInt;
Begin;
if not t.ReadMode then Exit;
a:=xtGetOneChar(t);
xtUndoLastRead(t);
xtEOL:=(a in [13,10]) or (a<0);
End;

Function xtEOF(var t:xtText):Boolean;
Var a:LongInt;
Begin;
if not t.ReadMode then Exit;
a:=xtGetOneChar(t);
xtUndoLastRead(t);
xtEOF:=(a<0);
End;
