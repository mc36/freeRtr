Const
  myFavoritSnmpClnt='c:\internet\client\snmp.code';
  extensionCmd='cmd';
  extensionRes='rep';
Type
  OneEntryRecord=record
    nam:String;                 {name of this entry}
    oid:String;                 {oid of this entry}
    pos:LongInt;                {position in file}
    end;
  AllEntriesRecord=array[1..1] of OneEntryRecord;
Var
  mibEntryDat:^AllEntriesRecord;
  mibEntryNum:LongInt;
  mibPath:String;
  targetServer:String;
  currentObject:String;
  tempBegin:String;
  descrBuf:array[1..1024*64] of byte;
  descrSiz,descrBeg:LongInt;
  oidCur,oidBeg:LongInt;



Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;


Procedure memoryResize(n:LongInt);
Var
  p:Pointer;
  i:LongInt;
Begin;
i:=n*sizeof(OneEntryRecord);
if (ExtendedMemoryResize(p,i)<i) then immErr('out of memory!');
mibEntryNum:=n;
mibEntryDat:=p^;
End;



Procedure mibCacheInit;
Begin;
memoryResize(0);
End;



Procedure mibCacheAdd(nam,oid:String;pos:LongInt);
Begin;
memoryResize(mibEntryNum+1);
mibEntryDat^[mibEntryNum].nam:=nam;
mibEntryDat^[mibEntryNum].oid:=oid;
mibEntryDat^[mibEntryNum].pos:=pos;
End;



Function mibCacheSearch(nam:String):LongInt;
Var i:LongInt;
Begin;
for i:=mibEntryNum downto 1 do if (mibEntryDat^[i].nam=nam) then begin;
  mibCacheSearch:=i;
  exit;
  end;
mibCacheSearch:=0;
End;



Function mibCacheFind(nam:String):String;
Var i:LongInt;
Begin;
i:=mibCacheSearch(nam);
if (i=0) then mibCacheFind:='' else mibCacheFind:=mibEntryDat^[i].oid;
End;



Procedure mibCacheTrunc(nam:String);
Var i,o:LongInt;
Begin;
nam:=nam+#0;
o:=0;
for i:=1 to mibEntryNum do if (copy(mibEntryDat^[i].nam,1,length(nam))=nam) then begin;
  inc(o);
  mibEntryDat^[o]:=mibEntryDat^[i];
  end;
memoryResize(o);
End;



Function mibCacheSave(a:String):Boolean;
Var
  t:xtText;
  i:LongInt;
Begin;
mibCacheSave:=True;
xErase(a);
xCreate(a);
if (xtOpen(t,a,false)<>0) then exit;
for i:=1 to mibEntryNum do xtWriteLn(t,BStr(mibEntryDat^[i].pos)+' '+mibEntryDat^[i].nam+' '+mibEntryDat^[i].oid);
xtClose(t);
mibCacheSave:=False;
End;


Function getNextWord(var t:xtText):String;
Label f1,f2;
Var
  a:string;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
getNextWord:='';
f1:
i:=xtGetOneChar(t);
if (i<0) then exit;
if (i in [13,10,0,9,32,255]) then goto f1;
ab0:=1;
ab[1]:=i;
if (i=$2d) then begin;
  i:=xtGetOneChar(t);
  xtUndoLastRead(t);
  if (i<>$2d) then goto f2;
  while not xtEOL(t) do xtRead(t,666);
  xtReadLn(t,666);
  goto f1;
  end;
if (i in [$30..$39]) then begin;
  repeat
    i:=xtGetOneChar(t);
    inc(ab0);
    ab[ab0]:=i;
    until not (i in [$30..$39]);
  xtUndoLastRead(t);
  dec(ab0);
  goto f2;
  end;
if (i in [$41..$5a,$61..$7a]) then begin;
  repeat
    i:=xtGetOneChar(t);
    inc(ab0);
    ab[ab0]:=i;
    until not (i in [$30..$39,$41..$5a,$61..$7a,$2d,$5f]);
  xtUndoLastRead(t);
  dec(ab0);
  goto f2;
  end;
f2:
getNextWord:=a;
End;



Function getQuotedWord(var t:xtText):String;
Label f1;
Var a,b:String;
Begin;
a:=getNextWord(t);
if (a<>'"') then begin;
  getQuotedWord:=a;
  exit;
  end;
b:='';
f1:
a:=getNextWord(t);
if (a='"') or (a='') then begin;
  b:=copy(b,2,666);
  getQuotedWord:='"'+b;
  exit;
  end;
if (a='\') then a:=getNextWord(t);
b:=b+' '+a;
goto f1;
End;



Function tryToOpenFile(var t:xtText;n:String):String;
Label f1;
Const
  extSiz=11;
  extDat:array[1..extSiz] of string=('',
    '.mib','_mib','-mib','.my','_my','-my',
    '-v1smi.mib','-v1smi.my','smi.mib','smi.my'
    );
Var
  a:String;
  i:LongInt;
Begin;
for i:=1 to extSiz do begin;
  a:=n+extDat[i];
  if (xtOpen(t,mibPath+a,true)=0) then goto f1;
  end;
a:='';
f1:
tryToOpenFile:=kicsi(a);
End;
