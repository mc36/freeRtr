Function ParseUpOneFile(name:String):String;
Label f1,f2;
Var
  t:xtText;
  a,b,c:String;
  i,o,p:LongInt;
  lstW,curW:String;
  lstP,curP:LongInt;

Function oneImp:Boolean;
Begin;
oneImp:=False;
p:=xtGetPos(t);
a:=getQuotedWord(t);
if (a=';') then exit;
curP:=p;
while (a<>'FROM') do begin;
  curP:=xtGetPos(t);
  a:=getQuotedWord(t);
  if (a='') then exit;
  end;
c:=getQuotedWord(t);
lstP:=xtGetPos(t);
oneImp:=True;
c:=ParseUpOneFile(c);
WriteLn('continuing '+name+'...');
xtSetPos(t,p);
while (xtGetPos(t)<curP) do begin;
  a:=getQuotedWord(t);
  if (a='') then exit;
  if (a=',') then continue;
  b:=mibCacheFind(c+#0+a);
  if (b='') then begin;
    WriteLn('failed to import '+a+'!');
    continue;
    end;
  mibCacheAdd(name+#0+a,b,0);
  end;
xtSetPos(t,lstP);
End;

Procedure skip4equ;
Label f1;
Begin;
f1:
a:=getQuotedWord(t);
if (a='') then exit;
if (a<>':') then goto f1;
if (getQuotedWord(t)<>':') then goto f1;
if (getQuotedWord(t)<>'=') then goto f1;
End;

Procedure readOID;
Label f1,f2;
Begin;
if (getQuotedWord(t)<>'{') then begin;
  WriteLn('bad oid definition for '+lstW+'!');
  exit;
  end;
c:='';
f1:
a:=getQuotedWord(t);
if (a='') then exit;
if (a='}') then begin;
  if (copy(c,1,1)='.') then c:=copy(c,2,666);
  mibCacheAdd(name+#0+lstW,c,lstP);
  exit;
  end;
i:=BVal(a);
if (BStr(i)=a) then begin; c:=c+'.'+a;goto f1; end;
b:=mibCacheFind(name+#0+a);
if (b='') then begin;
  if (a='ccitt') then b:='.0';
  if (a='iso') then b:='.1';
  if (a='joint-iso-ccitt') then b:='.2';
  end;
if (b='') then begin;
  b:=getQuotedWord(t);
  if (b<>'(') then goto f2;
  b:=getQuotedWord(t);
  i:=BVal(b);
  if (BStr(i)<>b) then goto f2;
  b:=getQuotedWord(t);
  if (b<>')') then goto f2;
  c:=c+'.'+BStr(i);
  mibCacheAdd(name+#0+a,c,0);
  goto f1;
  f2:
  WriteLn('oid '+a+' not found for '+lstW+'!');
  while (b<>'}') do begin;
    b:=getQuotedWord(t);
    if (b='') then exit;
    end;
  exit;
  end;
c:=c+b;
goto f1;
End;


Begin;
ParseUpOneFile:='';
a:=tryToOpenFile(t,name);
if (a='') then begin;
  WriteLn('failed to open '+name+'!');
  exit;
  end;
name:=a;
WriteLn('parsing '+name+'...');
ParseUpOneFile:=name;
a:=#0+name;
if (mibCacheFind(a)<>'') then begin;
  xtClose(t);
  exit;
  end;
mibCacheAdd(a,'.?',0);
f1:
curW:='';
curP:=0;
f2:
lstW:=curW;
lstP:=curP;
curP:=xtGetPos(t);
curW:=getQuotedWord(t);
if (curW='') then begin;
  xtClose(t);
  exit;
  end;
if (curW='IMPORTS') then begin;
  while oneImp do;
  goto f1;
  end;
if (curW='EXPORTS') then begin;
  repeat
    a:=getQuotedWord(t);
    if (a='') then goto f1;
    until (a=';');
  goto f1;
  end;
if (curW='MODULE-IDENTITY') then begin; skip4equ;readOID;goto f1; end;
if (curW='OBJECT-TYPE') then begin; skip4equ;readOID;goto f1; end;
if (curW='OBJECT') then if (getQuotedWord(t)='IDENTIFIER') then begin; lstP:=0;skip4equ;readOID;goto f1; end;

goto f2;
End;



Function ParseUpOneEntry(p,linmax:LongInt;var buffer):LongInt;
Label f1,f2,vege;
Const keywords='|SYNTAX|ACCESS|MAX-ACCESS|STATUS|DESCRIPTION|LAST-UPDATED|'+
 'ORGANIZATION|CONTACT-INFO|REVISION|INDEX|REFERENCE|';
Var
  buf:array[1..1] of byte absolute buffer;
  siz,lns:LongInt;
  i,o:LongInt;
  a:String;
  t:xtText;

Procedure fin;
Begin;
inc(siz);
buf[siz]:=0;
lns:=0;
End;

Procedure add(a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
if (lns=0) and (ab0=1) and (ab[1]=32) then exit;
if (lns+ab0>linmax) then fin;
move(ab[1],buf[siz+1],ab0);
inc(siz,ab0);
inc(lns,ab0);
End;

Procedure quot;
Label f1;
Begin;
f1:
a:=getNextWord(t);
if (a='') then exit;
if (a='"') then exit;
if (a<>'\') then begin;
  add(' ');
  add(a);
  goto f1;
  end;
a:=getNextWord(t);
add(' ');
add(a);
goto f1;
End;

Begin;
fillchar(t,sizeof(t),0);
siz:=0;
lns:=0;
if (p<1) or (p>mibEntryNum) then begin;
  add('entry not found!');
  goto vege;
  end;
a:=mibEntryDat^[p].nam;
i:=pos(#0,a);
add('name: '+copy(a,i+1,666));fin;
add('oid: '+mibEntryDat^[p].oid);fin;
a:=copy(a,1,i-1);
i:=mibEntryDat^[p].pos;
if (i=0) then begin;
  add('imported from other mib!');
  goto vege;
  end;
add('file: '+a+' @'+BStr(i));fin;
if (xtOpen(t,mibPath+a,true)<>0) then begin;
  add('error opening mib!');
  goto vege;
  end;
xtSetPos(t,i);
getNextWord(t);
fin;
f1:
a:=getNextWord(t);
if (a='') then goto vege;
if (a='"') then begin;
  quot;
  goto f1;
  end;
if (a=':') then begin;
  add(' ');
  a:=a+getNextWord(t);
  if (a<>'::') then begin; add(a);goto f1; end;
  a:=a+getNextWord(t);
  if (a<>'::=') then begin; add(a);goto f1; end;
  goto vege;
  end;
if (pos('|'+a+'|',keywords)<>0) then fin;
add(' ');
add(a);
goto f1;

vege:
fin;
xtClose(t);
ParseUpOneEntry:=siz;
End;
