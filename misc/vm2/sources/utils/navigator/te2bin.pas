{$stack 63k}
{$heap 127m}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc alap.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}

{$define name}

{$include bin.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

{$undef dup}
{$define dup}

{$include repair.inc}



Procedure swap(var a,b:LongInt);
Var c:LongInt;
Begin;
c:=a;
a:=b;
b:=c;
End;

Function getNextWord(var a:String;s:String):String;
Var i:LongInt;
Begin;
if (s='') then s:=' ';
i:=pos(s,a);
if (i<1) then i:=666;
getNextWord:=copy(a,1,i-1);
a:=copy(a,i+length(s),666);
End;

Function normalizeLine(a:String):String;
Begin;
a:=' '+a+' ';
kicserel(#9,' ',a);
kicserel(#8,' ',a);
kicserel(#7,' ',a);
kicserel(#0,' ',a);
kicserel(#255,' ',a);
kicserel('(',' ',a);
kicserel(')',' ',a);
kicserel('[',' ',a);
kicserel(']',' ',a);
kicserel(': ',':',a);
kicserel(' :',':',a);
kicserel(', ',',',a);
kicserel(' ,',',',a);
kicserel('  ',' ',a);
a:=copy(a,2,length(a)-2);
normalizeLine:=kicsi(a);
End;




Function extractAdverter(b:String):String;
Var a:String;
Begin;
extractAdverter:='';
if (getNextWord(b,':')<>'igp id') then exit;
extractAdverter:=getNextWord(b,',');
End;



Function extractLink(b:String):String;
Var
  a:String;
  i:LongInt;
Begin;
extractLink:='';
if (copy(getNextWord(b,':'),1,4)<>'link') then exit;
getNextWord(b,',');
a:=getNextWord(b,',');
b:=getNextWord(a,':');
if (b<>'nbr igp id') and (b<>'dr') then exit;
extractLink:=a;
End;



Label f1;
Var
  t:xtText;
  f:xFile;
  a,b:String;
  i,o:LongInt;
  nd:oneNodeRecord;
  cd:oneConnRecord;
  md:oneNameRecord;
BEGIN;
writeLn('te2bin v1.0, done by Mc at '#%date' '#%time'.');
a:=paramStr(1);
if (a='') then immErr('using: prog.code <text>');
writeln('reading '+a+'...');
xtOpen(t,a,true);

nodeN:=0;
nameN:=0;
connN:=0;

writeLn('reading nodes...');
xtSetPos(t,0);
fillchar(nd,sizeof(nd),0);
while not xtEOF(t) do begin;
  b:=normalizeLine(xtReadLn(t,666));
  a:=extractAdverter(b);
  if (a='') then continue;
  nd.nam:=addOneName(a,nodeN+1,0);
  addOneNode(a,nd);
  end;
WriteLn(BStr(nodeN)+' nodes found.');

WriteLn('sorting nodes...');
sortNodesName;

writeLn('reading links...');
xtSetPos(t,0);
fillchar(cd,sizeof(cd),0);
cd.s:=1;
while not xtEOF(t) do begin;
  b:=normalizeLine(xtReadLn(t,666));
  a:=extractAdverter(b);
  if (a<>'') then begin;
    i:=findOneNode(a);
    if (i<1) then immErr('invalid advertising router!');
    cd.a:=i;
    continue;
    end;
  a:=extractLink(b);
  if (a='') then continue;
  i:=findOneNode(a);
  if (i<1) then immErr('invalid link peer!');
  cd.b:=i;
  addOneConn(cd);swap(cd.a,cd.b);
  addOneConn(cd);swap(cd.a,cd.b);
  end;
WriteLn(BStr(connN)+' conns readed.');
xtClose(t);

WriteLn('preprocessing...');
sortConns;
sortNodesCoord;
sortNames;
findNodeConns;
orderEntries;

a:=paramStr(1)+'.bin';
writeln('writing '+a+'...');

xErase(a);
xCreate(a);
xOpen(f,a,xGenFilMod_rw);

WriteLn('writing data...');

writeDownTables(f);
xClose(f);

xtOpen(t,a,false);

WriteLn('writing names...');
for o:=1 to nameN do begin;
  a:=nameS[o];
  xtWrite(t,a);
  xtWrite(t,#0);
  end;

xtClose(t);
END.