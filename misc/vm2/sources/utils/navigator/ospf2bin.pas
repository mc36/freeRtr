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
kicserel(': ',':',a);
kicserel(' :',':',a);
kicserel('  ',' ',a);
a:=copy(a,2,length(a)-2);
normalizeLine:=kicsi(a);
End;




Function extractAdverter(b:String):String;
Var a:String;
Begin;
extractAdverter:='';
if (getNextWord(b,':')<>'advertising router') then exit;
extractAdverter:=getNextWord(b,' ');
End;



Function extractAreaBegin(b:String):String;
Var a:String;
Begin;
extractAreaBegin:='';
if (getNextWord(b,' ')<>'router') then exit;
if (getNextWord(b,' ')<>'link') then exit;
if (getNextWord(b,' ')<>'states') then exit;
if (getNextWord(b,' ')<>'area') then exit;
extractAreaBegin:=getNextWord(b,' ');
End;


Function extractAreaBorder(b:string):Boolean;
Begin;
extractAreaBorder:=(b='area border router');
End;


Function extractLink(var t:xtText;b:String):String;
Var
  typ,spd:LongInt;
  rtr,int:String;
  a:String;
  i:LongInt;
Begin;
extractLink:='';
if (getNextWord(b,':')<>'link connected to') then exit;
typ:=0;
if (pos('another router',b)<>0) then typ:=1;
if (pos('transit network',b)<>0) then typ:=2;
if (typ<1) then exit;
spd:=0;
rtr:='';
int:='';
while not xtEOF(t) do begin;
  b:=normalizeLine(xtReadLn(t,666));
  if (b='') then break;
  a:=getNextWord(b,':');
  if (a='tos 0 metrics') or (a='link metric') then begin;
    spd:=BVal(getNextWord(b,' '));
    continue;
    end;
  kicserel('link id ','',a);
  kicserel('link data ','',a);
  kicserel(' dr ',' ',a);
  if (a='neighbor interface id') or (a='router interface address') then begin;
    int:=getNextWord(b,' ');
    continue;
    end;
  if (a='neighboring router id') or (a='designated router address') or (a='neighbor router id') then begin;
    rtr:=getNextWord(b,' ');
    continue;
    end;
  end;
if (typ=1) then a:=rtr else begin;
  i:=BVal(int);
  if (i=0) then a:='' else a:=BStr(i);
  a:='net'+a+'-'+rtr;
  end;
a:='1234'+a;
move(spd,a[1],sizeof(spd));
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
writeLn('ospf2bin v1.0, done by Mc at '#%date' '#%time'.');
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
o:=0;
while not xtEOF(t) do begin;
  b:=normalizeLine(xtReadLn(t,666));
  if (extractAreaBegin(b)='0') then begin; o:=1;continue; end;
  a:=extractAdverter(b);
  if (a<>'') then goto f1;
  a:=copy(extractLink(t,b),5,666);
  if (copy(a,1,3)='net') then goto f1;
  continue;
  f1:
  nd.nam:=addOneName(a,nodeN+1,0);
  addOneNode(a,nd);
  end;
if (o=0) then begin;
  nd.nam:=addOneName('backbone',nodeN+1,0);
  addOneNode('a0',nd);
  end;
WriteLn(BStr(nodeN)+' nodes found.');

WriteLn('sorting nodes...');
sortNodesName;

writeLn('reading links...');
xtSetPos(t,0);
fillchar(cd,sizeof(cd),0);
while not xtEOF(t) do begin;
  b:=normalizeLine(xtReadLn(t,666));
  a:=extractAdverter(b);
  if (a<>'') then begin;
    i:=findOneNode(a);
    if (i<1) then immErr('invalid advertising router!');
    cd.a:=i;
    continue;
    end;
  if extractAreaBorder(b) then begin;
    i:=findOneNode('a0');
    if (i<1) then continue;
    cd.b:=i;
    cd.s:=9999999;
    addOneConn(cd);swap(cd.a,cd.b);
    addOneConn(cd);swap(cd.a,cd.b);
    continue;
    end;
  a:=extractLink(t,b);
  if (a='') then continue;
  move(a[1],i,sizeof(i));
  cd.s:=i;
  a:=copy(a,5,666);
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