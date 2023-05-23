{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc datetime.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc bugos.inc}
{$sysinc param.inc}
{$sysinc hex.inc}

{$include tagger.inc}
{$include output.inc}

Const
  PrgTxt='mirror v1.0';
  filesPerDir=1000;
Type
  indexMemoryType=array[1..1] of LongInt;
Var
  indexMemoryData:^indexMemoryType;

Procedure resizeMem(n:LongInt);
Var
  p:Pointer;
  i:LongInt;
Begin;
i:=n*sizeof(n);
if (ExtendedMemoryResize(p,i)<i) then immErr('error allocating memory!');
outputLin:=n;
indexMemoryData:=p^;
End;

Function getExtension(a:String):String;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
i:=pos('&',a);
if (i>0) then a:=copy(a,1,i-1);
i:=pos('?',a);
if (i>0) then a:=copy(a,1,i-1);
o:=666;
for i:=1 to ab0 do if (ab[i]=47) then o:=i;
a:=copy(a,o+1,666);
o:=666;
for i:=1 to ab0 do if (ab[i]=46) then o:=i;
a:=copy(a,o,6);
getExtension:=kicsi(a);
End;

Function expandNumber(i:LongInt):String;
Var a:String;
Begin;
a:=BStr(i);
while (length(a)<4) do a:='0'+a;
expandNumber:=a;
End;

Function getLinkName(i:LongInt):String;
Begin;
getLinkName:='../dir'+expandNumber(i div filesPerDir)+'/file'+expandNumber(i mod filesPerDir);
End;

Function getFileName(i:LongInt):String;
Begin;
getFileName:='dir'+expandNumber(i div filesPerDir)+'\file'+expandNumber(i mod filesPerDir);
End;

Function readOneOrdered(i:LongInt):String;
Var a:String;
Begin;
xSeek(outputData,indexMemoryData^[i]*sizeof(a));
xBlockRead(outputData,a,sizeof(a));
readOneOrdered:=a;
End;

Function readOneUnorder(i:LongInt):String;
Var a:String;
Begin;
xSeek(outputData,i*sizeof(a));
xBlockRead(outputData,a,sizeof(a));
readOneUnorder:=a;
End;

Procedure checkLinkOrder;
Var
  i:LongInt;
  a,b:String;
Begin;
b:=readOneOrdered(1);
for i:=2 to outputLin do begin;
  a:=readOneOrdered(i);
  if (a<b) then immErr('error in link order!');
  b:=a;
  end;
End;

Function ConvertOneLink(b:String):String;
Label f1,f2;
Var
  pb,pc,pe:LongInt;
  a:String;
  i,o:LongInt;
Begin;
b:=convertUrlString(b);
ConvertOneLink:=b;
if (kicsi(copy(b,1,length(currentUsr)))<>currentUsr) then exit;
if (pos('|'+kicsi(getExtension(b))+'|',currentTit)<>0) then exit;
b:=currentUsr+copy(b,length(currentUsr)+1,666);
if (outputLin<1) then begin; pc:=1;goto f2; end;
a:=readOneOrdered(1);
if (b<a) then begin; pc:=1;goto f2; end;
if (b=a) then begin; pc:=1;goto f1; end;
a:=readOneOrdered(outputLin);
if (b>a) then begin; pc:=outputLin+1;goto f2; end;
if (b=a) then begin; pc:=outputLin;goto f1; end;
pb:=1;
pe:=outputLin;
while (1=1) do begin;
  i:=pe-pb+1;
  pc:=(i div 2)+pb;
  a:=readOneOrdered(pc);
  if (a=b) then goto f1;
  if (a<b) then pb:=pc else pe:=pc;
  if (i>2) then continue;
  if (a<b) then inc(pc);
  goto f2;
  end;
f2:
xSeek(outputData,outputLin*sizeof(b));
xBlockWrite(outputData,b,sizeof(b));
resizeMem(outputLin+1);
move(indexMemoryData^[pc],indexMemoryData^[pc+1],(outputLin-pc)*sizeof(pc));
indexMemoryData^[pc]:=outputLin-1;
f1:
ConvertOneLink:=getLinkName(indexMemoryData^[pc])+getExtension(b);
End;

Procedure convertHtml(var src,trg:xtText);
Label f1,f2;
Var
  i,o,p:LongInt;
  tag:oneTagRecord;
  a:String;

Procedure doOneTag(a:String);
Var i,o:LongInt;
Begin;
i:=findOneTagEntry(tag,a);
if (i<1) then exit;
tag.d[i].v:=ConvertOneLink(tag.d[i].v);
inc(p);
End;

Begin;
currentBas:=convertUrlString(currentBas);
xtWrite(trg,'<html><!-- original: ');
xtWrite(trg,currentBas);
xtWriteLn(trg,' -->');
f1:
i:=xtGetOneChar(src);
if (i<1) then exit;
if (i<>60) then begin; xtPutOneChar(trg,i);goto f1; end;
o:=xtGetPos(src);
readUpOneTag(src,tag);
a:='|'+tag.t+'|';
p:=0;
if (pos(a,'|html|')<>0) then goto f1;
if (pos(a,'|param|')<>0) then begin;
  a:=kicsi(getOneTagValue(tag,'valuetype'));
  if (a<>'ref') and (a<>'object') then goto f2;
  doOneTag('value');
  end;
if (pos(a,'|meta|')<>0) then begin;
  if (kicsi(getOneTagValue(tag,'http-equiv'))<>'refresh') then goto f2;
  o:=findOneTagEntry(tag,'content');
  if (o<1) then goto f2;
  i:=pos('=',a);
  a:=copy(a,1,i)+ConvertOneLink(copy(a,i+1,666));
  inc(p);
  end;
if (pos(a,'|base|')<>0) then begin;
  a:=convertUrlString(getOneTagValue(tag,'href'));
  if (a<>'') then currentBas:=a;
  goto f1;
  end;
if (pos(a,'|form|')<>0) then doOneTag('action');
if (pos(a,'|head|')<>0) then doOneTag('profile');
if (pos(a,'|body|layer|')<>0) then doOneTag('background');
if (pos(a,'|div|span|marquee|')<>0) then doOneTag('datasrc');
if (pos(a,'|blockquote|q|ins|del|')<>0) then doOneTag('cite');
if (pos(a,'|a|link|area|')<>0) then doOneTag('href');
if (pos(a,'|img|frame|iframe|input|layer|script|bgsound|embed|')<>0) then doOneTag('src');
if (pos(a,'|img|frame|iframe|')<>0) then doOneTag('longdesc');
if (pos(a,'|img|input|object|')<>0) then doOneTag('usemap');
if (pos(a,'|object|')<>0) then doOneTag('classid');
if (pos(a,'|object|')<>0) then doOneTag('data');
if (pos(a,'|applet|')<>0) then doOneTag('code');
f2:
if (pos(a,'|!--|!doctype|')<>0) then p:=0;
xtPutOneChar(trg,60);
if (p<>0) then begin; writeOneTag(trg,tag);goto f1; end;
p:=xtGetPos(src);
xtSetPos(src,o);
for i:=1 to p-o do xtPutOneChar(trg,xtGetOneChar(src));
goto f1;
End;



Function doOneRound:Boolean;
Var
  a,b:String;
  i,o,p:LongInt;
  t1,t2:xtText;
  bb:Boolean;
  w:Word;
Begin;
doOneRound:=True;
inc(currentCol);
i:=outputLin-LinkListNum;
WriteLn('');
WriteLn('round #'+BStr(currentCol)+', i have to process '+BStr(i)+' files ('+BStr(outputLin)+' already done)');
if (i<1) then begin; doOneRound:=False;exit; end;
LineListNum:=outputLin;
xCreate(TemporaryCmds);
if (xtOpen(t1,TemporaryCmds,false)<>0) then immErr('error opening command file!');
xtSetPos(t1,0);
xtWriteLn(t1,'browser ('+PrgTxt+' for BugOS)');
xtWriteLn(t1,'automode 1');
if (copy(currentUsr,1,pos(':',currentUsr)-1)='http') then a:='clear' else a:='secure';
xtWriteLn(t1,a);
for p:=LinkListNum to outputLin-1 do begin;
  b:=readOneUnorder(p);
  a:=getFileName(p)+getExtension(b);
  xtWriteLn(t1,'get3 '+a+' '+b);
  xMkDir(copy(a,1,pos('\',a)-1));
  end;
xtWriteLn(t1,'quit');
xtTruncate(t1);
xtClose(t1);
if (xExec(myFavoritHttpClnt,TemporaryCmds,w)<>0) then w:=1;
if (w<>0) then immErr('error running client!');
WriteLn('processing files...');
for p:=LinkListNum to outputLin-1 do begin;
  currentBas:=readOneUnorder(p);
  a:=getFileName(p)+getExtension(currentBas);
  WriteLn(currentBas);
  if (xtOpen(t1,a,true)<>0) then immErr('error opening file!');
  bb:=not isThisHtmlFile(t1);
  xtClose(t1);
  if bb then continue;
  a:=xGetDir+a;
  b:=a+'.bak';
  xErase(b);
  if (xRename(a,b)<>0) then immErr('error renaming file!');
  xCreate(a);
  if (xtOpen(t1,b,true)<>0) then immErr('error opening source!');
  if (xtOpen(t2,a,false)<>0) then immErr('error opening target!');
  convertHtml(t1,t2);
  xtClose(t2);
  xtClose(t1);
  end;
LinkListNum:=LineListNum;
End;



Var
  t1,t2:xtText;
  i,o:LongInt;
  a:String;
BEGIN;
WriteLn(prgTxt+', done by Mc at '#%date' '#%time'.');
currentBas:=paramStr(1);
if (currentBas='') then immErr('using: mirror.code <url> [|ext|ext|] [base]');

currentBas:=convertUrlString(currentBas);
currentTit:='';
currentUsr:='';
resizeMem(0);
xErase(TemporaryData);
xCreate(TemporaryData);
if (xOpen(outputData,TemporaryData,xGenFilMod_rw)<>0) then immErr('error opening tempoorary file!');
ConvertOneLink(currentBas);
currentTit:=paramStr(2);
currentUsr:=paramStr(3);
if (currentUsr='') then currentUsr:=currentBas;
currentUsr:=kicsi(convertUrlString(currentUsr));
LinkListNum:=0;
currentCol:=0;

WriteLn('download: '+currentBas);
WriteLn('base url: '+currentUsr);
WriteLn(' not ext: '+currentTit);

while doOneRound do;

WriteLn('erasing temporary files...');
checkLinkOrder;
resizeMem(0);
xClose(outputData);
xErase(TemporaryCmds);
xErase(TemporaryData);
END.