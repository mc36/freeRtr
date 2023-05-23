Const
  TemporaryExt='tmpb';
  TemporaryText='lines.'+TemporaryExt;
  TemporaryLine='linepos.'+TemporaryExt;
  TemporaryData='links.'+TemporaryExt;
  TemporaryPost='posting.'+TemporaryExt;
  TemporarySlct='select.'+TemporaryExt;
  TemporaryCmds='commands.'+TemporaryExt;
  TemporaryUrls='url.'+TemporaryExt;
  TemporaryHtml='source.'+TemporaryExt;
  HistoryFileName='history';
  BookmarkFileName='bookmark';
  LinkListMax=256;
  LineListMax=512;
  myFavoritEditor='c:\utils\editor.code';
  myFavoritHttpClnt='c:\internet\client\http.code';
  myFavoritFtpClnt='c:\internet\client\ftp.code';
  myFavoritGopherClnt='c:\internet\client\gopher.code';
Type
  oneLinkRecord=record
    beg:LongInt;
    siz:LongInt;
    ps:LongInt;
    end;
  oneOutputLineRecord=record
    p:LongInt;
    s:Word;
    c:Word;
    end;
Var
  dataPath:String;
  sourceFile:xtText;
  outputText:xFile;
  outputLine:xFile;
  outputData:xFile;
  outputBuf:array[1..2*1024] of byte;
  outputSiz:LongInt;
  outputChr:LongInt;
  outputLin:LongInt;
  screenSizX:Word;
  screenSizY:Word;
  currentUsr:String;
  currentPwd:String;
  currentPrx:String;
  currentTit:String;
  currentUrl:String;
  currentBas:String;
  currentFnd:String;
  currentCol:LongInt;
  LinkListDat:array[1..LinkListMax] of oneLinkRecord;
  LineListDat:array[1..LineListMax] of record c:byte;a:byte; end;
  LinkListNum:LongInt;
  LineListNum:LongInt;


{types are:
1-color
2-text
3-link
4-formBeg
5-formEnd
6-submit
7-reset
8-button
9-input
10-file
11-checkbox
12-radio
13-selectBeg
14-selectEnd
15-selectDat
16-textarea
}



Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Function fileExists(a:String):Boolean;
Var f:xFile;
Begin;
fileExists:=false;
if (xOpen(f,a,xGenFilMod_r)<>0) then exit;
xClose(f);
fileExists:=true;
End;

Function conv2hex(i:LongInt):String;
Begin;
conv2hex:=byte2hextype(i shr 24)+byte2hextype(i shr 16)+byte2hextype(i shr 8)+byte2hextype(i);
End;

Function repairPath(a:String):String;
Begin;
if (copy(a,length(a),666)<>'\') then a:=a+'\';
repairPath:=a;
End;

Procedure getSpacesFromBegin(var a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
while (ab0>0) and (ab[1]=32) do a:=copy(a,2,666);
End;

Function getNextWord(var a:String):String;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
while (ab0>0) and (ab[1]=32) do a:=copy(a,2,666);
o:=666;
for i:=ab0 downto 1 do if (ab[i]=32) then o:=i;
getNextWord:=copy(a,1,o-1);
a:=copy(a,o+1,666);
End;

Function getCurrentDate:String;
Var a,b,c:Word;
function x(i:Word):String;var a:string; begin;a:=bstr(i);while (length(a)<2) do a:='0'+a;x:=a; end;
Begin;
xGetDate(a,b,c);
getCurrentDate:=x(a)+'-'+x(b)+'-'+x(c);
End;

Function getCurrentTime:String;
Var a,b,c:Word;
function x(i:Word):String;var a:string; begin;a:=bstr(i);while (length(a)<2) do a:='0'+a;x:=a; end;
Begin;
xGetTime(a,b,c);
getCurrentTime:=x(a)+':'+x(b)+':'+x(c);
End;

Function findLastSpace(a:String):LongInt;
Label f1;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
for i:=ab0 downto 1 do if (ab[i] in [32,44,46]) then goto f1;
i:=0;
f1:
findLastSpace:=i;
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

Procedure fillUpString(var a:String;max:LongInt);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
if (max<0) then max:=0;
if (max>255) then max:=255;
if (ab0>=max) then begin; ab0:=max;exit; end;
dec(max,ab0);
fillchar(a[ab0+1],max,32);
inc(ab0,max);
End;

Function FindNextWordStop(a:String;p:Word):Word;
Var i,o:Word;
Begin;
o:=length(a)+1;
for i:=length(a) downto p+1 do if (a[i]=' ') then o:=i;
while (copy(a,o,1)=' ') do inc(o);
FindNextWordStop:=o;
End;

Function FindPrevWordStop(a:String;p:Word):Word;
Var i,o:Word;
Begin;
if (p<1) then p:=1;
while (copy(a,p,1)=' ') and (p>1) do dec(p);
o:=0;
for i:=1 to p-1 do if (a[i]=' ') then o:=i;
FindPrevWordStop:=o+1;
End;



Function convertUrlString(a:String):String;
Label f1,f2,f3;
Var
  b,c:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  bb:array[0..1] of byte absolute b;
  bb0:byte absolute b;
  cb:array[0..1] of byte absolute c;
  cb0:byte absolute c;
  ub:array[0..1] of byte absolute currentBas;
  ub0:byte absolute currentBas;
  i,o,p:LongInt;
  httpLink:Boolean;
Begin;
cb0:=0;
if (ab0=0) then goto f3;
if (pos(':',a)<>0) then goto f1;
if (ab[1]=47) then begin;
  o:=1;
  for i:=ub0 downto pos(':',currentBas)+3 do if (ub[i]=47) then o:=i;
  a:=copy(currentBas,1,o-1)+a;
  goto f1;
  end;
o:=0;
for i:=1 to ub0 do if (ub[i]=47) then o:=i;
a:=copy(currentBas,1,o)+a;
f1:
httpLink:=(kicsi(copy(a,1,4))='http');
i:=pos(':',a);
if (ab[i+1]<>47) or (ab[i+1]<>47) then a:=copy(a,1,i-1)+'://'+copy(a,i+1,666);
if httpLink then begin;
  o:=0;
  for i:=1 to ab0 do if (ab[i]=35) then o:=i;
  if (o>0) then a:=copy(a,1,o-1);
  if (ab0=0) then a:=currentBas;
  end;
o:=0;
for i:=ab0 downto pos('://',a)+3 do if (ab[i]=47) then o:=i;
if (o<1) then a:=a+'/';
f2:
if (ab0=0) then goto f3;
o:=0;
for i:=ab0 downto 1 do if (ab[i]=47) then o:=i;
if httpLink then if (pos('?',c)<>0) then o:=0;
if (o<1) then begin; c:=c+a;goto f3; end;
b:=copy(a,1,o-1);
a:=copy(a,o+1,666);
if (b='.') then goto f2;
if (b<>'..') then begin;
  c:=c+b+'/';
  goto f2;
  end;
p:=666;
for i:=cb0 downto pos('://',c)+3 do if (cb[i]=47) then p:=i;
o:=0;
for i:=1 to cb0-1 do if (cb[i]=47) then o:=i;
if (o<p) then goto f2;
cb0:=o;
goto f2;
f3:
if httpLink then for o:=1 to cb0 do case cb[o] of
  32:cb[o]:=43;
  37:begin;
    a:=kicsi(copy(c,o+1,2));
    if (kicsi(byte2hextype(hextype2byte(a)))=a) then continue;
    c:=copy(c,1,o)+'25'+copy(c,o+1,666);
    goto f3;
    end;
  end;
convertUrlString:=c;
End;



Procedure inputOpen(a:String);
Begin;
if (xtOpen(sourceFile,a,true)<>0) then immErr('error opening input!');
End;

Procedure inputClose;
Begin;
xtClose(sourceFile);
End;



Procedure outputBegin;
Label f1;
Begin;
xCreate(dataPath+TemporaryText);
xCreate(dataPath+TemporaryLine);
xCreate(dataPath+TemporaryData);
if (xOpen(outputText,dataPath+TemporaryText,xGenFilMod_rw)<>0) then begin;
  f1:
  immErr('error opening output!');
  exit;
  end;
if (xOpen(outputLine,dataPath+TemporaryLine,xGenFilMod_rw)<>0) then goto f1;
if (xOpen(outputData,dataPath+TemporaryData,xGenFilMod_rw)<>0) then goto f1;
xTruncate(outputText);
xTruncate(outputLine);
xTruncate(outputData);
outputSiz:=0;
outputChr:=0;
outputLin:=0;
currentTit:=currentUrl;
currentBas:=convertUrlString(currentUrl);
currentCol:=7;
End;

Procedure outputClose;
Begin;
xClose(outputText);
xClose(outputLine);
xClose(outputData);
End;

Procedure outputErase;
Label f1,f2,f3;
Var
  f:xfile;
  d:xDirEntryRec;
Begin;
WriteLn('deleting temporary files...');
f1:
if (xDirOpen(f,dataPath)<>0) then exit;
f2:
if (xDirRead(f,d)<>0) then goto f3;
if (d.name='') then goto f3;
if (kicsi(xFileName(d.name,3))<>'.'+TemporaryExt) then goto f2;
xClose(f);
xErase(dataPath+d.name);
goto f1;
f3:
xClose(f);
End;



Procedure outputWriteBuf;
Var d:oneOutputLineRecord;
Begin;
d.p:=xFilePos(outputText);
d.s:=outputSiz;
d.c:=outputChr;
xBlockWrite(outputLine,d,sizeof(d));
xBlockWrite(outputText,outputBuf,outputSiz);
outputSiz:=0;
outputChr:=0;
inc(outputLin);
End;

Procedure outputReadBuf(o:LongInt);
Var d:oneOutputLineRecord;
Begin;
outputSiz:=0;
outputChr:=0;
if (o<0) then exit;
if (o>=outputLin) then exit;
xSeek(outputLine,o*sizeof(d));
xBlockRead(outputLine,d,sizeof(d));
outputSiz:=d.s;
outputChr:=d.c;
xSeek(outputText,d.p);
xBlockRead(outputText,outputBuf,outputSiz);
End;

Procedure outputAddTag(typ:byte;str:String);
Var
  ab:array[0..1] of byte absolute str;
  ab0:byte absolute str;
Begin;
str:='12'+str;
ab[1]:=typ;
ab[2]:=ab0-2;
if (outputSiz+ab0>sizeof(outputBuf)) then outputWriteBuf;
move(str[1],outputBuf[outputSiz+1],ab0);
inc(outputSiz,ab0);
End;

Function outputAddDat(a:string):String;
Var
  ab0:byte absolute a;
  i:LongInt;
Begin;
i:=xFilePos(outputData);
xBlockWrite(outputData,a,sizeof(a));
ab0:=sizeof(i);
move(i,a[1],sizeof(i));
outputAddDat:=a;
End;

Function outputGetTag(var ps:LongInt):String;
Var
  i:LongInt;
  a:String;
Begin;
if (ps>=outputSiz) then begin; outputGetTag:='';exit; end;
inc(ps);
i:=outputBuf[ps];
inc(ps);
move(outputBuf[ps],a,sizeof(a));
inc(ps,length(a));
outputGetTag:=chr(i)+a;
End;

Function outputGetDat(ps:LongInt):String;
Var a:String;
Begin;
xSeek(outputData,ps);
xBlockRead(outputData,a,sizeof(a));
outputGetDat:=a;
End;

Procedure outputPutDat(ps:LongInt;a:String);
Begin;
xSeek(outputData,ps);
xBlockWrite(outputData,a,sizeof(a));
End;



Procedure outputAddColor(col:LongInt);
Begin;
outputAddTag(1,chr(col));
End;

Procedure outputAddTextural(typ:LongInt;beg,txt:String);
Label f1,f2;
Var i,o:LongInt;
Begin;
if (txt='') then begin;
  outputAddTag(typ,beg);
  exit;
  end;
f1:
if (txt='') then exit;
o:=screenSizX-outputChr;
if (o<1) then begin;
  outputWriteBuf;
  outputAddColor(currentCol);
  goto f1;
  end;
if (length(txt)<=o) then begin;
  i:=666;
  goto f2;
  end;
i:=findLastSpace(copy(txt,1,o));
if (i>0) then goto f2;
if (length(txt)<=screenSizX) then begin;
  outputWriteBuf;
  outputAddColor(currentCol);
  goto f2;
  end;
i:=findLastSpace(copy(txt,1,screenSizX));
if (i>0) then begin;
  outputWriteBuf;
  outputAddColor(currentCol);
  goto f2;
  end;
i:=o;
f2:
o:=length(txt);
if (i>o) then i:=o;
outputAddTag(typ,beg+copy(txt,1,i));
inc(outputChr,i);
txt:=copy(txt,i+1,666);
goto f1;
End;

Procedure outputAddText(txt:String);
Begin;
if (txt='') then exit;
outputAddTextural(2,'',txt);
End;

Procedure outputAddLink(url,txt:String);
Begin;
if (url='') then exit;
url:=convertUrlString(url);
if (url='') then exit;
if (txt='') then txt:=' ';
outputAddTextural($83,outputAddDat(url),txt);
End;

Procedure outputAddFormBeg(url,met,enc,txt:String);
Var i:LongInt;
Begin;
if (url='') then exit;
url:=convertUrlString(url);
if (url='') then exit;
met:=kicsi(met);
i:=1;
if (enc='application/x-www-form-urlencoded') then i:=1;
if (enc='multipart/form-data') then i:=2;
url:=chr(i)+url;
i:=1;
if (met='get') then i:=1;
if (met='post') then i:=2;
if (met='put') then i:=3;
url:=chr(i)+url;
outputAddTextural($84,outputAddDat(url),txt);
outputAddTextural(2,'','-->');
End;

Procedure outputAddFormEntry(typ:LongInt;nam,val,txt:String);
Begin;
outputAddTextural(typ or $80,outputAddDat(nam),txt);
outputAddDat(val);
End;

Procedure outputAddFormEnd;
Begin;
outputAddTextural(2,'','<--');
outputAddFormEntry(14,'','','');
outputAddFormEntry(6,'','','form:submit');
outputAddFormEntry(5,'','','');
End;
