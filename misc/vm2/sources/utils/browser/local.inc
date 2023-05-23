Procedure append2bookmark(fil,nam,url:String);
Var t:xtText;
Begin;
fil:=dataPath+fil;
xCreate(fil);
if (kicsi(copy(url,1,pos(':',url)-1))='about') then exit;
if (xtOpen(t,fil,false)<>0) then exit;
xtWriteLn(t,'');
if (nam=#13) then begin;
  xtWriteLn(t,'---');
  end else begin;
  xtWriteLn(t,getCurrentDate+' '+getCurrentTime);
  xtWriteLn(t,nam);
  xtWriteLn(t,url);
  end;
xtClose(t);
End;



Procedure decodeCurrentUnknown;
Begin;
currentTit:='  error!';
outputAddColor(12);
outputAddText('unknown protocol requested!');
outputWriteBuf;
outputAddColor(7);
outputAddText('url: "');
outputAddColor(15);
outputAddText(currentUrl);
outputAddColor(7);
outputAddText('"');
outputWriteBuf;
End;

Procedure decodeCurrentAbout;
Begin;
currentTit:='about browser';
outputAddColor(7);
outputWriteBuf;
outputAddColor(15);
outputAddText(PrgTxt);
outputWriteBuf;
outputAddColor(7);
outputWriteBuf;
outputAddColor(8);
outputAddText('press f1 to get help!');
outputWriteBuf;
End;



Procedure decodeCurrentHistory(var t:xtText;dates:Boolean;a:String);
Label f1;
Var
  b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
currentTit:=a;
outputAddColor(15);
outputAddText(' '+a);
outputWriteBuf;
outputAddColor(7);
i:=ab0;
fillchar(a,sizeof(a),126);
ab0:=i+2;
outputAddText(a);
outputWriteBuf;
f1:
if xtEOF(t) then exit;
a:=xtReadLn(t,666);
if (ab0=00) then goto f1;
if (a='---') then begin;
  fillchar(a,sizeof(a),45);
  ab0:=screenSizX;
  outputAddColor(9);
  outputAddText(a);
  outputWriteBuf;
  goto f1;
  end;
if dates then begin;
  outputAddColor(7);
  outputAddText(copy(a,12,5)+' ');
  end;
b:=xtReadLn(t,666);
a:=xtReadLn(t,666);
if (ab0=0) then outputAddText(b) else outputAddLink(a,b);
outputWriteBuf;
goto f1;
End;



Procedure decodeCurrentDirectory(a:String);
Label f1,f2;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  d:xDirEntryRec;
  i:LongInt;
  f:xFile;

Function convDate(d:xDirEntryDateTimeRec):String;
Var a:String;
function x(i:LongInt):String;begin; a:=BStr(i);while (length(a)<2) do a:='0'+a;x:=a; end;
Begin;
convDate:=x(d.year)+'-'+x(d.month)+'-'+x(d.day)+' '+x(d.hour)+':'+x(d.minute)+':'+x(d.second)+' ';
End;

Begin;
kicserel('\',#13,a);
kicserel('/','\',a);
kicserel(#13,'/',a);
currentTit:=a;
outputAddColor(15);
outputAddText(' '+a);
outputWriteBuf;
outputAddColor(7);
i:=ab0;
fillchar(a,sizeof(a),126);
ab0:=i+2;
outputAddText(a);
outputWriteBuf;
outputAddLink('/','root');outputWriteBuf;
outputAddLink('../','parent');outputWriteBuf;
outputAddColor(15);
outputAddText('created             modified            owner    right size         name');
outputWriteBuf;
if (xDirOpen(f,currentTit)<>0) then exit;
f1:
if (xDirRead(f,d)<>0) then goto f2;
if (d.name='') then goto f2;
outputAddColor(7);
outputAddText(convDate(d.created)+convDate(d.modified));
a:=BStr(d.size);
while (length(a)<16) do a:=a+' ';
outputAddText(conv2hex(d.owner)+' '+copy(conv2hex(d.rights),7,666)+' '+a);
a:=d.name;
if (d.rights and xRights_Directory<>0) then a:=a+'/';
outputAddLink(currentUrl+a,d.name);
outputWriteBuf;
goto f1;
f2:
xClose(f);
End;



Procedure decodeCurrentTextfile(var t:xtText);
Begin;
while not xtEOF(t) do begin;
  outputAddText(xtRead(t,250));
  if not xtEOL(t) then continue;
  outputAddText(xtReadLn(t,250));
  outputWriteBuf;
  end;
End;



Procedure decodeCurrentBinary(var t:xtText);
Var
  a,b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  bb:array[0..1] of byte absolute b;
  bb0:byte absolute b;
  i,o,p:LongInt;
Begin;
o:=screenSizX div 4;
while ((o*4)+(o div 4)+10>=screenSizX) do dec(o);
while (o and 3<>0) do dec(o);
if (o<1) then o:=1;
if (o>64) then o:=64;
p:=0;
a:=conv2hex(p)+'  ';
bb0:=0;
while not xtEOF(t) do begin;
  i:=xtGetOneChar(t);
  a:=a+byte2hextype(i)+' ';
  if (bb0 and 3=3) then a:=a+' ';
  inc(bb0);
  bb[bb0]:=i;
  inc(p);
  if (bb0<o) then continue;
  outputAddText(a+b);
  outputWriteBuf;
  a:=conv2hex(p)+'  ';
  bb0:=0;
  end;
if (bb0=0) then exit;
fillUpString(a,(o*3)+(o div 4)+10);
outputAddText(a+b);
outputWriteBuf;
End;
