Const
  maximumTagsNumber=32;
Type
  oneTagRecord=record
    t:String;
    n:LongInt;
    d:array[1..maximumTagsNumber] of record
      n:String;
      v:String;
      end;
    end;



Function findOneTagEntry(var d:oneTagRecord;s:String):LongInt;
Var i:LongInt;
Begin;
findOneTagEntry:=0;
s:=kicsi(s);
for i:=1 to d.n do if (d.d[i].n=s) then begin;
  findOneTagEntry:=i;
  exit;
  end;
End;

Function getOneTagValue(var d:oneTagRecord;s:String):String;
Var i:LongInt;
Begin;
getOneTagValue:='';
i:=findOneTagEntry(d,s);
if (i>0) then getOneTagValue:=d.d[i].v;
End;



Function readUpOneQuoted(var t:xtText):LongInt;
Label f1,f2,err;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
Begin;
readUpOneQuoted:=-1;
p:=xtGetPos(t);
ab0:=0;
f1:
i:=xtGetOneChar(t);
if (i<0) then goto err;
if (i<>59) then begin;
  inc(ab0);
  if (ab0>32) then goto err;
  ab[ab0]:=i;
  goto f1;
  end;
i:=-1;
if (ab0<1) then goto err;
if (ab[1]=35) then begin;
  if (ab0<2) then goto f2;
  if (ab[2] in [88,120]) then ab[2]:=36;
  i:=BVal(copy(a,2,666));
  if (i>0) then goto f2;
  if (a='#0') or (a='#$0000') then goto f2;
  i:=-1;
  goto f2;
  end;
if (a='lt') then i:=60;
if (a='gt') then i:=62;
if (a='amp') then i:=38;
if (a='quot') then i:=34;
if (a='apos') then i:=39;
if (a='nbsp') then i:=32; {160}
if (a='iexcl') then i:=161;
if (a='cent') then i:=162;
if (a='pound') then i:=163;
if (a='curren') then i:=164;
if (a='yen') then i:=165;
if (a='brvbar') then i:=166;
if (a='sect') then i:=167;
if (a='uml') then i:=168;
if (a='copy') then i:=169;
if (a='ordf') then i:=170;
if (a='laquo') then i:=171;
if (a='not') then i:=172;
if (a='shy') then i:=173;
if (a='reg') then i:=174;
if (a='macr') then i:=175;
if (a='deg') then i:=176;
if (a='plusmn') then i:=177;
if (a='sup2') then i:=178;
if (a='sup3') then i:=179;
if (a='acute') then i:=180;
if (a='micro') then i:=181;
if (a='para') then i:=182;
if (a='middot') then i:=183;
if (a='cedil') then i:=184;
if (a='sup1') then i:=185;
if (a='ordm') then i:=186;
if (a='raquo') then i:=187;
if (a='frac14') then i:=188;
if (a='frac12') then i:=189;
if (a='frac34') then i:=190;
if (a='iquest') then i:=191;
if (a='Agrave') then i:=192;
if (a='Aacute') then i:=193;
if (a='Acirc') then i:=194;
if (a='Atilde') then i:=195;
if (a='Auml') then i:=196;
if (a='Aring') then i:=197;
if (a='AElig') then i:=198;
if (a='Ccedil') then i:=199;
if (a='Egrave') then i:=200;
if (a='Eacute') then i:=201;
if (a='Ecirc') then i:=202;
if (a='Euml') then i:=203;
if (a='Igrave') then i:=204;
if (a='Iacute') then i:=205;
if (a='Icirc') then i:=206;
if (a='Iuml') then i:=207;
if (a='ETH') then i:=208;
if (a='Ntilde') then i:=209;
if (a='Ograve') then i:=210;
if (a='Oacute') then i:=211;
if (a='Ocirc') then i:=212;
if (a='Otilde') then i:=213;
if (a='Ouml') then i:=214;
if (a='times') then i:=215;
if (a='Oslash') then i:=216;
if (a='Ugrave') then i:=217;
if (a='Uacute') then i:=218;
if (a='Ucirc') then i:=219;
if (a='Uuml') then i:=220;
if (a='Yacute') then i:=221;
if (a='THORN') then i:=222;
if (a='szlig') then i:=223;
if (a='agrave') then i:=224;
if (a='aacute') then i:=225;
if (a='acirc') then i:=226;
if (a='atilde') then i:=227;
if (a='auml') then i:=228;
if (a='aring') then i:=229;
if (a='aelig') then i:=230;
if (a='ccedil') then i:=231;
if (a='egrave') then i:=232;
if (a='eacute') then i:=233;
if (a='ecirc') then i:=234;
if (a='euml') then i:=235;
if (a='igrave') then i:=236;
if (a='iacute') then i:=237;
if (a='icirc') then i:=238;
if (a='iuml') then i:=239;
if (a='eth') then i:=240;
if (a='ntilde') then i:=241;
if (a='ograve') then i:=242;
if (a='oacute') then i:=243;
if (a='ocirc') then i:=244;
if (a='otilde') then i:=245;
if (a='ouml') then i:=246;
if (a='divide') then i:=247;
if (a='oslash') then i:=248;
if (a='ugrave') then i:=249;
if (a='uacute') then i:=250;
if (a='ucirc') then i:=251;
if (a='uuml') then i:=252;
if (a='yacute') then i:=253;
if (a='thorn') then i:=254;
if (a='yuml') then i:=255;
f2:
if (i<0) then begin;
  writeln('unknown quoted: '+a);
  err:
  xtSetPos(t,p);
  i:=38;
  end;
readUpOneQuoted:=i;
End;



Procedure readUpOneTag(var t:xtText;var d:oneTagRecord);

Function getOneWord(first:Boolean):String;
Label f1,f2,f3,f4;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,s:LongInt;
Begin;
getOneWord:='';
ab0:=0;
s:=-1;
f1:
i:=xtGetOneChar(t);
if (i<0) then goto f4;
if (i in [0..32]) then goto f1;
if (i=62) then begin; xtUndoLastRead(t);goto f4; end;
if (i=38) then begin; xtUndoLastRead(t);goto f2; end;
ab0:=1;
ab[1]:=i;
if (i=61) then goto f4;
if (i in [34,39,96]) then begin; s:=i;ab0:=0; end;
f2:
i:=xtGetOneChar(t);
if (i<0) then goto f4;
if (i=38) then begin;
  i:=readUpOneQuoted(t);
  if (i<0) then goto f1;
  goto f3;
  end;
if (s<0) then begin;
  if (i in [0..32]) then goto f4;
  if first and (i=61) then begin; xtUndoLastRead(t);goto f4; end;
  if (i=62) then begin; xtUndoLastRead(t);goto f4; end;
  goto f3;
  end;
if (i=s) then goto f4;
if (i in [0..32]) then i:=32;
f3:
if (ab0>=255) then goto f2;
inc(ab0);
ab[ab0]:=i;
goto f2;
f4:
getOneWord:=a;
End;

Procedure addEntity(a,b:String);
Begin;
if (d.n>=maximumTagsNumber) then exit;
inc(d.n);
d.d[d.n].n:=kicsi(a);
d.d[d.n].v:=b;
End;

Label f1,f2,f3,f4;
Var
  a,b:String;
  ab0:byte absolute a;
  i:LongInt;
Begin;
d.n:=0;
i:=xtGetPos(t);
a:=getOneWord(false);
if (copy(a,1,3)='!--') then begin;
  d.t:='!--';
  xtSetPos(t,i+3);
  goto f4;
  end;
if (a='/') then a:='/'+getOneWord(false);
if (ab0>1) and (copy(a,ab0,666)='/') then dec(ab0);
d.t:=kicsi(a);
f1:
a:=getOneWord(true);
if (a='') then goto f3;
f2:
b:=getOneWord(true);
if (a='') and (b='') then goto f3;
if (b<>'=') then begin;
  addEntity(a,'');
  a:=b;
  goto f2;
  end;
b:=getOneWord(false);
addEntity(a,b);
goto f1;
f3:
i:=xtGetOneChar(t);
if (i<>62) then xtUndoLastRead(t);
if (d.d[d.n].n='/') and (d.d[d.n].v='') then dec(d.n);
exit;
f4:
i:=xtGetOneChar(t);
if (i<0) then exit;
if (i<>45) then goto f4;
i:=xtGetOneChar(t);
if (i<>45) then goto f4;
while (1=1) do begin;
  i:=xtGetOneChar(t);
  if (i<0) then exit;
  if (i=45) then continue;
  if (i=62) then break;
  goto f4;
  end;
End;



Procedure writeOneTag(var t:xtText;var d:oneTagRecord);

Procedure putEncoded(a:String);
Label f1;
Var
  i,o:LongInt;
Begin;
f1:
i:=pos('&',a);
o:=pos('"',a);
if (i<1) and (o<1) then begin; xtWrite(t,a);exit; end;
if (i<1) then i:=666;
if (o<1) then o:=666;
if (i<o) then begin;
  xtWrite(t,copy(a,1,i-1));
  xtWrite(t,'&amp;');
  a:=copy(a,i+1,666);
  end else begin;
  xtWrite(t,copy(a,1,o-1));
  xtWrite(t,'&quot;');
  a:=copy(a,o+1,666);
  end;
goto f1;
End;

Var
  i:LongInt;
  a:String;
Begin;
xtWrite(t,d.t);
for i:=1 to d.n do begin;
  xtWrite(t,' ');
  putEncoded(d.d[i].n);
  a:=d.d[i].v;
  if (a='') then continue;
  xtWrite(t,'="');
  putEncoded(a);
  xtWrite(t,'"');
  end;
xtWrite(t,'>');
End;



Function isThisHtmlFile(Var t:xtText):Boolean;
Label f1,f2;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,p:LongInt;
  c:Char;
  cb:Byte absolute c;
Begin;
isThisHtmlFile:=False;
xtSetPos(t,0);
ab0:=0;
p:=0;
f1:
i:=xtGetOneChar(t);
if (i<0) then goto f2;
if (i in [13,10,32,0,9,255]) then goto f1;
cb:=i;
c:=LowCase(c);
inc(ab0);
ab[ab0]:=cb;
if (ab0>16) then a:=copy(a,2,666);
if (pos('<html',a)>0) then begin; isThisHtmlFile:=True;goto f2; end;
inc(p);
if (p>512) then goto f2;
goto f1;
f2:
xtSetPos(t,0);
End;
