Procedure decodeOneMessage(var t:xtText);
Var
  a,b,c,d:String;
  ab:array[0..255] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
  shd:Boolean;
  typ:string;
  nam:String;
  enc:Byte; {0=bin, 1=base64, 2=quotedprintable}

Function doAppend(bnd:String):Boolean;
Label f1,f2,f3,f4,f5;
Var i,o,p,s:LongInt;
Begin;
doAppend:=False;
if (bnd='') then bnd:=#13#10;
if shd then begin;
  f2:
  doAppend:=True;
  repeat
    if xtEOF(t) then exit;
    a:=xtReadLn(t,255);
    if (a=bnd+'--') then exit;
    until (a=bnd);
  doAppend:=False;
  exit;
  end;
WriteOneSeparator;
if (kicsi(typ)<>'text/plain') then begin;
  WriteOneString('type: '+typ);WriteOneChar(#10);
  WriteOneString('name: '+nam);WriteOneChar(#10);
  goto f2;
  end;
case enc of
  0:begin;
    f3:
    if xtEOF(t) then goto f1;
    a:=xtRead(t,250);
    if (a=bnd) then goto f1;
    if (a=bnd+'--') then goto f1;
    WriteOneString(a);
    while not xtEOL(t) do WriteOneString(xtRead(t,250));
    WriteOneString(xtReadLn(t,255));
    WriteOneChar(#10);
    goto f3;
    end;
  1:begin;
    f4:
    p:=0;
    s:=0;
    if xtEOF(t) then goto f1;
    a:=xtRead(t,250);
    if (a=bnd) then goto f1;
    if (a=bnd+'--') then goto f1;
    if xtEOL(t) then a:=a+xtReadLn(t,255);
    for i:=1 to ab0 do begin;
      o:=pos(a[i],Base64table);
      if (o<1) then continue;
      inc(s,6);
      p:=p or ((o-1) shl (16-s));
      if (s<8) then continue;
      WriteOneChar(chr(p shr 8));
      p:=(p and $ff) shl 8;
      dec(s,8);
      end;
    goto f4;
    end;
  2:begin;
    f5:
    shd:=false;
    if xtEOF(t) then goto f1;
    a:=xtRead(t,250);
    if (a=bnd) then goto f1;
    if (a=bnd+'--') then goto f1;
    while (a<>'') do begin;
      if (ab0<5) then a:=a+xtRead(t,128);
      if (ab0=1) and (ab[1]=$3d) then begin;
        shd:=true;
        ab0:=0;
        break;
        end;
      o:=666;
      for i:=1 to ab0 do if (ab[i]=$3d) then begin; o:=i;break; end;
      if (o<>1) then begin;
        WriteOneString(copy(a,1,o-1));
        a:=copy(a,o,255)+xtRead(t,16);
        continue;
        end;
      b:=copy(a,2,2);
      i:=hextype2byte(b);
      if (nagy(byte2hextype(i))<>b) then begin;
        WriteOneChar('=');
        a:=copy(a,2,255)+xtRead(t,16);
        continue;
        end;
      WriteOneChar(chr(i));
      a:=copy(a,4,255)+xtRead(t,16);
      end;
    a:=a+xtReadLn(t,255);
    WriteOneString(a);
    if shd then goto f5;
    WriteOneChar(#10);
    goto f5;
    end;
  else goto f2;
  end;
exit;
f1:
xtReadLn(t,255);
WriteOneChar(#10);
doAppend:=(a=bnd+'--');
End;

Function doLevel(level:Byte):Boolean;
Label f1,f2,f3,f4;
Var bnd:String;
Begin;
enc:=0;
typ:='text/plain';
nam:='';
bnd:='';
c:='';
b:=xtReadLn(t,255);
if (b='') then goto f4;
goto f2;
f1:
if (c<>'') then begin;
  b:='';
  goto f2;
  end;
f4:
if (bnd='') then begin; doLevel:=False;exit; end;
doLevel:=True;
bnd:='--'+bnd;
shd:=true;
goto f3;
f2:
b:=xLevesz(b+' '+c);
c:=xtReadLn(t,255);
if (c<>'') then if (c[1] in [#0,#9,#32,#255]) then goto f2;
i:=pos(':',b);
a:=kicsi(xLevesz(copy(b,1,i-1)));
b:=xLevesz(copy(b,i+1,255));
if (a='content-transfer-encoding') then begin;
  a:=GetNextParameter(b,d);
  enc:=0;
  if (a='base64') then enc:=1;
  if (a='quoted-printable') then enc:=2;
  goto f1;
  end;
if (a='content-type') then begin;
  typ:=GetNextParameter(b,d);
  while (b<>'') do begin;
    a:=GetNextParameter(b,d);
    if (a='boundary') then begin; bnd:=d;continue; end;
    if (a='name') then begin; nam:=d;continue; end;
    end;
  goto f1;
  end;
goto f1;
f3:
if xtEOF(t) then exit;
if doAppend(bnd) then exit;
shd:=doLevel(level+1);
goto f3;
End;

Begin;
xtSetPos(t,0);
shd:=doLevel(0);
doAppend('');
End;












Procedure decodeOneHeader(var t:xtText;var src,trg,dat,sbj:String);
Label f3,f4;
Var
  a,b,c:String;
  i:LongInt;
Begin;
xtSetPos(t,0);
src:='';
trg:='';
dat:='';
sbj:='';
c:='';
b:=xtReadLn(t,255);
goto f3;
f4:
if (c='') then begin;
  dat:=xLevesz(DecodeHeaderLines(dat));
  sbj:=xLevesz(DecodeHeaderLines(sbj));
  NormalizeOneAddress(xLevesz(DecodeHeaderLines(src)),a,src);
  NormalizeOneAddress(xLevesz(DecodeHeaderLines(trg)),b,trg);
  src:=a+' <'+src+'>';
  trg:=b+' <'+trg+'>';
  exit;
  end;
b:='';
f3:
b:=xLevesz(b+' '+c);
c:=xtReadLn(t,255);
if (c<>'') then if (c[1] in [#0,#9,#32,#255]) then goto f3;
i:=pos(':',b);
a:=kicsi(xLevesz(copy(b,1,i-1)));
b:=xLevesz(copy(b,i+1,255));
if (a='from') then begin; src:=b;goto f4; end;
if (a='to') then begin; trg:=b;goto f4; end;
if (a='subject') then begin; sbj:=b;goto f4; end;
if (a='date') then begin; dat:=b;goto f4; end;
goto f4;
End;
