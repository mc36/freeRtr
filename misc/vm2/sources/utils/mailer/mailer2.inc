Type
  OneMessageHeader=record
    subj:string;        {subject of message}
    src:string;         {source address}
    trg:string;         {target address}
    date:string[127];   {creation date}
    fn:string[126];     {filename}
    flg:Byte;           {flags: 40h-red, 80h-del}
    end;
  OneMailPartHeader=record
    level:Byte;         {which level}
    enc:Byte;           {0=bin, 1-base64, 2-quotedprintable}
    beg:LongInt;        {beginning offset}
    lst:LongInt;        {ending offset}
    typ:String[250];    {type of data}
    nam:String[250];    {name of data}
    end;
Const
  partlistExtension='tmpm';
  indexFileName='index.'+partlistExtension;
  UnpackedFilNam='unpacked.'+partlistExtension;
  AttachmentList='attach.'+partlistExtension;
  EditingFilNam='editing.'+partlistExtension;
  friendListing='friend.'+partlistExtension;
  myFavoriteEditor='c:\utils\editor.code';
  myFavoriteViewer='c:\utils\editor.code';
  replyTemplate='reply';
  newTemplate='new';
  friendOriginal='friends';
  UserConfiguration='usercfg';
  messageExtension='msg';
  headerExtension='hdr';




Procedure CreateFriendsIndex(a:String);
Label f1,f2;
Var
  f:xFile;
  t:xtText;
Begin;
WriteLn('indexing friends...');
xCreate(a+friendListing);
if (xOpen(f,a+friendListing,xGenFilMod_rw)<>0) then exit;
if (xtOpen(t,a+friendOriginal,true)<>0) then goto f2;
f1:
if xtEOF(t) then goto f2;
a:=xtReadLn(t,255);
if (a='') then goto f1;
if (a[1]=';') then goto f1;
xBlockWrite(f,a,sizeof(a));
goto f1;
f2:
xTruncate(f);
xClose(f);
xtClose(t);
End;

Procedure AddContactToFriends(path,a:string);
Var
  f:xFile;
  t:xtText;
Begin;
if (xOpen(f,path+friendListing,xGenFilMod_rw)=0) then begin;
  xSeek(f,xFileSize(f));
  xBlockWrite(f,a,sizeof(a));
  xClose(f);
  end;
if (xtOpen(t,path+friendOriginal,false)=0) then begin;
  xtWriteLn(t,a);
  xtClose(t);
  end;
End;

Procedure CreateMessageIndex(path:String);
Label f1,f2,f3,f4;
Var
  h1,h2:xFile;
  t:xtText;
  d:OneMessageHeader;
  sr:xDirEntryRec;
  a,b,c:String;
  i,o:LongInt;
Begin;
WriteLn('indexing messages...');
xCreate(path+indexFileName);
if (xOpen(h2,path+indexFileName,xGenFilMod_rw)<>0) then exit;
if (xDirOpen(h1,path)<>0) then begin; xClose(h2);exit; end;
f1:
if (xDirRead(h1,sr)<>0) then goto f2;
if (sr.name='') then goto f2;
if (kicsi(xFileName(sr.name,3))<>'.'+messageExtension) then goto f1;
Write(#13+sr.name);
if (xtOpen(t,path+sr.name,true)<>0) then goto f1;
fillChar(d,sizeof(d),0);
d.fn:=xFileName(sr.name,2);
c:='';
b:=xtReadLn(t,255);
goto f3;
f4:
if (c='') then begin;
  xBlockWrite(h2,d,sizeof(d));
  xtClose(t);
  goto f1;
  end;
b:='';
f3:
b:=xLevesz(b+' '+c);
c:=xtReadLn(t,255);
if (c<>'') then if (c[1] in [#0,#9,#32,#255]) then goto f3;
i:=pos(':',b);
a:=kicsi(xLevesz(copy(b,1,i-1)));
b:=xLevesz(copy(b,i+1,255));
if (a='from') then begin; d.src:=xLevesz(DecodeHeaderLines(b));goto f4; end;
if (a='to') then begin; d.trg:=xLevesz(DecodeHeaderLines(b));goto f4; end;
if (a='date') then begin; d.date:=xLevesz(DecodeHeaderLines(b));goto f4; end;
if (a='subject') then begin; d.subj:=xLevesz(DecodeHeaderLines(b));goto f4; end;
goto f4;
f2:
xTruncate(h2);
xClose(h2);
xDirClose(h1);
clrEol;
End;






Procedure FindMessageParts(a:String);
Var
  d:OneMailPartHeader;
  t:xtText;
  f:xFile;
  b,c,e:String;
  i:LongInt;
  shd:Boolean;

Procedure doAppend;
Begin;
if shd then exit;
xBlockWrite(f,d,sizeof(d));
End;

Function doLevel(level:Byte):Boolean;
Label f1,f2,f3,f4;
Var bnd:String;
Begin;
fillchar(d,sizeof(d),0);
d.enc:=0;
d.typ:='text/plain';
d.level:=level;
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
d.beg:=xtGetPos(t);
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
  a:=GetNextParameter(b,e);
  i:=0;
  if (a='base64') then i:=1;
  if (a='quoted-printable') then i:=2;
  d.enc:=i;
  goto f1;
  end;
if (a='content-type') then begin;
  a:=GetNextParameter(b,e);
  d.typ:=a;
  while (b<>'') do begin;
    a:=GetNextParameter(b,e);
    if (a='boundary') then begin; bnd:=e;continue; end;
    if (a='name') then begin; d.nam:=e;continue; end;
    end;
  goto f1;
  end;
goto f1;
f3:
if xtEOF(t) then begin; doAppend;exit; end;
d.lst:=xtGetPos(t);
a:=xtReadLn(t,255);
if (a=bnd+'--') then begin; doAppend;exit; end;
if (a<>bnd) then goto f3;
doAppend;
shd:=doLevel(level+1);
goto f3;
End;

Begin;
xCreate(a+'.'+partlistExtension);
if (xOpen(f,a+'.'+partlistExtension,xGenFilMod_rw)<>0) then exit;
if (xtOpen(t,a+'.'+messageExtension,true)<>0) then begin; xClose(f);exit; end;
shd:=doLevel(0);
d.lst:=xtFileSize(t);
doAppend;
xtClose(t);
xTruncate(f);
xClose(f);
End;










Function DecodeOneMailPart(a:String;var d:OneMailPartHeader;var ff:xFile):Boolean;
Var
  f:xFile;
  t:xtText;
  i,o,p,s:LongInt;
  ab:array[0..255] of byte absolute a;
  ab0:byte absolute a;
  buf:array[1..1024] of byte;
  siz:longInt;
  sft:Boolean;
  b:string;

procedure flush;
begin;
if (siz<1) then exit;
xBlockWrite(ff,buf,siz);
siz:=0;
end;

procedure addch(b:byte);
begin;
inc(siz);
buf[siz]:=b;
if (siz>=sizeof(buf)) then flush;
end;

procedure addst(a:String);
Var
  ab:array[0..255] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
begin;
for i:=1 to ab0 do addch(ab[i]);
end;

Begin;
DecodeOneMailPart:=True;
case d.enc of
  0:begin; {binary}
    if (xOpen(f,a,xGenFilMod_r)<>0) then exit;
    p:=d.beg;
    s:=d.lst;
    xSeek(f,p);
    while (p<s) do begin;
      i:=s-p;
      if (i>sizeof(buf)) then i:=sizeof(buf);
      xBlockRead(f,buf,i);
      xBlockWrite(ff,buf,i);
      inc(p,i);
      end;
    xClose(f);
    DecodeOneMailPart:=False;
    end;
  1:begin; {base64}
    if (xtOpen(t,a,true)<>0) then exit;
    t.fileSize:=d.lst;
    xtSetPos(t,d.beg);
    siz:=0;
    p:=0;
    s:=0;
    while not xtEOF(t) do begin;
      a:=xtRead(t,128);
      if xtEOL(t) then a:=a+xtReadLn(t,255);
      for i:=1 to ab0 do begin;
        o:=pos(a[i],Base64table);
        if (o<1) then continue;
        inc(s,6);
        p:=p or ((o-1) shl (16-s));
        while (s>=8) do begin;
          addch(p shr 8);
          p:=(p and $ff) shl 8;
          dec(s,8);
          end;
        end;
      end;
    flush;
    xtClose(t);
    DecodeOneMailPart:=False;
    end;
  2:begin; {quoted-printable}
    if (xtOpen(t,a,true)<>0) then exit;
    t.fileSize:=d.lst;
    xtSetPos(t,d.beg);
    siz:=0;
    a:='';
    while not xtEOF(t) do begin;
      sft:=false;
      a:=xtRead(t,128);
      while (a<>'') do begin;
        if (ab0<5) then a:=a+xtRead(t,128);
        if (ab0=1) and (ab[1]=$3d) then begin;
          sft:=true;
          ab0:=0;
          break;
          end;
        o:=666;
        for i:=1 to ab0 do if (ab[i]=$3d) then begin; o:=i;break; end;
        if (o<>1) then begin;
          addst(copy(a,1,o-1));
          a:=copy(a,o,255)+xtRead(t,16);
          continue;
          end;
        b:=copy(a,2,2);
        i:=hextype2byte(b);
        if (nagy(byte2hextype(i))<>b) then begin;
          addch($3d);
          a:=copy(a,2,255)+xtRead(t,16);
          continue;
          end;
        addch(i);
        a:=copy(a,4,255)+xtRead(t,16);
        end;
      a:=a+xtReadLn(t,255);
      addst(a);
      if sft then continue;
      addch(13);
      addch(10);
      end;
    flush;
    xtClose(t);
    DecodeOneMailPart:=False;
    end;
  end;
End;



Procedure MsgWriterBegin(var t:xtText;src,trg,subj:String);
Begin;
boundary:=BStr(random($7fffffff))+'='+GetDateForFiles+'='+BStr(random($7fffffff));
xtWriteLn(t,'From: '+src);
xtWriteLn(t,'To: '+trg);
xtWriteLn(t,'Subject: '+subj);
xtWriteLn(t,'Date: '+GetDateForHeader);
xtWriteLn(t,'MIME-Version: 1.0');
xtWriteLn(t,'Content-Type: multipart/mixed; boundary="'+boundary+'"');
xtWriteLn(t,'X-Mailer: '+ProggyName+' for BugOS');
NormalizeOneAddress(src,trg,src);
xtWriteLn(t,'Message-ID: <'+boundary+'='+src+'>');
xtWriteLn(t,'');
xtWriteLn(t,'this is a message in mime format!');
xtWriteLn(t,'');
End;

Procedure MsgWriterAddTxt(var t:xtText;a:String);
Label f1;
Var tt:xtText;
Begin;
if (xtOpen(tt,a,true)<>0) then exit;
xtWriteLn(t,'--'+boundary);
xtWriteLn(t,'Content-Type: text/plain; charset="us-ascii"');
xtWriteLn(t,'Content-Transfer-Encoding: 7bit');
xtWriteLn(t,'');
f1:
if xtEOF(tt) then begin;
  xtWriteLn(t,xtReadLn(tt,255));
  xtClose(tt);
  exit;
  end;
if xtEOL(tt) then xtWriteLn(t,xtReadLn(tt,255));
xtWrite(t,xtRead(tt,200));
goto f1;
End;

Procedure MsgWriterAddBin(var t:xtText;a:String);
Var
  f:xFile;
  i,o,p,s,q,w:LongInt;
  buf:array[1..3] of byte;
  siz:LongInt;

procedure addbit(d,s:longint);
begin;
inc(q,s);
w:=w or (d shl (16-q));
while (q>=6) do begin;
  a:=a+Copy(Base64table,(w shr 10)+1,1);
  w:=(w shl 6) and $ffff;
  dec(q,6);
  end;
end;


Begin;
if (xOpen(f,a,xGenFilMod_r)<>0) then exit;
a:=xFileName(a,2)+xFileName(a,3);
s:=xFileSize(f);
xtWriteLn(t,'--'+boundary);
xtWriteLn(t,'Content-Type: application/octet-stream; name="'+a+'"');
xtWriteLn(t,'Content-Transfer-Encoding: base64');
xtWriteLn(t,'Content-Disposition: attachment; filename="'+a+'"');
xtWriteLn(t,'');
xtWriteLn(t,'');
p:=0;
a:='';
q:=0;w:=0;
while (p<s) do begin;
  o:=s-p;
  if (o>sizeof(buf)) then o:=sizeof(buf);
  xBlockRead(f,buf,o);
  inc(p,o);
  for i:=1 to o do addbit(buf[i],8);
  if (length(a)<64) then continue;
  xtWriteLn(t,a);
  a:='';
  end;
addbit(0,5);
if (a<>'') then begin;
  while (Length(a) and 3<>0) do a:=a+'=';
  xtWriteLn(t,a);
  end;
xtWriteLn(t,'');
xClose(f);
End;

Procedure MsgWriterFinish(var t:xtText);
Begin;
xtWriteLn(t,'--'+boundary+'--');
End;
