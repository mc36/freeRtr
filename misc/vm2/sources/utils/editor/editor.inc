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

Procedure DoCaseChanging(mode:Byte);
Var
  b,a,c:String;
  i,o:LongInt;

Procedure doer;
Var
  i:LongInt;
  spc:Boolean;
Begin;
case mode of
  1:a:=kicsi(a);
  2:a:=nagy(a);
  3:begin;
    a:=kicsi(a);
    spc:=true;
    for i:=1 to length(a) do if (a[i]=' ') then spc:=true else if spc then begin; a[i]:=upCase(a[i]);spc:=false; end;
    end;
  end;
End;

Begin;
if VerticalMode then begin;
  if (SelBX>SelEX) then exit;
  if (SelBY>SelEY) then exit;
  for o:=SelBY to SelEY do begin;
    a:=ReadLine(o);
    b:=copy(a,1,SelBX-1);
    c:=copy(a,SelEX,255);
    a:=copy(a,SelBX,SelEX-SelBX);
    doer;
    WriteLine(o,b+a+c);
    end;
  exit;
  end;
if (SelBY=SelEY) then begin;
  if (SelBX>=SelEX) then exit;
  a:=ReadLine(SelBY);
  b:=copy(a,1,SelBX-1);
  c:=copy(a,SelEX,255);
  a:=copy(a,SelBX,SelEX-SelBX);
  doer;
  WriteLine(SelBY,b+a+c);
  exit;
  end;
if (SelBY>SelEY) then exit;
a:=ReadLine(SelBY);
b:=copy(a,1,SelBX-1);
a:=copy(a,SelBX,255);
doer;
WriteLine(SelBY,b+a);
for o:=SelBY+1 to SelEY-1 do begin;
  a:=ReadLine(o);
  doer;
  WriteLine(o,a);
  end;
a:=ReadLine(SelEY);
c:=copy(a,SelEX,255);
a:=copy(a,1,SelEX-1);
doer;
WriteLine(SelEY,a+c);
End;

Procedure DoJustification(wh:Byte);
Var
  i,o:LongInt;
  a,b,c:String;
Begin;
if not VerticalMode then exit;
if (SelBX>SelEX) then exit;
if (SelBY>SelEY) then exit;
for o:=SelBY to SelEY do begin;
  a:=ReadLine(o);
  b:=copy(a,1,SelBX-1);
  c:=copy(a,SelEX,255);
  a:=copy(a,SelBX,SelEX-SelBX);
  while (length(a)<SelEX-SelBX) do a:=a+' ';
  a:=' '+a+' ';
  kicserel('  ',' ',a);
  a:=copy(a,2,length(a)-2);
  if (a='') then continue;
  case wh of
    1:begin;{justify}
      if (pos(' ',a)=0) then continue;
      i:=length(a)+1;
      while (length(a)<SelEX-SelBX) do begin;
        dec(i);
        if (i<1) then i:=length(a);
        if (a[i]<>' ') then continue;
        insert(' ',a,i);
        while (i>0) do begin;
          if (a[i]<>' ') then break;
          dec(i);
          end;
        end;
      end;
    2:begin;{right}
      while (length(a)<SelEX-SelBX) do a:=' '+a;
      end;
    3:begin;{left}
      end;
    4:begin;{center}
      while (length(a)<SelEX-SelBX) do a:=' '+a+' ';
      end;
    else exit;
    end;
  while (length(a)<SelEX-SelBX) do a:=a+' ';
  a:=copy(a,1,SelEX-SelBX);
  WriteLine(o,b+a+c);
  end;
End;

Procedure AlignCheckPosition;
Var i:LongInt;
Begin;
if (CurX<1) then CurX:=1;
if (CurX>254) then CurX:=254;
if (CurY<1) then CurY:=1;
if (CurY>MemoryLines) then CurY:=MemoryLines;
i:=(ScrSizX shr 4)+1;
while (CurX-BegX>ScrSizX) do begin;
  inc(BegX,i);
  RefreshScr:=RefreshScr or 2;
  end;
while (BegX>=CurX) do begin;
  dec(BegX,i);
  RefreshScr:=RefreshScr or 2;
  end;
i:=(ScrSizY shr 3)+1;
while (CurY-BegY>ScrSizY-1) do begin;
  inc(BegY,i);
  RefreshScr:=RefreshScr or 2;
  end;
while (BegY>=CurY) do begin;
  dec(BegY,i);
  RefreshScr:=RefreshScr or 2;
  end;
if (BegX<0) then BegX:=0;
if (BegY<0) then BegY:=0;
End;

Procedure AlignCheckSelection;
procedure swp(var i,o:LongInt);var p:LongInt;begin;p:=i;i:=o;o:=p;end;
Begin;
if (SelBY>MemoryLines) then SelBY:=MemoryLines;
if (SelEY>MemoryLines) then SelEY:=MemoryLines;
if (SelBY>SelEY) then begin;
  swp(SelBX,SelEX);
  swp(SelBY,SelEY);
  end;
if (SelBY=SelEY) then if (SelBX>SelEX) then begin;
  swp(SelBX,SelEX);
  swp(SelBY,SelEY);
  end;
if not VerticalMode then exit;
if (SelBX>SelEX) then swp(SelBX,SelEX);
End;





Function ProcessOneKey(key:Word):Boolean;
Label kezd,vege,vege2;
Var
  a:String;
  i,o:LongInt;
  shift:Boolean;
  ox,oy:LongInt;

Procedure EndBut;
Begin;
RefreshScr:=RefreshScr or 4;
a:=ReadLine(CurY);
a:=Levesz(a);
CurX:=length(a)+1;
End;

Procedure DelBut;
Begin;
a:=ReadLine(CurY);
a:=Levesz(a);
if (CurX<=length(a)) then begin;
  delete(a,CurX,1);
  WriteLine(CurY,a);
  RefreshScr:=RefreshScr or 1;
  Exit;
  end;
while (length(a)<CurX-1) do a:=a+' ';
a:=a+ReadLine(CurY+1);
WriteLine(CurY,a);
DeleteLine(CurY+1);
RefreshScr:=RefreshScr or 2;
End;

Procedure DoDrawing;
Type
  OneDrawCharRec=record
    c:char;
    u:byte;
    d:byte;
    l:byte;
    r:byte;
    end;
Const
  DrawChrs:array[0..40] of OneDrawCharRec=(   (c:' ';u:0;d:0;l:0;r:0),
  (c:'Ä';u:0;d:0;l:1;r:1),(c:'Í';u:0;d:0;l:2;r:2),
  (c:'³';u:1;d:1;l:0;r:0),(c:'º';u:2;d:2;l:0;r:0),
  (c:'Ñ';u:0;d:1;l:2;r:2),(c:'Ò';u:0;d:2;l:1;r:1),
  (c:'Ø';u:1;d:1;l:2;r:2),(c:'×';u:2;d:2;l:1;r:1),
  (c:'Ï';u:1;d:0;l:2;r:2),(c:'Ð';u:2;d:0;l:1;r:1),
  (c:'Â';u:0;d:1;l:1;r:1),(c:'Ë';u:0;d:2;l:2;r:2),
  (c:'Å';u:1;d:1;l:1;r:1),(c:'Î';u:2;d:2;l:2;r:2),
  (c:'Á';u:1;d:0;l:1;r:1),(c:'Ê';u:2;d:0;l:2;r:2),
  (c:'¿';u:0;d:1;l:1;r:0),(c:'»';u:0;d:2;l:2;r:0),
  (c:'´';u:1;d:1;l:1;r:0),(c:'¹';u:2;d:2;l:2;r:0),
  (c:'Ù';u:1;d:0;l:1;r:0),(c:'¼';u:2;d:0;l:2;r:0),
  (c:'Ú';u:0;d:1;l:0;r:1),(c:'É';u:0;d:2;l:0;r:2),
  (c:'Ã';u:1;d:1;l:0;r:1),(c:'Ì';u:2;d:2;l:0;r:2),
  (c:'À';u:1;d:0;l:0;r:1),(c:'È';u:2;d:0;l:0;r:2),
  (c:'¸';u:0;d:1;l:2;r:0),(c:'·';u:0;d:2;l:1;r:0),
  (c:'µ';u:1;d:1;l:2;r:0),(c:'¶';u:2;d:2;l:1;r:0),
  (c:'¾';u:1;d:0;l:2;r:0),(c:'½';u:2;d:0;l:1;r:0),
  (c:'Õ';u:0;d:1;l:0;r:2),(c:'Ö';u:0;d:2;l:0;r:1),
  (c:'Æ';u:1;d:1;l:0;r:2),(c:'Ç';u:2;d:2;l:0;r:1),
  (c:'Ô';u:1;d:0;l:0;r:2),(c:'Ó';u:2;d:0;l:0;r:1)
  );

Function GetOneChar(x,y:LongInt):Byte;
Var
  i,o:Word;
  a:String;
Begin;
a:=ReadLine(y);
a:=copy(a,x,1);
if (x<1) then a:='';
o:=0;
for i:=1 to 40 do if (DrawChrs[i].c=a) then o:=i;
GetOneChar:=o;
End;

Function GetDifference(d1,d2:OneDrawCharRec):Word;

Function diff(a,b:Byte):Byte;
Var r:Byte;
Begin;
r:=3;
if (a<>0) and (b<>0) then r:=1;
if (a=b) then r:=0;
diff:=r;
End;

Var i:Word;
Begin;
i:=0;
inc(i,diff(d1.l,d2.l));
inc(i,diff(d1.r,d2.r));
inc(i,diff(d1.u,d2.u));
inc(i,diff(d1.d,d2.d));
GetDifference:=i;
End;


Var
  dat:OneDrawCharRec;
  i,o,p:LongInt;
Begin;
i:=CurX-ox;
o:=CurY-oy;
if (abs(i)+abs(o)<>1) then exit;
p:=GetOneChar(ox,oy);
dat:=DrawChrs[p];
dat.l:=DrawChrs[GetOneChar(ox-1,oy)].r;
dat.r:=DrawChrs[GetOneChar(ox+1,oy)].l;
dat.u:=DrawChrs[GetOneChar(ox,oy-1)].d;
dat.d:=DrawChrs[GetOneChar(ox,oy+1)].u;
if (i=-1) then dat.l:=DrawingMode;
if (i=+1) then dat.r:=DrawingMode;
if (o=-1) then dat.u:=DrawingMode;
if (o=+1) then dat.d:=DrawingMode;
if (dat.l+dat.r+dat.u+dat.d=DrawingMode) then begin;
  if (i=-1) then dat.r:=DrawingMode;
  if (i=+1) then dat.l:=DrawingMode;
  if (o=-1) then dat.d:=DrawingMode;
  if (o=+1) then dat.u:=DrawingMode;
  end;
p:=0;
o:=9999;
for i:=0 to 40 do if (GetDifference(dat,DrawChrs[i])<o) then begin;
  o:=GetDifference(dat,DrawChrs[i]);
  p:=i;
  end;
a:=ReadLine(oy);
while (length(a)<ox) do a:=a+' ';
a[ox]:=DrawChrs[p].c;
WriteLine(oy,a);
RefreshScr:=RefreshScr or 2;
End;

Begin;
ProcessOneKey:=False;
kezd:
shift:=false;
if (key and $fe00=0) then begin;{simple key}
  key:=key and $ff;
  if (key in [0,255,13,10,8,9]) then key:=ord(' ');
  a:=ReadLine(CurY);
  a:=a+dup(255,' ');
  if InsertMode then insert(chr(key),a,CurX) else a[curX]:=chr(key);
  WriteLine(CurY,a);
  inc(CurX);
  RefreshScr:=RefreshScr or 5;
  goto vege;
  end;
shift:=(key and $100<>0);
key:=key and $feff;
ox:=CurX;
oy:=CurY;
case key of
  $8001:RefreshScr:=$ff;
  $8002:begin;{tab}
    RefreshScr:=RefreshScr or 4;
    a:=ReadLine(CurY-1);
    if (CurX>=length(a)) then begin;
      CurX:=(CurX+7) and $fff8;
      inc(CurX);
      goto vege;
      end;
    o:=FindNextWordStop(a,CurX);
    CurX:=o;
    end;
  $8003:begin;{backspace}
    if (CurX>1) then begin;
      i:=CurX-1;
      EndBut;
      o:=CurX;
      CurX:=i;
      if (i<o) then DelBut;
      goto vege;
      end;
    if (CurY<2) then goto vege;
    dec(CurY);
    EndBut;
    DelBut;
    end;
  $8004:begin;{enter}
    RefreshScr:=RefreshScr or 4;
    if InsertMode then begin;
      if InsertLine(CurY+1) then exit;
      a:=ReadLine(CurY);
      WriteLine(CurY+0,copy(a,1,CurX-1));
      WriteLine(CurY+1,copy(a,CurX,255));
      RefreshScr:=RefreshScr or 2;
      end;
    if (CurY>=MemoryLines) then if InsertLine(CurY+1) then exit;
    inc(CurY);
    CurX:=1;
    end;
  $8006:begin;{insert}
    if shift then begin;
      ClipboardPaste;
      StatusLineClear;
      StatusLineFilNam;
      shift:=false;
      goto vege;
      end;
    RefreshScr:=RefreshScr or 4;
    InsertMode:=not InsertMode;
    end;
  $8007:begin;{delete}
    if shift then begin;
      if not ClipboardCopy then ClipboardDelete;
      StatusLineClear;
      StatusLineFilNam;
      shift:=false;
      goto vege;
      end;
    DelBut;
    end;
  $8206:begin;{ctrl+insert}
    ClipboardCopy;
    StatusLineClear;
    StatusLineFilNam;
    end;
  $8207:begin;{ctrl+delete}
    ClipboardDelete;
    StatusLineClear;
    StatusLineFilNam;
    end;
  $8008:begin;{home}
    RefreshScr:=RefreshScr or 4;
    CurX:=1;
    end;
  $8009:EndBut;{end}
  $8208:begin;{ctrl+home}
    CurY:=BegY+1;
    RefreshScr:=RefreshScr or 4;
    end;
  $8209:begin;{ctrl+end}
    CurY:=BegY+ScrSizY-1;
    RefreshScr:=RefreshScr or 4;
    end;
  $800a:begin;{pgup}
    RefreshScr:=RefreshScr or 4;
    dec(CurY,ScrSizY shr 1);
    end;
  $800b:begin;{pgdn}
    RefreshScr:=RefreshScr or 4;
    inc(CurY,ScrSizY shr 1);
    end;
  $820a:begin;{ctrl+pgup}
    RefreshScr:=RefreshScr or 4;
    CurY:=1;
    CurX:=1;
    end;
  $820b:begin;{ctrl+pgdn}
    RefreshScr:=RefreshScr or 4;
    CurY:=MemoryLines;
    EndBut;
    end;
  $820e:begin;{ctrl+left}
    RefreshScr:=RefreshScr or 4;
    a:=ReadLine(CurY);
    if (CurX<2) and (CurY>1) then begin;
      dec(CurY);
      EndBut;
      goto vege;
      end;
    if (CurX>length(a)+1) then begin;
      EndBut;
      goto vege;
      end;
    o:=FindPrevWordStop(a,CurX-1);
    CurX:=o;
    end;
  $820f:begin;{ctrl+right}
    RefreshScr:=RefreshScr or 4;
    a:=ReadLine(CurY);
    if (CurX>length(a)) then begin;
      inc(CurY);
      CurX:=1;
      goto vege;
      end;
    o:=FindNextWordStop(a,CurX-1);
    CurX:=o;
    end;
  $800c:begin;{up}
    RefreshScr:=RefreshScr or 4;
    dec(CurY);
    end;
  $800d:begin;{down}
    RefreshScr:=RefreshScr or 4;
    inc(CurY);
    end;
  $800e:begin;{left}
    RefreshScr:=RefreshScr or 4;
    dec(CurX);
    end;
  $800f:begin;{right}
    RefreshScr:=RefreshScr or 4;
    inc(CurX);
    end;
  $8014:begin;{f1}
    ProcessHelpScreen;
    StatusLineClear;
    StatusLineFilNam;
    RefreshScr:=RefreshScr or 7;
    end;
  $8015:begin;{f2}
    StatusLineWrite('saving file',ColSttS);
    if SaveCurrentFile(CurrFileName) then begin;
      StatusLineWrite('failed to save file!',ColSttE);
      ReadKey;
      end;
    StatusLineClear;
    StatusLineFilNam;
    end;
  $8016:begin;{f3}
    StatusLineWrite('loading file',ColSttS);
    RefreshScr:=RefreshScr or 2;
    if TestForSavedFile('reload the file') then goto vege;
    if LoadCurrentFile(CurrFileName) then begin;
      StatusLineWrite('failed to load file!',ColSttE);
      ReadKey;
      end;
    StatusLineClear;
    StatusLineFilNam;
    end;
  $8017:begin;{f4}
    inc(DrawingMode);
    if (DrawingMode>2) then DrawingMode:=0;
    RefreshScr:=RefreshScr or 4;
    end;
  $8018:begin;{f5}
    VerticalMode:=not VerticalMode;
    RefreshScr:=RefreshScr or 6;
    end;
  $8019:begin;{f6}
    a:=ReadLine(CurY);
    InsertLine(CurY+1);
    WriteLine(CurY+1,a);
    RefreshScr:=RefreshScr or 2;
    end;
  $801a:begin;{f7}
    if not shift then begin;
      if AskForOneText('find','enter text to find:',FindTxt1) then goto vege;
      end;
    if DoFindNextWordInText then goto vege;
    RefreshScr:=RefreshScr or 4;
    shift:=false;
    end;
  $841a:begin;{alt+f7}
    if DoFindPrevWordInText then goto vege;
    RefreshScr:=RefreshScr or 4;
    end;
  $801b:begin;{f8}
    if not shift then begin;
      if AskForOneText('replace','enter text to find:',FindTxt1) then goto vege;
      if AskForOneText('replace','enter text to replace to:',FindTxt2) then goto vege;
      end;
    if DoFindNextWordInText then goto vege;
    dec(CurX,length(FindTxt1));
    a:=ReadLine(CurY);
    delete(a,CurX,length(FindTxt1));
    insert(FindTxt2,a,CurX);
    WriteLine(CurY,a);
    inc(CurX,length(FindTxt2));
    RefreshScr:=RefreshScr or 6;
    shift:=false;
    end;
  $841b:begin;{alt+f8}
    if DoFindPrevWordInText then goto vege;
    a:=ReadLine(CurY);
    delete(a,CurX,length(FindTxt1));
    insert(FindTxt2,a,CurX);
    WriteLine(CurY,a);
    RefreshScr:=RefreshScr or 6;
    end;
  $801c:begin;{f9}
    a:=BStr(CurY);
    if AskForOneText('goto','enter line number 1..'+BStr(MemoryLines)+':',a) then goto vege;
    i:=BVal(a);
    if (BStr(i)<>a) then goto vege;
    CurY:=i;
    RefreshScr:=RefreshScr or 4;
    end;
  $801d:begin;{f10}
    StatusLineWrite('exiting',ColSttS);
    RefreshScr:=RefreshScr or 2;
    if TestForSavedFile('exit the editor') then goto vege;
    ProcessOneKey:=True;
    Exit;
    end;
  $0271:begin;{ctrl+q}
    a:=ReadLine(CurY);
    while (length(a)<CurX) do a:=a+' ';
    a:=copy(a,1,CurX-1)+getCurrentDate+copy(a,CurX,255);
    WriteLine(CurY,a);
    inc(curX,10);
    RefreshScr:=RefreshScr or 2;
    end;
  $0277:begin;{ctrl+w}
    a:=ReadLine(CurY);
    while (length(a)<CurX) do a:=a+' ';
    a:=copy(a,1,CurX-1)+getCurrentTime+copy(a,CurX,255);
    WriteLine(CurY,a);
    inc(curX,8);
    RefreshScr:=RefreshScr or 2;
    end;
  $0274:begin;{ctrl+t}
    RefreshScr:=RefreshScr or 2;
    a:=ReadLine(CurY);
    if (CurX>length(a)) then begin;
      DelBut;
      goto vege;
      end;
    o:=FindNextWordStop(a,CurX-1);
    a:=copy(a,1,CurX-1)+copy(a,o,255);
    WriteLine(CurY,a);
    end;
  $0279:begin;{ctrl+y}
    RefreshScr:=RefreshScr or 2;
    DeleteLine(CurY);
    CurX:=1;
    end;
  $026e:begin;{ctrl+n}
    RefreshScr:=RefreshScr or 2;
    InsertLine(CurY);
    CurX:=1;
    end;
  $046a:begin;{alt+j}
    DoJustification(1);
    RefreshScr:=RefreshScr or 2;
    end;
  $0472:begin;{alt+r}
    DoJustification(2);
    RefreshScr:=RefreshScr or 2;
    end;
  $046c:begin;{alt+l}
    DoJustification(3);
    RefreshScr:=RefreshScr or 2;
    end;
  $045b:begin;{alt+[}
    DoCaseChanging(1);
    RefreshScr:=RefreshScr or 2;
    end;
  $045d:begin;{alt+]}
    DoCaseChanging(2);
    RefreshScr:=RefreshScr or 2;
    end;
  $045c:begin;{alt+\}
    DoCaseChanging(3);
    RefreshScr:=RefreshScr or 2;
    end;
  $0461:begin;{alt+a}
    RefreshScr:=RefreshScr or 2;
    i:=SelectAsciiChar('insert ascii character');
    if (i<0) then goto vege;
    key:=i and $ff;
    goto kezd
    end;
  $0463:begin;{alt+c}
    DoJustification(4);
    RefreshScr:=RefreshScr or 2;
    end;
  end;
vege:
AlignCheckPosition;
if not shift then exit;
if (DrawingMode>0) then begin; DoDrawing;Exit; end;
RefreshScr:=RefreshScr or 2;
if (ox=SelBX) and (oy=SelBY) then begin;
  SelBX:=CurX;
  SelBY:=CurY;
  goto vege2;
  end;
if (ox=SelEX) and (oy=SelEY) then begin;
  SelEX:=CurX;
  SelEY:=CurY;
  goto vege2;
  end;
if not VerticalMode then begin;
  SelBX:=ox;
  SelBY:=oy;
  SelEX:=CurX;
  SelEY:=CurY;
  goto vege2;
  end;
if (ox=SelEX) and (oy=SelBY) then begin;
  SelEX:=CurX;
  SelBY:=CurY;
  goto vege2;
  end;
if (ox=SelBX) and (oy=SelEY) then begin;
  SelBX:=CurX;
  SelEY:=CurY;
  goto vege2;
  end;
SelBX:=ox;
SelBY:=oy;
SelEX:=CurX;
SelEY:=CurY;
goto vege2;
vege2:
AlignCheckSelection;
End;
