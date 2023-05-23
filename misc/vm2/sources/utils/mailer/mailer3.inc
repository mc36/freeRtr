Const MsgTarMax=8;
Var
  UsrSelBeg:LongInt;
  UsrSelCur:LongInt;
  MsgSelBeg:LongInt;
  MsgSelCur:LongInt;
  MsgSelDat:OneMessageHeader;
  MsgSelOld:OneMessageHeader;
  MsgPrtBeg:LongInt;
  MsgPrtCur:LongInt;
  MsgPrtDat:OneMailPartHeader;
  MsgTarAdr:array[1..MsgTarMax] of String;










Function SelectOneFriendGuy:String;
Label f1,f2,f3;
Var
  i,o,p,m:LongInt;
  a,b,c:String;
  f:xFile;
  w:Word;

Procedure putLine(n:LongInt);
Begin;
if (n<0) or (n>=m) then begin;
  a:=' ';
  end else begin;
  xSeek(f,n*sizeof(c));
  xBlockRead(f,c,sizeof(c));
  end;
NormalizeOneAddress(c,a,b);
o:=ScrSizX shr 1;
c:=dup(o,' ');
Write(copy(a+c,1,o));
Write(copy(b+c,1,o));
End;

Procedure alignChk;
Begin;
if (UsrSelCur>m) then UsrSelCur:=m;
if (UsrSelCur<1) then UsrSelCur:=1;
if (p<>UsrSelCur) then RefreshScr:=RefreshScr or $20;
o:=ScrSizY shr 3;
if (UsrSelCur<=UsrSelBeg) then begin;
  UsrSelBeg:=UsrSelCur-o;
  RefreshScr:=RefreshScr or $40;
  end;
i:=ScrSizY-2;
if (UsrSelCur>UsrSelBeg+i) then begin;
  UsrSelBeg:=UsrSelCur-i+o;
  RefreshScr:=RefreshScr or $40;
  end;
End;


Begin;
SelectOneFriendGuy:='';
if (xOpen(f,dataPath+friendListing,xGenFilMod_r)<>0) then exit;
m:=xFileSize(f) div sizeof(c);
RefreshScr:=$ff;
f1:
if (RefreshScr and $80<>0) then begin;
  ConsoleSize(ScrSizX,ScrSizY);
  textColor($07);clrscr;
  TextColor($2f);
  GotoXY(1,1);
  o:=ScrSizX shr 1;
  a:=dup(ScrSizX,' ');
  Write(copy('name'+a,1,o)+copy('address'+a,1,o));
  GotoXY(1,ScrSizY);
  Write(copy('user:            /'+BStr(m)+a,1,ScrSizX-1));
  RefreshScr:=$ff;
  end;
alignChk;
if (RefreshScr and $40<>0) then begin;
  TextColor($07);
  for i:=0 to ScrSizY-3 do begin;
    gotoXY(1,i+2);
    putLine(UsrSelBeg+i);
    end;
  RefreshScr:=RefreshScr or $20;
  end;
if (RefreshScr and $20<>0) then begin;
  if (RefreshScr=$20) then begin;
    GotoXY(1,p-UsrSelBeg+1);
    TextColor($07);
    putLine(p-1);
    end;
  TextColor($17);
  gotoXY(1,UsrSelCur-UsrSelBeg+1);
  putLine(UsrSelCur-1);
  TextColor($2f);
  gotoXY(7,ScrSizY);
  a:=dup(12,' ')+BStr(UsrSelCur);
  Write(copy(a,length(a)-10,255));
  gotoXY(1,UsrSelCur-UsrSelBeg+1);
  end;
RefreshScr:=0;
p:=UsrSelCur;
f2:
w:=ReadKey;
case w of
  $8001:RefreshScr:=$ff;
  $8014:begin;{f1}
    ProcessHelpScreen;
    RefreshScr:=$ff;
    end;
  $801d,$8005:begin;{f10}
    xClose(f);
    exit;
    end;
  $8008:begin;{home}
    UsrSelCur:=1;
    end;
  $8009:begin;{end}
    UsrSelCur:=m;
    end;
  $800a:begin;{pgup}
    dec(UsrSelCur,ScrSizY shr 1);
    end;
  $800b:begin;{pgdn}
    inc(UsrSelCur,ScrSizY shr 1);
    end;
  $800c:begin;{up}
    dec(UsrSelCur,1);
    end;
  $800d:begin;{down}
    inc(UsrSelCur,1);
    end;
  $8004:begin;{enter}
    if (UsrSelCur<1) or (UsrSelCur>m) then goto f2;
    xSeek(f,(UsrSelCur-1)*sizeof(c));
    xBlockRead(f,c,sizeof(c));
    xClose(f);
    SelectOneFriendGuy:=c;
    Exit;
    end;
  end;
alignChk;
if keypressed then goto f2;
goto f1;
End;










Function SelectMessagePart(firsText:Boolean):byte;
{0-exit, 1-read, 2-reply}
Label f1,f2;
Var
  f,ff:xFile;
  d:OneMailPartHeader;
  i,o,p,m:LongInt;
  a:String;
  w:Word;

Procedure putLine(n:LongInt);
Begin;
if (n<0) or (n>=m) then begin;
  fillchar(d,sizeof(d),0);
  end else begin;
  xSeek(f,n*sizeof(d));
  xBlockRead(f,d,sizeof(d));
  end;
o:=ScrSizX shr 1;
a:=dup(o,' ');
Write(copy(d.typ+a,1,o));
Write(copy(d.nam+a,1,o));
End;

Procedure alignChk;
Begin;
if (MsgPrtCur>m) then MsgPrtCur:=m;
if (MsgPrtCur<1) then MsgPrtCur:=1;
if (p<>MsgPrtCur) then RefreshScr:=RefreshScr or $20;
o:=ScrSizY shr 3;
if (MsgPrtCur<=MsgPrtBeg) then begin;
  MsgPrtBeg:=MsgPrtCur-o;
  RefreshScr:=RefreshScr or $40;
  end;
i:=ScrSizY-6;
if (MsgPrtCur>MsgPrtBeg+i) then begin;
  MsgPrtBeg:=MsgPrtCur-i+o;
  RefreshScr:=RefreshScr or $40;
  end;
End;


Begin;
SelectMessagePart:=0;
a:=dataPath+MsgSelDat.fn+'.'+partlistExtension;
if (xOpen(f,a,xGenFilMod_r)<>0) then begin;
  FindMessageParts(dataPath+MsgSelDat.fn);
  if (xOpen(f,a,xGenFilMod_r)<>0) then exit;
  end;
p:=0;
m:=xFileSize(f) div sizeof(d);
if firsText then begin;
  MsgPrtCur:=1;
  while (p<m) do begin;
    xBlockRead(f,d,sizeof(d));
    inc(p);
    if (copy(d.typ,1,5)='text/') then MsgPrtCur:=p;
    if (d.typ<>'text/plain') then continue;
    MsgPrtCur:=p;
    break;
    end;
  xSeek(f,(MsgPrtCur-1)*sizeof(MsgPrtDat));
  xBlockRead(f,MsgPrtDat,sizeof(MsgPrtDat));
  xClose(f);
  SelectMessagePart:=1;
  Exit;
  end;
RefreshScr:=$ff;
MsgPrtCur:=1;
MsgPrtBeg:=0;
f1:
if (RefreshScr and $80<>0) then begin;
  ConsoleSize(ScrSizX,ScrSizY);
  textColor($07);clrscr;
  GotoXY(1,1);Write('to: '+MsgSelDat.trg);
  GotoXY(1,2);Write('from: '+MsgSelDat.src);
  GotoXY(1,3);Write('subj: '+MsgSelDat.subj);
  GotoXY(1,4);Write('date: '+MsgSelDat.date);
  TextColor($2f);
  i:=ScrSizX shr 1;
  a:=dup(ScrSizX,' ');
  GotoXY(1,5);
  Write(copy('type'+a,1,i)+copy('name'+a,1,i));
  GotoXY(1,ScrSizY);
  Write(copy('part:            /'+BStr(m)+a,1,ScrSizX-1));
  RefreshScr:=$ff;
  end;
alignChk;
if (RefreshScr and $40<>0) then begin;
  TextColor($07);
  for i:=0 to ScrSizY-7 do begin;
    gotoXY(1,i+6);
    putLine(MsgPrtBeg+i);
    end;
  RefreshScr:=RefreshScr or $20;
  end;
if (RefreshScr and $20<>0) then begin;
  if (RefreshScr=$20) then begin;
    GotoXY(1,p-MsgPrtBeg+5);
    TextColor($07);
    putLine(p-1);
    end;
  TextColor($17);
  gotoXY(1,MsgPrtCur-MsgPrtBeg+5);
  putLine(MsgPrtCur-1);
  TextColor($2f);
  gotoXY(7,ScrSizY);
  a:=dup(12,' ')+BStr(MsgPrtCur);
  Write(copy(a,length(a)-10,255));
  gotoXY(1,MsgPrtCur-MsgPrtBeg+5);
  end;
RefreshScr:=0;
p:=MsgPrtCur;
f2:
w:=ReadKey;
case w of
  $8001:RefreshScr:=$ff;
  $8014:begin;{f1}
    ProcessHelpScreen;
    RefreshScr:=$ff;
    end;
  $801d,$8005:begin;{f10}
    xClose(f);
    exit;
    end;
  $8008:begin;{home}
    MsgPrtCur:=1;
    end;
  $8009:begin;{end}
    MsgPrtCur:=m;
    end;
  $800a:begin;{pgup}
    dec(MsgPrtCur,ScrSizY shr 1);
    end;
  $800b:begin;{pgdn}
    inc(MsgPrtCur,ScrSizY shr 1);
    end;
  $800c:begin;{up}
    dec(MsgPrtCur,1);
    end;
  $800d:begin;{down}
    inc(MsgPrtCur,1);
    end;
  $8004:begin;{enter}
    if (MsgPrtCur<1) or (MsgPrtCur>m) then goto f2;
    xSeek(f,(MsgPrtCur-1)*sizeof(MsgPrtDat));
    xBlockRead(f,MsgPrtDat,sizeof(MsgPrtDat));
    xClose(f);
    SelectMessagePart:=1;
    Exit;
    end;
  $0077,$0057,$0177,$0157:begin; {w}
    if (MsgPrtCur<1) or (MsgPrtCur>m) then goto f2;
    xSeek(f,(MsgPrtCur-1)*sizeof(d));
    xBlockRead(f,d,sizeof(d));
    RefreshScr:=$ff;
    a:=dataPath+d.nam;
    if AskForOneText('save','enter filename where to save '+d.typ+'...',a) then goto f1;
    if ShowErrorMessage(xCreate(a),'creating target') then goto f1;
    if ShowErrorMessage(xOpen(ff,a,xGenFilMod_rw),'opening target') then goto f1;
    if DecodeOneMailPart(dataPath+MsgSelDat.fn+'.'+messageExtension,d,ff) then ShowErrorMessage(1,'decoding message');
    xTruncate(ff);
    xClose(ff);
    goto f1;
    end;
  $0072,$0052,$0172,$0152:begin; {r}
    if (MsgPrtCur<1) or (MsgPrtCur>m) then goto f2;
    xSeek(f,(MsgPrtCur-1)*sizeof(MsgPrtDat));
    xBlockRead(f,MsgPrtDat,sizeof(MsgPrtDat));
    xClose(f);
    SelectMessagePart:=2;
    Exit;
    end;
  end;
alignChk;
if keypressed then goto f2;
goto f1;
End;
















Function ChooseOneMessage:byte; {0-exit, 1-read, 2-reply, 3-compose}
Label f1,f2,f3;
Var
  f:xFile;
  t:xtText;
  d:OneMessageHeader;
  a,b:String;
  i,o,p,m:LongInt;
  w:Word;

Procedure putLine(n:LongInt);
Begin;
if (n<0) or (n>=m) then begin;
  fillchar(d,sizeof(d),0);
  a:=' ';
  end else begin;
  xSeek(f,n*sizeof(d));
  xBlockRead(f,d,sizeof(d));
  a:='-';
  if (d.flg and $40<>0) then a:='r';
  if (d.flg and $80<>0) then a:='D';
  end;
Write(a+' ');
NormalizeOneAddress(d.src,a,b);
if (a='') then a:=b;
o:=ScrSizX shr 1;
b:=dup(o,' ');
Write(copy(a+b,1,o-2));
Write(copy(d.subj+b,1,o));
End;

Procedure alignChk;
Begin;
if (MsgSelCur>m) then MsgSelCur:=m;
if (MsgSelCur<1) then MsgSelCur:=1;
if (p<>MsgSelCur) then RefreshScr:=RefreshScr or $20;
o:=ScrSizY shr 3;
if (MsgSelCur<=MsgSelBeg) then begin;
  MsgSelBeg:=MsgSelCur-o;
  RefreshScr:=RefreshScr or $40;
  end;
i:=ScrSizY-2;
if (MsgSelCur>MsgSelBeg+i) then begin;
  MsgSelBeg:=MsgSelCur-i+o;
  RefreshScr:=RefreshScr or $40;
  end;
End;

Begin;
ChooseOneMessage:=0;
if (xOpen(f,dataPath+indexFileName,xGenFilMod_rw)<>0) then exit;
m:=xFileSize(f) div sizeof(d);
RefreshScr:=$ff;
f1:
if (RefreshScr and $80<>0) then begin;
  ConsoleSize(ScrSizX,ScrSizY);
  textColor($07);clrscr;
  TextColor($2f);
  i:=ScrSizX shr 1;
  a:=dup(ScrSizX,' ');
  GotoXY(1,1);
  Write(copy('F from'+a,1,i)+copy('subject'+a,1,i));
  GotoXY(1,ScrSizY);
  Write(copy('msg:            /'+BStr(m)+a,1,ScrSizX-1));
  RefreshScr:=$ff;
  end;
alignChk;
if (RefreshScr and $40<>0) then begin;
  TextColor($07);
  for i:=0 to ScrSizY-3 do begin;
    gotoXY(1,i+2);
    putLine(MsgSelBeg+i);
    end;
  RefreshScr:=RefreshScr or $20;
  end;
if (RefreshScr and $20<>0) then begin;
  if (RefreshScr=$20) then begin;
    GotoXY(1,p-MsgSelBeg+1);
    TextColor($07);
    putLine(p-1);
    end;
  TextColor($17);
  gotoXY(1,MsgSelCur-MsgSelBeg+1);
  putLine(MsgSelCur-1);
  TextColor($2f);
  gotoXY(6,ScrSizY);
  a:=dup(12,' ')+BStr(MsgSelCur);
  Write(copy(a,length(a)-10,255));
  gotoXY(1,MsgSelCur-MsgSelBeg+1);
  end;
RefreshScr:=0;
p:=MsgSelCur;
f2:
w:=ReadKey;
case w of
  $8001:RefreshScr:=$ff;
  $8014:begin;{f1}
    ProcessHelpScreen;
    RefreshScr:=$ff;
    end;
  $801d:begin;{f10}
    xClose(f);
    exit;
    end;
  $8008:begin;{home}
    MsgSelCur:=1;
    end;
  $8009:begin;{end}
    MsgSelCur:=m;
    end;
  $800a:begin;{pgup}
    dec(MsgSelCur,ScrSizY shr 1);
    end;
  $800b:begin;{pgdn}
    inc(MsgSelCur,ScrSizY shr 1);
    end;
  $800c:begin;{up}
    dec(MsgSelCur,1);
    end;
  $800d:begin;{down}
    inc(MsgSelCur,1);
    end;
  $0064,$0044,$0164,$0144:begin;{d}
    if (MsgSelCur<1) or (MsgSelCur>m) then goto f2;
    xSeek(f,(MsgSelCur-1)*sizeof(d));
    xBlockRead(f,d,sizeof(d));
    d.flg:=d.flg xor $80;
    xSeek(f,(MsgSelCur-1)*sizeof(d));
    xBlockWrite(f,d,sizeof(d));
    RefreshScr:=RefreshScr or $20;
    end;
  $8004:begin;{enter}
    if (MsgSelCur<1) or (MsgSelCur>m) then goto f2;
    xSeek(f,(MsgSelCur-1)*sizeof(MsgSelDat));
    xBlockRead(f,MsgSelDat,sizeof(MsgSelDat));
    if (SelectMessagePart(true)<1) then goto f2;
    d:=MsgSelDat;
    d.flg:=MsgSelDat.flg or $40;
    xSeek(f,(MsgSelCur-1)*sizeof(d));
    xBlockWrite(f,d,sizeof(d));
    xClose(f);
    ChooseOneMessage:=1;
    Exit;
    end;
  $0072,$0052,$0172,$0152:begin; {r}
    if (MsgSelCur<1) or (MsgSelCur>m) then goto f2;
    xSeek(f,(MsgSelCur-1)*sizeof(MsgSelDat));
    xBlockRead(f,MsgSelDat,sizeof(MsgSelDat));
    if (SelectMessagePart(true)<1) then goto f2;
    xClose(f);
    ChooseOneMessage:=2;
    Exit;
    end;
  $0066,$0046,$0166,$0146:begin; {f}
    if (MsgSelCur<1) or (MsgSelCur>m) then goto f2;
    xSeek(f,(MsgSelCur-1)*sizeof(d));
    xBlockRead(f,d,sizeof(d));
    AddContactToFriends(dataPath,d.src);
    end;
  $0063,$0043,$0163,$0143:begin; {c}
    xClose(f);
    ChooseOneMessage:=3;
    Exit;
    end;
  $0068,$0048,$0168,$0148:begin; {h}
    if (MsgSelCur<1) or (MsgSelCur>m) then goto f2;
    xSeek(f,(MsgSelCur-1)*sizeof(d));
    xBlockRead(f,d,sizeof(d));
    a:=dataPath+d.fn+'.'+messageExtension;
    if (xtOpen(t,a,true)<>0) then goto f3;
    TextColor($07);
    clrScr;
    Write('file: ');
    TextColor($0f);
    WriteLn(a);
    TextColor($07);
    WriteLn('');
    repeat
      a:=xtReadLn(t,255);
      WriteLn(a);
      until (a='');
    xtClose(t);
    TextColor($0f);
    Write('press any key to return!');
    ReadChrKey;
    RefreshScr:=$ff;
    end;
  $8204:begin;{ctrl+enter}
    if (MsgSelCur<1) or (MsgSelCur>m) then goto f2;
    xSeek(f,(MsgSelCur-1)*sizeof(MsgSelDat));
    xBlockRead(f,MsgSelDat,sizeof(MsgSelDat));
    i:=SelectMessagePart(false);
    if (i<1) then begin; RefreshScr:=$ff;goto f1; end;
    if (i=1) then begin;
      d:=MsgSelDat;
      d.flg:=MsgSelDat.flg or $40;
      xSeek(f,(MsgSelCur-1)*sizeof(d));
      xBlockWrite(f,d,sizeof(d));
      end;
    xClose(f);
    ChooseOneMessage:=i;
    Exit;
    end;
  end;
f3:
alignChk;
if keypressed then goto f2;
goto f1;
End;
