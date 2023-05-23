Function UnpackCurrentPart:Boolean;
Var
  f:xFile;
  a:String;
Begin;
UnpackCurrentPart:=True;
a:=dataPath+UnpackedFilNam;
xCreate(a);
if ShowErrorMessage(xOpen(f,a,xGenFilMod_rw),'open target') then exit;
if DecodeOneMailPart(dataPath+MsgSelDat.fn+'.'+messageExtension,MsgPrtDat,f) then begin;
  xClose(f);
  ShowErrorMessage(1,'decoding message');
  exit;
  end;
xTruncate(f);
xClose(f);
UnpackCurrentPart:=False;
End;




Function GenerateReplyMsg:Boolean;
Label f1,f2,f3;
Var
  t1,t2,t3:xtText;
  a,b,c:String;
  i,o:LongInt;

procedure put(c:string);
label f1;
begin;
xtSetPos(t3,0);
xtWrite(t1,c);
f1:
if xtEOF(t3) then exit;
if xtEOL(t3) then begin;
  xtWriteLn(t1,xtReadLn(t3,255));
  if not xtEOF(t3) then xtWrite(t1,c);
  end;
xtWrite(t1,xtRead(t3,128));
goto f1;
end;

Begin;
GenerateReplyMsg:=True;
a:=dataPath+EditingFilNam;
xCreate(a);
if ShowErrorMessage(xtOpen(t1,a,false),'opening target') then exit;
xtSetPos(t1,0);
if ShowErrorMessage(xtOpen(t2,dataPath+replyTemplate,true),'opening template') then begin;
  xtClose(t1);
  exit;
  end;
if ShowErrorMessage(xtOpen(t3,dataPath+UnpackedFilNam,true),'opening unpacked') then begin;
  xtClose(t2);
  xtClose(t1);
  exit;
  end;
f1:
if xtEOF(t2) then goto f3;
if xtEOL(t2) then xtWriteLn(t1,xtReadLn(t2,255));
c:=xtRead(t2,128);
f2:
i:=pos('\',c);
if (i<1) then begin;
  xtWrite(t1,c);
  goto f1;
  end;
xtWrite(t1,copy(c,1,i-1));
c:=copy(c,i+1,255)+xtRead(t2,i);
i:=pos('\',c);
if (i<1) then begin;
  xtReadLn(t2,255);
  goto f1;
  end;
a:=kicsi(copy(c,1,i-1));
c:=copy(c,i+1,255);
if (a='oldfrom') then begin;
  NormalizeOneAddress(MsgSelOld.src,a,b);
  if (a='') then a:=b;
  xtWrite(t1,a);
  goto f2;
  end;
if (a='oldto') then begin;
  NormalizeOneAddress(MsgSelOld.trg,a,b);
  if (a='') then a:=b;
  xtWrite(t1,a);
  goto f2;
  end;
if (a='olddate') then begin;
  xtWrite(t1,MsgSelOld.date);
  goto f2;
  end;
if (a='oldsubj') then begin;
  xtWrite(t1,MsgSelOld.subj);
  goto f2;
  end;
if (a='newfrom') then begin;
  NormalizeOneAddress(MsgSelDat.src,a,b);
  if (a='') then a:=b;
  xtWrite(t1,a);
  goto f2;
  end;
if (a='newto') then begin;
  NormalizeOneAddress(MsgSelDat.trg,a,b);
  if (a='') then a:=b;
  xtWrite(t1,a);
  goto f2;
  end;
if (a='newdate') then begin;
  xtWrite(t1,GetDateForHeader);
  goto f2;
  end;
if (a='newsubj') then begin;
  xtWrite(t1,MsgSelDat.subj);
  goto f2;
  end;
if (a='original') then begin;
  put('');
  goto f2;
  end;
if (a='quoted') then begin;
  xtWriteLn(t1,'>');
  put('> ');
  xtWriteLn(t1,'>');
  goto f2;
  end;

goto f1;
f3:
xtClose(t3);
xtClose(t2);
xtTruncate(t1);
xtClose(t1);
GenerateReplyMsg:=False;
End;




Function GenerateNewMsg:Boolean;
Label f1,f2,f3;
Var
  t1,t2:xtText;
  a,b,c:String;
  i,o:LongInt;
Begin;
GenerateNewMsg:=True;
a:=dataPath+EditingFilNam;
xCreate(a);
if ShowErrorMessage(xtOpen(t1,a,false),'opening target') then exit;
xtSetPos(t1,0);
if ShowErrorMessage(xtOpen(t2,dataPath+newTemplate,true),'opening template') then begin;
  xtClose(t1);
  exit;
  end;
f1:
if xtEOF(t2) then goto f3;
if xtEOL(t2) then xtWriteLn(t1,xtReadLn(t2,255));
c:=xtRead(t2,128);
f2:
i:=pos('\',c);
if (i<1) then begin;
  xtWrite(t1,c);
  goto f1;
  end;
xtWrite(t1,copy(c,1,i-1));
c:=copy(c,i+1,255)+xtRead(t2,i);
i:=pos('\',c);
if (i<1) then begin;
  xtReadLn(t2,255);
  goto f1;
  end;
a:=kicsi(copy(c,1,i-1));
c:=copy(c,i+1,255);
if (a='newfrom') then begin;
  a:=userName;
  if (a='') then a:=userAddr;
  xtWrite(t1,a);
  goto f2;
  end;
if (a='newto') then begin;
  NormalizeOneAddress(MsgSelDat.trg,a,b);
  if (a='') then a:=b;
  xtWrite(t1,a);
  goto f2;
  end;
if (a='newdate') then begin;
  xtWrite(t1,GetDateForHeader);
  goto f2;
  end;
if (a='newsubj') then begin;
  xtWrite(t1,MsgSelDat.subj);
  goto f2;
  end;

goto f1;
f3:
xtClose(t2);
xtTruncate(t1);
xtClose(t1);
GenerateNewMsg:=False;
End;






Function FindFirstTargetGuy:Boolean;
Var
  a:String;
  i:LongInt;
Begin;
FindFirstTargetGuy:=True;
for i:=1 to MsgTarMax do begin;
  a:=MsgTarAdr[i];
  if (a<>'') then break;
  end;
MsgSelDat.trg:=a;
if (a='') then exit;
FindFirstTargetGuy:=False;
End;

Function EditMessageHeader:Byte; {0-cancel, 1-save, 2-edit}
Label f1,f2;
Var
  i,o:LongInt;
  a:String;
  c:char;
  f:xFile;
Begin;
f1:
textColor($07);
ClrScr;
GotoXY(1,1);
Write('0 - subj: '+MsgSelDat.subj);
GotoXY(1,2);
Write('recipients:');
for i:=1 to MsgTarMax do begin;
  gotoXY(1,i+2);
  Write(BStr(i)+' - '+MsgTarAdr[i]);
  end;
gotoXY(1,MsgTarMax+3);
WriteLn('e - edit message...');
if (xOpen(f,dataPath+AttachmentList,xGenFilMod_rw)=0) then begin;
  i:=xFileSize(f) div sizeof(a);
  xClose(f);
  end else i:=0;
WriteLn('a - attach another file ('+BStr(i)+' already selected)...');
WriteLn('c - cancel writing...');
WriteLn('s - send message...');
WriteLn('');
Write('choose:');
f2:
c:=LowCase(ReadChrKey);
case c of
  '0':begin;
    a:=MsgSelDat.subj;
    if AskForOneText('subject','enter the subject of message:',a) then goto f1;
    MsgSelDat.subj:=a;
    goto f1;
    end;
  '1'..'8':begin;
    o:=BVal(c);
    a:=MsgTarAdr[o];
    if AskForOneText('recipient','enter address of recipient #'+BStr(o)+':',a) then begin;
      a:=SelectOneFriendGuy;
      if (a='') then goto f1;
      end;
    MsgTarAdr[o]:=a;
    goto f1;
    end;
  'a':begin;
    a:='';
    if AskForOneText('attach','enter name of file to attach:',a) then goto f1;
    if ShowErrorMessage(xOpen(f,a,xGenFilMod_r),'opening file') then goto f1;
    xClose(f);
    if (xOpen(f,dataPath+AttachmentList,xGenFilMod_rw)<>0) then goto f1;
    xSeek(f,xFileSize(f));
    xBlockWrite(f,a,sizeof(a));
    xClose(f);
    goto f1;
    end;
  's':begin;
    if FindFirstTargetGuy then goto f1;
    EditMessageHeader:=1;
    exit;
    end;
  'e':begin;
    if FindFirstTargetGuy then goto f1;
    EditMessageHeader:=2;
    exit;
    end;
  'c':begin; EditMessageHeader:=0;exit; end;
  end;
goto f2;
End;







Procedure SaveTheMessageForSending;
Var
  a,b:String;
  i:LongInt;
  t:xtText;
  f:xFile;
Begin;
WriteLn('saving message...');
b:=dataPath+generateRandomName(dataPath,'.'+messageExtension)+'.';
xCreate(b+messageExtension);
if (xtOpen(t,b+messageExtension,false)<>0) then exit;
MsgWriterBegin(t,MsgSelDat.src,MsgSelDat.trg,MsgSelDat.subj);
MsgWriterAddTxt(t,dataPath+EditingFilNam);
if (xOpen(f,dataPath+AttachmentList,xGenFilMod_r)<>0) then begin;
  xtClose(t);
  exit;
  end;
for i:=1 to xFileSize(f) div sizeof(a) do begin;
  xBlockRead(f,a,sizeof(a));
  MsgWriterAddBin(t,a);
  end;
xClose(f);
MsgWriterFinish(t);
xtClose(t);
xCreate(b+headerExtension);
if (xtOpen(t,b+headerExtension,false)<>0) then exit;
xtWriteLn(t,userAddr);
for i:=1 to MsgTarMax do begin;
  a:=MsgTarAdr[i];
  if (a='') then continue;
  NormalizeOneAddress(a,a,b);
  xtWriteLn(t,b);
  end;
xtClose(t);
End;






Procedure DeleteSelectedMessages;
Var
  f:xFile;
  d:OneMessageHeader;
  i:LongInt;
Begin;
WriteLn('deleting selected messages...');
if (xOpen(f,dataPath+indexFileName,xGenFilMod_r)<>0) then exit;
for i:=1 to xFileSize(f) div sizeof(d) do begin;
  xBlockRead(f,d,sizeof(d));
  if (d.flg and $80=0) then continue;
  xErase(dataPath+d.fn+'.'+headerExtension);
  xErase(dataPath+d.fn+'.'+messageExtension);
  end;
xClose(f);
End;

Procedure DeleteTemporaryFiles;
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
if (kicsi(xFileName(d.name,3))<>'.'+partlistExtension) then goto f2;
xClose(f);
xErase(dataPath+d.name);
goto f1;
f3:
xClose(f);
End;

Procedure PrepareHeaderForWriting;
Var
  a:String;
Begin;
MsgSelOld:=MsgSelDat;
MsgSelDat.subj:=GetSubjectForReply(MsgSelOld.subj);
MsgSelDat.trg:=MsgSelOld.src;
fillchar(MsgTarAdr,sizeof(MsgTarAdr),0);
MsgTarAdr[1]:=MsgSelDat.trg;
MsgSelDat.date:=GetDateForHeader;
if (userName='') then a:='' else a:='"'+UserName+'" ';
MsgSelDat.src:=a+'<'+UserAddr+'>';
a:=dataPath+AttachmentList;
xErase(a);
xCreate(a);
End;








Procedure executeProgramInMe(a,b:String);
Label f1,f2,f3;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p,q:LongInt;
  col:Byte;
  w:Word;

Procedure sendSize;
Begin;
w:=$8001;pipeLineSend(p,w,sizeof(w));
w:=ScrSizX;pipeLineSend(p,w,sizeof(w));
w:=ScrSizY-3;pipeLineSend(p,w,sizeof(w));
End;

Procedure clearScr;
Var a:String;
Begin;
ClrScr;
TextColor($2f);
a:=dup(ScrSizX,' ');
gotoXY(1,1);Write(copy('src: '+MsgSelDat.src+a,1,ScrSizX));
gotoXY(1,2);Write(copy('trg: '+MsgSelDat.trg+a,1,ScrSizX));
gotoXY(1,3);Write(copy('sbj: '+MsgSelDat.subj+a,1,ScrSizX));
gotoXY(1,4);
TextColor(col);
End;

Procedure recvMin(n:LongInt);
Label f1;
Begin;
f1:
i:=n-ab0;
if (i<1) then exit;
relequish;
if (pipeLineRecv(p,ab[ab0+1],i)<>0) then i:=0;
if (i>0) then begin; inc(ab0,i);goto f1; end;
pipeLineStats(p,o,i,i);
if (o<>0) then goto f1;
fillchar(a,sizeof(a),0);
ab0:=n;
End;

Begin;
i:=xExecInside(a,b,q,p);
if (i<>0) then goto f3;
col:=$07;
TextColor(col);
clearScr;
sendSize;
f1:
i:=255;
if (pipeLineRecv(p,a[1],i)<>0) then i:=0;
if (i>0) then begin;
  ab0:=i;
  f2:
  o:=0;
  for i:=1 to ab0 do if (ab[i]=0) then begin; o:=i;break; end;
  if (o<1) then begin; Write(a);goto f1; end;
  Write(copy(a,1,o-1));
  a:=copy(a,o+1,255);
  recvMin(1);
  case ab[1] of
    0:begin; {noop}
      a:=copy(a,2,255);
      goto f2;
      end;
    1:begin; {consolesize}
      sendSize;
      a:=copy(a,2,255);
      goto f2;
      end;
    2:begin; {clrscr}
      clearScr;
      a:=copy(a,2,255);
      goto f2;
      end;
    3:begin; {textcolor}
      recvMin(2);
      col:=ab[2];
      TextColor(col);
      a:=copy(a,3,255);
      goto f2;
      end;
    4:begin; {gotoxy}
      recvMin(5);
      move(ab[4],i,2);
      inc(i,3);
      move(i,ab[4],2);
      write(#0+copy(a,1,5));
      a:=copy(a,6,255);
      goto f2;
      end;
    5:begin; {specialchar}
      recvMin(2);
      write(#0+copy(a,1,2));
      a:=copy(a,3,255);
      goto f2;
      end;
    6:begin; {capability}
      a:=copy(a,2,255);
      w:=$8000;pipeLineSend(p,w,sizeof(w));
      w:=0;pipeLineSend(p,w,sizeof(w));
      goto f2;
      end;
    else goto f3;
    end;
  goto f1;
  end;
while keypressed do begin;
  w:=ReadKey;
  if (w=$8001) then begin;
    ConsoleSize(ScrSizX,ScrSizY);
    sendSize;
    continue;
    end;
  pipeLineSend(p,w,sizeof(w));
  end;
pipeLineStats(p,o,i,i);
if (o=0) then goto f3;
relequish;
goto f1;
f3:
pipeLineClose(p);
BugOS_KillProcess(q);
End;
