Procedure pasteFromClipboard;
Label f1,vege;
Var
  pipeId:LongInt;
  pipePs:LongInt;
  a:String;
  i,o,p:LongInt;
Begin;
i:=BugOS_findProcNam('clipboard.code');
if (i=0) then exit;
if (pipeLineCreate(pipeId,i,4096,true)<>0) then exit;
pipePs:=0;
if (xSeek(filHdr,filPos)<>0) then immErr('error seeking in file!');
f1:
i:=0;
move(i,bufDat[1],sizeof(i));
i:=pipePs;
move(i,bufDat[5],sizeof(i));
i:=1024;
move(i,bufDat[9],sizeof(i));
if (pipeLineSend(pipeId,bufDat,516)<>0) then goto vege;
i:=16;
while (i=i) do begin;
  Relequish;
  dec(i);
  if (i<0) then goto vege;
  o:=sizeof(bufDat);
  if (pipeLineRecv(pipeId,bufDat,o)<>0) then continue;
  if (o<>0) then break;
  end;
if (o<5) then goto vege;
i:=o-4;
if (xBlockWrite(filHdr,bufDat[5],i)<>0) then immErr('error writing file!');
inc(pipePs,i);
goto f1;
vege:
selBeg:=filPos;
selEnd:=filPos+pipePs-1;
pipeLineClose(pipeId);
End;

Procedure copy2clipboard;
Label vege,veg2;
Var
  pipeId:LongInt;
  a:String;
  i,o,p:LongInt;
Begin;
if (selBeg>selEnd) then exit;
i:=BugOS_findProcNam('clipboard.code');
if (i=0) then exit;
if (pipeLineCreate(pipeId,i,4096,true)<>0) then exit;
i:=2;
move(i,bufDat,sizeof(i));
a:=filNam;move(a,bufDat[5],sizeof(a));
a:='binary';move(a,bufDat[261],sizeof(a));
if (pipeLineSend(pipeId,bufDat,516)<>0) then goto vege;
o:=selBeg;
if (xSeek(filHdr,o)<>0) then immErr('error seeking in file!');
while (o<selEnd) do begin;
  p:=selEnd-o+1;
  if (p>512) then p:=512;
  if (xBlockRead(filHdr,bufDat[5],p)<>0) then immErr('error reading in file!');
  i:=3;
  move(i,bufDat[1],sizeof(i));
  i:=16;
  while (i=i) do begin;
    dec(i);
    if (i<0) then goto veg2;
    if (pipeLineSend(pipeId,bufDat,p+4)=0) then break;
    Relequish;
    end;
  inc(o,p);
  end;
vege:
i:=4;
move(i,bufDat,sizeof(i));
i:=16;
while (i=i) do begin;
  dec(i);
  if (i<0) then goto veg2;
  if (pipeLineSend(pipeId,bufDat,4)=0) then break;
  Relequish;
  end;
veg2:
pipeLineClose(pipeId);
End;





Procedure alignCheck;
Var i,o:LongInt;
Begin;
if (filPos>filSiz) then filPos:=filSiz;
if (filPos<0) then filPos:=0;
o:=ScrSizM*(ScrSizY shr 3);
if (filPos<filScr) then begin;
  filScr:=filPos-o;
  RefreshScr:=RefreshScr or $40;
  end;
i:=ScrSizM*(ScrSizY-3);
if (filPos>filScr+i) then begin;
  filScr:=filPos-i+o;
  RefreshScr:=RefreshScr or $40;
  end;
End;


Function ProcessKey(w:Word):Boolean;
Label kezd,vege;
Var
  a:String;
  i,o:LongInt;
Begin;
ProcessKey:=False;
kezd:
case w of
  $8001:RefreshScr:=$ff;
  $8014:begin;{f1}
    ProcessHelpScreen;
    RefreshScr:=1;
    end;
  $8018:begin;{f5}
    a:='';
    if AskForOneText('goto','byte position to seek to:',a) then goto vege;
    i:=BVal(a);
    if (i<1) then goto vege;
    filPos:=i;
    RefreshScr:=RefreshScr or $20;
    end;
  $801d:begin;{f10}
    ProcessKey:=True;
    goto vege;
    end;
  $8002:begin;{tab}
    filBin:=not filBin;
    filHex:=false;
    RefreshScr:=RefreshScr or $10;
    end;
  $8008:begin;{home}
    filPos:=0;
    RefreshScr:=RefreshScr or $20;
    end;
  $8009:begin;{end}
    filPos:=filSiz;
    RefreshScr:=RefreshScr or $20;
    end;
  $800a:begin;{pgup}
    dec(filPos,(ScrSizY shr 1)*ScrSizM);
    RefreshScr:=RefreshScr or $20;
    end;
  $800b:begin;{pgdn}
    inc(filPos,(ScrSizY shr 1)*ScrSizM);
    RefreshScr:=RefreshScr or $20;
    end;
  $800c:begin;{up}
    dec(filPos,ScrSizM);
    RefreshScr:=RefreshScr or $20;
    end;
  $800d:begin;{down}
    inc(filPos,ScrSizM);
    RefreshScr:=RefreshScr or $20;
    end;
  $800e:begin;{left}
    RefreshScr:=RefreshScr or $10;
    if not filBin then begin;
      filHex:=not filHex;
      if not filHex then goto vege;
      end;
    dec(filPos);
    RefreshScr:=RefreshScr or $20;
    end;
  $800f:begin;{right}
    RefreshScr:=RefreshScr or $10;
    if not filBin then begin;
      filHex:=not filHex;
      if filHex then goto vege;
      end;
    inc(filPos);
    RefreshScr:=RefreshScr or $20;
    end;
  $820e:begin;{ctrl+left}
    dec(filScr);
    RefreshScr:=RefreshScr or $40;
    end;
  $820f:begin;{ctrl+right}
    inc(filScr);
    RefreshScr:=RefreshScr or $40;
    end;
  $820a:begin;{ctrl+pgup}
    dec(filPos,ScrSizM*(ScrSizY-2));
    RefreshScr:=RefreshScr or $20;
    end;
  $820b:begin;{ctrl+pgdn}
    inc(filPos,ScrSizM*(ScrSizY-2));
    RefreshScr:=RefreshScr or $20;
    end;
  $8208:begin;{ctrl+home}
    dec(filPos,ScrSizM*ScrSizY*4);
    RefreshScr:=RefreshScr or $20;
    end;
  $8209:begin;{ctrl+end}
    inc(filPos,ScrSizM*ScrSizY*4);
    RefreshScr:=RefreshScr or $20;
    end;
  $0461:begin;{alt+a}
    i:=SelectAsciiChar('place character');
    if (i<0) then goto vege;
    if (xSeek(filHdr,FilPos)<>0) then immErr('error seeking in file!');
    if (xBlockWrite(filHdr,i,1)<>0) then immErr('error writing file!');
    filSiz:=xFileSize(filHdr);
    bufSiz:=0;
    RefreshScr:=RefreshScr or $60;
    goto vege;
    end;
  $0462:begin;{alt+b}
    SelBeg:=filPos;
    RefreshScr:=RefreshScr or $40;
    end;
  $0465:begin;{alt+e}
    SelEnd:=filPos-1;
    RefreshScr:=RefreshScr or $40;
    end;
  $0466:begin;{alt+f}
    i:=SelectAsciiChar('fill selected area');
    if (i<0) then goto vege;
    fillChar(bufDat,sizeof(bufDat),i);
    bufSiz:=0;
    o:=selBeg;
    if (xSeek(filHdr,o)<>0) then immErr('error seeking in file!');
    while (o<selEnd) do begin;
      i:=selEnd-o+1;
      if (i>sizeof(bufDat)) then i:=sizeof(bufDat);
      if (xBlockWrite(filHdr,bufDat,i)<>0) then immErr('error writing in file!');
      inc(o,i);
      end;
    filSiz:=xFileSize(filHdr);
    RefreshScr:=RefreshScr or $60;
    goto vege;
    end;
  $0469:begin;{alt+i}
    pasteFromClipboard;
    filSiz:=xFileSize(filHdr);
    bufSiz:=0;
    RefreshScr:=RefreshScr or $60;
    goto vege;
    end;
  $046f:begin;{alt+o}
    copy2clipboard;
    filSiz:=xFileSize(filHdr);
    bufSiz:=0;
    RefreshScr:=RefreshScr or $60;
    goto vege;
    end;
  $0474:begin;{alt+t}
    if (xSeek(filHdr,filPos)<>0) then immErr('error seeking in file!');
    if (xTruncate(filHdr)<>0) then immErr('error truncating file!');
    filSiz:=xFileSize(filHdr);
    bufSiz:=0;
    RefreshScr:=RefreshScr or $60;
    goto vege;
    end;
  $8006:begin;{insert}
    a:='';
    if AskForOneText('insert space','number of bytes to insert:',a) then goto vege;
    i:=BVal(a);
    if (i<1) then goto vege;
    selEnd:=i;
    o:=filSiz;
    while (o>filPos) do begin;
      i:=o-filPos;
      if (i>sizeof(bufDat)) then i:=sizeof(bufDat);
      dec(o,i);
      if (xSeek(filHdr,o)<>0) then immErr('error seeking in file!');
      if (xBlockRead(filHdr,bufDat,i)<>0) then immErr('error reading in file!');
      if (xSeek(filHdr,o+selEnd)<>0) then immErr('error seeking in file!');
      if (xBlockWrite(filHdr,bufDat,i)<>0) then immErr('error writing in file!');
      end;
    selBeg:=filPos;
    inc(selEnd,filPos);
    o:=filPos;
    fillChar(bufDat,sizeof(bufDat),0);
    if (xSeek(filHdr,o)<>0) then immErr('error seeking in file!');
    while (o<selEnd) do begin;
      i:=selEnd-o;
      if (i>sizeof(bufDat)) then i:=sizeof(bufDat);
      if (xBlockWrite(filHdr,bufDat,i)<>0) then immErr('error writing in file!');
      inc(o,i);
      end;
    dec(selEnd);
    filSiz:=xFileSize(filHdr);
    bufSiz:=0;
    RefreshScr:=RefreshScr or $60;
    goto vege;
    end;
  $8007:begin;{delete}
    a:='';
    if AskForOneText('delete space','number of bytes to delete:',a) then goto vege;
    i:=BVal(a);
    if (i<1) then goto vege;
    bufSiz:=i;
    o:=filPos+bufSiz;
    while (o<filSiz) do begin;
      i:=filSiz-o;
      if (i>sizeof(bufDat)) then i:=sizeof(bufDat);
      if (xSeek(filHdr,o)<>0) then immErr('error seeking in file!');
      if (xBlockRead(filHdr,bufDat,i)<>0) then immErr('error reading in file!');
      if (xSeek(filHdr,o-bufSiz)<>0) then immErr('error seeking in file!');
      if (xBlockWrite(filHdr,bufDat,i)<>0) then immErr('error writing in file!');
      inc(o,i);
      end;
    if (xSeek(filHdr,filSiz-bufSiz)<>0) then immErr('error seeking in file!');
    if (xTruncate(filHdr)<>0) then immErr('error truncating file!');
    filSiz:=xFileSize(filHdr);
    bufSiz:=0;
    RefreshScr:=RefreshScr or $60;
    goto vege;
    end;
  else begin;
    if (w and $fe00<>0) then goto vege;
    if not filBin then begin;
      a:='0'+upCase(chr(w));
      w:=hextype2byte(a);
      if (a<>byte2hextype(w)) then goto vege;
      i:=readCharacter(filPos);
      if filHex then i:=i and $f0 else begin; w:=w shl 4;i:=i and $0f; end;
      w:=w or i;
      end;
    if (xSeek(filHdr,FilPos)<>0) then immErr('error seeking in file!');
    if (xBlockWrite(filHdr,w,1)<>0) then immErr('error writing file!');
    filSiz:=xFileSize(filHdr);
    bufSiz:=0;
    RefreshScr:=RefreshScr or $60;
    w:=$800f;
    goto kezd;
    end;
  end;
vege:
alignCheck;
End;
