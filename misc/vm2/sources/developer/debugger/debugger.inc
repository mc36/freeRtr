Procedure instructionGoto(i:LongInt);
Var
  d:OneIndexRecord;
  a,b:String;

Function QuickReadOne(i:LongInt):String;
Begin;
xSeek(indxFil,i*sizeof(d));
xBlockRead(indxFil,d,sizeof(d));
a[0]:=chr(sizeof(i));
WriteLongMSB(a[1],d.o);
QuickReadOne:=a;
End;

Procedure QuickSwapOne(i,o:LongInt);Begin;End;

{$sysinc quicksrt.inc}

Begin;
b[0]:=chr(sizeof(i));
WriteLongMSB(b[1],i);
i:=QuickFind(0,indxMax-1,b)+1;
if (i<1) then exit;
instCur:=i;
instBeg:=i;
End;

Procedure stackGoto(i:LongInt);
Begin;
i:=i div registerDat.d[stackRegIdx].siz;
stckCur:=i;
stckBeg:=0;
End;

Function doGeneralKey(i:LongInt):LongInt;
Label f1;
Var
  d:OneIndexRecord;
  o:LongInt;
  a:String;
Begin;
doGeneralKey:=0;
case i of
  $8001:RefreshScr:=$ff;
  $801d:begin;{f10}
    doGeneralKey:=1;
    end;
  $801a:begin;{f7}
    registerOld:=registerDat;
    debug_writeRegsVal(registerDat);
    i:=debug_execute;
    f1:
    debug_readRegsVal(registerDat);
    if (i<>0) then begin;
      case i of
        00:a:='no error';
        01:a:='invalid opcode';
        02:a:='address out of range';
        03:a:='io accessed';
        04:a:='division by zero';
        else a:='unknown error code: '+BStr(i);
        end;
      ShowErrorMessage('error executing program:',a);
      end;
    RefreshScr:=$40;
    decodeFields(registerDat.d[flagsRegIdx].val,flagsRegDat);
    if (registerOld.d[stackRegIdx].val=registerDat.d[stackRegIdx].siz*stckCur) then stackGoto(registerDat.d[stackRegIdx].val);
    xSeek(indxFil,(instCur-1)*sizeof(d));
    xBlockRead(indxFil,d,sizeof(d));
    if (registerOld.d[instrRegIdx].val=d.o) then instructionGoto(registerDat.d[instrRegIdx].val);
    doGeneralKey($0431+WindowNum);
    end;
  $801b:begin;{f8}
    if (instCur<1) or (instCur>=indxMax) then exit;
    o:=instCur;
    instructionGoto(registerDat.d[instrRegIdx].val);
    xSeek(indxFil,(instCur)*sizeof(d));
    xBlockRead(indxFil,d,sizeof(d));
    instCur:=o;
    o:=d.o;
    registerOld:=registerDat;
    debug_writeRegsVal(registerDat);
    repeat
      i:=debug_execute;
      debug_readRegsVal(registerDat);
      if (i<>0) then goto f1;
      if keypressed then begin; ReadKey;goto f1; end;
      until (registerDat.d[instrRegIdx].val=o);
    goto f1;
    end;
  $8017:begin;{f4}
    if (instCur<1) or (instCur>indxMax) then exit;
    xSeek(indxFil,(instCur-1)*sizeof(d));
    xBlockRead(indxFil,d,sizeof(d));
    o:=d.o;
    registerOld:=registerDat;
    debug_writeRegsVal(registerDat);
    repeat
      i:=debug_execute;
      debug_readRegsVal(registerDat);
      if (i<>0) then goto f1;
      if keypressed then begin; ReadKey;goto f1; end;
      until (registerDat.d[instrRegIdx].val=o);
    goto f1;
    end;
  $801c:begin;{f9}
    registerOld:=registerDat;
    debug_writeRegsVal(registerDat);
    repeat
      i:=debug_execute;
      if (i<>0) then goto f1;
      until keypressed;
    ReadKey;
    goto f1;
    end;
  $8014:begin;{f1}
    ProcessHelpScreen;
    RefreshScr:=$40;
    end;
  $0431:begin;{alt+1}
    RefreshScr:=$40;
    i:=ScrSizY-7;
    doRangeChk(instBeg,instCur,1,indxMax,i);
    doRangeChk(regsBeg,regsCur,1,registerDat.n,i);
    doRangeChk(flgsBeg,flgsCur,1,flagsRegDat.n,i);
    doRangeChk(dumpBeg,dumpCur,0,memoSiz-1,4*8);
    doRangeChk(stckBeg,stckCur,0,(memoSiz div registerDat.d[stackRegIdx].siz)-1,4);
    if (windowNum<>0) then mixedPos:=windowNum;
    windowNum:=0;
    end;
  $0432:begin;{alt+2}
    doRangeChk(instBeg,instCur,1,indxMax,ScrSizY-2);
    windowNum:=1;
    RefreshScr:=$40;
    end;
  $0433:begin;{alt+3}
    doRangeChk(regsBeg,regsCur,1,registerDat.n,ScrSizY-2);
    windowNum:=2;
    RefreshScr:=$40;
    end;
  $0434:begin;{alt+4}
    doRangeChk(flgsBeg,flgsCur,1,flagsRegDat.n,ScrSizY-2);
    windowNum:=3;
    RefreshScr:=$40;
    end;
  $0435:begin;{alt+5}
    doRangeChk(dumpBeg,dumpCur,0,memoSiz-1,(ScrSizY-2)*8);
    windowNum:=4;
    RefreshScr:=$40;
    end;
  $0436:begin;{alt+6}
    doRangeChk(stckBeg,stckCur,0,(memoSiz div registerDat.d[stackRegIdx].siz)-1,ScrSizY-2);
    windowNum:=5;
    RefreshScr:=$40;
    end;
  else doGeneralKey:=2;
  end;
End;



Function doMixedKey(i:LongInt):Boolean;
Begin;
doMixedKey:=True;
case i of
  $8002:begin;{tab}
    mixedPos:=(mixedPos mod 5)+1;
    RefreshScr:=RefreshScr or $20;
    end;
  $8102:begin;{shift+tab}
    dec(mixedPos);
    if (mixedPos<1) then mixedPos:=5;
    RefreshScr:=RefreshScr or $20;
    end;
  else doMixedKey:=False;
  end;
End;



Procedure doInstructionKey(i,scr:LongInt);
Var
  buf:array[1..1024] of byte;
  d:OneIndexRecord;
  o,p,q:LongInt;
  a,b:String;
  t:xtText;
  f:xFile;
Begin;
case i of
  $800c:begin;{up}
    RefreshScr:=RefreshScr or $20;
    dec(instCur);
    end;
  $800d:begin;{down}
    RefreshScr:=RefreshScr or $20;
    inc(instCur);
    end;
  $800a:begin;{pgup}
    RefreshScr:=RefreshScr or $20;
    dec(instCur,scr shr 1);
    end;
  $800b:begin;{pgdn}
    RefreshScr:=RefreshScr or $20;
    inc(instCur,scr shr 1);
    end;
  $820a:begin;{ctrl+pgup}
    RefreshScr:=RefreshScr or $20;
    dec(instCur,scr);
    end;
  $820b:begin;{ctrl+pgdn}
    RefreshScr:=RefreshScr or $20;
    inc(instCur,scr);
    end;
  $8008:begin;{home}
    RefreshScr:=RefreshScr or $20;
    dec(instCur,scr shl 3);
    end;
  $8009:begin;{end}
    RefreshScr:=RefreshScr or $20;
    inc(instCur,scr shl 3);
    end;
  $8208:begin;{ctrl+home}
    RefreshScr:=RefreshScr or $20;
    instCur:=instBeg;
    end;
  $8209:begin;{ctrl+end}
    RefreshScr:=RefreshScr or $20;
    instCur:=instBeg+scr-1;
    end;
  $046f:begin;{alt+o}
    i:=registerDat.d[instrRegIdx].val;
    instructionGoto(i);
    RefreshScr:=RefreshScr or $20;
    end;
  $046e:begin;{alt+n}
    xSeek(indxFil,(instCur-1)*sizeof(d));
    xBlockRead(indxFil,d,sizeof(d));
    registerDat.d[instrRegIdx].val:=d.o;
    RefreshScr:=RefreshScr or $20;
    end;
  $0467:begin;{alt+g}
    RefreshScr:=RefreshScr or $40;
    xSeek(indxFil,(instCur-1)*sizeof(d));
    xBlockRead(indxFil,d,sizeof(d));
    a:='$'+toHex(d.o,4);
    if AskForOneText('goto','enter offset where to go:',a) then exit;
    i:=BVal(a);
    if (i=0) and (a<>'0') and (a<>'$0') then exit;
    instructionGoto(i);
    end;
  $0461:begin; {alt+a}
    if (instCur<1) or (instCur>indxMax) then exit;
    RefreshScr:=RefreshScr or $40;
    xSeek(indxFil,(instCur-1)*sizeof(d));
    xBlockRead(indxFil,d,sizeof(d));
    a:='';
    if AskForOneText('assembler','enter assembler instruction to place to $'+toHex(d.o,4)+':',a) then exit;
    if (a='') then exit;
    b:=tempFileName+'.asm';
    xErase(b);
    xCreate(b);
    if (xtOpen(t,b,false)<>0) then exit;
    xtWriteLn(t,'org 0'+toHex(d.o,4)+'h');
    xtWriteLn(t,plat_add);
    xtWriteLn(t,a);
    xtClose(t);
    textColor($07);clrscr;RefreshScr:=$ff;
    if (doExecute('compiling...',plat_asm,b)<>0) then exit;
    b:=tempFileName+'.obj';
    if (doExecute('linking...',plat_link,b+' /dob /extcod')<>0) then exit;
    b:=tempFileName+'.cod';
    if (xOpen(f,b,xGenFilMod_r)<>0) then exit;
    i:=xFileSize(f);
    if (i>sizeof(buf)) then i:=sizeof(buf);
    xBlockRead(f,buf,i);
    xClose(f);
    debug_writeMem(d.o,i,buf);
    end;
  $0464:begin; {alt+d}
    RefreshScr:=RefreshScr or $40;
    xSeek(indxFil,0);
    xBlockRead(indxFil,d,sizeof(d));
    p:=d.o;
    xSeek(indxFil,(indxMax-1)*sizeof(d));
    xBlockRead(indxFil,d,sizeof(d));
    xtSetPos(listFil,d.p);
    a:=xtReadLn(listFil,255);
    kicserel('  ',' ',a);
    a:=copy(a,pos(' ',a)+1,255);
    i:=pos(' ',a);
    q:=d.o+(i shr 1);
    a:='$'+tohex(p,4)+'-$'+toHex(q,4);
    if AskForOneText('disassembler','enter interval to disassemble:',a) then exit;
    i:=pos('-',a);
    p:=BVal(copy(a,1,i-1));
    q:=BVal(copy(a,i+1,255));
    if (q>memoSiz) then exit;
    if (p>=q) then exit;
    textColor($07);clrscr;RefreshScr:=$ff;
    textColor($0a);Write('downloading code...');
    textColor($07);WriteLn('');
    WriteLn('begin: '+tohex(p,4));
    WriteLn('  end: '+tohex(q,4));
    WriteLn(' size: '+BStr(q-p));
    b:=tempFileName+'.cod';
    xCreate(b);
    if (xOpen(f,b,xGenFilMod_rw)<>0) then exit;
    o:=p;
    while (p<q) do begin;
      i:=q-p;
      if (i>sizeof(buf)) then i:=sizeof(buf);
      fillchar(buf,i,0);
      debug_readMem(p,i,buf);
      xBlockWrite(f,buf,i);
      inc(p,i);
      end;
    xTruncate(f);
    xClose(f);
    WriteLn('');
    xtClose(listFil);
    a:=tempFileName+'.lst';
    xErase(a);
    if (doExecute('disassembling...',plat_dis,b+' out='+a+' org=$'+tohex(o,4))<>0) then exit;
    xtOpen(listFil,tempFileName+'.lst',true);
    GenerateIndex;
    end;
  end;
doRangeChk(instBeg,instCur,1,indxMax,scr);
End;



Procedure doRegistersKey(i,scr:LongInt);
Var
  a:String;
Begin;
case i of
  $800c:begin;{up}
    RefreshScr:=RefreshScr or $20;
    dec(regsCur);
    end;
  $800d:begin;{down}
    RefreshScr:=RefreshScr or $20;
    inc(regsCur);
    end;
  $800a:begin;{pgup}
    RefreshScr:=RefreshScr or $20;
    dec(regsCur,scr shr 1);
    end;
  $800b:begin;{pgdn}
    RefreshScr:=RefreshScr or $20;
    inc(regsCur,scr shr 1);
    end;
  $820a:begin;{ctrl+pgup}
    RefreshScr:=RefreshScr or $20;
    dec(regsCur,scr);
    end;
  $820b:begin;{ctrl+pgdn}
    RefreshScr:=RefreshScr or $20;
    inc(regsCur,scr);
    end;
  $8008:begin;{home}
    RefreshScr:=RefreshScr or $20;
    dec(regsCur,scr shl 3);
    end;
  $8009:begin;{end}
    RefreshScr:=RefreshScr or $20;
    inc(regsCur,scr shl 3);
    end;
  $8208:begin;{ctrl+home}
    RefreshScr:=RefreshScr or $20;
    regsCur:=regsBeg;
    end;
  $8209:begin;{ctrl+end}
    RefreshScr:=RefreshScr or $20;
    regsCur:=regsBeg+scr-1;
    end;
  $002b:begin; {+}
    if (regsCur<1) or (regsCur>registerDat.n) then exit;
    RefreshScr:=RefreshScr or $20;
    inc(registerDat.d[regsCur].val);
    end;
  $002d:begin; {-}
    if (regsCur<1) or (regsCur>registerDat.n) then exit;
    RefreshScr:=RefreshScr or $20;
    dec(registerDat.d[regsCur].val);
    end;
  $012b:begin; {shift++}
    if (regsCur<1) or (regsCur>registerDat.n) then exit;
    RefreshScr:=RefreshScr or $20;
    inc(registerDat.d[regsCur].val,$100);
    end;
  $012d:begin; {shift+-}
    if (regsCur<1) or (regsCur>registerDat.n) then exit;
    RefreshScr:=RefreshScr or $20;
    dec(registerDat.d[regsCur].val,$100);
    end;
  $022b:begin; {ctrl++}
    if (regsCur<1) or (regsCur>registerDat.n) then exit;
    RefreshScr:=RefreshScr or $20;
    inc(registerDat.d[regsCur].val,$10000);
    end;
  $022d:begin; {ctrl+-}
    if (regsCur<1) or (regsCur>registerDat.n) then exit;
    RefreshScr:=RefreshScr or $20;
    dec(registerDat.d[regsCur].val,$10000);
    end;
  $042b:begin; {alt++}
    if (regsCur<1) or (regsCur>registerDat.n) then exit;
    RefreshScr:=RefreshScr or $20;
    inc(registerDat.d[regsCur].val,$1000000);
    end;
  $042d:begin; {alt+-}
    if (regsCur<1) or (regsCur>registerDat.n) then exit;
    RefreshScr:=RefreshScr or $20;
    dec(registerDat.d[regsCur].val,$1000000);
    end;
  $0463:begin; {alt+c}
    if (regsCur<1) or (regsCur>registerDat.n) then exit;
    RefreshScr:=RefreshScr or $40;
    a:='$'+toHex(registerDat.d[regsCur].val,registerDat.d[regsCur].siz);
    if AskForOneText('change register','new value of '+registerDat.d[regsCur].nam+':',a) then exit;
    i:=BVal(a);
    if (i=0) and (a<>'0') and (a<>'$0') then exit;
    registerDat.d[regsCur].val:=i;
    end;
  end;
doRangeChk(regsBeg,regsCur,1,registerDat.n,scr);
decodeFields(registerDat.d[flagsRegIdx].val,flagsRegDat);
End;



Procedure doFlagsKey(i,scr:LongInt);
Begin;
case i of
  $800c:begin;{up}
    RefreshScr:=RefreshScr or $20;
    dec(flgsCur);
    end;
  $800d:begin;{down}
    RefreshScr:=RefreshScr or $20;
    inc(flgsCur);
    end;
  $800a:begin;{pgup}
    RefreshScr:=RefreshScr or $20;
    dec(flgsCur,scr shr 1);
    end;
  $800b:begin;{pgdn}
    RefreshScr:=RefreshScr or $20;
    inc(flgsCur,scr shr 1);
    end;
  $820a:begin;{ctrl+pgup}
    RefreshScr:=RefreshScr or $20;
    dec(flgsCur,scr);
    end;
  $820b:begin;{ctrl+pgdn}
    RefreshScr:=RefreshScr or $20;
    inc(flgsCur,scr);
    end;
  $8008:begin;{home}
    RefreshScr:=RefreshScr or $20;
    dec(flgsCur,scr shl 3);
    end;
  $8009:begin;{end}
    RefreshScr:=RefreshScr or $20;
    inc(flgsCur,scr shl 3);
    end;
  $8208:begin;{ctrl+home}
    RefreshScr:=RefreshScr or $20;
    flgsCur:=flgsBeg;
    end;
  $8209:begin;{ctrl+end}
    RefreshScr:=RefreshScr or $20;
    flgsCur:=flgsBeg+scr-1;
    end;
  $002b:begin; {+}
    if (flgsCur<1) or (flgsCur>flagsRegDat.n) then exit;
    RefreshScr:=RefreshScr or $20;
    inc(flagsRegDat.d[flgsCur].val);
    end;
  $002d:begin; {-}
    if (flgsCur<1) or (flgsCur>flagsRegDat.n) then exit;
    RefreshScr:=RefreshScr or $20;
    dec(flagsRegDat.d[flgsCur].val);
    end;
  $0020:begin; {space}
    if (flgsCur<1) or (flgsCur>flagsRegDat.n) then exit;
    RefreshScr:=RefreshScr or $20;
    flagsRegDat.d[flgsCur].val:=flagsRegDat.d[flgsCur].val xor 1;
    end;
  $30..$39,$61..$66,$41..$46,$141..$146:begin; {0..9,a..f}
    if (flgsCur<1) or (flgsCur>flagsRegDat.n) then exit;
    RefreshScr:=RefreshScr or $20;
    flagsRegDat.d[flgsCur].val:=BVal('$'+chr(i));
    end;
  end;
doRangeChk(flgsBeg,flgsCur,1,flagsRegDat.n,scr);
encodeFields(registerDat.d[flagsRegIdx].val,flagsRegDat);
decodeFields(registerDat.d[flagsRegIdx].val,flagsRegDat);
End;




Procedure doDumpKey(i,scr:LongInt);
Var
  a:String;
  b:Byte;
Begin;
if (dumpAct<2) then begin;
  if (i and $feff in [$30..$39,$61..$66,$41..$46]) then begin;
    debug_readMem(dumpCur,1,b);
    i:=BVal('$'+chr(i));
    if (dumpAct=0) then b:=(b and $f)+(i shl 4) else b:=(b and $f0)+i;
    debug_writeMem(dumpCur,1,b);
    i:=$800f;
    end;
  end else begin;
  if (i and $fe00=$00) then begin;
    b:=i;
    debug_writeMem(dumpCur,1,b);
    i:=$800f;
    end;
  end;
case i of
  $800e:begin;{left}
    RefreshScr:=RefreshScr or $20;
    if (dumpAct<2) then begin;
      dumpAct:=(dumpAct-1) and 1;
      if (dumpAct<1) then exit;
      end;
    dec(dumpCur);
    end;
  $800f:begin;{right}
    RefreshScr:=RefreshScr or $20;
    if (dumpAct<2) then begin;
      dumpAct:=(dumpAct+1) and 1;
      if (dumpAct>0) then exit;
      end;
    inc(dumpCur);
    end;
  $800c:begin;{up}
    RefreshScr:=RefreshScr or $20;
    dec(dumpCur,8);
    end;
  $800d:begin;{down}
    RefreshScr:=RefreshScr or $20;
    inc(dumpCur,8);
    end;
  $800a:begin;{pgup}
    RefreshScr:=RefreshScr or $20;
    dec(dumpCur,scr shl 2);
    end;
  $800b:begin;{pgdn}
    RefreshScr:=RefreshScr or $20;
    inc(dumpCur,scr shl 2);
    end;
  $820a:begin;{ctrl+pgup}
    RefreshScr:=RefreshScr or $20;
    dec(dumpCur,scr shl 3);
    end;
  $820b:begin;{ctrl+pgdn}
    RefreshScr:=RefreshScr or $20;
    inc(dumpCur,scr shl 3);
    end;
  $8008:begin;{home}
    RefreshScr:=RefreshScr or $20;
    dec(dumpCur,scr shl 6);
    end;
  $8009:begin;{end}
    RefreshScr:=RefreshScr or $20;
    inc(dumpCur,scr shl 6);
    end;
  $8208:begin;{ctrl+home}
    RefreshScr:=RefreshScr or $20;
    dumpCur:=dumpBeg;
    end;
  $8209:begin;{ctrl+end}
    RefreshScr:=RefreshScr or $20;
    dumpCur:=dumpBeg+(scr*8)-1;
    end;
  $0454,$0474:begin;{alt+t}
    RefreshScr:=RefreshScr or $20;
    if (dumpAct<2) then dumpAct:=2 else dumpAct:=0;
    end;
  $0467:begin;{alt+g}
    RefreshScr:=RefreshScr or $40;
    a:='$'+toHex(dumpCur,4);
    if AskForOneText('goto','enter offset where to go:',a) then exit;
    i:=BVal(a);
    if (i=0) and (a<>'0') and (a<>'$0') then exit;
    if (dumpAct<2) then dumpAct:=0;
    dumpCur:=i;
    dumpBeg:=i;
    end;
  end;
doRangeChk(dumpBeg,dumpCur,0,memoSiz-1,scr*8);
End;




Procedure doStackKey(i,scr:LongInt);
Var
  o:LongInt;
  a:String;
Begin;
o:=registerDat.d[stackRegIdx].siz;
case i of
  $800c:begin;{up}
    RefreshScr:=RefreshScr or $20;
    dec(stckCur);
    end;
  $800d:begin;{down}
    RefreshScr:=RefreshScr or $20;
    inc(stckCur);
    end;
  $800a:begin;{pgup}
    RefreshScr:=RefreshScr or $20;
    dec(stckCur,scr shr 1);
    end;
  $800b:begin;{pgdn}
    RefreshScr:=RefreshScr or $20;
    inc(stckCur,scr shr 1);
    end;
  $820a:begin;{ctrl+pgup}
    RefreshScr:=RefreshScr or $20;
    dec(stckCur,scr);
    end;
  $820b:begin;{ctrl+pgdn}
    RefreshScr:=RefreshScr or $20;
    inc(stckCur,scr);
    end;
  $8008:begin;{home}
    RefreshScr:=RefreshScr or $20;
    dec(stckCur,scr shl 3);
    end;
  $8009:begin;{end}
    RefreshScr:=RefreshScr or $20;
    inc(stckCur,scr shl 3);
    end;
  $8208:begin;{ctrl+home}
    RefreshScr:=RefreshScr or $20;
    stckCur:=stckBeg;
    end;
  $8209:begin;{ctrl+end}
    RefreshScr:=RefreshScr or $20;
    stckCur:=stckBeg+scr-1;
    end;
  $046f:begin;{alt+o}
    i:=registerDat.d[stackRegIdx].val;
    stackGoto(i);
    RefreshScr:=RefreshScr or $20;
    end;
  $046e:begin;{alt+n}
    registerDat.d[stackRegIdx].val:=stckCur*o;
    RefreshScr:=RefreshScr or $20;
    end;
  $0467:begin;{alt+g}
    RefreshScr:=RefreshScr or $40;
    a:='$'+toHex(stckCur*o,4);
    if AskForOneText('goto','enter offset where to go:',a) then exit;
    i:=BVal(a);
    if (i=0) and (a<>'0') and (a<>'$0') then exit;
    stackGoto(i);
    end;
  end;
doRangeChk(stckBeg,stckCur,0,(memoSiz div o)-1,scr);
End;
