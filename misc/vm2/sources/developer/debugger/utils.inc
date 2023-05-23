Type
  OneIndexRecord=record
    p:LongInt;
    o:LongInt;
    end;
Const
  ColNorm=$07;
  ColPanB=$02;
  ColPanN=$07;
  ColPanI=$0e;
  ColPanS=$17;
  ColWinB=$71;
  ColWinT=$70;
  ColErrB=$47;
  ColErrT=$4f;
Var
  listFil:xtText;
  indxFil:xFile;
  indxMax:LongInt;
  plat_name:String;
  plat_asm:String;
  plat_link:String;
  plat_dis:String;
  plat_emu:String;
  plat_add:String;
  codeBeg:LongInt;
  codeSiz:LongInt;
  memoSiz:LongInt;
  tempFileName:String;


Function toHex(v,s:Longint):String;
Var
  a:String;
  i:LongInt;
Begin;
a:='';
for i:=1 to s do begin;
  a:=byte2hextype(v)+a;
  v:=v shr 8;
  end;
toHex:=a;
End;

Function MVal(a:String):LongInt;
Var
  i:LongInt;
Begin;
a:=kicsi(a);
i:=BVal(copy(a,1,length(a)-1));
if (copy(a,length(a),1)='k') then begin; MVal:=i*1024;exit; end;
if (copy(a,length(a),1)='m') then begin; MVal:=i*1024*1024;exit; end;
MVal:=BVal(a);
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


Procedure GenerateIndex;
Const max=64;
Var
  buf:array[1..max] of OneIndexRecord;
  d:OneIndexRecord;
  siz:longint;
  i,o:longint;
  a:String;

procedure flush;
begin;
if (siz>0) then xBlockWrite(indxFil,buf,siz*sizeof(d));
siz:=0;
end;

procedure add;
begin;
if (siz>=max) then flush;
inc(siz);
buf[siz]:=d;
end;

Begin;
textColor($0a);Write('indexing...');
textColor($07);WriteLn('');
xSeek(indxFil,0);
xtSetPos(listFil,0);
while (xtReadLn(listFil,255)<>'') do;
while (xtReadLn(listFil,255)<>'') do;
siz:=0;
indxMax:=-1;
repeat
  d.p:=xtGetPos(listFil)+1;
  a:=xtReadLn(listFil,255);
  inc(indxMax);
  d.o:=BVal(copy(a,1,pos(' ',a)-1));
  add;
  until (a='');
flush;
xTruncate(indxFil);
End;

Procedure doRangeChk(Var beg,cur:LongInt;min,max,scr:LongInt);
Begin;
if (cur>max) then cur:=max;
if (cur<min) then cur:=min;
if (cur<beg) then beg:=cur;
if (cur>=beg+scr) then beg:=cur-scr+1;
if (beg>max) then beg:=max;
if (beg<min) then beg:=min;
End;

Function doExecute(act,prg,par:String):LongInt;
Var
  i:LongInt;
  w:Word;
Begin;
textColor($0a);Write(act);
textColor($07);WriteLn('');
i:=pos(' ',prg);
if (i>0) then begin;
  par:=par+copy(prg,i,255);
  prg:=copy(prg,1,i-1);
  end;
i:=xExec(prg,par,w);
WriteLn('');
if (i=0) then i:=w;
if (i<>0) then begin;
  textColor($0c);Write('error! press any key!');
  ReadKey;
  textColor($07);WriteLn('');
  end;
doExecute:=i;
End;

Procedure doUploadCode;
Var
  buf:array[1..1024] of byte;
  i,o,p:LongInt;
Begin;
textColor($0a);Write('uploading code...');
textColor($07);WriteLn('');
p:=0;
xSeek(indxFil,0);
while (p<codeSiz) do begin;
  i:=codeSiz-p;
  if (i>sizeof(buf)) then i:=sizeof(buf);
  xBlockRead(indxFil,buf,i);
  debug_writeMem(codeBeg+p,i,buf);
  inc(p,i);
  end;
WriteLn('');
End;


Procedure startEmulator;
Var
  i,o,p:LongInt;
Begin;
if (pipeLineBegListen<>0) then immErr('error starting listener!');
textColor($0a);Write('starting emulator...');
textColor($07);WriteLn('');
i:=pos(' ',plat_emu);
if (i<1) then i:=666;
if (xExecBgnd(copy(plat_emu,1,i-1),BStr(memoSiz)+' '+BStr(myProcessID)+copy(plat_emu,i,255),debuggerProc)<>0) then immErr('error starting emulator!');
debuggerPipe:=0;
while (debuggerPipe=0) do begin;
  Relequish;
  if (pipeLineGetIncoming(i)<>0) then i:=0;
  if (i<1) then begin;
    if BugOS_ProcessExists(debuggerProc) then continue;
    immErr('emulator exited without connection!');
    end;
  if (pipeLineStats(i,p,o,o)<>0) then p:=0;
  if (p<>debuggerProc) then begin; pipeLineClose(i);continue; end;
  debuggerPipe:=i;
  end;
WriteLn(' pid: '+toHex(debuggerProc,4));
WriteLn('pipe: '+toHex(debuggerPipe,4));
WriteLn('');
End;


Procedure decodeFields(x:LongInt;var d:OneRegisterListRecord);
Var
  i,o,p:LongInt;
Begin;
for i:=1 to d.n do begin;
  o:=(1 shl d.d[i].siz)-1;
  p:=d.d[i].pos;
  d.d[i].val:=(x shr p) and o;
  end;
End;

Procedure encodeFields(var x:LongInt;var d:OneRegisterListRecord);
Var
  i,o,p,r:LongInt;
Begin;
for i:=1 to d.n do begin;
  o:=(1 shl d.d[i].siz)-1;
  p:=d.d[i].pos;
  r:=(d.d[i].val and o) shl p;
  o:=o shl p;
  x:=x and (not o);
  x:=x or r;
  end;
End;
