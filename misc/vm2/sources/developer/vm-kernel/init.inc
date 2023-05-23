Procedure executeLoop;
Label f1,f2;
Var cur,act,rnd,num,ofs:LongInt;

procedure exec1(var proc);
var d:oneProcessRecord absolute proc;
begin;
case d.status of
  1:begin; processExecute(d);releq2console; end;
  2:begin; processLoading(d); end;
  end;
end;

procedure exec2(var proc);
var d:oneProcessRecord absolute proc;
begin;
if (d.status<>1) then exit;
if (d.relequish>=4) then exit;
inc(timesActiveStarts);
inc(d.activeRun);
processExecute(d);
releq2console;
end;

Begin;
{$ifdef debug}writeLn('running...');{$endif}
f1:
num:=listingUsed(listingProc);
if (num<1) then immErr('last process terminated!');
for cur:=num downto 1 do begin;
  ofs:=listingSeqNum(listingProc,cur);
  if (ofs=0) then continue;
  ofs:=listingDataOfs(listingProc,ofs);
  if (ofs=0) then continue;
  exec1(totalMemoryByte^[ofs]);
  end;
rnd:=0;
f2:
act:=timesActiveStarts;
for cur:=listingUsed(listingProc) downto 1 do begin;
  ofs:=listingSeqNum(listingProc,cur);
  if (ofs=0) then continue;
  ofs:=listingDataOfs(listingProc,ofs);
  if (ofs=0) then continue;
  exec2(totalMemoryByte^[ofs]);
  end;
inc(rnd);
if (rnd>16) then goto f1;
if (act<>timesActiveStarts) then goto f2;
releq2console;
Relequish;
goto f1;
End;







Procedure initializeAll;

procedure addLine(var b:string;a:string);
begin;
b:=b+a+#13#10;
end;

procedure makeListing(var d:oneListingRecord;m:LongInt);
begin;
fillchar(d,sizeof(d),0);
d.maximum:=m;
d.number:=0;
m:=memoryGetCont(m*sizeof(LongInt),kernelProcNum);
d.beginning:=m div sizeof(LongInt);
if (m=0) then immErr('error allocating list!');
if (m mod sizeof(LongInt)<>0) then immErr('list alignment failed!');
end;

procedure patchProc(var proc);
var d:oneProcessRecord absolute proc;
begin;
d.right:=extraRightsMask;
end;

function loadImage(var proc;a:String):LongInt;
var
  d:oneProcessRecord absolute proc;
  buf:array[1..1024] of byte;
  i,o,p:LongInt;
  f:xFile;
Begin;
loadImage:=0;
if (xOpen(f,a,xGenFilMod_r)<>0) then exit;
p:=xFileSize(f);
if processResize(d,p) then exit;
o:=0;
while (o<p) do begin;
  i:=p-o;
  if (i>sizeof(buf)) then i:=sizeof(buf);
  if (xBlockRead(f,buf,i)<>0) then exit;
  if processCopyTo(d,buf,i,o) then exit;
  inc(o,i);
  end;
xClose(f);
loadImage:=o;
End;

function moveImage(var proc;orig:LongInt):LongInt;
var
  d:oneProcessRecord absolute proc;
  hdr:oneExecutableRecord;
  i:LongInt;
begin;
moveImage:=0;
if processResize(d,sizeof(hdr)) then exit;
move(totalMemoryByte^[orig],hdr,sizeof(hdr));
if processCopyTo(d,hdr,sizeof(hdr),0) then exit;
if processCheck1(d) then exit;
i:=ReadLongLSB(hdr.codeS);
if processResize(d,i) then exit;
if processCopyTo(d,totalMemoryByte^[orig],i,0) then exit;
moveImage:=i;
end;

Var
  origOfs,procID,procOfs:LongInt;
  pt:Pointer;
  i,o:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
Begin;
kernelLog1:='';
kernelLog2:='';
kernelInf1:='';
kernelInf2:='';
addLine(kernelLog1,'ﬂ€€ﬂﬂ€‹   Coded by Mc   ‹€ﬂﬂﬂ€‹ ‹€ﬂﬂﬂ€‹');
addLine(kernelLog1,' €€  €€ ‹‹  ‹‹   ‹‹‹ ‹‹ €€   €€ ﬂ€‹  ﬂﬂ');
addLine(kernelLog1,' €€ﬂﬂ€‹ €€  €€  €€  €€  €€   €€   ﬂﬂ€‹');
addLine(kernelLog2,' €€  €€ €€  €€  €€  €€  €€   €€ ‹‹   €€');
addLine(kernelLog2,'‹€€‹‹€ﬂ ﬂ€‹‹ﬂ€‹ ﬂ€‹‹€€  ﬂ€‹‹‹€ﬂ ﬂ€‹‹‹€ﬂ');
addLine(kernelLog2,'                ‹‹  €€');
addLine(kernelLog2,'                 ﬂﬂﬂﬂ   Virtual Machine');
addLine(kernelInf1,'name: Virtual Machine');
addLine(kernelInf1,'version: 2.1');
addLine(kernelInf1,'date: '#%date);
addLine(kernelInf1,'time: '#%time);
addLine(kernelInf1,'coder: Mc');
addLine(kernelInf2,'platform: '+getCompilerPlatform);
addLine(kernelInf2,'compiler: '+getCompilerNamePas);
addLine(kernelInf2,'assembler: '+getCompilerNameAsm);
Write(kernelLog1);WriteLn(kernelLog2);WriteLn('');

{$ifdef debug}writeLn('allocating memory...');{$endif}
totalMemorySize:=ExtendedMemoryResize(pt,16*1024*1024);
totalMemoryByte:=pt^;
totalMemoryLong:=pt^;
totalMemorySize:=totalMemorySize and memoryPageNegM;
totalMemoryPhys:=$40000;
totalMemoryPage:=totalMemorySize shr memoryPageBits;
if (totalMemorySize<256*1024) then immErr('not enough memory!');
{$ifdef debug}writeLn('bytes='+BStr(totalMemorySize)+' pages='+BStr(totalMemoryPage));{$endif}
fillchar(totalMemoryByte^,totalMemoryPage*sizeof(LongInt),0);
o:=(totalMemoryPage+16)*sizeof(LongInt);
o:=(o+memoryPageMask) shr memoryPageBits;
for i:=0 to o-1 do totalMemoryLong^[i]:=kernelProcNum;
for i:=totalMemoryPage to totalMemoryPage+3 do totalMemoryLong^[i]:=kernelProcNum;

{$ifdef debug}writeLn('getting parameters...');{$endif}
origOfs:=0;
kernelConsoleRaw:=0;
defaultScreenX:=79;
defaultScreenY:=24;
a:=GetAllParameters;
if (ab[0]<1) then immErr('using: vm.code <file>');
if (ab[1]+ab[2]+ab[3]=255*3) then begin;
  {params: -1,extmemPhys,drivePhys,screen,n*(resFirst,resLast)}
  move(ab[4],totalMemoryPhys,sizeof(totalMemoryPhys));
  move(ab[8],origOfs,sizeof(origOfs));
  dec(origOfs,totalMemoryPhys);
  move(ab[12],i,sizeof(i));
  defaultScreenX:=i and $ffff;
  defaultScreenY:=i shr 16;
  {$ifdef debug}writeLn('physical='+BStr(totalMemoryPhys)+' romdrive='+BStr(origOfs)+' screen='+BStr(defaultScreenX)+'x'+BStr(defaultScreenY));{$endif}
  a:=copy(a,16,666);
  while (a<>'') do begin;
    move(a[1],i,sizeof(i));
    move(a[5],o,sizeof(o));
    dec(o,i);
    dec(i,totalMemoryPhys);
    {$ifdef debug}writeLn('  reserved='+BStr(i)+' size='+BStr(o));{$endif}
    memoryAlloCont(i,o,kernelProcNum);
    a:=copy(a,9,666);
    end;
  kernelConsoleRaw:=1;
  end;

{$ifdef debug}writeLn('building lists...');{$endif}
crc32build;
fillchar(listingDrvr,sizeof(listingDrvr),0);
listingNextID:=1;
timesRelequished:=0;
timesFullRounds:=0;
timesHalfRounds:=0;
timesActiveStarts:=0;
kernelConsoleData:=0;
kernelConsoleCtrl:=0;
kernelBigBuf:=memoryGetCont(96*1024,kernelProcNum);
if (kernelBigBuf=0) then immErr('error allocating buffer!');
makeListing(listingProc,1024);
makeListing(listingPipe,2048);
makeListing(listingFile,512);

{$ifdef debug}writeLn('creating process...');{$endif}
procID:=processCreate('startupramdrive.code','!!!startup!!!','',0,0,0,0,0,0);
if (procID=0) then immErr('error creating process!');
{$ifdef debug}writeLn('accessing process...');{$endif}
procOfs:=listingOfs(listingProc,procID);
if (procOfs=0) then immErr('error accessing process!');
if (origOfs<>0) then begin;
  {$ifdef debug}writeLn('moving romdrive...');{$endif}
  o:=moveImage(totalMemoryByte^[procOfs],origOfs);
  end else begin;
  {$ifdef debug}writeLn('loading '+a+'...');{$endif}
  o:=loadImage(totalMemoryByte^[procOfs],a);
  end;
{$ifdef debug}writeLn('got '+BStr(o)+' bytes...');{$endif}
if (o=0) then immErr('error accessing image!');
{$ifdef debug}writeLn('checking header...');{$endif}
if processCheck1(totalMemoryByte^[procOfs]) then immErr('bad image!');
{$ifdef debug}writeLn('checking crc...');{$endif}
if processCheck2(totalMemoryByte^[procOfs],o) then immErr('corrupted image!');
{$ifdef debug}writeLn('starting process...');{$endif}
if processStart(totalMemoryByte^[procOfs]) then immErr('error starting image!');
patchProc(totalMemoryByte^[procOfs]);
End;
