{$heap 127k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc memory.inc}
{$sysinc param.inc}
{$sysinc hex.inc}
{$sysinc bin.inc}
{$sysinc crt.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

{$include lowlevel.inc}
{$include utils.inc}
{$include screen.inc}
{$include debugger.inc}


Label f1,f2,f3,f4;
Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('debugger v1.0, done by Mc at '#%date' '#%time'.');
if (ParamCount<2) then begin;
  WriteLn('using: debugger.code <platform> <filename> [options]');
  WriteLn('options:');
  WriteLn('        mem=bytes');
  WriteLn('        cod={beg/end}');
  halt(1);
  end;

plat_name:=kicsi(ParamStr(1));
a:=paramStr(0);
a:=xFileName(a,1)+xFileName(a,2)+'.cfg';
if (xtOpen(listFil,a,true)<>0) then immErr('error opening config file!');
f1:
if xtEOF(listFil) then immErr('platform not found!');
a:=xtReadLn(listFil,255);
if (a='') then goto f1;
if (kicsi(a)=plat_name) then goto f2;
while (xtReadLn(listFil,255)<>'') do;
goto f1;
f2:
plat_name:=a;
plat_asm:=xtReadLn(listFil,255);
plat_link:=xtReadLn(listFil,255);
plat_dis:=xtReadLn(listFil,255);
plat_emu:=xtReadLn(listFil,255);
plat_add:=xtReadLn(listFil,255);
memoSiz:=MVal(xtReadLn(listFil,255));
xtClose(listFil);
codeBeg:=0;
for i:=3 to paramCount do begin;
  a:=kicsi(paramStr(i));
  if (copy(a,1,4)='mem=') then begin; memoSiz:=MVal(copy(a,5,255));continue; end;
  if (a='cod=beg') then begin; codeBeg:=0;continue; end;
  if (a='cod=end') then begin; codeBeg:=1;continue; end;
  end;

windowNum:=0;
mixedPos:=1;
instBeg:=1;instCur:=1;
regsBeg:=1;regsCur:=1;
flgsBeg:=1;flgsCur:=1;
stckBeg:=0;stckCur:=0;
dumpBeg:=0;dumpCur:=0;dumpAct:=0;
BugOS_MyProcessInfo(myProcessID,i,o);
tempFileName:='debugger-'+toHex(myProcessID,4)+'-temp';

WriteLn('');
textColor($0b);
WriteLn('platform: '+plat_name);
WriteLn('  memory: '+BStr(memoSiz));
WriteLn('    file: '+ParamStr(2));
WriteLn('');

startEmulator;
if (xOpen(indxFil,ParamStr(2),xGenFilMod_r)<>0) then immErr('error opening source!');
codeSiz:=xFileSize(indxFil);
if (codeBeg<>0) then codeBeg:=memoSiz-codeSiz;
if (codeSiz+16>memoSiz) then immErr('code too big!');
doUploadCode;
xClose(indxFil);

debug_readRegNames(registerDat);
debug_readRegsVal(registerDat);
debug_readBitNames(flagsRegIdx,flagsRegDat);
decodeFields(registerDat.d[flagsRegIdx].val,flagsRegDat);
if (codeSiz>0) then begin;
  registerDat.d[instrRegIdx].val:=codeBeg;
  if (codeBeg<>0) then i:=codeBeg else i:=memoSiz;
  registerDat.d[stackRegIdx].val:=i;
  end;
registerOld:=registerDat;
debug_writeRegsVal(registerDat);
debug_readRegsVal(registerDat);

a:=tempFileName+'.lst';
xErase(a);
if (doExecute('disassembling...',plat_dis,paramStr(2)+' out='+a+' org=$'+tohex(codeBeg,4))<>0) then halt(1);

a:=tempFileName+'.idx';
xCreate(a);
xOpen(indxFil,a,xGenFilMod_rw);
xtOpen(listFil,tempFileName+'.lst',true);
GenerateIndex;

RefreshScr:=$ff;
goto f4;
f3:
i:=ReadKey;
case doGeneralKey(i) of
  0:goto f4;
  1:begin;
    GotoXY(1,ScrSizY);
    textColor($07);
    WriteLn('');
    xClose(indxFil);
    xtClose(listFil);
    xErase(tempFileName+'.idx');
    xErase(tempFileName+'.lst');
    xErase(tempFileName+'.obj');
    xErase(tempFileName+'.asm');
    xErase(tempFileName+'.cod');
    Halt(0);
    end;
  end;
case windowNum of
  0:begin;
    if doMixedKey(i) then goto f4;
    case mixedPos of
      1:doInstructionKey(i,ScrSizY-7);
      2:doRegistersKey(i,ScrSizY-7);
      3:doFlagsKey(i,ScrSizY-7);
      4:doDumpKey(i,4);
      5:doStackKey(i,4);
      end;
    end;
  1:doInstructionKey(i,ScrSizY-2);
  2:doRegistersKey(i,ScrSizY-2);
  3:doFlagsKey(i,ScrSizY-2);
  4:doDumpKey(i,ScrSizY-2);
  5:doStackKey(i,ScrSizY-2);
  end;
f4:
if keypressed then goto f3;
if (RefreshScr and $80<>0) then begin;
  PanelResizeAll;
  textColor(ColNorm);clrscr;
  RefreshScr:=$40;
  end;
if (RefreshScr and $40<>0) then begin;
  case windowNum of
    0:DisplayMixedPanel;
    1:DisplaySimplePanel(false);
    2:DisplaySimplePanel(true);
    3:DisplaySimplePanel(true);
    4:DisplaySimplePanel(false);
    5:DisplaySimplePanel(false);
    end;
  RefreshScr:=$20;
  end;
if (RefreshScr and $20<>0) then begin;
  case windowNum of
    0:FillMixedPanel;
    1:FillInstructions;
    2:FillRegisters;
    3:FillFlagsPanel;
    4:FillDumpPanel;
    5:FillStackPanel;
    end;
  RefreshScr:=1;
  end;
if (RefreshScr and $01<>0) then begin;
  case windowNum of
    0:CursorMixedPanel;
    1:CursorInstructions;
    2:CursorRegisters;
    3:CursorFlagsPanel;
    4:CursorDumpPanel;
    5:CursorStackPanel;
    end;
  end;
RefreshScr:=0;
goto f3;
END.