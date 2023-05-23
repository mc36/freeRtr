{$stack 128k}
{$heap 768k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc alap.inc}
{$sysinc bin.inc}
{$sysinc hex.inc}
{$sysinc datetime.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}

Const PrgTxt='sPas v1.7, done by Mc at '#%date' '#%time'.';


Procedure clreol;
begin;
write(#13'                                                                '#13);
end;

{$include io.inc}
{$include mem.inc}
{$include calc.inc}
{$include equal.inc}
{$include main.inc}


Var
  MyDataRecord1:MemoryOneRecord;
  MyDataRecord2:MemoryOneRecord;
  MyDataRecord3:MemoryOneRecord;
  a,b:String;
BEGIN;
WriteLn(PrgTxt);

MemData1:=@MyDataRecord1;
MemData2:=@MyDataRecord2;
MemData3:=@MyDataRecord3;

b:=ParamStr(1);
if (b='') then begin;
  WriteLn('using: sPas.code <pasFile> [platform]');
  Halt(2);
  end;

a:=paramStr(0);
a:=xFileName(a,1)+xFileName(a,2)+'.cfg';
if (xtOpen(OutCodeHandler,a,true)<>0) then begin;
  WriteLn('error reading configuration!');
  Halt(2);
  end;
UsrLibraryPath:=RepairPath(GetNextCfgLine(OutCodeHandler));
PlatformPath:=RepairPath(GetNextCfgLine(OutCodeHandler));

a:=ParamStr(2);
if (a='') then a:='uniform';
WriteLn('Using '+a+' platform...');
if (xtOpen(OutCodeHandler,PlatformPath+a+'.platform',true)<>0) then begin;
  WriteLn('error reading platform!');
  Halt(2);
  end;
defAddrSize:=BVal(GetNextCfgLine(OutCodeHandler));
defOffsSize:=BVal(GetNextCfgLine(OutCodeHandler));
defIntgSize:=BVal(GetNextCfgLine(OutCodeHandler));
defMxItSize:=BVal(GetNextCfgLine(OutCodeHandler));
defStckSize:=BVal(GetNextCfgLine(OutCodeHandler));
defHeapSize:=BVal(GetNextCfgLine(OutCodeHandler));
defAlignNum:=BVal(GetNextCfgLine(OutCodeHandler));
xtClose(OutCodeHandler);

CompileOneFile(b,a);

Halt(0);
END.