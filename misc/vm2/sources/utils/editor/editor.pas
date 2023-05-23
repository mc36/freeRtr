{$stack 1k}
{$heap 16k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc bin.inc}
{$sysinc hex.inc}
{$sysinc datetime.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}

Const
  PrgTxt='Editor v1.0, done by Mc at '#%date' '#%time'.';
  ColNorm:Byte=$07;
  ColSelc:Byte=$17;
  ColStat:Byte=$20;
  ColSttP:Byte=$2f;
  ColSttS:Byte=$2a;
  ColSttE:Byte=$2c;
  ColWinB:Byte=$71;
  ColWinT:Byte=$70;

{$include memory.inc}
{$include fileio.inc}
{$include screen.inc}
{$include clipboard.inc}
{$include editor.inc}



Label f1;
Var w:Word;
BEGIN;
WriteLn(PrgTxt);
ClearDocument;
ConsoleSize(ScrSizX,ScrSizY);

CurrFileName:=ParamStr(1);
if (CurrFileName='') then begin;
  WriteLn('using: editor.code <filename>');
  Halt(1);
  end;
WriteLn('loading '+CurrFileName+'...');
LoadCurrentFile(CurrFileName);

textColor(ColNorm);clrscr;
StatusLineClear;
StatusLineFilNam;
StatusLineCurPos;
WriteTheText;
PutTheCursor;

f1:
w:=ReadKey;
if ProcessOneKey(w) then begin;
  GotoXY(1,ScrSizY);
  textColor($07);
  WriteLn('');
  Halt(0);
  end;
if keypressed then goto f1;
if (RefreshScr and $80<>0) then begin;
  ConsoleSize(ScrSizX,ScrSizY);
  textColor(ColNorm);clrscr;
  StatusLineClear;
  end;
if (RefreshScr and 1<>0) then WriteOneLine(CurY-BegY);
if (RefreshScr and 2<>0) then WriteTheText;
if (RefreshScr and 4<>0) then StatusLineCurPos;
if (RefreshScr<>0) then PutTheCursor;
RefreshScr:=0;
goto f1;
END.