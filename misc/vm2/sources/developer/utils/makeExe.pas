{$heap 7k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc memory.inc}
{$sysinc filesys.inc}

Type
  EXEheaderRec=record
    Sign:Word;
    SizeL:Word;
    SizeH:Word;
    RelocNum:Word;
    HeaderSize:Word;
    MemMin:Word;
    MemMax:Word;
    StackSeg:Word;
    StackPnt:Word;
    ChkSum:Word;
    InitIP:Word;
    InitCS:Word;
    FirstReloc:Word;
    OvrNum:Word;
    reserved:longint;
    end;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Var
  f:xFile;
  d:EXEheaderRec;
  i:LongInt;
  a:String;
BEGIN;
WriteLn('exe header creator v1.0, done by Mc at '#%date' '#%time'.');
a:=paramStr(1);
if (a='') then immErr('using: mkexe <filename>');
if (xOpen(f,a,xGenFilMod_rw)<>0) then immErr('error opening file!');
i:=xFileSize(f);
fillchar(d,sizeof(d),0);
WriteWordLSB(d.InitIP,$100);
WriteWordLSB(d.InitCS,$fff0);
WriteWordLSB(d.StackSeg,$fff0);
WriteWordLSB(d.StackPnt,$100);
WriteWordLSB(d.MemMin,0);
WriteWordLSB(d.MemMax,$ffff);
WriteWordLSB(d.RelocNum,0);
WriteWordLSB(d.FirstReloc,0);
WriteWordLSB(d.HeaderSize,2);
WriteWordLSB(d.OvrNum,0);
WriteWordLSB(d.ChkSum,0);
WriteWordLSB(d.Sign,$5a4d);
WriteWordLSB(d.SizeL,i mod 512);
i:=i div 512;
if (d.SizeL<>0) then inc(i);
WriteWordLSB(d.SizeH,i);
xBlockWrite(f,d,sizeof(d));
xClose(f);
WriteLn('successful!');
END.