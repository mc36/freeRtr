{$heap 63k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
Const
  screenMax=16384;
  outputMax=512;
Var
  screenData:array[0..screenMax-1] of record c:Byte;a:Byte; end;
  outputData:array[1..outputMax] of byte;
  outputSize:LongInt;
  screenMaxX:LongInt;
  screenMaxY:LongInt;
  screenPosX:LongInt;
  screenPosY:LongInt;
