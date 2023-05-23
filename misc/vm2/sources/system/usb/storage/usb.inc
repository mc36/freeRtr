{-$define debug_usb}
Var
  pipeLine:LongInt;
  bufferD:array[1..256] of longInt;
  bufferB:array[1..1024] of byte absolute bufferD;
  bufferS:longInt;
  device,maxPack,speed:Byte;
{$ifdef debug_usb}
const debug_sequ:LongInt=100000;
{$endif}

Procedure usbExchange(i:LongInt);
Label f1;
{$ifdef debug_usb}
Var
  a:String;
  f:xFile;
{$endif}
Begin;
{$ifdef debug_usb}
inc(debug_sequ);
a:='trans-'+BStr(debug_sequ)+'.cmd';
xCreate(a);
xOpen(f,a,xGenFilMod_rw);
xBlockWrite(f,bufferD,i);
xClose(f);
{$endif}
pipeLineSend(pipeLine,bufferD,i);
f1:
Relequish;
bufferS:=sizeof(bufferD);
if (pipeLineRecv(pipeLine,bufferD,bufferS)<>0) then goto f1;
if (bufferS<1) then goto f1;
{$ifdef debug_usb}
inc(debug_sequ);
a:='trans-'+BStr(debug_sequ)+'.rep';
xCreate(a);
xOpen(f,a,xGenFilMod_rw);
xBlockWrite(f,bufferD,bufferS);
xClose(f);
{$endif}
End;
