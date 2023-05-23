{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc memory.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

{$include \sources\system\serial\serial.inc}

Var
  mouseBuf:array[1..3] of byte;
  mousePos:Byte;
  MouseMovX:LongInt;
  MouseMovY:LongInt;
  MouseButt:LongInt;  {1=left, 2=right}

Procedure MouseAddChar(b:Byte);
Label f1;
Var i:ShortInt;
Begin;
if (b and $80<>0) then begin;
  f1:
  mousePos:=0;
  exit;
  end;
if (mousePos<1) then begin;
  if (b and $40=0) then goto f1;
  end else begin;
  if (b and $40<>0) then goto f1;
  end;
inc(mousePos);
mouseBuf[mousePos]:=b;
if (mousePos<3) then exit;
MouseButt:=0;
if (mouseBuf[1] and $20<>0) then inc(MouseButt,1);
if (mouseBuf[1] and $10<>0) then inc(MouseButt,2);
i:=((mouseBuf[1] and $3) shl 6) or mouseBuf[2];
inc(MouseMovX,i);
i:=((mouseBuf[1] and $c) shl 4) or mouseBuf[3];
inc(MouseMovY,i);
goto f1;
End;



Label f1,f2;
Var
  pck:record
    b:Byte;
    x:LongInt;
    y:Longint;
    end;
  buf:array[1..1024] of byte;
  i,o,p:LongInt;
  oldButt:LongInt;
  pipe:LongInt;
BEGIN;
WriteLn('serial mouse driver v1.0, done by Mc at '#%date' '#%time'.');
serialProc:=BugOS_findProcNam('serial.code');
serialPort:=BVal(paramStr(1));
if (serialPort<1) then immErr('using: mouseserial.code <port>');
SerialOpen;

serialBuff[1]:=3;       {modem control}
serialBuff[2]:=$03;     {dtr=1; rts=1}
serialCmd(2);

serialBuff[1]:=5;       {line control}
serialBuff[2]:=1200;    {speed=1200}
serialBuff[3]:=0;       {speed}
serialBuff[4]:=7;       {data bits=7}
serialBuff[5]:=0;       {parity=none}
serialBuff[6]:=1;       {stop bits=1}
serialBuff[7]:=0;       {break=0}
serialCmd(7);

serialBuff[1]:=7;       {flow control}
serialBuff[2]:=0;       {dtr=0, rts=0}
serialCmd(2);

pipeLineBegListen;
BugOS_SignDaemoning;

f1:
relequish;
p:=sizeof(buf);
pipeLineRecv(serialData,buf,p);
if (pipeLineGetIncoming(pipe)<>0) then goto f1;
mousePos:=0;
MouseMovX:=0;
MouseMovY:=0;
MouseButt:=0;
oldButt:=0;
f2:
relequish;
pipeLineStats(pipe,i,o,p);
if (i=0) then begin;
  pipeLineClose(pipe);
  goto f1;
  end;
if (oldButt<>MouseButt) or (MouseMovX<>0) or (MouseMovY<>0) then begin;
  pck.b:=MouseButt;
  pck.x:=MouseMovX;
  pck.y:=MouseMovY;
  if (pipeLineSend(pipe,pck,sizeof(pck))<>0) then goto f2;
  MouseMovX:=0;
  MouseMovY:=0;
  oldButt:=MouseButt;
  end;
p:=sizeof(buf);
if (pipeLineRecv(serialData,buf,p)<>0) then goto f2;
if (p<1) then goto f2;
for i:=1 to p do MouseAddChar(buf[i]);
goto f2;
END.