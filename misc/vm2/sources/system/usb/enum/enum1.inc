Var
  pipeLine:LongInt;
  bufferD:array[1..256] of longInt;
  bufferB:array[1..1024] of byte absolute bufferD;
  bufferS:longInt;
  device,maxPack,speed,config,iface:Byte;

Procedure usbExchange(i:LongInt);
Label f1;
Begin;
pipeLineSend(pipeLine,bufferD,i);
f1:
Relequish;
bufferS:=sizeof(bufferD);
if (pipeLineRecv(pipeLine,bufferD,bufferS)<>0) then goto f1;
if (bufferS<1) then goto f1;
End;


Function doPort(i:LongInt):Boolean;
Begin;
write('port '+bstr(i)+': ');
bufferD[1]:=3;bufferD[2]:=i;usbExchange(8);
bufferD[1]:=2;bufferD[2]:=i;usbExchange(8);
bufferD[1]:=4;bufferD[2]:=i;usbExchange(8);
WriteLn('connect:'+BStr(bufferD[2] and 1)+'  enable:'+BStr((bufferD[2] shr 1) and 1)+'  speed:'+BStr(bufferD[3]));
speed:=bufferD[3];
maxPack:=0;
doPort:=(bufferD[2] and 3<>3);
End;

Function doMove:Boolean;
Begin;
doMove:=True;
WriteLn('enumerating to '+BStr(device)+'...');
bufferD[1]:=5;
bufferD[2]:=0;
bufferD[3]:=0;
bufferD[4]:=1;
bufferD[5]:=speed;
bufferD[6]:=0;
bufferB[25]:=0;                 {bmType}
bufferB[26]:=5;                 {bmReq}
WriteWordLSB(bufferB[27],device);       {wValue}
WriteWordLSB(bufferB[29],0);    {wIndex}
WriteWordLSB(bufferB[31],0);    {wLength}
usbExchange(32);
if (bufferD[2]<>0) then exit;
bufferD[1]:=6;
bufferD[2]:=128;
bufferD[3]:=0;
bufferD[4]:=0;
bufferD[5]:=0;
bufferD[6]:=speed;
bufferD[7]:=1;
usbExchange(28);
if (bufferD[2]<>0) then exit;
doMove:=False;
End;

Function doConfig:Boolean;
Begin;
doConfig:=True;
WriteLn('configuring to '+BStr(config)+'...');
bufferD[1]:=5;
bufferD[2]:=device;
bufferD[3]:=0;
bufferD[4]:=1;
bufferD[5]:=speed;
bufferD[6]:=0;
bufferB[25]:=0;                 {bmType}
bufferB[26]:=9;                 {bmReq}
WriteWordLSB(bufferB[27],config);       {wValue}
WriteWordLSB(bufferB[29],0);    {wIndex}
WriteWordLSB(bufferB[31],0);    {wLength}
usbExchange(32);
if (bufferD[2]<>0) then exit;
bufferD[1]:=6;
bufferD[2]:=128;
bufferD[3]:=device;
bufferD[4]:=0;
bufferD[5]:=0;
bufferD[6]:=speed;
bufferD[7]:=1;
usbExchange(28);
if (bufferD[2]<>0) then exit;
doConfig:=False;
End;


Function getDesc(grp,num:Byte):Boolean;
Label f1,f2,f3;
Var
  buf:array[1..1024] of byte;
  siz,tog:LongInt;
  i:LongInt;
Begin;
getDesc:=True;
bufferD[1]:=5;
bufferD[2]:=device;
bufferD[3]:=0;
bufferD[4]:=1;
bufferD[5]:=speed;
bufferD[6]:=0;
bufferB[25]:=$80;               {bmType}
bufferB[26]:=6;                 {bmReq}
bufferB[27]:=num;               {wValue.low}
bufferB[28]:=grp;               {wValue.high}
WriteWordLSB(bufferB[29],0);    {wIndex}
WriteWordLSB(bufferB[31],sizeof(bufferD)-1);    {wLength}
usbExchange(32);
if (bufferD[2]<>0) then begin;
  f3:
  bufferS:=0;
  exit;
  end;
tog:=1;
siz:=0;
f1:
bufferD[1]:=6;
bufferD[2]:=128;
bufferD[3]:=device;
bufferD[4]:=0;
bufferD[5]:=0;
bufferD[6]:=speed;
bufferD[7]:=tog;
usbExchange(28);
if (bufferD[2]<>0) then goto f2;
tog:=tog xor 1;
i:=bufferS-8;
move(bufferB[9],buf[siz+1],i);
inc(siz,i);
if (i>maxPack) then maxPack:=i;
if (i<maxPack) then goto f2;
goto f1;
f2:
bufferS:=siz;
move(buf,bufferB,siz);
getDesc:=False;
End;
