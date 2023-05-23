Type
  OneCommandBlockRecord=record
    sign:LongInt;               {43425355h}
    seq:LongInt;
    xferLen:LongInt;
    flags:Byte;                 {80h=in, 00h=out}
    lun:Byte;                   {0..max-1}
    cmdLen:Byte;
    cmd:array[1..16] of byte;
    end;
  OneStatusBlockRecord=record
    sign:LongInt;               {53425355h}
    seq:LongInt;
    xferDif:LongInt;
    status:Byte;
    end;
Var
  maxDrives:Byte;
  inputNum,outputNum:Byte;
  inputSiz,outputSiz:Byte;
  inputTog,outputTog:Byte;
  sequenceNumber:LongInt;


Procedure protocolGetParams;
Var
  a:String;
  i:LongInt;
Begin;
inputNum:=0;
outputNum:=0;
for i:=paramCount downto 4 do begin;
  a:=paramStr(i);
  if (copy(a,2,2)='ib') then begin;
    inputNum:=BVal(copy(a,1,1));
    inputSiz:=BVal(copy(a,4,255));
    end;
  if (copy(a,2,2)='ob') then begin;
    outputNum:=BVal(copy(a,1,1));
    outputSiz:=BVal(copy(a,4,255));
    end;
  end;
if (inputNum=0) or (outputNum=0) then immErr('error in endpoints!');
inputTog:=0;
outputTog:=0;

bufferD[1]:=5;
bufferD[2]:=device;
bufferD[3]:=0;
bufferD[4]:=1;
bufferD[5]:=speed;
bufferD[6]:=0;
bufferB[25]:=$a1;               {bmType}
bufferB[26]:=$fe;               {bmReq}
WriteWordLSB(bufferB[27],0);    {wValue}
WriteWordLSB(bufferB[29],0);    {wIndex}
WriteWordLSB(bufferB[31],1);    {wLength}
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
maxDrives:=bufferB[9]+1;
End;



Function protocolTransmit(var bufCmd,bufDat;lun,cmd,data:LongInt):LongInt;
Var
  buf:array[1..1] of byte absolute bufDat;
  c:OneCommandBlockRecord;
  s:OneStatusBlockRecord;
  i,o,p:LongInt;
Begin;
protocolTransmit:=-666;
inc(sequenceNumber);
fillchar(c,sizeof(c),0);
WriteLongLSB(c.sign,$43425355);
c.seq:=sequenceNumber;
WriteLongLSB(c.xferLen,data);
c.flags:=$00;
c.lun:=lun;
c.cmdLen:=cmd;
move(bufCmd,c.cmd,cmd);
bufferD[1]:=5;
bufferD[2]:=device;
bufferD[3]:=outputNum;
bufferD[4]:=0;
bufferD[5]:=speed;
bufferD[6]:=outputTog;
move(c,bufferB[25],sizeof(c));
usbExchange(24+sizeof(c));
protocolTransmit:=-bufferD[2];
if (bufferD[2]<>0) then exit;
outputTog:=outputTog xor 1;
p:=0;
while (p<data) do begin;
  o:=data-p;
  if (o>outputSiz) then o:=outputSiz;
  bufferD[1]:=5;
  bufferD[2]:=device;
  bufferD[3]:=outputNum;
  bufferD[4]:=0;
  bufferD[5]:=speed;
  bufferD[6]:=outputTog;
  move(buf[p+1],bufferB[25],o);
  usbExchange(24+o);
  protocolTransmit:=-bufferD[2];
  if (bufferD[2]<>0) then exit;
  outputTog:=outputTog xor 1;
  inc(p,o);
  end;
bufferD[1]:=6;
bufferD[2]:=128;
bufferD[3]:=device;
bufferD[4]:=inputNum;
bufferD[5]:=0;
bufferD[6]:=speed;
bufferD[7]:=inputTog;
usbExchange(28);
protocolTransmit:=-bufferD[2];
if (bufferD[2]<>0) then exit;
inputTog:=inputTog xor 1;
protocolTransmit:=-666;
if (bufferS-8<>sizeof(s)) then exit;
move(bufferB[9],s,sizeof(s));
if (ReadLongLSB(s.sign)<>$53425355) then exit;
if (s.seq<>sequenceNumber) then exit;
protocolTransmit:=s.status;
End;


Function protocolReceive(var bufCmd,bufDat;lun,cmd:LongInt;var data:LongInt):LongInt;
Var
  buf:array[1..1] of byte absolute bufDat;
  c:OneCommandBlockRecord;
  s:OneStatusBlockRecord;
  i,o,p:LongInt;
Begin;
protocolReceive:=-666;
inc(sequenceNumber);
fillchar(c,sizeof(c),0);
WriteLongLSB(c.sign,$43425355);
c.seq:=sequenceNumber;
WriteLongLSB(c.xferLen,data);
c.flags:=$80;
c.lun:=lun;
c.cmdLen:=cmd;
move(bufCmd,c.cmd,cmd);
bufferD[1]:=5;
bufferD[2]:=device;
bufferD[3]:=outputNum;
bufferD[4]:=0;
bufferD[5]:=speed;
bufferD[6]:=outputTog;
move(c,bufferB[25],sizeof(c));
usbExchange(24+sizeof(c));
protocolReceive:=-bufferD[2];
if (bufferD[2]<>0) then exit;
outputTog:=outputTog xor 1;
p:=0;
while (p<data) do begin;
  bufferD[1]:=6;
  bufferD[2]:=128;
  bufferD[3]:=device;
  bufferD[4]:=inputNum;
  bufferD[5]:=0;
  bufferD[6]:=speed;
  bufferD[7]:=inputTog;
  usbExchange(28);
  protocolReceive:=-bufferD[2];
  if (bufferD[2]<>0) then exit;
  inputTog:=inputTog xor 1;
  o:=bufferS-8;
  move(bufferB[9],buf[p+1],o);
  inc(p,o);
  if (o<inputSiz) then break;
  end;
data:=p;
bufferD[1]:=6;
bufferD[2]:=128;
bufferD[3]:=device;
bufferD[4]:=inputNum;
bufferD[5]:=0;
bufferD[6]:=speed;
bufferD[7]:=inputTog;
usbExchange(28);
protocolReceive:=-bufferD[2];
if (bufferD[2]<>0) then exit;
inputTog:=inputTog xor 1;
protocolReceive:=-666;
if (bufferS-8<>sizeof(s)) then exit;
move(bufferB[9],s,sizeof(s));
if (ReadLongLSB(s.sign)<>$53425355) then exit;
if (s.seq<>sequenceNumber) then exit;
protocolReceive:=s.status;
End;
