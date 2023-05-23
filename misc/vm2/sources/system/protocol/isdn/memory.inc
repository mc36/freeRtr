Type
  OneConnectionRecord=record
    stat:LongInt;               {status: 0-free, 1-disable, 2-begPvc, 3-dataConn}
                                {4-begSvc, 5-wait4conn, 6-interface}
    chn:LongInt;                {channel number}
    mode:LongInt;               {mode: 0-free, 1-disabled, 2-pvc, 3-svc}
    cmnd:String;                {command to start}
    time:LongInt;               {time action taken}
    procP:LongInt;              {process id}
    pipeP:LongInt;              {pipeline id}
    cid:String[15];             {call reference with remote}
    bear:LongInt;               {bearer capability}
    srcA:String[31];            {source phone number}
    trgA:String[31];            {target phone number}
    end;
Const
  maxLayer1packs=64;
  v110speedValueTab:array[0..31] of LongInt=(
    0,600,1200,2400,3600,4800,7200,8000,9600,14400,16000,19200,32000,
    38400,48000,56000,0,0,57600,28800,24000,134,100,0,0,50,75,110,150,
    200,300,12000);
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  layer1packDat:array[1..maxLayer1packs] of String[127];
  layer1packSnd:LongInt;
  layer1packWrt:LongInt;
  layer1packNum:LongInt;
  layer1seqSend:LongInt;
  layer1seqRecv:LongInt;
  layer1lasTime:LongInt;
  layer1tryDone:LongInt;
  layer1devName:String;
  layer1devPipe:LongInt;
  controlChannel:LongInt;
  termEndptId:LongInt;
  pollFinalXor:LongInt;         {0-dte, else-dce}
  myPhoneNum:String;
  dialingNum:String;
  callRefValue:LongInt;
  currWaitProc:LongInt;


Function ResizeMem(n:LongInt):Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
ResizeMem:=True;
i:=n*sizeof(OneConnectionRecord);
if (ExtendedMemoryResize(p,i)<i) then exit;
ConnectionNum:=n;
ConnectionDat:=p^;
ResizeMem:=False;
End;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Procedure dumpOnePacket(nam:String;var buffer;siz:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  i:LongInt;
Begin;
Write(nam+' '+BStr(siz)+':');
for i:=1 to siz do write(' '+byte2hextype(buf[i]));
WriteLn('');
End;



Procedure layer1openDevice(a:String);
Var
  buf:array[1..1024] of byte;
  i,o,p:LongInt;
Begin;
WriteLn('process: '+a);
o:=BugOS_findProcNam(a);
if (o=0) then immErr('process not found!');
WriteLn('process#: '+BStr(o));
i:=pipeLineCreate(layer1devPipe,o,65536,true);
if (i<>0) then immErr('unabled to create pipeline!');
WriteLn('pipeline#: '+BStr(layer1devPipe));
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(layer1devPipe,buf,i)<>0) then i:=0;
if (i<1) then immErr('initial packet not received!');
move(buf[1],p,sizeof(p));
WriteLn('address size: '+BStr(p));
move(buf[5],i,sizeof(i));
WriteLn('packet size: '+BStr(i));
o:=17;
Write('station address: ');
for i:=1 to p do begin;
  write(byte2hextype(buf[o])+'-');
  inc(o);
  end;
WriteLn(#8' ');
Write('broadcast address: ');
for i:=1 to p do begin;
  write(byte2hextype(buf[o])+'-');
  inc(o);
  end;
WriteLn(#8' ');
a:='';
while (buf[o]<>0) do begin;
  a:=a+chr(buf[o]);
  inc(o);
  end;
layer1devName:=a;
writeln('device name: "'+a+'"');
End;

Function convertToHexstring(a:String):String;
Var
  b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
b:='';
for i:=1 to ab0 do b:=b+byte2hextype(ab[i]);
convertToHexstring:=b;
End;



Function findOneCallerId(cid:String):LongInt;
Label f1;
Var i:LongInt;
Begin;
if (cid='') then begin; i:=0;goto f1; end;
for i:=1 to ConnectionNum do if (ConnectionDat^[i].cid=cid) then goto f1;
i:=0;
f1:
findOneCallerId:=i;
End;

Function findOneChannelD(c:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to ConnectionNum do if (ConnectionDat^[i].chn=c) then goto f1;
i:=0;
f1:
findOneChannelD:=i;
End;

Function findOneProcessNum(n:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
if (n=0) then begin; i:=0;goto f1; end;
for i:=1 to ConnectionNum do if (ConnectionDat^[i].procP=n) then goto f1;
i:=0;
f1:
findOneProcessNum:=i;
End;

Procedure exchangeStrings(var b,c:String);
Var a:String;
Begin;
a:=b;
b:=c;
c:=a;
End;
