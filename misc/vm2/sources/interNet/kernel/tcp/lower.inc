Const
  TCPprotocolNum=$06;
  UDPprotocolNum=$11;
Type
  OneAddressRecord=record
    a1:LongInt;
    a2:LongInt;
    a3:LongInt;
    a4:LongInt;
    end;
  OnePacketRecord=record
    a:OneAddressRecord;
    b:OneAddressRecord;
    p:Byte;
    d:Array[1..4*1024] of byte;
    s:LongInt;
    end;
Var
  LocalAddress:OneAddressRecord;
  LowerProcess:LongInt;
  LowerPipeline:LongInt;
  lastSent:LongInt;


Function doOneIPrequest(a:String;closeAfter:Boolean):String;
Label f1;
Var
  pip:LongInt;
  i,o:LongInt;
Begin;
relequish;
if (pipeLineCreate(pip,LowerProcess,65536,true)<>0) then begin;
  WriteLn('unabled to create pipeline!');
  Halt(1);
  end;
if (pipeLineSend(pip,a[1],length(a))<>0) then begin;
  WriteLn('unabled to send data over pipeline!');
  Halt(1);
  end;
i:=0;
f1:
inc(i);
if (i>128) then begin;
  WriteLn('unabled to receive data from pipeline!');
  Halt(1);
  end;
relequish;
o:=255;
if (pipeLineRecv(pip,a[1],o)<>0) then o:=0;
if (o=0) then goto f1;
if closeAfter then pipeLineClose(pip) else LowerPipeline:=pip;
a[0]:=chr(o);
doOneIPrequest:=a;
End;

Function SendOneLowerPacket(var pck:OnePacketRecord):Boolean;
Begin;
SendOneLowerPacket:=(pipeLineSend(LowerPipeline,pck,pck.s+33)<>0);
if (pck.s>20) then lastSent:=LowerProcess;
End;

Function RecvOneLowerPacket(var pck:OnePacketRecord):Boolean;
Label err;
Var i:LongInt;
Begin;
i:=sizeof(pck);
if (pipeLineRecv(LowerPipeline,pck,i)<>0) then begin;
  err:
  RecvOneLowerPacket:=False;
  exit;
  end;
if (i<33) then begin;
  WriteLn('got a too short packet!');
  goto err;
  end;
pck.s:=i-33;
RecvOneLowerPacket:=True;
End;
