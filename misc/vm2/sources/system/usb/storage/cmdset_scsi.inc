Var
  blocksMax:LongInt;
  blockSize:LongInt;

Function cmdsetCapacity(lun:LongInt):Boolean;
Var
  buf:array[1..16] of byte;
  i,o:LongInt;
Begin;
cmdsetCapacity:=True;
blocksMax:=0;
blockSize:=0;
fillchar(buf,sizeof(buf),0);
buf[1]:=$25;
buf[2]:=lun shl 5;
i:=8;
if (protocolReceive(buf,buf,lun,12,i)<>0) then exit;
if (i<>8) then exit;
blocksMax:=ReadLongMSB(buf[1]);
blockSize:=ReadLongMSB(buf[5]);
cmdsetCapacity:=False;
End;

Function cmdsetRead(lun,sec:LongInt;var buffer):Boolean;
Var
  buf:array[1..16] of byte absolute buffer;
  i,o:LongInt;
Begin;
cmdsetRead:=True;
fillchar(buf,sizeof(buf),0);
buf[1]:=$28;
buf[2]:=lun shl 5;
WriteLongMSB(buf[3],sec);
buf[9]:=1;
i:=blockSize;
if (protocolReceive(buf,buf,lun,12,i)<>0) then exit;
if (i<>blockSize) then exit;
cmdsetRead:=False;
End;

Function cmdsetWrite(lun,sec:LongInt;var buffer):Boolean;
Var
  buf:array[1..16] of byte;
  i,o:LongInt;
Begin;
cmdsetWrite:=True;
fillchar(buf,sizeof(buf),0);
buf[1]:=$2a;
buf[2]:=lun shl 5;
WriteLongMSB(buf[3],sec);
buf[9]:=1;
if (protocolTransmit(buf,buffer,lun,12,blockSize)<>0) then exit;
cmdsetWrite:=False;
End;
