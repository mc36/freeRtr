Type
  oneXOTheaderRecord=record
    ver:Word;                   {version=0}
    len:Word;                   {length of data}
    end;



Procedure sendDataPack(pip,len:LongInt;var buffer);
Var
  buf:array[1..1] of byte absolute buffer;
  dat:oneXOTheaderRecord absolute buffer;
Begin;
move(buf,buf[sizeof(dat)+1],len);
dat.ver:=0;
WriteWordMSB(dat.len,len);
pipeLineSend(pip,buf,len+sizeof(dat));
End;



Function receiveHeader(pip:LongInt):LongInt;
Var
  dat:oneXOTheaderRecord;
  i,o,p:LongInt;
Begin;
receiveHeader:=-1;
if (pipeLineStats(pip,p,i,o)<>0) then p:=0;
if (p=0) then exit;
if (i<sizeof(dat)) then begin; receiveHeader:=0;exit; end;
i:=sizeof(dat);
if (pipeLineRecv(pip,dat,i)<>0) then i:=0;
if (i<>sizeof(dat)) then exit;
receiveHeader:=-2;
if (dat.ver<>0) then exit;
i:=ReadWordMSB(dat.len);
if (i<1) or (i>$1000) then exit;
receiveHeader:=i;
End;
