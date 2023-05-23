Type
  oneSTUNheaderRecord=record
    typ:Word;                   {type=0831}
    cmd:Word;                   {0=data, 2=init}
    len:Word;                   {length of data}
    grp:Byte;                   {group}
    end;



Procedure sendInitPack(pip,grp:LongInt);
Const len=$1e;
Var
  buf:array[1..512] of byte;
  dat:oneSTUNheaderRecord absolute buf;
Begin;
fillchar(buf,sizeof(buf),0);
WriteWordMSB(dat.typ,$831);
WriteWordMSB(dat.cmd,2);
WriteWordMSB(dat.len,len);
dat.grp:=grp;
pipeLineSend(pip,buf,len+sizeof(dat));
End;



Procedure sendDataPack(pip,grp,len:LongInt;var buffer);
Var
  buf:array[1..1] of byte absolute buffer;
  dat:oneSTUNheaderRecord absolute buffer;
Begin;
move(buf,buf[sizeof(dat)+1],len);
WriteWordMSB(dat.typ,$831);
dat.cmd:=0;
WriteWordMSB(dat.len,len);
dat.grp:=grp;
pipeLineSend(pip,buf,len+sizeof(dat));
End;



Function receiveHeader(pip:LongInt;var grp:LongInt):LongInt;
Var
  dat:oneSTUNheaderRecord;
  i,o,p:LongInt;
Begin;
receiveHeader:=0;
grp:=-2;
if (pipeLineStats(pip,p,i,o)<>0) then p:=0;
if (p=0) then exit;
if (i<sizeof(dat)) then begin; grp:=-1;exit; end;
i:=sizeof(dat);
if (pipeLineRecv(pip,dat,i)<>0) then i:=0;
if (i<>sizeof(dat)) then exit;
grp:=-3;
if (readWordMSB(dat.typ)<>$831) then exit;
i:=ReadWordMSB(dat.len);
if (i<1) or (i>$1000) then exit;
receiveHeader:=i;
grp:=dat.grp;
End;
