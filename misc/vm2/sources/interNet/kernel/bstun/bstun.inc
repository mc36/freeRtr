Type
  oneSTUNheaderRecord=record
    typ:Word;                   {type=0ebd}
    cmd:Word;                   {0=data, 2=init}
    len:Word;                   {length of data}
    grp:Byte;                   {group}
    un1:Byte;                   {unknown}
    un2:Byte;                   {unknown}
    un3:Byte;                   {unknown}
    un4:Byte;                   {unknown}
    end;


Procedure sendInitPack(pip,grp:LongInt);
Const len=$1b;
Var
  buf:array[1..512] of byte;
  dat:oneSTUNheaderRecord absolute buf;
Begin;
fillchar(buf,sizeof(buf),0);
WriteWordMSB(dat.typ,$ebd);
WriteWordMSB(dat.cmd,2);
WriteWordMSB(dat.len,len+3);
dat.grp:=grp;
pipeLineSend(pip,buf,len+sizeof(dat));
End;



Procedure sendDataPack(pip,grp,len:LongInt;var buffer);
Var
  buf:array[1..1] of byte absolute buffer;
  dat:oneSTUNheaderRecord absolute buffer;
Begin;
move(buf,buf[sizeof(dat)+1],len);
WriteWordMSB(dat.typ,$ebd);
dat.cmd:=0;
WriteWordMSB(dat.len,len+3);
dat.grp:=grp;
dat.un3:=$21;
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
if (readWordMSB(dat.typ)<>$ebd) then exit;
i:=ReadWordMSB(dat.len)-3;
if (i<1) or (i>$1000) then exit;
receiveHeader:=i;
grp:=dat.grp;
End;





Function processCommandPipe(pipe:LongInt):Boolean;
Label f1;
Var
  bufB:array[1..1024] of Byte;
  bufD:array[1..1] of LongInt absolute bufB;
  bufD1:LongInt absolute bufB;
  i,o,p:LongInt;
Begin;
processCommandPipe:=True;
pipeLineStats(pipe,i,o,p);
if (i=0) then exit;
processCommandPipe:=false;
p:=sizeof(bufB);
if (pipeLineRecv(pipe,bufD,p)<>0) then exit;
if (p<sizeof(bufD1)) then exit;
case bufD1 of
  00:begin; {read line status counters}
    bufD[2]:=0; {overrun errors}
    bufD[3]:=0; {parity errors}
    bufD[4]:=0; {framing errors}
    bufD[5]:=0; {break detects}
    bufD[6]:=0; {current line status}
    i:=6*sizeof(bufD1);
    end;
  01:begin; {read modem status counters}
    bufD[2]:=0; {cts changes}
    bufD[3]:=0; {dsr changes}
    bufD[4]:=0; {ring indicator changes}
    bufD[5]:=0; {data carrier detect changes}
    bufD[6]:=11; {current modem status}
    i:=6*sizeof(bufD1);
    end;
  02:begin; {read modem control status}
    bufD[2]:=3; {current modem control}
    i:=2*sizeof(bufD1);
    end;
  03:begin; {set modem control value}
    i:=1*sizeof(bufD1);
    end;
  04:begin; {read line status}
    bufD[2]:=28800; {line speed (bit/sec)}
    bufD[3]:=0; {line speed high dword}
    bufD[4]:=8; {byte length in bits}
    bufD[5]:=0; {parity}
    bufD[6]:=1; {stop bits}
    bufD[7]:=0; {send break}
    i:=7*sizeof(bufD1);
    end;
  05:begin; {write line status}
    i:=1*sizeof(bufD1);
    end;
  06:begin; {read flow control}
    bufD[2]:=1; {used flow control}
    i:=2*sizeof(bufD1);
    end;
  07:begin; {write flow control}
    i:=1*sizeof(bufD1);
    end;
  08:begin; {driver buffer status}
    bufD[2]:=0; {bytes waiting in rx buffer}
    bufD[3]:=0; {bytes waiting in tx buffer}
    i:=3*sizeof(bufD1);
    end;
  09:begin; {clear driver rx buffer}
    i:=1*sizeof(bufD1);
    end;
  10:begin; {clear driver tx buffer}
    i:=1*sizeof(bufD1);
    end;
  11:begin; {clear driver rx and tx buffers}
    i:=1*sizeof(bufD1);
    end;
  else exit;
  end;
pipeLineSend(pipe,bufD,i);
End;
