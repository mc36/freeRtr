{$ifdef kilo}
const OnePacketSize=1024;
{$else}
const OnePacketSize=128;
{$endif}
Type
  OnePacketHeaderRecord=record
    typ:byte;
    num:byte;
    mun:byte;
    end;



Procedure xmod_wait(tim:LongInt);
Label f1;
Var
  i,o:LongInt;
  buf:array[1..512] of byte;
Begin;
timer2start;
o:=CurrentTime;
f1:
relequish;
timer2start;
if (GetTimePast(o)>=tim) then exit;
goto f1;
End;


Procedure xmod_flush(tim:LongInt);
Label f1;
Var
  i,o:LongInt;
  buf:array[1..512] of byte;
Begin;
timer2start;
o:=CurrentTime;
f1:
relequish;
timer2start;
i:=sizeof(buf);
if (pipeLineRecv(pipe,buf,i)<>0) then i:=0;
if (i>0) then o:=CurrentTime;
if (GetTimePast(o)>=tim) then exit;
goto f1;
End;



Procedure xmod_sendReq(c:Byte);
Begin;
pipeLineSend(pipe,c,sizeof(c));
End;



Function xmod_recvReq:LongInt;
Label f1;
Var
  i,o:LongInt;
  buf:array[1..512] of byte;
Begin;
xmod_recvReq:=-1;
timer2start;
o:=CurrentTime;
f1:
relequish;
timer2start;
if (GetTimePast(o)>3) then exit;
i:=sizeof(buf);
if (pipeLineRecv(pipe,buf,i)<>0) then i:=0;
if (i<1) then goto f1;
if (i>1) then exit;
xmod_recvReq:=buf[1];
End;





Procedure xmod_sendPck(typ,seq,siz:LongInt;var buffer);
Var
  buf:array[1..1] of byte absolute buffer;
  hdr:OnePacketHeaderRecord;
  i:LongInt;
  w:Word;
  b:Byte;
Begin;
hdr.typ:=typ;
hdr.num:=seq;
hdr.mun:=not seq;
while (pipeLineSend(pipe,hdr,sizeof(hdr))<>0) do relequish;
while (pipeLineSend(pipe,buf,siz)<>0) do relequish;
{$ifdef crc}
w:=0;
crc16update(w,buf,siz);
WriteWordMSB(hdr,w);
siz:=2;
{$else}
b:=0;
chksum8update(b,buf,siz);
hdr.typ:=b;
siz:=1;
{$endif}
while (pipeLineSend(pipe,hdr,siz)<>0) do relequish;
End;




Function xmod_recvPck(var seq,siz:LongInt;var buffer):String;
Var
  buf:array[1..1] of byte absolute buffer;
  hdr:OnePacketHeaderRecord;
  i,o,s:LongInt;
  w:Word;
  b:Byte;

function rx(s,t:longint;var buf):boolean;
label f1;
var i,o,p:longint;
begin;
rx:=true;
timer2start;
p:=CurrentTime;
f1:
relequish;
timer2start;
if (GetTimePast(p)>t) then exit;
pipeLineStats(pipe,i,o,i);
if (o<s) then goto f1;
i:=s;
if (pipeLineRecv(pipe,buf,i)<>0) then i:=0;
if (i<>s) then exit;
rx:=false;
end;

Begin;
siz:=-1;
xmod_recvPck:='';
if rx(1,1,hdr) then begin;
  xmod_recvPck:='header not received';
  exit;
  end;
case hdr.typ of
  controlCharacterSOH:s:=128;
  controlCharacterSTX:s:=1024;
  controlCharacterEOT:begin; seq:=0;siz:=0;exit; end;
  else begin;
    xmod_recvPck:='invalid header type';
    exit;
    end;
  end;
if rx(sizeof(hdr)-1,1,hdr.num) then begin;
  xmod_recvPck:='sequence not received';
  exit;
  end;
seq:=hdr.num;
if (seq<>hdr.mun xor $ff) then begin;
  xmod_recvPck:='sequence check error';
  exit;
  end;
if rx(s,3,buf) then begin;
  xmod_recvPck:='data not received';
  exit;
  end;
{$ifdef crc}
w:=0;
crc16update(w,buf,s);
o:=2;
{$else}
b:=0;
chksum8update(b,buf,s);
o:=1;
{$endif}
if rx(o,1,hdr) then begin;
  xmod_recvPck:='checksum not received';
  exit;
  end;
{$ifdef crc}
if (ReadWordMSB(hdr)<>w) then begin;
{$else}
if (hdr.typ<>b) then begin;
{$endif}
  xmod_recvPck:='checksum not match';
  exit;
  end;
siz:=s;
End;
