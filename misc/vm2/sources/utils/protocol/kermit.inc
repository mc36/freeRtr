Var
  output_bufDat:array[1..512] of byte;
  output_bufSiz:LongInt;
  input_bufDat:array[1..512] of byte;
  input_bufSiz:LongInt;
  input_bufPos:LongInt;
  currChck:LongInt;     {check type; 1=chk8, 2=chk16, 3=crc}
  currMark:LongInt;     {mark character}
  currPadC:LongInt;     {pad character}
  currPadN:LongInt;     {number of pads}
  currEoPc:LongInt;     {packet terminator}
  currCtPf:LongInt;     {control prefix}
  currBnPf:LongInt;     {binary prefix}
  currRpPf:LongInt;     {repeat prefix}
  currWind:LongInt;     {window size}
  currPack:LongInt;     {packet size}
  currShIn:LongInt;     {shift in prefix}
  currShOt:LongInt;     {shift out prefix}
  currShft:LongInt;     {current shift status}




Procedure outbuf_flush;
Begin;
if (output_bufSiz<1) then exit;
while (pipeLineSend(pipe,output_bufDat,output_bufSiz)<>0) do relequish;
output_bufSiz:=0;
End;

Procedure inbuf_reload;
Var i:LongInt;
Begin;
if (input_bufPos>=input_bufSiz) then begin;
  input_bufSiz:=0;
  input_bufPos:=0;
  end;
i:=sizeof(input_bufDat)-input_bufSiz;
if (i<1) then exit;
if (pipeLineRecv(pipe,input_bufDat[input_bufSiz+1],i)<>0) then i:=0;
inc(input_bufSiz,i);
End;

Procedure inbuf_flush;
Var i:LongInt;
Begin;
repeat
  input_bufSiz:=0;
  inbuf_reload;
  until (input_bufSiz=0);
End;

Function inbuf_check:Boolean;
Begin;
inbuf_reload;
inbuf_check:=(input_bufSiz>input_bufPos);
End;





Procedure inbuf_ungetOne;
Begin;
dec(input_bufPos);
if (input_bufPos<0) then input_bufPos:=0;
End;

Function inbuf_get(tim:LongInt):LongInt;
Label f1;
Var i,o:LongInt;
Begin;
inbuf_get:=-1;
timer2start;
o:=CurrentTime;
f1:
if (input_bufPos<input_bufSiz) then begin;
  inc(input_bufPos);
  inbuf_get:=input_bufDat[input_bufPos];
  exit;
  end;
if (GetTimePast(o)>=tim) then exit;
relequish;
timer2start;
inbuf_reload;
goto f1;
End;

Procedure inbuf_flush2;
Label f1;
Begin;
f1:
while (inbuf_get(0)>=0) do;
relequish;
if inbuf_check then goto f1;
End;

Procedure output_put(b:Byte);
Begin;
if (output_bufSiz>=sizeof(output_bufDat)) then outbuf_flush;
inc(output_bufSiz);
output_bufDat[output_bufSiz]:=b;
End;





Procedure putCheckType1(var buffer;i:LongInt);
Var b:byte absolute buffer;
Begin;
inc(i,i shr 6);
b:=(i and $3f)+32;
End;

Procedure putCheckType2(var buffer;i:LongInt);
Var buf:array[1..2] of byte absolute buffer;
Begin;
buf[1]:=((i shr 6) and $3f)+32;
buf[2]:=(i and $3f)+32;
End;

Procedure putCheckType3(var buffer;i:LongInt);
Var buf:array[1..3] of byte absolute buffer;
Begin;
buf[1]:=((i shr 12) and $3f)+32;
buf[2]:=((i shr 6) and $3f)+32;
buf[3]:=(i and $3f)+32;
End;





Procedure enQuoteOnePacket(var inBuf,outBuf;inSiz:LongInt;var outSiz:LongInt);
Var
  inB:array[1..1] of byte absolute inBuf;
  outB:array[1..1] of byte absolute outBuf;
  i,o,p:LongInt;
procedure add(i:LongInt);begin; inc(outSiz);outB[outSiz]:=i; end;
Begin;
outSiz:=0;
for p:=1 to inSiz do begin;
  i:=inB[p];
  if (currBnPf>=0) then if (i and $80<>0) then begin;
    add(currBnPf);
    i:=i and $7f;
    end;
  if (i and $60=0) or (i and $7f=$7f) then begin;
    add(currCtPf);
    add(i xor $40);
    continue;
    end;
  if (i=currCtPf) or (i=currBnPf) or (i=currRpPf) then add(currCtPf);
  add(i);
  end;
End;



Procedure deQuoteOnePacket(var inBuf,outBuf;inSiz:LongInt;var outSiz:LongInt);
Label f1;
Var
  inB:array[1..1] of byte absolute inBuf;
  outB:array[1..1] of byte absolute outBuf;
  i,o,p,x,t:LongInt;
procedure add(i:LongInt);begin; inc(outSiz);outB[outSiz]:=i; end;
function get:longint;begin; inc(p);if (p<=inSiz) then get:=inB[p] else get:=-123; end;
Begin;
outSiz:=0;
p:=0;
f1:
i:=get;
if (i=currShIn) then begin; currShft:=$00;goto f1; end;
if (i=currShOt) then begin; currShft:=$80;goto f1; end;
if (i=currRpPf) then begin; t:=get-32;i:=get; end else t:=1;
if (i=currBnPf) then begin; x:=currShft xor $80;i:=get; end else x:=currShft;
if (i=currCtPf) then begin;
  i:=get;
  if (i and $60=$40) or (i and $7f=$3f) then x:=x or $40;
  end;
if (i<0) then exit;
i:=i xor x;
for o:=1 to t do add(i);
goto f1;
End;





Procedure sendOnePacket(var buffer;size,seq,typ:LongInt);
Label xmit;
Var
  bufB:array[1..1] of byte absolute buffer;
  hdrB:array[1..32] of byte;
  chkB:array[1..16] of byte;
  hdrS:LongInt;
  i,o,p,q:LongInt;
  w:Word;
  b:Byte;
Begin;
seq:=seq and $3f;
i:=size+currChck;
if (i<95) then begin;
  hdrB[1]:=size+currChck+34;
  hdrB[2]:=seq+32;
  hdrB[3]:=typ;
  hdrS:=3;
  goto xmit;
  end;
hdrB[1]:=32;
hdrB[2]:=seq+32;
hdrB[3]:=typ;
hdrB[4]:=(i div 95)+32;
hdrB[5]:=(i mod 95)+32;
b:=0;
chksum8update(b,hdrB,5);
putCheckType1(hdrB[6],b);
hdrS:=6;
xmit:
case currChck of
  1:begin;
    b:=0;
    chksum8update(b,hdrB,hdrS);
    chksum8update(b,bufB,size);
    putCheckType1(chkB,b);
    end;
  2:begin;
    w:=0;
    chksum16update(w,hdrB,hdrS);
    chksum16update(w,bufB,size);
    putCheckType2(chkB,w);
    end;
  3:begin;
    w:=0;
    ccitt16update(w,hdrB,hdrS);
    ccitt16update(w,bufB,size);
    putCheckType3(chkB,w);
    end;
  else exit;
  end;
for i:=1 to currPadN do output_put(currPadC);
output_put(currMark);
for i:=1 to hdrS do output_put(hdrB[i]);
for i:=1 to size do output_put(bufB[i]);
for i:=1 to currChck do output_put(chkB[i]);
output_put(currEoPc);
outbuf_flush;
End;




Function recvOnePacket(tim:LongInt;var buffer;var size,seq,typ:LongInt):Boolean;
Label f1,f2;
Var
  bufB:array[1..1] of byte absolute buffer;
  hdrB:array[1..32] of byte;
  chkB:array[1..16] of byte;
  hdrS:LongInt;
  i,o,p,q:LongInt;
  tm:LongInt;
  w:Word;
  b:Byte;
Begin;
recvOnePacket:=True;
timer2start;
tm:=CurrentTime;
f1:
if (GetTimePast(tm)>=tim) then exit;
i:=inbuf_get(tim);
if (i<0) then exit;
if (i=currMark) then goto f2;
if (i<32) then goto f1;
inbuf_flush2;
goto f1;
f2:
o:=1;
hdrS:=3;
while (o<=hdrS) do begin;
  if (GetTimePast(tm)>=tim) then exit;
  i:=inbuf_get(tim);
  if (i<0) then exit;
  if (i<32) or (i>127) then goto f1;
  hdrB[o]:=i;
  inc(o);
  if (o>2) then continue;
  if (hdrB[1]=32) then hdrS:=6;
  end;
seq:=(hdrB[2]-32) and $3f;
typ:=hdrB[3];
if (hdrS<=3) then begin;
  size:=hdrB[1]-34;
  end else begin;
  b:=0;
  chksum8update(b,hdrB,5);
  putCheckType1(b,b);
  if (b<>hdrB[6]) then goto f1;
  size:=hdrB[4]*95+hdrB[5]-3072;
  end;
dec(size,currChck);
if (size<0) or (size>2048) then goto f1;
for o:=1 to size do begin;
  if (GetTimePast(tm)>=tim) then exit;
  i:=inbuf_get(tim);
  if (i<0) then exit;
  bufB[o]:=i;
  end;
p:=5;
for o:=1 to currChck do begin;
  if (GetTimePast(tm)>=tim) then exit;
  i:=inbuf_get(tim);
  if (i<0) then exit;
  if (i<32) or (i>127) then goto f1;
  chkB[p]:=i;
  inc(p);
  end;
case currChck of
  1:begin;
    b:=0;
    chksum8update(b,hdrB,hdrS);
    chksum8update(b,bufB,size);
    putCheckType1(chkB,b);
    end;
  2:begin;
    w:=0;
    chksum16update(w,hdrB,hdrS);
    chksum16update(w,bufB,size);
    putCheckType2(chkB,w);
    end;
  3:begin;
    w:=0;
    ccitt16update(w,hdrB,hdrS);
    ccitt16update(w,bufB,size);
    putCheckType3(chkB,w);
    end;
  else exit;
  end;
p:=5;
for o:=1 to currChck do begin;
  if (chkB[o]<>chkB[p]) then goto f1;
  inc(p);
  end;
recvOnePacket:=False;
i:=inbuf_get(1);
if (i<>currEoPc) then inbuf_ungetOne;
End;
