Var
  quoteMask:array[0..255] of boolean;
  output_bufDat:array[1..512] of byte;
  output_bufSiz:LongInt;
  input_bufDat:array[1..512] of byte;
  input_bufSiz:LongInt;
  input_bufPos:LongInt;
  canCount:LongInt;
  sawAbort:Boolean;
  hasCrc32:Boolean;
  hasFulDp:Boolean;
  hasOvrIo:Boolean;



Procedure maskOutChar(b:Byte);
Begin;
quoteMask[b and $7f]:=true;
quoteMask[b or $80]:=true;
End;

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
dec(canCount);
if (input_bufPos<0) then input_bufPos:=0;
End;

Function inbuf_getBin(tim:LongInt):LongInt;
Label f1;
Var i,o:LongInt;
Begin;
inbuf_getBin:=-1;
timer2start;
o:=CurrentTime;
f1:
if (input_bufPos<input_bufSiz) then begin;
  inc(input_bufPos);
  i:=input_bufDat[input_bufPos];
  if (i and $7f in [controlCharacterDC1,controlCharacterDC3]) then goto f1;
  inbuf_getBin:=i;
  if (i<>controlCharacterCAN) then begin;
    canCount:=0;
    exit;
    end;
  inc(canCount);
  if (canCount>=6) then sawAbort:=true;
  exit;
  end;
if (GetTimePast(o)>=tim) then exit;
relequish;
timer2start;
inbuf_reload;
goto f1;
End;

Function inbuf_getEsc(tim:LongInt):LongInt;
Label f1;
Var i,o:LongInt;
Begin;
inbuf_getEsc:=-1;
f1:
i:=inbuf_getBin(tim);
if (i<0) then exit;
if (i<>controlCharacterCAN) then begin;
  inbuf_getEsc:=i;
  exit;
  end;
i:=inbuf_getBin(tim);
if (i<0) then exit;
if (i and $60=$40) then begin;
  inbuf_getEsc:=i xor $40;
  exit;
  end;
if (i=$6c) then begin; inbuf_getEsc:=$7f;exit; end;
if (i=$6d) then begin; inbuf_getEsc:=$ff;exit; end;
inbuf_getEsc:=i or $100;
End;


Procedure inbuf_flush2;
Label f1;
Begin;
f1:
while (inbuf_getBin(0)>=0) do;
relequish;
if inbuf_check then goto f1;
End;






Procedure output_putBin(b:Byte);
Begin;
if (output_bufSiz>=sizeof(output_bufDat)) then outbuf_flush;
inc(output_bufSiz);
output_bufDat[output_bufSiz]:=b;
End;

Procedure output_putEsc(b:Byte);
Begin;
if (output_bufSiz>=sizeof(output_bufDat)-1) then outbuf_flush;
if quoteMask[b] then begin;
  inc(output_bufSiz);
  output_bufDat[output_bufSiz]:=controlCharacterCAN;
  if (b and $7f=$7f) then b:=(b shr 8)+$6c else b:=b xor $40;
  end;
inc(output_bufSiz);
output_bufDat[output_bufSiz]:=b;
End;





Function dumpOneHeader(var srcbuf):String;
Var
  buf:array[1..1] of byte absolute srcbuf;
  a:String;
Begin;
case buf[1] of
   0:a:='zrqinit';
   1:a:='zrinit';
   2:a:='zsinit';
   3:a:='zack';
   4:a:='zfile';
   5:a:='zskip';
   6:a:='znak';
   7:a:='zabort';
   8:a:='zfin';
   9:a:='zrpos';
  10:a:='zdata';
  11:a:='zeof';
  12:a:='zferr';
  13:a:='zcrc';
  14:a:='zchallenge';
  15:a:='zcompl';
  16:a:='zcan';
  17:a:='zfreecnt';
  18:a:='zcommand';
  19:a:='zstderr';
  else a:='unknown:'+BStr(buf[1]);
  end;
a:=a+' '+BStr(buf[2])+' '+BStr(buf[3])+' '+BStr(buf[4])+' '+BStr(buf[5]);
dumpOneHeader:=a;
End;





Procedure sendOneHeader(var srcbuf;c32,hex:Boolean);
Const hexTab:array[0..15] of byte=($30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$61,$62,$63,$64,$65,$66);
Var
  buf:array[1..32] of byte;
  siz:LongInt;
  i,o:LongInt;
  w:Word;
Begin;
if hex then c32:=false;
move(srcbuf,buf,5);
siz:=5;
if c32 then begin;
  i:=$ffffffff;
  crc32update(i,buf,5);
  WriteLongLSB(buf[6],not i);
  inc(siz,4);
  end else begin;
  w:=0;
  crc16update(w,buf,5);
  WriteWordMSB(buf[6],w);
  inc(siz,2);
  end;
if hex then output_putBin(42);
output_putBin(42);
output_putBin(controlCharacterCAN);
if hex then i:=1 else i:=0;
if c32 then i:=i or 2;
output_putBin(i+$41);
if hex then begin;
  for i:=1 to siz do begin;
    o:=buf[i];
    output_putBin(hexTab[o shr 4]);
    output_putBin(hexTab[o and 15]);
    end;
  output_putBin(13);
  output_putBin(10);
  output_putBin(controlCharacterDC1);
  end else begin;
  for i:=1 to siz do output_putEsc(buf[i]);
  end;
outbuf_flush;
End;






Procedure sendOneFrame(var srcbuf;siz:LongInt;hdr:Byte;c32:Boolean);
Var
  srcbufb:array[1..1] of byte absolute srcbuf;
  buf:array[1..32] of byte;
  i,o:LongInt;
  w:Word;
Begin;
for i:=1 to siz do output_putEsc(srcbufb[i]);
if c32 then begin;
  i:=$ffffffff;
  crc32update(i,srcbuf,siz);
  crc32update(i,hdr,1);
  WriteLongLSB(buf,not i);
  siz:=4;
  end else begin;
  w:=0;
  crc16update(w,srcbuf,siz);
  crc16update(w,hdr,1);
  WriteWordMSB(buf,w);
  siz:=2;
  end;
output_putBin(controlCharacterCAN);
output_putBin(hdr);
for i:=1 to siz do output_putEsc(buf[i]);
outbuf_flush;
End;






Function recvOneHeader(var srcbuf;var c32:Boolean;tim:LongInt):LongInt;
Label f1;
Const hexTab:array[0..255] of byte=(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,100,101,102,103,104,105,
  106,107,108,109,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,110,111,112,113,114,115,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
Var
  buf:array[1..32] of byte;
  hex:Boolean;
  ps,sz,tm:LongInt;
  i,o:LongInt;
  w:Word;
Begin;
recvOneHeader:=-1;
timer2start;
tm:=CurrentTime;
f1:
if sawAbort then exit;
if (GetTimePast(tm)>=tim) then exit;
i:=inbuf_getBin(tim);
if (i<0) then exit;
if (i<>controlCharacterCAN) then goto f1;
i:=inbuf_getBin(tim);
if (i<0) then exit;
if not (i in [$41..$44]) then goto f1;
i:=(i-1) and 3;
hex:=(i and 1<>0);
c32:=(i and 2<>0);
if hex then c32:=false;
sz:=7;
if c32 then inc(sz,2);
for ps:=1 to sz do begin;
  if (GetTimePast(tm)>=tim) then exit;
  if sawAbort then exit;
  if hex then begin;
    o:=inbuf_getBin(tim);
    if (o<0) then exit;
    o:=hexTab[o]-100;
    if (o<0) then goto f1;
    i:=inbuf_getBin(tim);
    if (i<0) then exit;
    i:=hexTab[i]-100;
    if (i<0) then goto f1;
    i:=(o shl 4) or i;
    end else begin;
    i:=inbuf_getEsc(tim);
    if (i<0) then exit;
    if (i and $ff00<>0) then goto f1;
    end;
  buf[ps]:=i;
  end;
if c32 then begin;
  i:=$ffffffff;
  crc32update(i,buf,5);
  if (ReadLongLSB(buf[6])<>not i) then goto f1;
  end else begin;
  w:=0;
  crc16update(w,buf,5);
  if (ReadWordMSB(buf[6])<>w) then goto f1;
  end;
move(buf,srcbuf,5);
recvOneHeader:=0;
if not hex then exit;
i:=inbuf_getBin(1);
if (i and $7f=13) then i:=inbuf_getBin(1);
if (i<0) then exit;
if (i and $7f<>10) then inbuf_ungetOne;
End;





Function recvOneFrame(var srcbuf;var siz:LongInt;tim:LongInt;c32:Boolean):LongInt;
Label f1;
Var
  buffer:array[1..1] of byte absolute srcbuf;
  buf:array[1..32] of byte;
  ps,tm:LongInt;
  i,o:LongInt;
  tp:Byte;
  w:Word;
Begin;
recvOneFrame:=-1;
timer2start;
tm:=CurrentTime;
ps:=0;
f1:
if sawAbort then exit;
if (GetTimePast(tm)>=tim) then exit;
i:=inbuf_getEsc(tim);
if (i<0) then exit;
if (i and $ff00=0) then begin;
  inc(ps);
  if (ps>siz) then exit;
  buffer[ps]:=i;
  goto f1;
  end;
if not (i in [$168..$16b]) then exit;
siz:=ps;
tp:=i;
if c32 then ps:=4 else ps:=2;
for o:=1 to ps do begin;
  i:=inbuf_getEsc(tim);
  if (i<0) then exit;
  if (i and $ff00<>0) then exit;
  buf[o]:=i;
  end;
if c32 then begin;
  i:=$ffffffff;
  crc32update(i,buffer,siz);
  crc32update(i,tp,1);
  if (ReadLongLSB(buf)<>not i) then exit;
  end else begin;
  w:=0;
  crc16update(w,buffer,siz);
  crc16update(w,tp,1);
  if (ReadWordMSB(buf)<>w) then exit;
  end;
recvOneFrame:=tp;
End;
