{frequency shift keying;  v21=1080,1750;  bell=1170,2125}
Var
  dataBits:LongInt;
  parityBit:LongInt; {0=none, 1=odd, 2=even, 3=set0, 4=set1}
  samPerBit:LongInt;
  decodBuf:array[1..16*1024] of LongInt;
  encodBuf:array[1..16*1024] of LongInt;
  decodSiz:LongInt;
  decodPos:LongInt;
  encodSiz:LongInt;
  decodShi:array[1..sinusTableSiz] of LongInt;
  decodChi:array[1..sinusTableSiz] of LongInt;
  decodSlo:array[1..sinusTableSiz] of LongInt;
  decodClo:array[1..sinusTableSiz] of LongInt;
  encodShi:array[1..sinusTableSiz] of LongInt;
  encodSlo:array[1..sinusTableSiz] of LongInt;
  decodPrc:LongInt;
  decodStp:LongInt;
  decodRem:LongInt;
  decodPar:LongInt;
  decodFrm:LongInt;
  decodOvr:LongInt;
  decodCar:LongInt;
  encodPos:LongInt;
  encodVal:LongInt;
  encodPrv:LongInt;
  encodSqr:LongInt;


Procedure fskInitialize(bps,prec,rxF,txF:LongInt);
Begin;
samPerBit:=sampleRate div bps;
decodPrc:=(samPerBit*prec) div 100;
decodStp:=samPerBit div decodPrc;
decodRem:=samPerBit-(decodStp*decodPrc);
makeDFTtabs(encodShi,decodChi,txF+100,1);
makeDFTtabs(encodSlo,decodClo,txF-100,1);
makeDFTtabs(decodShi,decodChi,rxF+100,127);
makeDFTtabs(decodSlo,decodClo,rxF-100,127);
decodSiz:=samPerBit;
decodPos:=samPerBit;
fillchar(decodBuf,sizeof(decodBuf),0);
encodSiz:=0;
encodPos:=samPerBit+64;
encodVal:=0;
encodPrv:=-1000;
encodSqr:=0;
dataBits:=8;
parityBit:=0;
decodPar:=0;
decodFrm:=0;
decodOvr:=0;
decodCar:=0;
End;

Procedure purgeDecodeBuf;
Var i:LongInt;
Begin;
if (decodPos>decodSiz) then decodPos:=decodSiz;
i:=decodPos-samPerBit;
if (i<0) then exit;
dec(decodSiz,i);
dec(decodPos,i);
if (decodSiz<0) then decodSiz:=0;
move(decodBuf[i+1],decodBuf,decodSiz*4);
End;

Procedure putToDecodeBuf(var buffer;siz:LongInt);
Var
  buf:array[1..1] of Integer absolute buffer;
  i,o:LongInt;
Begin;
siz:=siz shr 2;
purgeDecodeBuf;
if (decodSiz+siz>sizeof(decodBuf) shr 2) then begin;
  inc(decodPos,siz);
  purgeDecodeBuf;
  inc(decodOvr);
  end;
o:=1;
for i:=1 to siz do begin;
  inc(decodSiz);
  decodBuf[decodSiz]:=(buf[o]+buf[o+1]) div 2;
  inc(o,2);
  end;
decodCar:=0;
End;

Function getFromEncodeBuf(var buffer;siz:LongInt):LongInt;
Var
  buf:array[1..1] of Integer absolute buffer;
  i,o,p:LongInt;
Begin;
siz:=siz shr 2;
if (siz>encodSiz) then siz:=encodSiz;
o:=1;
for i:=1 to siz do begin;
  p:=encodBuf[i];
  buf[o]:=p;
  buf[o+1]:=p;
  inc(o,2);
  end;
dec(encodSiz,siz);
move(encodBuf[siz+1],encodBuf,encodSiz*4);
getFromEncodeBuf:=siz shl 2;
End;




Function calcCurrDFT:LongInt;
Var f1,f2:LongInt;
Begin;
f1:=calcDFTvals(decodBuf[decodPos+1],decodSlo,decodClo,samPerBit);
f2:=calcDFTvals(decodBuf[decodPos+1],decodShi,decodChi,samPerBit);
if (f1+f2<3000000) then begin; dec(decodCar);calcCurrDFT:=0;exit; end;
inc(decodCar);
calcCurrDFT:=f1-f2;
End;

Function getOneBit:LongInt;
Var i,o,p:LongInt;
Begin;
p:=0;
for o:=1 to decodPrc do begin;
  i:=calcCurrDFT;
  inc(decodPos,decodStp);
  if (i>0) then inc(p);
  end;
inc(decodPos,decodRem);
if (p>decodPrc shr 1) then getOneBit:=1 else getOneBit:=0;
End;

Function getOneByte:LongInt;
Label f1,f2;
Var i,o,p,q:LongInt;
Begin;
getOneByte:=-1;
f1:
if (decodSiz-decodPos<12*samPerBit) then exit;
if (calcCurrDFT>0) then begin; inc(decodPos,decodStp);goto f1; end;
if (getOneBit>0) then begin;
  dec(decodPos,samPerBit shr 1);
  goto f1;
  end;
inc(decodPos,samPerBit shr 2);
q:=0;p:=0;
for o:=0 to dataBits-1 do begin;
  i:=getOneBit;
  inc(q,i shl o);
  inc(p,i);
  end;
case parityBit of
  0:;
  1:if (p and 1=getOneBit) then begin; inc(decodPar);goto f1; end;
  2:if (p and 1<>getOneBit) then begin; inc(decodPar);goto f1; end;
  3:if (getOneBit<>0) then begin; inc(decodPar);goto f1; end;
  4:if (getOneBit<>1) then begin; inc(decodPar);goto f1; end;
  else begin; inc(decodPar);goto f1; end;
  end;
if (getOneBit<>1) then begin; inc(decodFrm);goto f1; end;
dec(decodPos,samPerBit shr 2);
getOneByte:=q;
End;




Procedure addOneSinWave(var sinTable);
Var
  tab:array[1..1] of LongInt absolute sinTable;
  i,o,p,q,r:LongInt;
Begin;
if (encodPos>sinusTableSiz-samPerBit-4) then encodPos:=samPerBit+32;
p:=encodPos;o:=9999999;
for i:=1 to samPerBit+1 do begin;
  q:=tab[p]-encodVal;
  r:=tab[p-1]-encodPrv;
  if (q<0) then q:=-q;
  if (r<0) then r:=-r;
  inc(r,q);
  if (r<o) then begin; encodPos:=p;o:=r; end;
  if (o=0) then break;
  dec(p);
  end;
move(tab[encodPos+1],encodBuf[encodSiz+1],samPerBit*4);
if (encodSqr<>0) then begin;
  for i:=encodPos+1 to encodPos+samPerBit do begin;
    if (encodBuf[i]<0) then encodBuf[i]:=-32000 else encodBuf[i]:=32000;
    end;
  end;
inc(encodSiz,samPerBit);
encodVal:=encodBuf[encodSiz];
encodPrv:=encodBuf[encodSiz-1];
inc(encodPos,samPerBit);
End;

Procedure addOneBit(p:LongInt);
Begin;
if (p=0) then addOneSinWave(encodSlo) else addOneSinWave(encodShi)
End;

Procedure addOneByte(p:LongInt);
Var i,o,q:LongInt;
Begin;
q:=0;
addOneBit(1);
for o:=0 to dataBits-1 do begin;
  i:=p and 1;
  p:=p shr 1;
  addOneBit(i xor 1);
  inc(q,i);
  end;
q:=q and 1;
case parityBit of
  1:addOneBit(q);
  2:addOneBit(q xor 1);
  3:addOneBit(1);
  4:addOneBit(0);
  end;
addOneBit(0);
addOneBit(0);
End;
