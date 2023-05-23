{goertzel's discrete fourier transform}

Procedure makeDFTtabs(var tabS,tabC;freq,diver:LongInt);
Var
  bufS:array[0..1] of LongInt absolute tabS;
  bufC:array[0..1] of LongInt absolute tabC;
  i,o:LongInt;
Begin;
for o:=0 to sinusTableSiz-1 do begin;
  i:=((o*freq) div (sampleRate div sinusTableSiz)) mod sinusTableSiz;
  bufS[o]:=sinusTableDat[i] div diver;
  i:=((sinusTableSiz shr 2)+i) mod sinusTableSiz;
  bufC[o]:=sinusTableDat[i] div diver;
  end;
End;


Function calcDFTvals(var input,tabS,tabC;siz:LongInt):LongInt;
Var
  buf:array[0..1] of LongInt absolute input;
  bufS:array[0..1] of LongInt absolute tabS;
  bufC:array[0..1] of LongInt absolute tabC;
  re,im:LongInt;
  i,o:LongInt;
Begin;
re:=0;
im:=0;
o:=0;
for o:=0 to siz-1 do begin;
  i:=buf[o];
  inc(im,bufS[o]*i);
  inc(re,bufC[o]*i);
  end;
re:=re div (siz shl 9);
im:=im div (siz shl 9);
calcDFTvals:=re*re+im*im;
End;
