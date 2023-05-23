{ dual tone multi frequency             1209 1336 1477 1633
                                    697   1    2    3    A
                                    770   4    5    6    B
                                    852   7    8    9    C
                                    941   *    0    #    D     }
Const
  DTMFcharacters:String[31]='123a456b789c*0#d';
  DTMFcolFreqs:array[1..4] of LongInt=(1209,1336,1477,1633);
  DTMFrowFreqs:array[1..4] of LongInt=(697,770,852,941);
Var lastDTMFdetection:LongInt;



Procedure generateDTMFtone(a:String);
Var i,o,p,f1,f2:LongInt;
Begin;
i:=pos(a,DTMFcharacters)-1;
if (i<0) then begin;
  for i:=1 to sampleRate shr 2 do begin; inc(encodSiz);encodBuf[encodSiz]:=0; end;
  exit;
  end;
f1:=DTMFcolFreqs[(i and 3)+1];
f2:=DTMFrowFreqs[(i shr 2)+1];
for o:=1 to sampleRate div 6 do begin;
  i:=((o*f1) div (sampleRate div sinusTableSiz)) mod sinusTableSiz;
  p:=sinusTableDat[i];
  i:=((o*f2) div (sampleRate div sinusTableSiz)) mod sinusTableSiz;
  inc(p,sinusTableDat[i]);
  inc(encodSiz);
  encodBuf[encodSiz]:=p div 2;
  end;
for i:=1 to sampleRate div 10 do begin; inc(encodSiz);encodBuf[encodSiz]:=0; end;
End;



Procedure generateAnyTone(freq:LongInt);
Var i,o:LongInt;
Begin;
if (freq<10) or (freq>20000) then begin;
  for i:=1 to sampleRate shr 2 do begin; inc(encodSiz);encodBuf[encodSiz]:=0; end;
  exit;
  end;
for o:=1 to sampleRate div 6 do begin;
  i:=((o*freq) div (sampleRate div sinusTableSiz)) mod sinusTableSiz;
  inc(encodSiz);
  encodBuf[encodSiz]:=sinusTableDat[i];
  end;
for i:=1 to sampleRate div 10 do begin; inc(encodSiz);encodBuf[encodSiz]:=0; end;
End;



Function detectDTMFcodes:String;
Const
  myStep=256;
  mySize=myStep shl 3;
Var
  sinTab:array[1..sinusTableSiz] of LongInt;
  cosTab:array[1..sinusTableSiz] of LongInt;
  freqCol:array[1..4] of LongInt;
  freqRow:array[1..4] of LongInt;

Procedure doOneFreq(var val:LongInt;freq:LongInt);
Var i,p:LongInt;
Begin;
val:=0;
makeDFTtabs(sinTab,cosTab,freq,127);
p:=1;
while (p<mySize) do begin;
  i:=calcDFTvals(decodBuf[decodPos+p],sinTab,cosTab,myStep);
  inc(val,i shr 16);
  inc(p,myStep);
  end;
End;

Function findMax(var buffer):LongInt;
Var
  buf:array[1..1] of LongInt absolute buffer;
  i,o,p:LongInt;
Begin;
findMax:=-1;
o:=-99999;
p:=0;
for i:=1 to 4 do if (buf[i]>o) then begin;
  p:=i;
  o:=buf[i];
  end;
if (o<128) then exit;
findMax:=p;
End;

Var i,o,p:LongInt;
Begin;
detectDTMFcodes:='';
if (decodSiz-decodPos<mySize) then exit;
for i:=1 to 4 do begin;
  doOneFreq(freqCol[i],DTMFcolFreqs[i]);
  doOneFreq(freqRow[i],DTMFrowFreqs[i]);
  end;
inc(decodPos,mysize);
o:=findMax(freqCol);
p:=findMax(freqRow);
if (o<0) or (p<0) then p:=666;
p:=p*4+o-4;
if (p=lastDTMFdetection) then exit;
lastDTMFdetection:=p;
detectDTMFcodes:=copy(DTMFcharacters,p,1);
End;
