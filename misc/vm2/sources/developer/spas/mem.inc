Const
  MemType_label=1;
  MemType_cnst=2;
  MemType_type=3;
  MemType_var=4;
  MemType_prcB=5;
  MemType_prcE=6;
  MemType_prcC=7;
  MemType_sign=120;

  DatType_shrt=1;
  DatType_byte=2;
  DatType_char=3;
  DatType_bool=4;
  DatType_intg=5;
  DatType_word=6;
  DatType_dint=7;
  DatType_dwrd=8;
  DatType_qint=9;
  DatType_qwrd=10;
  DatType_strg=11;
  DatType_pntr=12;
  DatType_untp=13;

  TypData_name=1;
  TypData_data=2;
  TypData_beg=3;
  TypData_end=4;
  TypData_arr=5;

  VarType_cnst=1;  {const variable}
  VarType_norm=2;  {normal variable}
  VarType_tptr=3;  {typed pointer}
  VarType_absl=4;  {variable on other var}
  VarType_pnrm=5;  {normal parameter}
  VarType_pflt=6;  {var parameter}
  VarType_punt=7;  {typeless parameter}



Const
  MemoryMaxRecord=63*1024;
Type
  MemoryOneRecord=record
    s:Word;
    d:array[1..MemoryMaxRecord+2] of char;
    end;
Var
  MemoryCurSize:LongInt;
  MemoryCurPos:LongInt;
  MemData1:^MemoryOneRecord;
  MemData2:^MemoryOneRecord;
  MemData3:^MemoryOneRecord;
  CurrProcPos:LongInt;
  CurrProcNum:LongInt;
  CurrProcRet:Byte;
  TempStrVarMax:LongInt;
  TempStrVarCur:LongInt;
  TempNumVarMax:LongInt;
  TempNumVarCur:LongInt;
  TempLabelNum:LongInt;
  DefinitionsList:String;

Type
  TheExtendedMemoryBufferType=array[0..15] of byte;
Var
  TheExtendedMemoryBufferData:^TheExtendedMemoryBufferType;



Function GetTempStrVarNum:LongInt;
Begin;
inc(TempStrVarCur);
if (TempStrVarCur>TempStrVarMax) then TempStrVarMax:=TempStrVarCur;
GetTempStrVarNum:=TempStrVarCur;
End;

Function GetTempNumVarNum:LongInt;
Begin;
inc(TempNumVarCur);
if (TempNumVarCur>TempNumVarMax) then TempNumVarMax:=TempNumVarCur;
GetTempNumVarNum:=TempNumVarCur;
End;

Function MemoryRead(var d:MemoryOneRecord):Boolean;
Var w:Word;
Begin;
MemoryRead:=True;
fillchar(d,sizeof(d),0);
if (MemoryCurPos+2>MemoryCurSize) then Exit;
move(TheExtendedMemoryBufferData^[MemoryCurPos],w,sizeof(w));
if (MemoryCurPos+2+longint(w)>MemoryCurSize) then Exit;
move(TheExtendedMemoryBufferData^[MemoryCurPos],d,w+sizeof(w));
MemoryRead:=False;
End;

Procedure MemoryAppend(var d:MemoryOneRecord);
Var
  p:pointer;
  i,o:LongInt;
Begin;
i:=d.s+4;
move(d.s,d.d[d.s+1],sizeof(d.s));
o:=MemoryCurSize+i+1024;
if (ExtendedMemoryResize(p,o)<o) then SignError('error allocating memory');
TheExtendedMemoryBufferData:=p^;
move(d,TheExtendedMemoryBufferData^[MemoryCurSize],i);
inc(MemoryCurSize,i);
End;

Function MemoryNext:Boolean;
Var w:Word;
Begin;
MemoryNext:=True;
if (MemoryCurPos+2>MemoryCurSize) then Exit;
move(TheExtendedMemoryBufferData^[MemoryCurPos],w,sizeof(w));
if (MemoryCurPos+4+longint(w)>MemoryCurSize) then Exit;
inc(MemoryCurPos,4+longint(w));
MemoryNext:=False;
End;

Function MemoryBack:Boolean;
Var w:Word;
Begin;
MemoryBack:=True;
if (MemoryCurPos-2<0) then Exit;
move(TheExtendedMemoryBufferData^[MemoryCurPos-2],w,sizeof(w));
if (MemoryCurPos-4-longint(w)<0) then Exit;
dec(MemoryCurPos,4+longint(w));
MemoryBack:=False;
End;

Procedure MemoryPut2end(nam:String;typ:Byte;var d:MemoryOneRecord);
Begin;
nam:=chr(typ)+nam[0]+nam;
if (length(nam)>250) then SignError('out of memory');
if (d.s+length(nam)>MemoryMaxRecord) then SignError('out of memory');
move(d.d[1],d.d[1+length(nam)],d.s);
move(nam[1],d.d[1],length(nam));
inc(d.s,length(nam));
MemoryAppend(d);
End;

Procedure MemoryDelLast(var d:MemoryOneRecord);
Var i:LongInt;
Begin;
MemoryCurPos:=MemoryCurSize;
if MemoryBack then SignError('out of memory');
i:=MemoryCurPos;
if MemoryRead(d) then SignError('out of memory');
MemoryCurSize:=i;
MemoryCurPos:=i;
End;

Function MemoryGetName(var d:MemoryOneRecord):String;
Var a:String;
Begin;
move(d.d[2],a,sizeof(a));
a:=d.d[1]+a;
move(d.d[length(a)+2],d.d[1],d.s);
dec(d.s,length(a)+1);
MemoryGetName:=a;
End;

Procedure SkipProcBeginning(var tmp:MemoryOneRecord);
Label f1;
Begin;
repeat
  if MemoryBack then Exit;
  MemoryRead(tmp);
  until (tmp.d[1]=chr(MemType_prcB));
if MemoryNext then Exit;
End;

Function MemoryFind(nam:String;typ:Byte;global:Boolean;var d:MemoryOneRecord):LongInt;
Label f1,f2;
Var
  num,mask:Byte;
  a:String;
Begin;
MemoryFind:=-1;
if (nam='') then Exit;
mask:=$ff;
if (typ=0) then begin; mask:=0;typ:=0; end;
nam:=kicsi(nam);
MemoryCurPos:=MemoryCurSize;
while not MemoryBack do begin;
  MemoryRead(d);
  num:=ord(d.d[1]);
  move(d.d[2],a,sizeof(a));
  a:=kicsi(a);
  if (a<>nam) then goto f1;
  if (num and mask<>typ) then goto f1;
  MemoryFind:=MemoryCurPos;
  Exit;
  f1:
  if not global then if (MemoryCurPos<CurrProcPos) then Exit;
  if (num=MemType_prcC) then SkipProcBeginning(d);
  end;
End;

Function FindOwnerProc(orig:LongInt;NumberNeeded:Boolean;var d:MemoryOneRecord):LongInt;
Label f1,f2;
Var
  oldps:LongInt;
  nm,ps:LongInt;
Begin;
FindOwnerProc:=-1;
oldps:=MemoryCurPos;
MemoryCurPos:=orig;
if MemoryRead(d) then SignError('out of memory');
f1:
ps:=MemoryCurPos;
if MemoryBack then begin;
  nm:=0;
  ps:=0;
  goto f2;
  end;
MemoryRead(d);
if (d.d[1]=chr(MemType_prcC)) then begin;
  SkipProcBeginning(d);
  MemoryBack;
  goto f1;
  end;
if (d.d[1]<>chr(MemType_prcB)) then goto f1;
MemoryGetName(d);
move(d.d[2],nm,sizeof(nm));
f2:
if NumberNeeded then FindOwnerProc:=nm else FindOwnerProc:=ps;
MemoryCurPos:=oldps;
End;

Function GetStandardLenType(len:word;sig:boolean):LongInt;
Var i:LongInt;
Begin;
case len of
  1:if sig then i:=DatType_shrt else i:=DatType_byte;
  2:if sig then i:=DatType_intg else i:=DatType_word;
  4:if sig then i:=DatType_dint else i:=DatType_dwrd;
  8:if sig then i:=DatType_qint else i:=DatType_qwrd;
  else i:=-1;
  end;
GetStandardLenType:=i;
End;

Function GetStandardTypes(s:String):Byte;
Var i:Byte;
Begin;
s:=kicsi(s);
i:=0;
if (s='shortint') then i:=DatType_shrt;
if (s='byte') then i:=DatType_byte;
if (s='char') then i:=DatType_char;
if (s='boolean') then i:=DatType_bool;
if (defMxItSize>=2) then begin;
  if (s='integer') then i:=DatType_intg;
  if (s='word') then i:=DatType_word;
  end;
if (defMxItSize>=4) then begin;
  if (s='longint') then i:=DatType_dint;
  if (s='longword') then i:=DatType_dwrd;
  end;
if (defMxItSize>=8) then begin;
  if (s='quadint') then i:=DatType_qint;
  if (s='quadword') then i:=DatType_qwrd;
  end;
if (s='pointer') then i:=DatType_pntr;
if (s='string') then i:=DatType_strg;
if (s='defint') then i:=GetStandardLenType(defIntgSize,true);
if (s='defword') then i:=GetStandardLenType(defIntgSize,false);
GetStandardTypes:=i;
End;

Function GetStandardTypeLen(s:String):LongInt;
Var
  i,o:LongInt;
Begin;
o:=-1;
str2bin(s,i,sizeof(i));
delete(s,1,sizeof(i));
case i of
  DatType_shrt,DatType_byte:o:=1;
  DatType_char,DatType_bool:o:=1;
  DatType_intg,DatType_word:o:=2;
  DatType_dint,DatType_dwrd:o:=4;
  DatType_qint,DatType_qwrd:o:=8;
  DatType_pntr:o:=defAddrSize;
  DatType_strg:begin; str2bin(s+#255#0#0#0,o,sizeof(o));inc(o); end;
  end;
GetStandardTypeLen:=o;
End;

Function GetStandardTypeSig(s:String):Boolean;
Var
  i:longInt;
  b:Boolean;
Begin;
str2bin(s,i,sizeof(i));
b:=(i in [DatType_shrt,DatType_intg,DatType_dint,DatType_qint]);
GetStandardTypeSig:=b;
End;

Function GetStandardTypeCast(s:String):LongInt;
Var
  i,o:LongInt;
Begin;
o:=-1;
str2bin(s,i,sizeof(i));
case i of
  DatType_shrt:o:=1;
  DatType_byte:o:=1;
  DatType_char:o:=2;
  DatType_bool:o:=1;
  DatType_intg:o:=1;
  DatType_word:o:=1;
  DatType_dint:o:=1;
  DatType_dwrd:o:=1;
  DatType_qint:o:=1;
  DatType_qwrd:o:=1;
  DatType_pntr:o:=1;
  DatType_strg:o:=2;
  end;
GetStandardTypeCast:=o;
End;

Function GetStandardIntPrefix(s:String):String;
Var b:String;
Begin;
if (GetStandardTypeCast(s)<>1) then SignError('integer variable expected');
if GetStandardTypeSig(s) then b:='s' else b:='u';
b:=b+DataSizesList[GetStandardTypeLen(s)];
GetStandardIntPrefix:=b;
End;

Procedure MemoryStr2rec(var d:MemoryOneRecord;a:String);
Begin;
move(a[1],d.d,sizeof(a));
d.s:=length(a);
End;

Function MemoryRec2str(var d:MemoryOneRecord):String;
Var
  a:String;
  i:Word;
Begin;
i:=d.s;
if (i>$ff) then i:=$ff;
move(d.d,a[1],i);
a[0]:=chr(i);
MemoryRec2str:=a;
End;

Procedure MemoryRecAppend(var tar,src:MemoryOneRecord);
Begin;
if (tar.s+src.s>MemoryMaxRecord) then SignError('out of memory');
move(src.d,tar.d[tar.s+1],src.s);
inc(tar.s,src.s);
End;

Procedure MemoryRecAppendStr(var d:MemoryOneRecord;s:string);
Begin;
if (length(s)>250) then SignError('out of memory');
if (d.s+length(s)>MemoryMaxRecord) then SignError('out of memory');
move(s[1],d.d[d.s+1],length(s));
inc(d.s,length(s));
End;


Procedure MemoryRecReplace(var d:MemoryOneRecord;sb,se:word;b:string);
Begin;
if (length(b)>250) then SignError('out of memory');
if (d.s+length(b)>MemoryMaxRecord) then SignError('out of memory');
move(d.d[se+1],d.d[sb+1],d.s-se);
dec(d.s,se-sb);
move(d.d[sb+1],d.d[sb+length(b)+1],d.s-sb);
move(b[1],d.d[sb+1],length(b));
inc(d.s,length(b));
End;

Procedure MemoryRecGetPart(var tar,src:MemoryOneRecord;pb,pe:word);
Begin;
if (pb>pe) then SignError('out of memory');
pe:=pe-pb;
move(src.d[pb+1],tar.d,pe);
tar.s:=pe;
End;

Function MemoryRecCompare(var d1,d2:MemoryOneRecord):Boolean;
Var i:Word;
Begin;
MemoryRecCompare:=True;
if (d1.s<>d2.s) then Exit;
for i:=1 to d1.s do if (d1.d[i]<>d2.d[i]) then Exit;
MemoryRecCompare:=False;
End;

Procedure MemoryRecExchangeTwo(var d1,d2:MemoryOneRecord);
Begin;
MemoryAppend(d1);
MemoryAppend(d2);
MemoryDelLast(d1);
MemoryDelLast(d2);
End;











Function ReadUpExpression(ending:String;var d,tmp:MemoryOneRecord):String;
Label f1,f2;
Var
  a,b:String;
  i:LongInt;
  zaro1:LongInt;
  SkipResolveN,SkipResolveO:Boolean;

Procedure AddOne(tp:Byte;s:String);
Begin;
s:=chr(tp)+s[0]+s;
MemoryRecAppendStr(d,s);
End;

Begin;
ReadUpExpression:='';
ending:=kicsi(ending);
zaro1:=0;
fillchar(d,sizeof(d),0);
SkipResolveN:=False;
f1:
SkipResolveO:=SkipResolveN;SkipResolveN:=False;
b:=GetNextWord;a:=kicsi(b);
if (b='') then SignError('unexpected end of file');
if (pos(#13+a+#13,ending)<>0) and (zaro1=0) then begin;
  ReadUpExpression:=a;
  Exit;
  end;
if (a='(') then inc(zaro1);
if (a=')') then begin;
  dec(zaro1);
  if (zaro1<0) then begin;
    a:=copy(ending,2,length(ending)-2);
    kicserel(#13,'" or "',a);
    SignError('"'+a+'" expected');
    end;
  end;
if (a[1]='{') then goto f1;
if (a[1]='#') then begin;
  AddOne(MemType_cnst,#2+copy(b,2,255));
  goto f1;
  end;
if (a[1]='$') then begin;
  delete(a,1,1);
  i:=BVal(a);
  AddOne(MemType_cnst,#1+bin2str(i,sizeof(i)));
  goto f1;
  end;
if not (a[1] in ['a'..'z','A'..'Z']) then begin;
  if (a='.') then SkipResolveN:=True;
  if (a='.') and SkipResolveO then SkipResolveN:=False;
  if (length(a)<>1) then SignError('out of memory');
  AddOne(MemType_sign,a);
  goto f1;
  end;
if SkipResolveO then goto f2;
i:=GetStandardTypes(a);
if (i>0) then begin;
  AddOne(MemType_type,#1+bin2str(i,sizeof(i)));
  goto f1;
  end;
i:=MemoryFind(b,0,true,tmp);
if (i<0) then begin;
  f2:
  AddOne(MemType_sign,a);
  goto f1;
  end;
a:=MemoryGetName(tmp);
case ord(a[1]) of
  MemType_cnst:begin;
    a:=bin2str(tmp.d,tmp.s);
    AddOne(MemType_cnst,a);
    goto f1;
    end;
  MemType_type:begin;
    a:=#2+bin2str(i,sizeof(i));
    AddOne(MemType_type,a);
    goto f1;
    end;
  MemType_var:begin;
    a:=bin2str(i,sizeof(i));
    AddOne(MemType_var,a);
    goto f1;
    end;
  MemType_prcB:begin;
    a:=bin2str(i,sizeof(i));
    AddOne(MemType_prcB,a);
    goto f1;
    end;
  else SignError('error in expression');
  end;
goto f1;
End;








Function EvaluateConstant(var d:MemoryOneRecord):String;
Var
  typ:Byte;
  ps:Word;

Function GetNext:String;
Var
  t:char;
  a:String;
Begin;
GetNext:='';
t:=d.d[ps+1];
move(d.d[ps+2],a,sizeof(a));
a:=t+a;
if (ps+length(a)+1>d.s) then Exit;
inc(ps,length(a)+1);
GetNext:=a;
End;


Function gnw(var s:String):String;
Var i:Word;
Begin;
i:=pos(' ',s);
if (i=0) then i:=$100;
gnw:=copy(s,1,i-1);
delete(s,1,i);
End;

Function glw(var s:String):String;
Var i:Word;
Begin;
s:=xLevesz(s);
i:=posL(' ',s);
glw:=copy(s,i+1,255);
s:=copy(s,1,i);
End;

Function gnn(var s:String;var o:LongInt):Boolean;
Var
  bol:Boolean;
  a:String;
Begin;
gnn:=true;
bol:=false;
repeat
  a:=gnw(s);
  if (a='-') then bol:=not bol;
  until (a<>'+') and (a<>'-');
o:=bval(a);
if (bstr(o)<>a) then Exit;
if bol then o:=-o;
gnn:=false;
End;

Function CalcOneString(c:String):String;
Label f1;
Var
  a,b:String;
  i,o:LongInt;
Begin;
CalcOneString:='';
b:=xLevesz(UnPlugString(c));
c:='';
while (b<>'') do begin;
  a:=gnw(b);
  if (a='not') then begin;
    if gnn(b,i) then Exit;
    a:=bstr(not i);
    end;
  c:=c+a+' ';
  end;

b:=c;
c:='';
while (b<>'') do begin;
  a:=gnw(b);
  if (a='*') then begin;
    a:=glw(c);if gnn(a,i) then Exit;
    if gnn(b,o) then Exit;
    a:=bstr(i*o);
    end;
  if (a='div') then begin;
    a:=glw(c);if gnn(a,i) then Exit;
    if gnn(b,o) then Exit;
    a:=bstr(i div o);
    end;
  if (a='mod') then begin;
    a:=glw(c);if gnn(a,i) then Exit;
    if gnn(b,o) then Exit;
    a:=bstr(i mod o);
    end;
  if (a='shl') then begin;
    a:=glw(c);if gnn(a,i) then Exit;
    if gnn(b,o) then Exit;
    a:=bstr(i shl o);
    end;
  if (a='shr') then begin;
    a:=glw(c);if gnn(a,i) then Exit;
    if gnn(b,o) then Exit;
    a:=bstr(i shr o);
    end;
  c:=c+a+' ';
  end;

b:=c;
if gnn(b,o) then Exit;
while (b<>'') do begin;
  a:=gnw(b);
  if (a='and') then begin;
    if gnn(b,i) then Exit;
    o:=o and i;
    goto f1;
    end;
  if (a='or') then begin;
    if gnn(b,i) then Exit;
    o:=o or i;
    goto f1;
    end;
  if (a='xor') then begin;
    if gnn(b,i) then Exit;
    o:=o xor i;
    goto f1;
    end;
  b:=a+' '+b;
  if gnn(b,i) then Exit;
  inc(o,i);
  f1:
  end;
CalcOneString:=bin2str(o,sizeof(o));
End;

Function CalcOnePart(sb,se:Word):String;
Label f1,f2,f3,vege;
Var
  a,b,c:String;
  op:Word;
  i,o:LongInt;
Begin;
CalcOnePart:='';
b:='';
if (typ=2) then goto f2;
ps:=sb;
repeat
  op:=ps;
  a:=GetNext;
  if (a='') then Exit;
  if (a[1]=chr(MemType_sign)) then begin;
    delete(a,1,1);
    if (a='(') then goto f1;
    if (a=')') then goto f1;
    if (a<>'+') then begin;
      b:=b+' '+a;
      goto f1;
      end;
    if (length(b)>250) then Exit;
    a:=CalcOneString(b);
    if (a='') then begin;
      b:=b+' +';
      goto f1;
      end;
    str2bin(a,i,sizeof(i));
    b:=BStr(i)+' +';
    goto f1;
    end;
  if (a[1]=chr(MemType_cnst)) then begin;
    if (a[2]<>chr(typ)) then Exit;
    delete(a,1,2);
    str2bin(a,i,sizeof(i));
    b:=b+' '+bstr(i);
    goto f1;
    end;
  if (a[1]=chr(MemType_type)) then begin;
    if (a[2]<>#1) then Exit;
    delete(a,1,2);
    str2bin(a,i,sizeof(i));
    a:=GetNext;
    if (a[1]<>chr(MemType_cnst)) then Exit;
    if (a[2]<>chr(typ)) then Exit;
    delete(a,1,2);
    str2bin(a,o,sizeof(o));
    case i of
      DatType_shrt:o:=shortint(o);
      DatType_byte:o:=byte(o);
      DatType_intg:o:=integer(o);
      DatType_word:o:=word(o);
      DatType_dint:o:=longint(o);
      DatType_dwrd:o:=longword(o);
      DatType_qint:o:=quadint(o);
      DatType_qwrd:o:=quadword(o);
      else Exit;
      end;
    b:=b+' '+bstr(o);
    goto f1;
    end;
  Exit;
  f1:
  until (ps>=se);
if (length(b)>250) then Exit;
b:=CalcOneString(b);
if (b='') then exit;
goto vege;

f2:
ps:=sb;
repeat
  a:=GetNext;
  if (a='') then Exit;
  if (a[1]=chr(MemType_sign)) then begin;
    delete(a,1,1);
    if (a<>'+') then Exit;
    goto f3;
    end;
  if (a[1]<>chr(MemType_cnst)) then Exit;
  if (a[2]<>chr(typ)) then Exit;
  delete(a,1,2);
  b:=b+a;
  f3:
  until (ps>=se);
goto vege;

vege:
b:=chr(typ)+b;
b:=chr(MemType_cnst)+b[0]+b;
CalcOnePart:=b;
End;

Label f1;
Var
  a:String;
  i,o:Word;
Begin;
EvaluateConstant:='';
ps:=0;
repeat
  a:=GetNext;
  if (a='') then Exit;
  until (a[1]=chr(MemType_cnst));
typ:=ord(a[2]);
f1:
ps:=0;o:=0;
repeat
  i:=ps;
  a:=GetNext;
  if (a=chr(MemType_sign)+'(') then o:=i;
  if (a=chr(MemType_sign)+')') then begin;
    a:=CalcOnePart(o,ps);
    if (a='') then Exit;
    MemoryRecReplace(d,o,ps,a);
    goto f1;
    end;
  until (a='');
a:=CalcOnePart(0,d.s);
if (a='') then Exit;
MemoryRecReplace(d,0,d.s,a);
ps:=0;
a:=GetNext;
if (GetNext<>'') then Exit;
if (copy(a,1,1)<>chr(MemType_cnst)) then Exit;
if (copy(a,2,1)<>chr(typ)) then Exit;
delete(a,1,1);
EvaluateConstant:=a;
End;











Function GetOffsetInType(var typ,out,inp:MemoryOneRecord;var RecSize:LongInt):LongInt;
Var
  inptmp:string;
  ps,ops,ips:Word;

Procedure AddOne(tp:Byte;s:String);
Begin;
s:=chr(tp)+s[0]+s;
MemoryRecAppendStr(out,s);
End;

Function GetNext:String;
Var
  t:char;
  a:String;
Begin;
GetNext:='';
t:=typ.d[ps+1];
move(typ.d[ps+2],a,sizeof(a));
a:=t+a;
if (ps+length(a)+1>typ.s) then Exit;
inc(ps,length(a)+1);
GetNext:=a;
End;

Function GetNextWord:String;
Var
  t:char;
  a:String;
Begin;
GetNextWord:='';
if (inptmp<>'') then begin;
  move(inptmp[1],a,sizeof(a));
  delete(inptmp,1,length(a)+1);
  GetNextWord:=Chr(MemType_sign)+a;
  Exit;
  end;
t:=inp.d[ips+1];
move(inp.d[ips+2],a,sizeof(a));
a:=t+a;
if (ips+length(a)+1>inp.s) then Exit;
inc(ips,length(a)+1);
GetNextWord:=a;
End;

Procedure PutBackWord(s:String);
Begin;
if (s='') then Exit;
s:=s[0]+s;
if (length(s)>250) then SignError('out of memory');
inptmp:=inptmp+s;
End;

Procedure CopyOneBracket;
Label f1,f2;
Var
  a:String;
  o:LongInt;
Begin;
o:=0;
f1:
a:=GetNextWord;
if (a='') then SignError('error in expression');
if (a=Chr(MemType_sign)+'[') then inc(o);
if (a=Chr(MemType_sign)+']') then dec(o);
if (o<0) then goto f2;
AddOne(ord(a[1]),copy(a,2,255));
goto f1;
f2:
End;

Procedure SkipThisEntry;
Label f1,f2;
Var
  a:String;
  level:LongInt;
  i:LongInt;
Begin;
level:=0;
f1:
a:=GetNext;
if (a='') then SignError('error in expression');
if (a[1]=chr(TypData_beg)) then begin;
  AddOne(MemType_sign,'+');
  AddOne(MemType_sign,'(');
  inc(level);
  goto f2;
  end;
if (a[1]=chr(TypData_end)) then begin;
  AddOne(MemType_sign,')');
  dec(level);
  goto f2;
  end;
if (a[1]=chr(TypData_name)) then goto f1;
if (a[1]=chr(TypData_data)) then begin;
  delete(a,1,1);
  i:=GetStandardTypeLen(a);
  if (i<0) then SignError('integer expression expected');
  AddOne(MemType_sign,'+');
  AddOne(MemType_cnst,#1+bin2str(i,sizeof(i)));
  goto f2;
  end;
if (a[1]=chr(TypData_arr)) then begin;
  delete(a,1,5);
  AddOne(MemType_sign,'+');
  AddOne(MemType_cnst,#1+a);
  AddOne(MemType_sign,'*');
  goto f1;
  end;

SignError('error in expression');
f2:
if (level>0) then goto f1;
End;


Label f1,f2,f3,f4;
Var
  a,b:String;
  i,o:LongInt;
Begin;
ps:=0;
ips:=0;
inptmp:='';
i:=-1;
fillchar(out,sizeof(out),0);
if (inp.s=0) then begin;
  o:=ps;
  a:=GetNext;
  if (copy(a,1,1)<>chr(TypData_data)) then begin;
    ps:=o;
    goto f4;
    end;
  str2bin(copy(a,2,255),i,sizeof(i));
  goto f4;
  end;


f1:
ops:=ips;
b:=GetNextWord;
if (b='') then goto f3;
if (copy(b,1,1)<>Chr(MemType_sign)) then SignError('unexpected end of file');
delete(b,1,1);
f3:
o:=ps;
a:=GetNext;
if (a='') then SignError('unexpected end of file');
if (b='') then goto f2;
if (a[1]=chr(TypData_beg)) then begin;
  if (b<>'.') then goto f2;
  goto f1;
  end;
if (a[1]=chr(TypData_end)) then SignError('invalid qualifier: '+b);
if (a[1]=chr(TypData_name)) then begin;
  a:=kicsi(copy(a,2,255));
  if (kicsi(b)=a) then goto f1;
  SkipThisEntry;
  goto f3;
  end;
if (a[1]=chr(TypData_data)) then goto f2;
if (a[1]=chr(TypData_arr)) then begin;
  if (b<>'[') then goto f2;
  AddOne(MemType_sign,'+');
  MemoryAppend(typ);
  AddOne(MemType_sign,'(');
  CopyOneBracket;
  AddOne(MemType_sign,'-');
  AddOne(MemType_cnst,#1+copy(a,2,4));
  AddOne(MemType_sign,')');
  MemoryDelLast(typ);
  AddOne(MemType_sign,'*');
  AddOne(MemType_sign,'(');
  i:=ps;SkipThisEntry;ps:=i;
  AddOne(MemType_sign,')');
  goto f1;
  end;

SignError('error in expression');
f2:
ips:=ops;
ps:=o;
move(a[2],i,sizeof(i));
if (a[1]<>chr(TypData_data)) then i:=-1;
if (i=DatType_strg) and (b='[') then begin;
  if (GetNextWord<>Chr(MemType_sign)+'[') then SignError('[ expected');
  MemoryAppend(typ);
  AddOne(MemType_sign,'+');
  CopyOneBracket;
  MemoryDelLast(typ);
  i:=DatType_char;
  a:=#0+bin2str(i,sizeof(i));
  end;
f4:
GetOffsetInType:=i;
if (i<0) then begin;
  MemoryAppend(out);
  fillchar(out,sizeof(out),0);
  SkipThisEntry;
  a:=EvaluateConstant(out);
  if (copy(a,1,1)<>#1) then SignError('integer expression expected');
  delete(a,1,1);
  str2bin(a,i,sizeof(i));
  MemoryDelLast(out);
  end else begin;
  delete(a,1,1);
  i:=GetStandardTypeLen(a);
  end;
if (i<0) then SignError('integer expression expected');
if (out.s=0) then AddOne(MemType_cnst,#1#0#0#0#0);
RecSize:=i;
if (ips<>0) then begin;
  dec(inp.s,ips);
  move(inp.d[ips+1],inp.d,inp.s);
  end;
if (inptmp<>'') then SignError('error in expression');
End;




Procedure ReadUpTypeDef(var d,tmp:MemoryOneRecord);
Var
  begins:LongInt;

Procedure AddOne(tp:Byte;s:String);
Begin;
s:=chr(tp)+s[0]+s;
MemoryRecAppendStr(d,s);
End;


Label f1,f2,f3,f4,f5;
Var
  a,b:String;
  i,o:LongInt;
Begin;
fillchar(d,sizeof(d),0);
begins:=0;
goto f1;

f3:
a:=GetNextWord;
if (a[1]='{') then goto f3;
if (a='') then SignError('unexpected end of file');
if (kicsi(a)='end') then begin;
  AddOne(TypData_end,'');
  dec(begins);
  goto f4;
  end;
TestOneIdentifier(a);
AddOne(TypData_Name,a);
a:=GetNextWord;
if (a<>':') then SignError(': expected');
f1:
a:=kicsi(GetNextWord);
if (a='') then SignError('unexpected end of file');
if (a='record') then begin;
  AddOne(TypData_beg,'');
  inc(begins);
  goto f3;
  end;
if (a='array') then begin;
  if (GetNextWord<>'[') then SignError('[ expected');
  f5:
  MemoryAppend(d);
  ReadUpExpression(#13'.'#13,tmp,d);
  MemoryDelLast(d);
  if (GetNextWord<>'.') then SignError('.. expected');
  a:=EvaluateConstant(tmp);
  if (copy(a,1,1)<>#1) then SignError('integer expression expected');
  delete(a,1,1);
  str2bin(a,o,sizeof(o));
  MemoryAppend(d);
  b:=ReadUpExpression(#13','#13']'#13,tmp,d);
  MemoryDelLast(d);
  a:=EvaluateConstant(tmp);
  if (copy(a,1,1)<>#1) then SignError('integer expression expected');
  delete(a,1,1);
  str2bin(a,i,sizeof(i));
  i:=i-o+1;
  if (i<=0) then SignError('lower bound greater than upper bound');
  a:=bin2str(o,sizeof(o))+bin2str(i,sizeof(i));
  AddOne(TypData_arr,a);
  if (b=',') then goto f5;
  if (b<>']') then SignError('] expected');
  if (kicsi(GetNextWord)<>'of') then SignError('of expected');
  goto f1;
  end;
i:=GetStandardTypes(a);
if (i>0) then begin;
  b:=bin2str(i,sizeof(i));
  o:=0;
  if (i<>DatType_strg) then goto f2;
  a:=GetNextWord;
  if (a<>'[') then begin;
    PutBackWord(a);
    PutBackWord(']');
    PutBackWord('$255');
    end;
  MemoryAppend(d);
  ReadUpExpression(#13']'#13,tmp,d);
  MemoryDelLast(d);
  a:=EvaluateConstant(tmp);
  if (copy(a,1,1)<>#1) then SignError('integer expression expected');
  delete(a,1,1);
  str2bin(a,o,sizeof(o));
  if (o<1) or (o>$ff) then SignError('invalid string length');
  b:=b+bin2str(o,sizeof(o));
  f2:
  AddOne(TypData_data,b);
  goto f4;
  end;
i:=MemoryFind(a,MemType_type,true,tmp);
if (i<0) then SignError('unknown identifier: '+a);
MemoryGetName(tmp);
MemoryRecAppend(d,tmp);
goto f4;
f4:
if (begins>0) then begin;
  a:=GetNextWord;
  if (a<>';') then SignError('; expected');
  goto f3;
  end;
tmp.s:=0;
GetOffsetInType(d,tmp,tmp,o);
End;




Procedure ReadUpTypedConst(var typ,tmp:MemoryOneRecord);
Var
  ps:Word;
  arynum:LongInt;
  deepth:LongInt;

Function GetNext:String;
Var
  t:char;
  a:String;
Begin;
GetNext:='';
t:=typ.d[ps+1];
move(typ.d[ps+2],a,sizeof(a));
a:=t+a;
if (ps+length(a)+1>typ.s) then Exit;
inc(ps,length(a)+1);
GetNext:=a;
End;

Procedure xPush(s:String);
Begin;
move(s[1],tmp.d,length(s));
tmp.s:=length(s);
MemoryAppend(tmp);
inc(arynum);
End;

Function xPop:String;
Var
  s:String;
  i:Word;
Begin;
MemoryDelLast(tmp);
dec(arynum);
i:=tmp.s;if (i>$ff) then i:=$ff;
move(tmp.d,s[1],i);
s[0]:=chr(i);
xPop:=s;
End;


Label f1,f2;
Var
  a,b:String;
  i,o:LongInt;
Begin;
ps:=0;
move(tmp.d[1],i,sizeof(i));
move(tmp.d[5],o,sizeof(o));
PutConstLn('');
PutConstTy(MemType_prcB);
PutConstLn('; - '+BStr(o));
PutConstLn('label '+GetNameConst(i));
arynum:=0;
deepth:=0;
f1:
a:=GetNext;
if (a='') then SignError('error in expression');
if (a[1]=chr(TypData_name)) then begin;
  b:=GetNextWord;
  if (b<>';') then SignError('; expected');
  delete(a,1,1);
  b:=GetNextWord;
  if (kicsi(a)<>kicsi(b)) then SignError('filed identifier expected: '+a);
  b:=GetNextWord;
  if (b<>':') then SignError(': expected');
  PutConstLn(';  '+a);
  goto f2;
  end;
if (a[1]=chr(TypData_data)) then begin;
  delete(a,1,1);
  MemoryAppend(typ);
  b:=ReadUpExpression(#13';'#13')'#13','#13,tmp,typ);
  MemoryDelLast(typ);
  PutBackWord(b);
  b:=EvaluateConstant(tmp);
  if (b='') then SignError('error in expression');
  o:=GetStandardTypeCast(a);
  if (b[1]<>chr(o)) then case o of
    1:SignError('integer constant expected');
    2:SignError('string constant expected');
    3:SignError('boolean expression expected');
    else SignError('type mismatch');
    end;
  delete(b,1,1);
  if (o=1) then begin;
    str2bin(b,o,sizeof(o));
    case GetStandardTypeLen(a) of
      1:a:='b';
      2:a:='w';
      4:a:='d';
      8:a:='q';
      else SignError('type mismatch');
      end;
    putConstLn('def'+a+' '+BStr(o));
    goto f2;
    end;
  str2bin(a,i,sizeof(i));
  if (i=DatType_strg) then begin;
    str2bin(copy(a,5,255),i,sizeof(i));
    b:=copy(b,1,i);
    b:=b[0]+b;
    end;
  o:=GetStandardTypeLen(a);
  PutOutConstant(b,o);
  goto f2;
  end;
if (a[1]=chr(TypData_beg)) then begin;
  a:=GetNextWord;
  if (a<>'(') then SignError('( expected');
  inc(deepth);
  PutBackWord(';');
  goto f2;
  end;
if (a[1]=chr(TypData_end)) then begin;
  a:=GetNextWord;
  if (a<>')') then SignError(') expected');
  dec(deepth);
  goto f2;
  end;
if (a[1]=chr(TypData_arr)) then begin;
  delete(a,1,5);
  a:=a+bin2str(ps,sizeof(ps))+bin2str(deepth,sizeof(deepth));
  xPush(a);
  a:=GetNextWord;
  if (a<>'(') then SignError('( expected');
  deepth:=0;
  PutBackWord(',');
  goto f2;
  end;

SignError('error in expression');
f2:
if (deepth>0) then goto f1;
if (arynum<1) then begin;
  PutConstTy(MemType_prcE);
  Exit;
  end;
a:=xPop;
str2bin(a,i,sizeof(i));
delete(a,1,sizeof(i));
dec(i);
if (i<0) then begin;
  delete(a,1,sizeof(ps));
  str2bin(a,deepth,sizeof(deepth));
  a:=GetNextWord;
  if (a<>')') then SignError(') expected');
  goto f2;
  end;
str2bin(a,ps,sizeof(ps));
a:=bin2str(i,sizeof(i))+a;
xPush(a);
deepth:=0;
a:=GetNextWord;
if (a<>',') then SignError(', expected');
goto f1;
End;
