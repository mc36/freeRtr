Function CodeOneCalc(var d,tmp,tm2:MemoryOneRecord;typ,siz:word):String;
{typ: 0-proc, 1-number, 2-string, 3-boolean, 4-address, 5,6-case
return: 0-procres, 1-numres, 2-strres, 3-jmpcres, 5,6-case}
Var
  ps,po:Word;

Function GetNext(var d:MemoryOneRecord;var ps:word):String;
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

Procedure AddOne(var d:MemoryOneRecord;tp:Byte;s:String);
Begin;
s:=chr(tp)+s[0]+s;
MemoryRecAppendStr(d,s);
End;

Procedure SkipToEndOfProcDef;
Label f1,f2;
Var
  a:String;
  i:LongInt;
Begin;
i:=ps;
a:=GetNext(d,ps);
if (a<>chr(MemType_sign)+'(') then begin;
  ps:=i;
  Exit;
  end;
i:=0;
f1:
po:=ps;
a:=GetNext(d,ps);
if (a='') then SignError('error in expression');
if (a=chr(MemType_sign)+'(') then inc(i);
if (a=chr(MemType_sign)+')') then dec(i);
if (i<0) then goto f2;
goto f1;
f2:
End;

Function SkipToSign(b:String):Boolean;
Label f1,f2;
Var
  a:String;
  i:LongInt;
Begin;
SkipToSign:=True;
i:=0;
f1:
po:=ps;
a:=GetNext(d,ps);
if (a='') then Exit;
if (i=0) and (a=chr(MemType_sign)+b) then goto f2;
if (a=chr(MemType_sign)+'(') then inc(i);
if (a=chr(MemType_sign)+')') then dec(i);
goto f1;
f2:
SkipToSign:=False;
End;

Procedure SkipToEndOfProcPar;
Label f1,f2;
Var
  a:String;
  i:LongInt;
Begin;
i:=0;
f1:
po:=ps;
a:=GetNext(d,ps);
if (a='') then SignError('error in expression');
if (a=chr(MemType_sign)+'(') then inc(i);
if (a=chr(MemType_sign)+')') then dec(i);
if (a=chr(MemType_sign)+'[') then inc(i);
if (a=chr(MemType_sign)+']') then dec(i);
if (a=chr(MemType_sign)+',') and (i=0) then goto f2;
if (i<0) then goto f2;
goto f1;
f2:
End;





Function CodeNextVariablePos(reg:string;AllowPtrOf:Boolean):String;
Label f1,vege;
Var
  VarNum,VarTyp,VarSiz:LongInt;
  HasGetPtr:Boolean;
  HasPtrOf:Boolean;
  HasTypecast:Boolean;
  a:String;
  i,o:LongInt;

Function IsThereGetPtr:Boolean;
Begin;
IsThereGetPtr:=False;
po:=ps;
a:=GetNext(d,ps);
if (a=chr(MemType_sign)+'^') then IsThereGetPtr:=True else ps:=po;
End;


Begin;
HasGetPtr:=False;
HasPtrOf:=False;
HasTypecast:=False;
VarTyp:=-2;
VarSiz:=-1;
a:=GetNext(d,ps);
if (copy(a,1,1)=chr(MemType_type)) then begin;
  HasTypecast:=True;
  str2bin(copy(a,3,255),VarTyp,sizeof(VarTyp));
  if (a[2]=#1) then begin;
    VarTyp:=-VarTyp;
    VarSiz:=GetStandardTypeLen(copy(a,3,255));
    end;
  if (GetNext(d,ps)<>chr(MemType_sign)+'(') then SignError('( expected');
  a:=CodeNextVariablePos(reg,AllowPtrOf);
  if (GetNext(d,ps)<>chr(MemType_sign)+')') then SignError(') expected');
  if (VarTyp<0) then begin;
    VarTyp:=-VarTyp;
    if (reg='') then goto vege;
    str2bin(copy(a,5,255),i,sizeof(i));
    if (i>=VarSiz) then goto vege;
    VarSiz:=i;
    str2bin(a,i,sizeof(i));
    VarTyp:=i;
    goto vege;
    end;
  goto f1;
  end;
if AllowPtrOf and (a=chr(MemType_sign)+'@') then begin;
  HasPtrOf:=True;
  a:=GetNext(d,ps);
  end;
if (copy(a,1,1)<>chr(MemType_var)) then SignError('variable identifier expected');
str2bin(copy(a,2,255),MemoryCurPos,sizeof(MemoryCurPos));
VarNum:=MemoryCurPos;
if MemoryRead(tm2) then SignError('out of memory');
MemoryGetName(tm2);
a:=MemoryRec2str(tm2);
str2bin(copy(a,2,255),VarTyp,sizeof(VarTyp));
case ord(a[1]) of
  VarType_cnst,VarType_norm,VarType_pnrm:;
  VarType_absl,VarType_pflt:HasGetPtr:=True;
  VarType_tptr:begin;
    HasGetPtr:=IsThereGetPtr;
    if not HasGetPtr then VarTyp:=-2;
    end;
  VarType_punt:begin; HasGetPtr:=True;VarTyp:=-2; end;
  else SignError('out of memory');
  end;
if HasPtrOf then VarTyp:=-2;
if (reg<>'') then begin;
  i:=FindOwnerProc(VarNum,True,tm2);
  PutOutProcAddr(reg,GetNameProc(i),GetNameVar(VarNum));
  if HasGetPtr then PutLine('addrLod '+reg+' ['+reg+']');
  end;
f1:
HasGetPtr:=False;
if (VarTyp>=0) then begin;
  MemoryCurPos:=VarTyp;
  if MemoryRead(tm2) then SignError('out of memory');
  a:=MemoryGetName(tm2);
  MemoryAppend(d);
  MemoryRecGetPart(tmp,d,ps,d.s);
  VarTyp:=GetOffsetInType(tm2,d,tmp,VarSiz);
  MemoryAppend(d);
  a:=EvaluateConstant(d);
  MemoryDelLast(d);
  o:=tmp.s;
  if (reg<>'') then begin;
    if (copy(a,1,1)=#1) then begin;
      str2bin(copy(a,2,255),i,sizeof(i));
      if (i<>0) then PutLine('add '+DataSizesList[defOffsSize]+' '+reg+' '+BStr(i));
      a:=#1#0#0#0#0;
      end;
    if (a<>#1#0#0#0#0) then begin;
      PutLine('push '+DataSizesList[defOffsSize]+' '+reg);
      a:=CodeOneCalc(d,tmp,tm2,1,GetStandardLenType(defOffsSize,false));
      if (copy(a,1,1)<>#1) then SignError('out of memory');
      PutLine('pop '+DataSizesList[defOffsSize]+' '+reg);
      PutLine('add '+DataSizesList[defOffsSize]+' '+reg+' a');
      end;
    end;
  MemoryDelLast(d);
  ps:=d.s-o;
  if (VarTyp=DatType_pntr) then HasGetPtr:=IsThereGetPtr;
  HasTypecast:=False;
  end;
if (reg<>'') and HasGetPtr then PutLine('addrLod '+reg+' ['+reg+']');
if (VarTyp<0) and not HasTypecast then begin;
  i:=VarTyp;
  VarTyp:=DatType_untp;
  if (i=-2) then begin; VarSiz:=defAddrSize;VarTyp:=DatType_pntr; end;
  end;
vege:
CodeNextVariablePos:=bin2str(VarTyp,sizeof(VarTyp))+bin2str(VarSiz,sizeof(VarSiz));
End;






Procedure KillTrailingParentheses;
Label f1,f2,f3;
Var
  a:String;
  o:Word;
  i:LongInt;
Begin;
ps:=0;
a:=GetNext(d,ps);
if (a<>chr(MemType_sign)+'(') then goto f1;
o:=ps;
if SkipToSign(')') then goto f1;
if (ps<>d.s) then goto f1;
MemoryRecGetPart(tmp,d,o,po);
d:=tmp;
f1:
ps:=0;
f2:
po:=ps;
a:=GetNext(d,ps);
if (a='') then goto f3;
if (a<>chr(MemType_sign)+'sizeof') then goto f2;
MemoryRecGetPart(tmp,d,0,po);
MemoryAppend(tmp);
if (GetNext(d,ps)<>chr(MemType_sign)+'(') then SignError('( expected');
o:=ps;
if SkipToSign(')') then SignError(') expected');
MemoryRecGetPart(tmp,d,ps,d.s);
MemoryAppend(tmp);
MemoryRecGetPart(tmp,d,o,po);
d:=tmp;
ps:=0;
a:=GetNext(d,ps);
case ord(a[1]) of
  MemType_var:begin;
    ps:=0;
    a:=CodeNextVariablePos('',false);
    if (d.s<>ps) then signError('error in sizeof expression');
    a:=copy(a,5,4);
    end;
  MemType_type:begin;
    case a[2] of
      #1:begin;
        a:=copy(a,3,255);
        i:=GetStandardTypeLen(a);
        end;
      #2:begin;
        str2bin(copy(a,3,255),MemoryCurPos,sizeof(MemoryCurPos));
        if MemoryRead(d) then SignError('out of memory');
        MemoryGetName(d);
        tm2.s:=0;
        GetOffsetInType(d,tmp,tm2,i);
        end;
      else signError('error in sizeof expression');
      end;
    a:=bin2str(i,sizeof(i));
    end;
  else signError('error in sizeof expression');
  end;
a:=chr(MemType_cnst)+#5#1+a;
MemoryDelLast(tmp);
MemoryDelLast(d);
MemoryRecAppendStr(d,a);
MemoryRecAppend(d,tmp);
goto f1;
f3:
End;

Function TestForBoolean:Byte;
{0=none, 1=<, 2=>, 4==, 8=in}
Label f1,f2,f3;
Var
  a:String;
  i,o:Word;
Begin;
TestForBoolean:=0;
ps:=0;
f1:
po:=ps;
a:=GetNext(d,ps);
if (a='') then Exit;
case ord(a[1]) of
  MemType_sign:begin;
    delete(a,1,1);
    if (a='(') then begin;
      ps:=po;
      SkipToEndOfProcDef;
      goto f1;
      end;
    if (a='[') then begin;
      SkipToSign(']');
      goto f1;
      end;
    if (a='<') then goto f2;
    if (a='=') then goto f2;
    if (a='>') then goto f2;
    if (a='in') then goto f2;
    end;
  MemType_prcB:SkipToEndOfProcDef;
  MemType_var:;
  MemType_type:;
  MemType_cnst:;
  end;
goto f1;
f2:
ps:=po;
o:=0;
f3:
i:=ps;
a:=GetNext(d,ps);
if (a=chr(MemType_sign)+'<') then begin; o:=o or 1;goto f3; end;
if (a=chr(MemType_sign)+'>') then begin; o:=o or 2;goto f3; end;
if (a=chr(MemType_sign)+'=') then begin; o:=o or 4;goto f3; end;
if (a=chr(MemType_sign)+'in') then begin; o:=o or 8;goto f3; end;
ps:=i;
if (o=7) or (o=0) then SignError('invalid relation definied');
if (o and 8<>0) and (o<>8) then SignError('invalid relation definied');
TestForBoolean:=o;
End;


Function GetTypeMaxSize(o:LongInt):LongInt;
Label f1,f2,f3;
Var
  i,p,q:LongInt;
  a:String;
Begin;
ps:=0;
f1:
po:=ps;
a:=GetNext(d,ps);
if (a='') then goto f2;
i:=0;
case ord(a[1]) of
  MemType_cnst:begin;
    if (a[2]=#1) then i:=GetStandardLenType(defIntgSize,true) else i:=DatType_strg;
    end;
  MemType_type,MemType_var:begin;
    f3:
    ps:=po;
    a:=CodeNextVariablePos('',true);
    str2bin(a,i,sizeof(i));
    end;
  MemType_prcB:begin;
    SkipToEndOfProcDef;
    str2bin(copy(a,2,255),MemoryCurPos,sizeof(MemoryCurPos));
    repeat
      MemoryNext;
      if MemoryRead(tm2) then SignError('out of memory');
      a:=MemoryGetName(tm2);
      until (a[1]=chr(MemType_prcE));
    a:=MemoryRec2str(tm2);
    str2bin(a,i,sizeof(i));
    end;
  MemType_sign:begin;
    delete(a,1,1);
    if (a='(') then begin;
      q:=ps;
      ps:=po;
      SkipToEndOfProcDef;
      MemoryRecGetPart(tm2,d,q,po);
      p:=ps;
      MemoryAppend(d);
      move(tm2,d,sizeof(d));
      i:=TestForBoolean;
      MemoryDelLast(d);
      ps:=p;
      if (i<>0) then i:=DatType_bool else ps:=q;
      a:='';
      end;
    if (a='@') then goto f3;
    end;
  end;
if (i=0) then goto f1;
if (i>o) then o:=i;
goto f1;
f2:
if (o=DatType_bool) then o:=DatType_byte;
GetTypeMaxSize:=o;
End;



Procedure CodeNumCalc;
Var
  osiz:word;
  opre:string[3];


Function ReadUpNextNumber(HasData:Boolean):Boolean;
Label f1,f2,f3,f4;
Var
  HasSign:Boolean;
  HasNot:Boolean;
  a,b:String;
  i,o,p:LongInt;
Begin;
ReadUpNextNumber:=True;
HasSign:=False;
HasNot:=False;
f1:
po:=ps;
a:=GetNext(d,ps);
if (a='') then Exit;
if (a=chr(MemType_sign)+'-') then begin;
  HasSign:=not HasSign;
  goto f1;
  end;
if (a=chr(MemType_sign)+'+') then begin;
  HasSign:=HasSign;
  goto f1;
  end;
if (a=chr(MemType_sign)+'@') then begin;
  goto f4;
  end;
if (a=chr(MemType_sign)+'not') then begin;
  HasNot:=not HasNot;
  goto f1;
  end;
if (a[1]=chr(MemType_type)) then begin;
  goto f4;
  end;
if (a[1]=chr(MemType_cnst)) then begin;
  str2bin(copy(a,3,255),i,sizeof(i));
  if HasNot then i:=not i;
  if HasSign then i:=-i;
  if HasData then a:='b' else a:='a';
  PutLine('move '+opre+opre+' '+a+' '+BStr(i));
  goto f2;
  end;
if (a=chr(MemType_sign)+'(') then begin;
  if HasData then PutLine('push '+opre[2]+' a');
  o:=ps;
  ps:=po;
  SkipToEndOfProcDef;
  MemoryRecGetPart(tmp,d,o,po);
  MemoryAppend(d);
  a:=CodeOneCalc(tmp,d,tm2,1,osiz);
  if (copy(a,1,1)<>#1) then SignError('out of memory');
  MemoryDelLast(d);
  if HasData then PutLine('move '+opre+opre+' b a');
  goto f3;
  end;
if (a[1]=chr(MemType_var)) then begin;
  f4:
  ps:=po;
  if HasData then PutLine('push '+opre[2]+' a');
  p:=po;
  a:=CodeNextVariablePos('src',true);
  str2bin(a,i,sizeof(i));
  if (i=DatType_pntr) or (i=DatType_untp) then SignError('pointer is not compatible with integer');
  a:=GetStandardIntPrefix(a);
  if hasData then b:='b' else b:='a';
  PutLine('movR d'+opre+a+' '+b+' [src]');
  f3:
  if hasData then b:='b' else b:='a';
  if HasNot then PutLine('not '+opre[2]+' '+b);
  if HasSign then PutLine('neg '+opre[2]+' '+b);
  if HasData then PutLine('pop '+opre[2]+' a');
  goto f2;
  end;
if (a[1]=chr(MemType_prcB)) then begin;
  if HasData then PutLine('push '+opre[2]+' a');
  str2bin(copy(a,2,255),MemoryCurPos,sizeof(MemoryCurPos));
  repeat
    MemoryNext;
    if MemoryRead(tmp) then SignError('out of memory');
    a:=MemoryGetName(tmp);
    until (a[1]=chr(MemType_prcE));
  b:=MemoryRec2str(tmp);
  o:=po;
  SkipToEndOfProcDef;
  MemoryAppend(d);
  MemoryRecGetPart(tm2,d,o,ps);
  a:=CodeOneCalc(tm2,tmp,d,0,0);
  MemoryDelLast(d);
  a:=GetStandardIntPrefix(b);
  if hasData then b:='b' else b:='a';
  PutLine('movR d'+opre+a+' '+b+' [src]');
  goto f3;
  end;

SignError('error in expression');
f2:
ReadUpNextNumber:=False;
End;

Function ReadUpNextInstruction:Byte;
{1=*, 2=div, 3=mod, 4=shr, 5=shl, 6=and, 7=or, 8-xor, 9=+, 10=-  0-nothing}
Label f1;
Var
  a:String;
  o:integer;
Begin;
ReadUpNextInstruction:=0;
a:=GetNext(d,ps);
if (a='') then Exit;
if (copy(a,1,1)<>chr(MemType_sign)) then goto f1;
delete(a,1,1);
o:=-1;
if (a='*') then o:=1;
if (a='div') then o:=2;
if (a='mod') then o:=3;
if (a='shr') then o:=4;
if (a='shl') then o:=5;
if (a='and') then o:=6;
if (a='or') then o:=7;
if (a='xor') then o:=8;
if (a='+') then o:=9;
if (a='-') then o:=10;
if (o<0) then goto f1;
ReadUpNextInstruction:=o;
Exit;
f1:
SignError('invalid integer instruction');
End;

Procedure CodeOneMuvelet(i:Word);
Var a:String;
Begin;
case i of
  1:PutLine('mul '+opre+' a b');
  2:PutLine('div '+opre+' a b');
  3:PutLine('mod '+opre+' a b');
  4:PutLine('shr '+opre[2]+' a b');
  5:PutLine('shl '+opre[2]+' a b');
  6:PutLine('and '+opre[2]+' a b');
  7:PutLine('or '+opre[2]+' a b');
  8:PutLine('xor '+opre[2]+' a b');
  9:PutLine('add '+opre[2]+' a b');
  10:PutLine('sub '+opre[2]+' a b');
  else SignError('out of memory');
  end;
End;



Label f1,f2,f3,f4,f5;
Var
  a:String;
  b:boolean;
  i,o:LongInt;
Begin;
osiz:=siz;
opre:=GetStandardIntPrefix(chr(siz)+#0#0#0);
ps:=0;
if ReadUpNextNumber(false) then begin;
  PutLine('sub '+opre[2]+' a a');
  Exit;
  end;

o:=0;b:=false;
i:=ReadUpNextInstruction;
if (i in [1..5]) then goto f2;
o:=i;
f1:
b:=true;
if (o=0) then Exit;
if ReadUpNextNumber(true) then SignError('integer expression expected');
i:=ReadUpNextInstruction;
if (i in [1..5]) then goto f2;
f5:
CodeOneMuvelet(o);
o:=i;
goto f1;
f2:
if b then begin;
  PutLine('push '+opre[2]+' a');
  PutLine('move '+opre+opre+' a b');
  end;
f3:
if ReadUpNextNumber(true) then SignError('integer expression expected');
CodeOneMuvelet(i);
i:=ReadUpNextInstruction;
if (i=0) then goto f4;
if not (i in [1..5]) then goto f4;
goto f3;
f4:
if b then begin;
  PutLine('move '+opre+opre+' b a');
  PutLine('pop '+opre[2]+' a');
  end;
if not b then begin;
  o:=i;
  goto f1;
  end;
goto f5;
End;



Procedure ProcessCodingForBol(reg:char);
Var
  i,o:LongInt;
  a:String;
Begin;
if (typ<>DatType_bool) then begin;
  a:=CodeOneCalc(d,tmp,tm2,1,typ);
  if (copy(a,1,1)<>#1) then SignError('out of memory');
  a:=GetStandardIntPrefix(chr(typ)+#0#0#0);
  if (reg<>'a') then PutLine('move '+a+a+' '+reg+' a');
  end else begin;
  a:=CodeOneCalc(d,tmp,tm2,2,0);
  if (copy(a,1,1)<>#2) then SignError('out of memory');
  PutLine('movR dubub '+reg+' [src+1]');
  end;
End;

Procedure ProcessOneInPart(mylab:longint);
Label f1,f2;
Var
  i,o:LongInt;
  a:String;
Begin;
if (mylab=-1) then goto f2;
ps:=0;SkipToSign(',');
MemoryRecGetPart(tmp,d,ps,d.s);
MemoryAppend(tmp);
d.s:=po;
ps:=0;
if not SkipToSign('.') then goto f1;
a:=GetStandardIntPrefix(chr(typ)+#0#0#0);
PutLine('push '+a[2]+' b');
ProcessCodingForBol('a');
PutLine('pop '+a[2]+' b');
PutLine('comp '+a+a+' a b');
PutLine('jmpc e '+GetNameProc(CurrProcNum)+GetNameTmpLbl(mylab+1));
MemoryDelLast(d);
Exit;

f1:
o:=TempLabelNum;inc(TempLabelNum,1);
if (GetNext(d,ps)<>chr(MemType_sign)+'.') then SignError('.. expected');
MemoryRecGetPart(tmp,d,ps,d.s);
MemoryAppend(tmp);
d.s:=po;
a:=GetStandardIntPrefix(chr(typ)+#0#0#0);
PutLine('push '+a[2]+' b');
ProcessCodingForBol('a');
PutLine('pop '+a[2]+' b');
PutLine('comp '+a+a+' b a');
PutLine('jmpc b '+GetNameProc(CurrProcNum)+GetNameTmpLbl(o+1));
MemoryDelLast(d);
a:=GetStandardIntPrefix(chr(typ)+#0#0#0);
PutLine('push '+a[2]+' b');
ProcessCodingForBol('a');
PutLine('pop '+a[2]+' b');
PutLine('comp '+a+a+' b a');
PutLine('jmpc be '+GetNameProc(CurrProcNum)+GetNameTmpLbl(mylab+1));
PutLine('label '+GetNameProc(CurrProcNum)+GetNameTmpLbl(o+1));
MemoryDelLast(d);
Exit;

f2:
typ:=GetTypeMaxSize(0);
a:=chr(typ)+#0#0#0;
o:=GetStandardTypeCast(a);
a:=CodeOneCalc(d,tmp,tm2,o,typ);
if (copy(a,1,1)<>chr(o)) then SignError('error in expression');
i:=1;
if (o=1) then begin;
  a:=GetStandardIntPrefix(chr(typ)+#0#0#0);
  PutLine('move '+a+a+' b a');
  i:=0;
  end;
if (o=2) then begin;
  PutLine('movR dubub b [src+1]');
  typ:=DatType_bool;
  i:=0;
  end;
if (i<>0) then SignError('error in expression');
Exit;
End;


Function FindNextProcParam(var d:MemoryOneRecord):String;
Label f1,f2;
Var a:String;
Begin;
FindNextProcParam:='';
f1:
MemoryNext;
if MemoryRead(d) then SignError('out of memory');
a:=MemoryGetName(d);
if (a[1]=chr(MemType_prcE)) then goto f2;
if (a[1]=chr(MemType_var)) then goto f2;
goto f1;
f2:
FindNextProcParam:=a;
End;

Procedure CodeGetParam(a:String;procnm,varpos:LongInt);
Label f1,f2,f3;
Var
  i,o:LongInt;
Begin;
if (a='') then a:=#0;
case ord(a[1]) of
  VarType_pnrm:goto f1;
  VarType_pflt:goto f2;
  VarType_punt:goto f3;
  else SignError('out of memory');
  end;
Exit;

f1:
str2bin(copy(a,2,255),MemoryCurPos,sizeof(MemoryCurPos));
if MemoryRead(tmp) then SignError('out of memory');
MemoryGetName(tmp);
po:=0;
a:=GetNext(tmp,po);
if (copy(a,1,1)<>chr(TypData_data)) then begin;
  MemoryAppend(d);
  d.s:=0;
  GetOffsetInType(tmp,tm2,d,i);
  MemoryDelLast(d);
  a:=CodeNextVariablePos('src',false);
  str2bin(copy(a,5,255),o,sizeof(o));
  if (i<>o) then SignError('type mismatch');
  PutOutPreProcAddr('trg',procnm,varpos);
  a:='u'+DataSizesList[defOffsSize];
  PutLine('move '+a+a+' c '+BStr(o));
  PutLine('sysCall memCopy');
  Exit;
  end;
i:=ps;
SkipToEndOfProcPar;
ps:=po;
MemoryRecGetPart(tm2,d,i,po);
delete(a,1,1);
i:=GetStandardTypeCast(a);
if (i=1) then begin;
  o:=GetStandardTypeLen(a);
  str2bin(a,i,sizeof(i));
  MemoryAppend(d);
  a:=CodeOneCalc(tm2,d,tmp,1,i);
  if (copy(a,1,1)<>#1) then SignError('out of memory');
  MemoryDelLast(d);
  PutOutPreProcAddr('trg',procnm,varpos);
  a:=GetStandardIntPrefix(chr(i)+#0#0#0);
  PutLine('movW d'+a+a+' [trg] a');
  Exit;
  end;
if (i<>2) then SignError('error in expression');
o:=GetStandardTypeLen(a);
MemoryAppend(d);
a:=CodeOneCalc(tm2,d,tmp,2,o);
if (copy(a,1,1)<>#2) then SignError('out of memory');
MemoryDelLast(d);
if (o=1) then begin;
  PutLine('movR dubub a [src+1]');
  PutOutPreProcAddr('trg',procnm,varpos);
  PutLine('movW dubub [trg] a');
  Exit;
  end;
PutOutPreProcAddr('trg',procnm,varpos);
a:='u'+DataSizesList[defOffsSize];
PutLine('move '+a+a+' c '+BStr(o-1));
PutLine('call StringCopy');
Exit;

f2:
str2bin(copy(a,2,255),MemoryCurPos,sizeof(MemoryCurPos));
if MemoryRead(tmp) then SignError('out of memory');
MemoryGetName(tmp);
MemoryAppend(d);
d.s:=0;
GetOffsetInType(tmp,tm2,d,i);
MemoryDelLast(d);
a:=CodeNextVariablePos('src',false);
str2bin(copy(a,5,255),o,sizeof(o));
if (i<>o) then SignError('type mismatch');
PutOutPreProcAddr('trg',procnm,varpos);
PutLine('addrSav [trg] src');
Exit;

f3:
a:=CodeNextVariablePos('src',true);
PutOutPreProcAddr('trg',procnm,varpos);
PutLine('addrSav [trg] src');
Exit;
End;





Label prc,prc1,prc2;
Label num;
Label str,str1;
Label bol,bol1,bol2,bol3,bol4;
Var
  i,o,p:LongInt;
  a:String;
Begin;
CodeOneCalc:='';
KillTrailingParentheses;
if (typ=5) then begin;
  ProcessOneInPart(-1);
  i:=typ;
  CodeOneCalc:=#5+bin2str(i,sizeof(i));
  Exit;
  end;
if (typ=6) then begin;
  AddOne(d,MemType_sign,',');
  typ:=siz;
  i:=TempLabelNum;inc(TempLabelNum,2);
  repeat
    ProcessOneInPart(i);
    until (d.s=0);
  CodeOneCalc:=#6+bin2str(i,sizeof(i));
  Exit;
  end;
if (typ=1) and (siz=DatType_bool) then siz:=DatType_byte;
if (typ=3) then begin; typ:=1;siz:=DatType_bool; end;
MemoryAppend(d);
a:=EvaluateConstant(d);
MemoryDelLast(d);
if (a<>'') then begin;
  if (a[1]<>chr(typ)) then SignError('type mismatch');
  delete(a,1,1);
  case typ of
    1:begin;
      str2bin(a,i,sizeof(i));
      a:=chr(siz)+#0#0#0;
      a:=GetStandardIntPrefix(a);
      PutLine('move '+a+a+' a '+BStr(i));
      CodeOneCalc:=chr(typ);
      Exit;
      end;
    2:a:=a;
    else SignError('error in expression');
    end;
  d.s:=0;
  AddOne(d,MemType_cnst,chr(typ)+a);
  end;
if (typ=4) then begin;
  i:=siz;
  typ:=0;
  siz:=0;
  ps:=0;
  if (i and $7f=1) then a:='src' else a:='trg';
  a:=CodeNextVariablePos(a,i and $80=0);
  CodeOneCalc:=a;
  MemoryRecGetPart(tmp,d,ps,d.s);
  if (ps<>d.s) then SignError('error in expression');
  exit;
  end;


o:=TestForBoolean;
if (o<>0) then goto bol;

if (typ=1) then goto num;
if (typ=0) then goto prc;
if (typ=2) then goto str;
SignError('error in expression');

str:
ps:=0;
dec(siz);
if (siz<1) then siz:=$222;
p:=GetTempStrVarNum;
PutOutProcAddr('trg',GetNameProc(CurrProcNum),GetNameTmpStr(p));
PutLine('sub b a a');
PutLine('movW dubub [trg] a');
if (siz>$ff) then siz:=$ff;
str1:
po:=ps;
a:=GetNext(d,ps);
if (a='') then begin;
  PutOutProcAddr('src',GetNameProc(CurrProcNum),GetNameTmpStr(p));
  dec(TempStrVarCur);
  CodeOneCalc:=#2;
  Exit;
  end;
if (a=Chr(MemType_sign)+'+') then goto str1;
if (a=Chr(MemType_sign)+')') then goto str1;
if (a=Chr(MemType_sign)+'(') then goto str1;
case ord(a[1]) of
  MemType_prcB:begin;
    i:=po;
    SkipToEndOfProcDef;
    MemoryAppend(d);
    MemoryRecGetPart(tm2,d,i,ps);
    a:=CodeOneCalc(tm2,d,tmp,0,0);
    MemoryDelLast(d);
    PutOutProcAddr('trg',GetNameProc(CurrProcNum),GetNameTmpStr(p));
    str2bin(copy(a,2,255),i,sizeof(i));
    if (i=DatType_char) then begin;
      PutLine('movR dubub a [src]');
      PutLine('call StringAppendChr');
      end else begin;
      PutLine('call StringAppendStr');
      end;
    end;
  MemType_var:begin;
    ps:=po;
    a:=CodeNextVariablePos('src',false);
    if (GetStandardTypeCast(a)<>2) then SignError('string variable expected');
    PutOutProcAddr('trg',GetNameProc(CurrProcNum),GetNameTmpStr(p));
    str2bin(a,i,sizeof(i));
    if (i=DatType_char) then begin;
      PutLine('movR dubub a [src]');
      PutLine('call StringAppendChr');
      end else begin;
      PutLine('call StringAppendStr');
      end;
    end;
  MemType_cnst:begin;
    delete(a,1,1);
    if (a[1]<>#2) then SignError('string constant expected');
    delete(a,1,1);
    a:=a[0]+a;
    inc(NumberConst);
    PutConstLn('');
    PutConstTy(MemType_prcB);
    PutConstLn('; - '+BStr(CurrProcNum));
    PutConstLn('label '+GetNameConst(NumberConst));
    o:=length(a);
    PutOutConstant(a,o);
    PutConstTy(MemType_prcE);
    PutLine('codeOfs src '+GetNameConst(NumberConst));
    i:=GetTempStrVarNum;
    dec(TempStrVarCur);
    PutOutProcAddr('trg',GetNameProc(CurrProcNum),GetNameTmpStr(i));
    a:='u'+DataSizesList[defOffsSize];
    PutLine('move '+a+a+' c '+BStr(o));
    PutLine('sysCall codeCopy');
    PutOutProcAddr('src',GetNameProc(CurrProcNum),GetNameTmpStr(i));
    PutOutProcAddr('trg',GetNameProc(CurrProcNum),GetNameTmpStr(p));
    PutLine('call StringAppendStr');
    end;
  else SignError('string expression expected');
  end;
goto str1;
Exit;



num:
p:=siz;
if (siz=0) then siz:=GetStandardLenType(defIntgSize,true);
siz:=GetTypeMaxSize(siz);
CodeNumCalc;
if (p<>DatType_bool) then begin;
  CodeOneCalc:=#1;
  Exit;
  end;
a:=GetStandardIntPrefix(chr(typ)+#0#0#0);
PutLine('and '+a[2]+' a 1');
PutLine('comp '+a+a+' a 0');
CodeOneCalc:=#3'ne';
Exit;


bol:
if (typ<>1) then SignError('type mismatch');
MemoryRecGetPart(tmp,d,ps,d.s);
d.s:=po;
if (o and 8<>0) then goto bol3;
MemoryAppend(tmp);
i:=GetTypeMaxSize(0);
MemoryDelLast(tmp);
i:=GetStandardTypeCast(chr(i)+#0#0#0);
if (i=2) then goto bol2;
if (i<>1) then SignError('error in expression');
typ:=GetStandardLenType(defIntgSize,true);
MemoryAppend(tmp);
typ:=GetTypeMaxSize(typ);
MemoryDelLast(tmp);
MemoryRecExchangeTwo(d,tmp);
MemoryAppend(tmp);
typ:=GetTypeMaxSize(typ);
a:=CodeOneCalc(d,tmp,tm2,1,typ);
if (copy(a,1,1)<>#1) then SignError('out of memory');
MemoryDelLast(tmp);
a:=GetStandardIntPrefix(chr(typ)+#0#0#0);
PutLine('push '+a[2]+' a');
a:=CodeOneCalc(tmp,d,tm2,1,typ);
if (copy(a,1,1)<>#1) then SignError('out of memory');
a:=GetStandardIntPrefix(chr(typ)+#0#0#0);
PutLine('pop '+a[2]+' b');
bol1:
a:=GetStandardIntPrefix(chr(typ)+#0#0#0);
PutLine('comp '+a+a+' a b');
case o and 7 of
  1:a:='b';
  2:a:='a';
  3:a:='ne';
  4:a:='e';
  5:a:='be';
  6:a:='ae';
  else SignError('error in expression');
  end;
CodeOneCalc:=#3+a;
if (siz=DatType_bool) then Exit;
PutLine('setc '+a+' '+DataSizesList[GetStandardTypeLen(chr(siz)+#0#0#0)]+' a');
CodeOneCalc:=#1;
Exit;

bol2:
MemoryAppend(tmp);
a:=CodeOneCalc(d,tmp,tm2,2,0);
if (copy(a,1,1)<>#2) then SignError('out of memory');
MemoryDelLast(tmp);
inc(TempStrVarCur);
PutLine('push '+DataSizesList[defOffsSize]+' src');
a:=CodeOneCalc(tmp,d,tm2,2,0);
if (copy(a,1,1)<>#2) then SignError('out of memory');
PutLine('pop '+DataSizesList[defOffsSize]+' trg');
dec(TempStrVarCur);
PutLine('call StringCompare');
typ:=DatType_byte;
goto bol1;

bol3:
MemoryAppend(tmp);
ProcessOneInPart(-1);
MemoryDelLast(d);
ps:=0;
if (GetNext(d,ps)<>Chr(MemType_sign)+'[') then SignError('[ expected');
MemoryRecReplace(d,0,ps,'');
ps:=0;
repeat
  po:=ps;
  a:=GetNext(d,ps);
  until (ps>=d.s);
if (a<>Chr(MemType_sign)+']') then SignError('] expected');
d.s:=po;
AddOne(d,MemType_sign,',');
i:=TempLabelNum;inc(TempLabelNum,2);
repeat
  ProcessOneInPart(i);
  until (d.s=0);
a:=GetStandardIntPrefix(chr(siz)+#0#0#0);
PutLine('move '+a+a+' a 0');
PutLine('jump '+GetNameProc(CurrProcNum)+GetNameTmpLbl(i+2));
PutLine('label '+GetNameProc(CurrProcNum)+GetNameTmpLbl(i+1));
PutLine('move '+a+a+' a 1');
PutLine('label '+GetNameProc(CurrProcNum)+GetNameTmpLbl(i+2));
CodeOneCalc:=#1;
if (siz<>DatType_bool) then Exit;
PutLine('comp '+a+a+' a 0');
CodeOneCalc:=#3'ne';
Exit;


prc:
PutLine(';calling sequence begins here...');
ps:=0;
a:=GetNext(d,ps);
if (copy(a,1,1)<>chr(MemType_prcB)) then SignError('procedure or function identifier expected');
str2bin(copy(a,2,255),MemoryCurPos,sizeof(MemoryCurPos));
if MemoryRead(tmp) then SignError('out of memory');
a:=MemoryGetName(tmp);
PutLine(';invoking '+copy(a,2,255)+'...');
a:=MemoryRec2str(tmp);
str2bin(copy(a,2,255),o,sizeof(o));
PutType(MemType_prcC);
PutLine('; - '+BStr(o));
a:=GetNameProc(o);
PutLine('procAllocBeg '+a+'ds '+a+'vars');
a:=FindNextProcParam(tmp);
if (copy(a,1,1)=chr(MemType_prcE)) then goto prc1;
if (GetNext(d,ps)<>Chr(MemType_sign)+'(') then SignError('( expected');
i:=MemoryCurPos;
prc2:
a:=MemoryRec2str(tmp);
CodeGetParam(a,o,i);
MemoryCurPos:=i;
a:=FindNextProcParam(tmp);
i:=MemoryCurPos;
if (copy(a,1,1)=Chr(MemType_prcE)) then begin;
  if (GetNext(d,ps)<>Chr(MemType_sign)+')') then SignError(') expected');
  goto prc1;
  end;
if (GetNext(d,ps)<>Chr(MemType_sign)+',') then SignError(', expected');
goto prc2;
prc1:
if (GetNext(d,ps)<>'') then SignError('; expected');
a:=GetNameProc(o);
PutLine('procAllocEnd '+a+'ds '+a+'vars');
PutLine('call '+a);
PutLine('procFree '+a+'ds '+a+'vars');
PutLine(';calling sequence ends here...');
a:=MemoryRec2str(tmp);
CodeOneCalc:=#0+a;
Exit;
End;
