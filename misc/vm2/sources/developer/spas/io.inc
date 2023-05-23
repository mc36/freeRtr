Type
  LongWord=LongInt;
  QuadInt=LongInt;
  QuadWord=QuadInt;

Const AllFileHndMax=10;
Var
  AllFileHndDat:array[1..AllFileHndMax] of record
    fn:String;
    ps:LongInt;
    ln:LongInt;
    end;
  AllFileHndPos:Integer;
  CurrentFileHnd:xtText;
  CurrentFileLin:LongInt;
  ExpressionBegin:LongInt;
  UnNeededWord:String;
  UnNeededChar:String;


Const
  DataSizesList:array[1..8] of Char=('b','w','?','d','?','?','?','q');

Var
  NumberConst:LongInt;
  NumberProc:LongInt;
  OutCodeHandler:xtText;
  OutDataHandler:xtText;
  TemporaryPath:String;
  UsrLibraryPath:String;
  PlatformPath:String;
  defAddrSize:LongInt;
  defOffsSize:LongInt;
  defIntgSize:LongInt;
  defMxItSize:LongInt;
  defStckSize:LongInt;
  defHeapSize:LongInt;
  defAlignNum:LongInt;




Procedure SignError(p:string);
Var
  a:String;
Begin;
write(#13);clreol;
writeln('error: '+p);
if (AllFileHndPos>0) then begin;
  writeLn('file: '+AllFileHndDat[AllFileHndPos].fn);
  writeLn('line: '+BStr(ExpressionBegin)+'..'+BStr(CurrentFileLin));
  end;
writeln('exiting...');
halt(1);
End;

Function GetFullDateString:String;
Var w1,w2,w3:Word;
Begin;
xGetDate(w1,w2,w3);
GetFullDateString:=Right('0000'+BStr(w1),4)+
              +'-'+Right('00'+BStr(w2),2)+
              +'-'+Right('00'+BStr(w3),2);
End;

Function GetFullTimeString:String;
Var w1,w2,w3:Word;
Begin;
xGetTime(w1,w2,w3);
GetFullTimeString:=Right('00'+BStr(w1),2)+
              +':'+Right('00'+BStr(w2),2)+
              +':'+Right('00'+BStr(w3),2);
End;


Function dup(n:Byte;c:Char):String;
Var
  a:string;
  i:byte;
Begin;
a:='';
for i:=1 to n do a:=a+c;
dup:=a;
End;

Function xLevesz(a:String):String;
Begin;
Kicserel(#9,' ',a);
Kicserel(#255,' ',a);
Kicserel(#0,' ',a);
while (Copy(a,1,1)=' ') do Delete(a,1,1);
xLevesz:=Levesz(a);
End;

Function UnPlugString(a:String):String;
Begin;
Kicserel(#9,' ',a);
Kicserel(#255,' ',a);
Kicserel(#0,' ',a);
Kicserel('  ',' ',a);
UnPlugString:=a;
End;

Function Hex(d:LongInt;n:Byte):String;
Var
  i:Byte;
  a:String[10];
Begin;
a:='';
for i:=1 to n do begin;
  a:=Byte2HexType(d)+a;
  d:=d shr 8;
  end;
Hex:=a;
End;

Function bin2str(var d;s:Word):String;
Var a:String;
Begin;
if (s>$ff) then s:=$ff;
move(d,a[1],s);
a[0]:=chr(s);
bin2str:=a;
End;

Function str2bin(a:String;var d;s:Word):Boolean;
Begin;
str2bin:=True;
if (length(a)<s) then Exit;
move(a[1],d,s);
delete(a,1,s);
str2bin:=False;
End;

Function PosL(SubStr,s:String):Byte;
Var i,o:Byte;
Begin;
o:=0;
for i:=1 to length(s) do if (copy(s,i,length(subStr))=subStr) then o:=i;
PosL:=o;
End;

Function xtReWrite(var t:xtText;s:String):LongInt;
Begin;
xErase(s);
xCreate(s);
xtReWrite:=xtOpen(t,s,false);
End;

Function GetNextCfgLine(var t:xtText):String;
Label f1;
Var
  a:String;
  i:Word;
Begin;
GetNextCfgLine:='';
f1:
if xtEOF(t) then Exit;
a:=xtReadLn(t,255);
i:=pos(';',a);
if (i=0) then i:=$200;
a:=copy(a,1,i-1);
a:=xLevesz(UnPlugString(a));
if (a='') then goto f1;
GetNextCfgLine:=a;
End;

Function RepairPath(a:String):String;
Begin;
if (a='') then a:='.';
if (copy(a,length(a),1)<>'\') then a:=a+'\';
RepairPath:=a;
End;

Procedure alignOneNumber(var p:LongInt);
Var i:LongInt;
Begin;
i:=p mod defAlignNum;
if (i=0) then exit;
inc(p,defAlignNum-i);
End;


Function GetTypeFromAsmLine(a:String):LongInt;
Const beg=';-->';
Begin;
GetTypeFromAsmLine:=-1;
if (copy(a,1,length(beg))<>beg) then Exit;
delete(a,1,length(beg));
GetTypeFromAsmLine:=BVal(a);
End;

Function GetPrcNmFromAsmLine(a:String):LongInt;
Const beg=' - ';
Var i:LongInt;
Begin;
i:=pos(beg,a);
delete(a,1,i+length(beg)-1);
GetPrcNmFromAsmLine:=BVal(a);
End;

Function FindOneTypeInSource(var t:xtText;n:LongInt):Boolean;
Var
  a:String;
  i:LongInt;
Begin;
FindOneTypeInSource:=True;
repeat
  if xtEOF(t) then Exit;
  a:=xtReadLn(t,255);
  i:=GetTypeFromAsmLine(a);
  until (i=n);
FindOneTypeInSource:=False;
End;

Function GetWordFromSepyList(var a:String):String;
Var i:Word;
Begin;
i:=pos(' ',a);
if (i=0) then i:=$100;
GetWordFromSepyList:=copy(a,1,i-1);
delete(a,1,i);
End;





Function GetTemporaryName(a:String):String;
Begin;
GetTemporaryName:=TemporaryPath+a;
End;

Function GetNameConst(i:LongInt):String;
Begin;
GetNameConst:='const_'+hex(i,4);
End;

Function GetNameVar(i:LongInt):String;
Begin;
GetNameVar:='var_'+hex(i,4);
End;

Function GetNameProc(i:LongInt):String;
Begin;
GetNameProc:='proc_'+hex(i,4);
End;

Function GetNameLabel(i:LongInt):String;
Begin;
GetNameLabel:='label_'+hex(i,4);
End;

Function GetNameTmpLbl(i:LongInt):String;
Begin;
GetNameTmpLbl:='tmplab_'+hex(i,4);
End;

Function GetNameTmpStr(n:LongInt):String;
Begin;
GetNameTmpStr:='tmpstr_'+hex(n,4);
End;

Function GetNameTmpNum(n:LongInt):String;
Begin;
GetNameTmpNum:='tmpnum_'+hex(n,4);
End;

Procedure TestOneIdentifier(a:String);
Var i,o:Word;
Begin;
o:=0;
a:=kicsi(a);
if (a='') then o:=1;
if not (a[1] in ['a'..'z']) then o:=1;
for i:=1 to length(a) do if not (a[1] in ['a'..'z','0'..'9','_']) then o:=1;
if (o=0) then exit;
SignError('invalid identifier: '+a);
End;







Procedure PutLine(s:String);
Begin;
xtWriteLn(OutCodeHandler,s);
End;

Procedure PutType(b:Byte);
Begin;
xtWriteLn(OutCodeHandler,';-->'+BStr(b));
End;

Procedure PutConstLn(s:String);
Begin;
xtWriteLn(OutDataHandler,s);
End;

Procedure PutConstTy(b:Byte);
Begin;
xtWriteLn(OutDataHandler,';-->'+BStr(b));
End;



Procedure PutOutConstant(a:String;len:LongInt);
Var
  c:String;
  i:LongInt;

Procedure addOne(ch:char);
Begin;
if (c<>'') then c:=c+',';
c:=c+bstr(ord(ch));
if (length(c)<60) then exit;
xtWriteLn(OutDataHandler,'defb '+c);
c:='';
End;

Begin;
a:=copy(a,1,len);
c:='';
for i:=1 to length(a) do addOne(a[i]);
dec(len,length(a));
for i:=1 to len do addOne(#0);
if (c<>'') then xtWriteLn(OutDataHandler,'defb '+c);
End;

Procedure PutOutProcAddr(adr,prc,vr:String);
Begin;
PutLine('procAddr '+adr+' '+prc+'ds');
PutLine('add '+DataSizesList[defOffsSize]+' '+adr+' '+prc+vr);
End;

Procedure PutOutPreProcAddr(adr:String;prc,vr:LongInt);
Begin;
PutLine('procAddr '+adr+' -');
PutLine('add '+DataSizesList[defOffsSize]+' '+adr+' '+GetNameProc(prc)+GetNameVar(vr));
End;





Procedure WriteFilePosition;
Begin;
Write(#13'(');
Write(Copy(BStr(CurrentFileLin)+'      ',1,6));
Write(') ');
Write(AllFileHndDat[AllFileHndPos].fn);
Write(#13);
End;


Function OpenOneFile(s:String):Boolean;
Begin;
OpenOneFile:=True;
if (AllFileHndPos>0) then begin;
  WriteFilePosition;
  AllFileHndDat[AllFileHndPos].ps:=xtGetPos(CurrentFileHnd);
  AllFileHndDat[AllFileHndPos].ln:=CurrentFileLin;
  xtClose(CurrentFileHnd);
  end;
if (AllFileHndPos+1>AllFileHndMax) then Exit;
if (xtOpen(CurrentFileHnd,s,true)<>0) then Exit;
inc(AllFileHndPos);
AllFileHndDat[AllFileHndPos].fn:=s;
CurrentFileLin:=1;
Write(#13);ClrEol;
WriteFilePosition;
OpenOneFile:=False;
End;

Function CloseCurrFile:Boolean;
Begin;
CloseCurrFile:=True;
WriteFilePosition;
xtClose(CurrentFileHnd);
if (AllFileHndPos-1<1) then Exit;
dec(AllFileHndPos);
CurrentFileLin:=AllFileHndDat[AllFileHndPos].ln;
if (xtOpen(CurrentFileHnd,AllFileHndDat[AllFileHndPos].fn,true)<>0) then Exit;
xtSetPos(CurrentFileHnd,AllFileHndDat[AllFileHndPos].ps);
Write(#13);ClrEol;
WriteFilePosition;
CloseCurrFile:=False;
End;

Function GetNextChar(throughEOL:boolean):String;
Begin;
GetNextChar:='';
if (UnNeededChar<>'') then begin;
  GetNextChar:=copy(UnNeededChar,1,1);
  delete(UnNeededChar,1,1);
  Exit;
  end;
if xtEOF(CurrentFileHnd) then Exit;
if xtEOL(CurrentFileHnd) then begin;
  if not throughEOL then Exit;
  GetNextChar:=' ';
  UnNeededChar:=UnNeededChar+xtReadLn(CurrentFileHnd,255);
  inc(CurrentFileLin);
  WriteFilePosition;
  Exit;
  end;
GetNextChar:=xtRead(CurrentFileHnd,1);
End;

Function GetNextWord:String;

Function GetNum:string;
Label f1,Vege;
Var
  a,b:String;
  i,o,p:LongInt;
Begin;
b:='';GetNum:=b;
f1:
a:=GetNextChar(true);
if (a='') then SignError('unexpected end of file');
if (a[1] in ['0'..'9','a'..'z','A'..'Z']) then begin; b:=b+a;goto f1; end;
if (b='') and (a='$') then begin; b:=a;goto f1; end;
UnNeededChar:=a+UnNeededChar;
if (b='') then SignError('error in integer constant');
b:=nagy(b);
if (copy(b,1,2)='0X') then b:='$'+copy(b,3,255);
if (copy(b,length(b),1)='H') then b:='$'+copy(b,1,length(b)-1);
if (b[1] in ['0'..'9']) and (copy(b,length(b),1)='B') then begin;
  b:=copy(b,1,length(b)-1);
  while (length(b)<8*4) do b:='0'+b;
  if (length(b)>8*4) then SignError('error in integer constant');
  p:=0;
  while (b<>'') do begin;
    a:=copy(b,1,8);
    delete(b,1,8);
    o:=BinType2Byte(a);
    if (Byte2BinType(o)<>a) then SignError('error in integer constant');
    p:=(p shl 8) or o;
    end;
  Goto Vege;
  end;
if (copy(b,1,1)<>'$') then begin;
  while (copy(b,1,1)='0') do delete(b,1,1);
  if (b='') then b:='0';
  p:=BVal(b);
  if (BStr(p)<>b) then SignError('error in integer constant');
  Goto Vege;
  end;
delete(b,1,1);
while (length(b)<2*4) do b:='0'+b;
if (length(b)>2*4) then SignError('error in integer constant');
p:=0;
while (b<>'') do begin;
  a:=copy(b,1,2);
  delete(b,1,2);
  o:=HexType2Byte(a);
  if (Byte2HexType(o)<>a) then SignError('error in integer constant');
  p:=(p shl 8) or o;
  end;
vege:
GetNum:=bstr(p);
End;

Function readSpecialString:String;
Var
  a,b:String;
  i:Word;
Begin;
b:='';
for i:=1 to 4 do begin;
  a:=GetNextChar(false);
  b:=b+a;
  end;
a:=kicsi(b);
b:='';
if (a='date') then b:=GetFullDateString;
if (a='time') then b:=GetFullTimeString;
if (a='ver1') then b:=Copy(PrgTxt,1,pos(',',PrgTxt)-1);
if (a='ver2') then b:=PrgTxt;
readSpecialString:=b;
End;

Label f1,f2,f3,f4,f5,f6,f7,vege;
Var
  a,b:String;
Begin;
GetNextWord:='';
if (UnNeededWord<>'') then begin;
  move(UnNeededWord[1],a,sizeof(a));
  GetNextWord:=a;
  delete(UnNeededWord,1,length(a)+1);
  goto vege;
  end;
b:='';
f1:
a:=GetNextChar(true);
if (a='') then goto vege;
if (UnPlugString(a)=' ') then goto f1;
if (a='{') then begin; b:='{';goto f2; end;
if (a[1] in ['0'..'9','$']) then begin;
  UnNeededChar:=a+UnNeededChar;
  GetNextWord:='$'+GetNum;
  goto vege;
  end;
if (a[1] in ['a'..'z','A'..'Z']) then begin; b:=a;goto f3; end;
if (a='(') then begin;
  a:=GetNextChar(false);
  if (a='*') then begin; b:='{';goto f4; end;
  UnNeededChar:=a+UnNeededChar;
  a:='(';
  end;
if (a[1] in ['''','#']) then begin; b:='#';goto f5; end;
GetNextWord:=a;
goto vege;
f2:
a:=GetNextChar(true);
if (a='') then SignError('unexpected end of file');
if (a<>'}') then begin; b:=b+a;goto f2; end;
GetNextWord:=b;
goto vege;
f3:
a:=GetNextChar(true);
if (a='') then SignError('unexpected end of file');
if (a[1] in ['a'..'z','A'..'Z','_','0'..'9']) then begin; b:=b+a;goto f3; end;
UnNeededChar:=a+UnNeededChar;
GetNextWord:=b;
goto vege;
f4:
a:=GetNextChar(true);
if (a='') then SignError('unexpected end of file');
b:=b+a;
if (length(b)>250) then delete(b,120,1);
if (copy(b,length(b)-1,2)<>'*)') then goto f4;
GetNextWord:=copy(b,1,length(b)-2);
goto vege;
f5:
if (a='#') then begin;
  a:=GetNextChar(false);
  if (a='%') then begin;
    a:=readSpecialString;
    if (a='') then SignError('error in char expression');
    b:=b+a;
    a:=GetNextChar(true);
    goto f7;
    end;
  UnNeededChar:=a+UnNeededChar;
  a:=GetNum;
  b:=b+chr(bval(a));
  a:=GetNextChar(true);
  goto f7;
  end;
f6:
a:=GetNextChar(false);
if (a='') then SignError('string constant exceeds line');
if (a<>'''') then begin; b:=b+a;goto f6; end;
a:=GetNextChar(true);
if (a='''') then begin; b:=b+a;goto f6; end;
f7:
if (a='') then SignError('unexpected end of file');
if (a[1] in ['''','#']) then goto f5;
if (UnPlugString(a)=' ') then begin; a:=GetNextChar(true);goto f7; end;
UnNeededChar:=a+UnNeededChar;
GetNextWord:=b;
goto vege;

vege:
Exit;
End;

Procedure PutBackWord(a:String);
Begin;
if (a='') then Exit;
a:=a[0]+a;
UnNeededWord:=a+UnNeededWord;
if (length(UnNeededWord)>250) then SignError('out of memory');
End;
