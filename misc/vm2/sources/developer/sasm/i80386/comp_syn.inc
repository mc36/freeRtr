Procedure CompileOneSyntaxLine;

Procedure AddPureTextToLine(s:String);
Var
  i:Byte;
Begin;
for i:=1 to Length(s) do OutStr:=OutStr+SepCh+b2h(ord(s[i]));
End;

Function GetFullDateString:String;
Var
  w1,w2,w3:Word;
Begin;
xGetDate(w1,w2,w3);
GetFullDateString:=Right(ZeroFill+BStr(w1),4)+
              +'-'+Right(ZeroFill+BStr(w2),2)+
              +'-'+Right(ZeroFill+BStr(w3),2);
End;

Function GetFullTimeString:String;
Var
  w1,w2,w3:Word;
Begin;
xGetTime(w1,w2,w3);
GetFullTimeString:=Right(ZeroFill+BStr(w1),2)+
              +':'+Right(ZeroFill+BStr(w2),2)+
              +':'+Right(ZeroFill+BStr(w3),2);
End;

Function CompileOneDefinieAnyStuff(Str:String;Siz:Byte):Boolean;
Label Folyt1,Folyt2;
Var
  a,b:String;
  i,o:LongInt;
  CurrDupNum:LongInt; {-x=flush, 0=none, +x=active}
Begin;
CompileOneDefinieAnyStuff:=True;
CurrDupNum:=0;
Folyt1:
i:=Pos(',',Str);
a:=Copy(Str,1,i-1);
Delete(Str,1,i);
a:=xLevesz(a);
if (a='') then Exit;
i:=Pos(' dup ',a);
if (i<>0) then begin;
  if (CurrDupNum<>0) then Exit;
  Str:=a+','+Str;
  a:=Copy(Str,1,i-1);
  delete(Str,1,i+5);
  a:=AnalizeNumbers(a);
  if (a='') then Exit;
  o:=4;
  if (Copy(a,1,o)<>Copy(ZeroFill,1,o)) then Exit;
  delete(a,1,o);
  o:=h2w(a);if (w2h(o)<>a) then Exit;
  if (o<1) or (o>32768) then Exit;
  CurrDupNum:=o;
  i:=Pos(')',Str);
  if (Copy(Str,i-2,2)=InternalNumber+',') then Delete(Str,i-1,1);
  OutStr:=OutStr+SepCh+'dup'+SpxCh+'beg';
  Goto Folyt2;
  end;
if (Right(a,1)=')') then begin;
  if (CurrDupNum<=0) then Exit;
  a:=Copy(a,1,Length(a)-1);
  CurrDupNum:=-CurrDupNum;
  end;
if (Right(a,1)=InternalNumber) then begin;
  a:=Copy(a,1,Length(a)-1);
  while (a<>'') do begin;
    b:=Copy(a,1,2);
    Delete(a,1,2);
    b:=AnalizeNumbers('0'+b+'h');
    if (b='') then Exit;
    b:=Hex2ProciDat(b,Siz);
    if (b='') then Exit;
    OutStr:=OutStr+SepCh+b;
    end;
  Goto Folyt2;
  end;
b:=AnalizeNumbers(a);
if (b<>'') then begin;
  b:=Hex2ProciDat(b,Siz);
  if (b='') then Exit;
  OutStr:=OutStr+SepCh+b;
  Goto Folyt2;
  end;
b:='offset ';
if (Copy(a,1,Length(b))=b) then begin;
  Delete(a,1,Length(b));
  b:=AnalizeLabelName(xLevesz(a));
  if (b='') then Exit;
  a:=DataSizeNames[Siz]+SpxCh+'ofs'+SpxCh+a;
  OutStr:=OutStr+SepCh+a;
  Goto Folyt2;
  end;
b:='segment ';
if (Copy(a,1,Length(b))=b) then begin;
  Delete(a,1,Length(b));
  b:=AnalizeLabelName(xLevesz(a));
  if (b='') then Exit;
  a:=DataSizeNames[Siz]+SpxCh+'seg'+SpxCh+a;
  OutStr:=OutStr+SepCh+a;
  Goto Folyt2;
  end;
b:=AnalizeLabelName(a);
if (b<>'') then begin;
  a:=DataSizeNames[Siz]+SpxCh+a;
  OutStr:=OutStr+SepCh+a;
  Goto Folyt2;
  end;
if (a='?') then begin;
  a:=DataSizeNames[Siz]+SpxCh+a;
  OutStr:=OutStr+SepCh+a;
  Goto Folyt2;
  end;
if (a='%date') then begin;
  AddPureTextToLine(GetFullDateString);
  Goto Folyt2;
  end;
if (a='%time') then begin;
  AddPureTextToLine(GetFullTimeString);
  Goto Folyt2;
  end;
if (a='%ver1') then begin;
  AddPureTextToLine(Copy(PrgTxt,1,pos(',',PrgTxt)-1));
  Goto Folyt2;
  end;
if (a='%ver2') then begin;
  AddPureTextToLine(PrgTxt);
  Goto Folyt2;
  end;

Exit;
Folyt2:
if (CurrDupNum<0) then begin;
  OutStr:=OutStr+SepCh+'dup'+SpxCh+'end'+SpxCh+BStr(-CurrDupNum);
  CurrDupNum:=0;
  end;
if (Length(OutStr)>252) then Exit;
if (Str<>'') then Goto Folyt1;
CompileOneDefinieAnyStuff:=False;
End;


Label Vege;
Const
  Str2_Data='|db|dt|dw|dd|';
  Str2_Const='|equ|';
Const
  pn=5;
Var
  pd:array[1..pn] of string[64];
  a,b,c,d:String;
  i,o:LongInt;
Begin;
if (OutOk=True) then Exit;
OutStr:='';
b:=InStr;
b:=xLevesz(b)+' ';

for o:=1 to pn do begin;
  i:=Pos(' ',b);
  pd[o]:=Copy(b,1,i-1);
  delete(b,1,i);
  end;

if BenneVan(Str2_Data,pd[2]) then begin;
  a:=pd[1];
  a:=AnalizeLabelName(a);
  if (a='') then Exit;
  OutStr:='label'+SpxCh+a;
  for i:=2 to pn do pd[i-1]:=pd[i];
  pd[pn]:=''
  end;
if BenneVan(Str2_Data,pd[1]) then begin;
  b:=InStr;c:=Kicsi(pd[1]);
  repeat
    i:=Pos(' ',b);
    a:=Kicsi(Copy(b,1,i-1));
    delete(b,1,i);
    until (a=c);
  i:=-1;
  if (c='db') then i:=1;
  if (c='dt') then i:=1;
  if (c='dw') then i:=2;
  if (c='dd') then i:=4;
  if (i=-1) then Exit;
  if CompileOneDefinieAnyStuff(b+',',i) then Exit;
  Goto Vege;
  end;
if BenneVan(Str2_Const,pd[2]) then begin;
  if (pd[4]<>'') then Exit;
  a:=pd[1];
  a:=AnalizeLabelName(a);
  if (a='') then Exit;
  OutStr:='const'+SpxCh+a+SpxCh;
  a:=pd[3];
  a:=AnalizeNumbers(a);
  if (a='') then Exit;
  While (Copy(a,1,1)='0') do Delete(a,1,1);
  if (a='') then a:='0';
  OutStr:=OutStr+a;
  Goto Vege;
  end;

if (Right(pd[1],1)=':') and (pd[2]+pd[3]='') then begin;
  a:=pd[1];
  if (Copy(a,Length(a),1)<>':') then Exit;
  a:=Copy(a,1,Length(a)-1);
  a:=AnalizeLabelName(a);
  if (a='') then Exit;
  OutStr:='label'+SpxCh+a;
  Goto Vege;
  end;

if (pd[1]='proc') and (pd[2]<>'') and (pd[3]+pd[4]='') then begin;
  a:=pd[2];
  a:=AnalizeLabelName(a);
  if (a='') then Exit;
  OutStr:='label'+SpxCh+a;
  Goto Vege;
  end;
if (pd[1]='endp') and (pd[2]+pd[3]='') then begin;
  Goto Vege;
  end;

if (pd[1]='segment') and (pd[2]<>'') and (pd[3]+pd[4]='') then begin;
  a:=pd[2];
  a:=AnalizeLabelName(a);
  if (a='') then Exit;
  OutStr:='segment'+SpxCh+a;
  Goto Vege;
  end;
if (pd[1]='ends') and (pd[2]+pd[3]='') then begin;
  Goto Vege;
  end;

if (pd[1]='use32') and (pd[2]+pd[3]='') then begin;
  CurrentCodeStyle:=1;
  Goto Vege;
  end;
if (pd[1]='use16') and (pd[2]+pd[3]='') then begin;
  CurrentCodeStyle:=0;
  Goto Vege;
  end;
if (pd[1]='align') and (pd[3]+pd[4]='') then begin;
  a:=pd[2];
  a:=AnalizeNumbers(a);
  if (a='') then Exit;
  i:=4;
  if (Copy(a,1,i)<>Copy(ZeroFill,1,i)) then Exit;
  Delete(a,1,i);
  OutStr:='align'+SpxCh+a;
  Goto Vege;
  end;



if (pd[1]='incbin') and (pd[2]<>'') and (pd[3]+pd[4]='') then begin;
  a:=pd[2];
  Kicserel('\','/',a);
  OutStr:='incbin'+SpxCh+a;
  Goto Vege;
  end;

if (pd[1]='org') and (pd[3]+pd[4]='') then begin;
  a:=pd[2];
  a:=AnalizeNumbers(a);
  if (a='') then Exit;
  OutStr:='org'+SpxCh+a;
  Goto Vege;
  end;

Exit;

Vege:
OutOk:=True;
End;
