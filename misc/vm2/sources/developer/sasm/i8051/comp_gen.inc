Const
  InternalNumber=#13;
  SepCh='\';
  SpxCh=':';
  ZeroFill='00000000000000000000000000000000000000000';
Const
  DataSizeNames:Array[1..4] of String[6]=('byte','word','','dword');

Var
  InStr:String;
  OutStr:String;
  OutOk:Boolean;


Type
  OneRegType=record
    Num:Byte;{1..8=r0..r7, 9=a}
    end;
Type
  OneMemPointRec=record
    Typ:Byte;
    Reg:LongInt;        {1; 1=r0, 2=r1}
    Num:LongInt;        {2}
    Lab:String;         {3}
    end;
Type
  OneParameterRec=record
    Typ:Byte;
    Str:String[128];
    Reg:OneRegType;          {1}
    Mem:OneMemPointRec;      {2}
    Num:String[128];         {3}
    Lab:String[128];         {4}
    end;

Procedure clreol;
begin;
write(#13'                                                                '#13);
end;

Function b2h(b:Byte):String;
Begin;
b2h:=byte2hextype(b);
End;

Function h2b(b:String):Byte;
Begin;
h2b:=hextype2byte(b);
End;

Function h2w(b:String):Word;
Begin;
h2w:=hextype2byte(Copy(b,1,2))*256+hextype2byte(Copy(b,3,2));
End;

Function w2h(w:Word):String;
Begin;
w2h:=byte2hextype(w div 256)+byte2hextype(w mod 256);
End;


Function RepairOneFileName(a,b:String):String;
Var i,o:Byte;
Begin;
o:=0;
for i:=1 to length(b) do if (b[i]='\') then o:=i;
b:=copy(b,1,o);
if (copy(a,2,1)<>':') and (copy(a,1,1)<>'\') then a:=b+a;
RepairOneFileName:=Kicsi(a);
End;


Function fPos(a,s:String):Byte;
Var
  i,o:LongInt;
Begin;
o:=0;
for i:=1 to Length(s) do if (Copy(s,i,Length(a))=a) then o:=i;
fPos:=o;
End;

Function xLevesz(s:String):String;
Begin;
s:=Levesz(s);
While (Copy(s,1,1)=' ') do Delete(s,1,1);
xLevesz:=s;
End;

Function BenneVan(a,b:String):Boolean;
Begin;
BenneVan:=(Pos('|'+b+'|',a)<>0);
End;


Function RepairOutData(s:String):String;
Begin;
if (Copy(s,1,1)<>SepCh) then s:=SepCh+s;
if (Copy(s,Length(s),1)<>SepCh) then s:=s+SepCh;
RepairOutData:=s;
End;

Function DecodeDataSizeName(s:String):Byte;
Var
  i:Byte;
Begin;
i:=0;
if (s='byte')  then i:=1;
if (s='word')  then i:=2;
if (s='dword') then i:=4;
if (s='fword') then i:=6;
if (s='qword') then i:=8;
if (s='tbyte') then i:=10;
DecodeDataSizeName:=i;
End;


Function Hex2ProciDat(s:String;c:Byte):String;
Var
  a,b:String;
  i,o:LongInt;
Begin;
b:='';
for o:=1 to c do begin;
  i:=Length(s)-2;
  a:=Copy(s,i+1,2);
  s:=Copy(s,1,i);
  b:=a+SepCh+b;
  end;
b:=copy(b,1,length(b)-1);
for i:=1 to Length(s) do if (Copy(s,1,1)<>Copy(s,i,1)) then b:='';
Hex2ProciDat:=b;
End;



Function AnalizeNumbers(s:String):String;
Label Vege;
Var
  i,o,p:LongInt;
  ob:Array[1..4] of Byte absolute o;
  a,b:String;
  c:char;
  Typ:Byte;
Begin;
AnalizeNumbers:='';
if (s[1]='#') then s:=copy(s,2,255);
if not (s[1] in ['-','+','0'..'9']) then Exit;
if (s[Length(s)] in ['0'..'9']) then s:=s+'d';
While (Copy(s,1,1)='0') and (Length(s)>2) do Delete(s,1,1);
if (Length(s)=0) then Exit;
Case s[Length(s)] of
  'b':Typ:=1;
  'd':Typ:=2;
  'h':Typ:=3;
  InternalNumber:Typ:=4;
  else Exit;
  end;
s:=Nagy(Copy(s,1,Length(s)-1));
if (Typ=4) then begin; b:=s;Goto Vege; end;

if (Typ=1) then begin;
  b:='';
  While (s<>'') do begin;
    a:=Right(ZeroFill+s,8);
    s:=Copy(s,1,Length(s)-8);
    i:=bintype2byte(a);
    if (byte2bintype(i)<>a) then Exit;
    b:=b2h(i)+b;
    end;
  s:=b;
  Typ:=3;
  end;
if (Typ=2) then begin;
  o:=BVal(s);
  if (BStr(o)<>s) then Exit;
  b:='';
  While (o<>0) do begin;
    b:=b2h(ob[1])+b;
    o:=o shr 8;
    end;
  s:=b;
  Typ:=3;
  end;
if (Typ=3) then begin;
  if (Length(s)>8) then Exit;
  s:=Right(ZeroFill+s,8);
  b:=s;
  for i:=4 downto 1 do begin;
    a:=Copy(s,1,2);Delete(s,1,2);
    p:=h2b(a);
    if (b2h(p)<>a) then Exit;
    ob[i]:=p;
    end;
  end;
if (s<>'') then Exit;

Vege:
AnalizeNumbers:=b;
End;


Function AnalizeLabelName(s:String):String;
Var
  i:LongInt;
  a:String;
  Typ:Byte;
Begin;
AnalizeLabelName:='';
if (copy(s,1,1)='#') then begin;
  s:=copy(s,2,255);
  end;
Typ:=0;
a:='offset ';
if (Copy(s,1,Length(a))=a) then begin;
  delete(s,1,Length(a));
  Typ:=1;
  end;
a:='segment ';
if (Copy(s,1,Length(a))=a) then begin;
  delete(s,1,Length(a));
  Typ:=2;
  end;
a:='';
for i:=1 to Length(s) do begin;
  if (s[i] in ['_','a'..'z','0'..'9']) then a:=a+s[i];
  end;
if (a[1] in ['0'..'9']) then a:=#13;
if (Length(a)>80) then a:=#13;
if (a=s) then begin;
  a:='';
  if (Typ=1) then a:='ofs'+SpxCh;
  if (Typ=2) then a:='seg'+SpxCh;
  AnalizeLabelName:=a+s;
  end;
End;



Function ConvertNumOrLabDatas(p:OneParameterRec;len:Byte):String;
Var
  a:String;
Begin;
a:='';
if (p.Typ=3) then begin;
  {Num}
  a:=Right(ZeroFill+p.Num,len*2);
  a:=Hex2ProciDat(a,Len);
  end;
if (p.Typ=4) then begin;
  {Label}
  a:=p.Lab;
  a:='msb'+SpxCh+DataSizeNames[Len]+SpxCh+a;
  end;
ConvertNumOrLabDatas:=a;
End;




Procedure AnalizeRegs(a:String;var r:OneRegType);
Var i:LongInt;
Begin;
r.Num:=0;
i:=0;
if (a='a') then i:=9;
if (copy(a,1,1)='r') then begin;
  a:=copy(a,2,666);
  i:=bval(a);
  if (bstr(i)<>a) then exit;
  if (i<0) or (i>7) then exit;
  inc(i);
  end;
if (i<>0) then r.Num:=i;
End;



Function AnalizeCondition(s:String):LongInt;
{0=none, 1=nz, 2=z, 3=nc, 4=c, 5=po, 6=pe, 7=p, 8=m}
Var i:LongInt;
Begin;
i:=0;
if (s='nz') then i:=1;
if (s='z') then i:=2;
if (s='nc') then i:=3;
if (s='c') then i:=4;
AnalizeCondition:=i;
End;


Function ConvertMemoryDatas(p:OneMemPointRec;len:Byte):String;
Var
  a:String;
Begin;
a:='';
if (p.Typ=2) then begin;
  {Num}
  a:=Right(ZeroFill+w2h(p.Num),len*2);
  a:=Hex2ProciDat(a,Len);
  end;
if (p.Typ=3) then begin;
  {Label}
  a:=p.Lab;
  a:='msb'+SpxCh+DataSizeNames[Len]+SpxCh+a;
  end;
ConvertMemoryDatas:=a;
End;

Procedure AnalizeMemPoint(s:String;var m:OneMemPointRec);
Var
  a:String;
  i,o:LongInt;
begin;
m.Typ:=0;
m.Reg:=0;
m.Num:=0;
m.Lab:='';

if (copy(s,1,2)='@r') then begin;
  a:=copy(s,3,666);
  i:=BVal(a);
  if (BStr(i)<>a) then exit;
  if (i<0) or (i>1) then exit;
  m.Reg:=i+1;
  m.Typ:=1;
  Exit;
  end;

if (copy(s,1,1)<>'(') then exit;
if (copy(s,length(s),1)<>')') then exit;
s:=Copy(s,2,Length(s)-2);

a:=AnalizeNumbers(s);
if (a<>'') then begin;
  m.Num:=BVal('$'+a);
  m.Typ:=2;
  Exit;
  end;

a:=AnalizeLabelName(s);
if (a<>'') then begin;
  m.Lab:=a;
  m.Typ:=3;
  Exit;
  end;

End;



Function convertRelAddr(var p:OneParameterRec;siz:LongInt):String;
Var a:String;
Begin;
convertRelAddr:='';
case p.Typ of
  3:a:='numofs'+SpxCh+p.Num;
  4:a:='ofs?'+SpxCh+p.Lab;
  else exit;
  end;
a:='rela2end'+SpxCh+a;
if (siz and $10=0) then a:='signed'+SpxCh+a;
if (siz and $20=0) then a:=DataSizeNames[siz and $7]+SpxCh+a;
if (siz and $40=0) then a:=SepCh+a;
convertRelAddr:=a;
End;



Procedure AnalizeParameter(s:String;mode:Byte;Var d:OneParameterRec);
Var
  r:OneRegType;
  m:OneMemPointRec;
  a:String;
  i:LongInt;
Begin;
d.Typ:=0;
d.Str:=s;
AnalizeRegs(s,r);
if (r.num<>0) then begin; d.Typ:=1;d.Reg:=r;Exit; end;
AnalizeMemPoint(s,m);
if (m.Typ<>0) then begin; d.Typ:=2;d.Mem:=m;Exit; end;
a:=AnalizeNumbers(s);
if (a<>'') then    begin; d.Typ:=3;d.Num:=a;Exit; end;
a:=AnalizeLabelName(s);
if (a<>'') then    begin; d.Typ:=4;d.Lab:=a;Exit; end;
End;
