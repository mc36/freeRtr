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
    Num:Byte;{1=a 2=b 3=ccr 4=x 5=y}
    end;
Type
  OneMemPointRec=record
    Typ:Byte;
    Reg:LongInt;        {x=1, y=2}
    Num:LongInt;        {1}
    Lab:String;         {2}
    end;
Type
  OneParameterRec=record
    Typ:Byte;
    Str:String[128];
    Reg:OneRegType;          {1}
    Num:String[128];         {2}
    Mem:OneMemPointRec;      {3}
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
if (p.Typ=2) then begin;
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



Function ConvertMemoryDatas(p:OneMemPointRec;len:Byte):String;
Var
  a:String;
Begin;
a:='';
if (p.Typ=1) then begin;
  {Num}
  a:=Right(ZeroFill+w2h(p.Num),len*2);
  a:=Hex2ProciDat(a,Len);
  end;
if (p.Typ=2) then begin;
  {Label}
  a:=p.Lab;
  a:='msb'+SpxCh+DataSizeNames[Len]+SpxCh+a;
  end;
ConvertMemoryDatas:=a;
End;


Procedure AnalizeRegs(a:String;var r:OneRegType);
Var i:LongInt;
Begin;
i:=0;
if (a='a') then i:=1;
if (a='b') then i:=2;
if (a='ccr') then i:=3;
if (a='x') then i:=4;
if (a='y') then i:=5;
if (i<>0) then begin;
  r.Num:=i;
  exit;
  end;
r.Num:=0;
End;


Procedure AnalizeMemPoint(s:String;var m:OneMemPointRec);
Var
  a,b,c:String;
  i,o:LongInt;
begin;
m.Typ:=0;
m.Reg:=0;
m.Num:=0;
m.Lab:='';
if (copy(s,1,1)<>'(') then exit;
if (copy(s,length(s),1)<>')') then exit;
s:=Copy(s,2,Length(s)-2);

Kicserel(',','+',s);
s:='+'+s;
Kicserel(' +','+',s);
Kicserel('+ ','+',s);
Kicserel(' -','-',s);
Kicserel('- ','-',s);

repeat
  b:=s;
  Kicserel('-+','-',s);
  Kicserel('+-','-',s);
  Kicserel('--','+',s);
  Kicserel('++','+',s);
  until (s=b);

Kicserel('+',#13,s);Kicserel(#13,' + ',s);
Kicserel('-',#13,s);Kicserel(#13,' - ',s);

o:=0;
s:='  '+s+'  ';
i:=Kicserel('+ x ','',s);
if (i<>0) then o:=o or 1;
s:='  '+s+'  ';
i:=Kicserel('+ y ','',s);
if (i<>0) then o:=o or 2;
if (o=3) then exit;
m.reg:=o;

s:=xLevesz(s);
Kicserel(' +','+',s);
Kicserel('+ ','+',s);
Kicserel(' -','-',s);
Kicserel('- ','-',s);

if (s='') then begin;
  if (m.reg=0) then exit;
  m.Typ:=1;
  exit;
  end;

c:=Copy(s,1,1);
Delete(s,1,1);
if (c='-') or (c='+') then else Exit;

a:=AnalizeNumbers(s);
if (a<>'') then begin;
  b:=Right(a,8);
  a:=Copy(b,1,4);
  i:=h2w(a);
  i:=i shl 16;
  i:=i+h2w(Copy(b,5,4));
  if (c='-') then i:=-i;
  m.Num:=i;
  m.Typ:=1;
  Exit;
  end;

if (c<>'+') then Exit;
a:=AnalizeLabelName(s);
if (a<>'') then begin;
  m.Lab:=a;
  m.Typ:=2;
  Exit;
  end;

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
a:=AnalizeNumbers(s);
if (a<>'') then     begin; d.Typ:=2;d.Num:=a;Exit; end;
AnalizeMemPoint(s,m);
if (m.Typ<>0) then begin; d.Typ:=3;d.Mem:=m;Exit; end;
a:=AnalizeLabelName(s);
if (a<>'') then     begin; d.Typ:=4;d.Lab:=a;Exit; end;
End;
